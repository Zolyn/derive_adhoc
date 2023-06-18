//! Extract examples from the reference manual

#![allow(dead_code)] // XXXX

use crate::*;
use crate::directly::*;

type LineNo = usize;

#[derive(Debug)]
enum InputItem {
    Bullet {
        lno: LineNo,
        bullet: String,
    },
    BlockQuote {
        lno: LineNo,
        options: String,
        content: String,
    },
    Heading {
        lno: LineNo,
        /// number of hashes
        depth: usize,
        text: String,
    },
    Paragraph {
        lno: LineNo,
        content: String,
    },
    ForDirective {
        lno: LineNo,
        for_: String,
    },
    StructsDirective {
        lno: LineNo,
    },
    ForToplevelsConcatDirective {
        lno: LineNo,
        toplevels: Vec<String>,
    },
}

type Preprocessed = Vec<InputItem>;

use InputItem::*;

const INPUT_FILE: &str = "doc/reference.md";

fn wrong(lno: LineNo, msg: impl Display) -> ! {
    eprintln!("{INPUT_FILE}:{lno}: {msg}");
    panic!("unexpected content in documentation");
}

fn read_preprocess() -> Preprocessed {
    let _ = dbg!(std::env::current_dir());

    let file = File::open(format!("../{INPUT_FILE}")).unwrap();

    let is_directive = |l: &str| m!(l, r"\s*<!--##examples\b");

    let mut lines = BufReader::new(file)
        .lines()
        .map(|l| l.expect("file read error").to_owned())
        .enumerate()
        .map(|(lno, l)| (lno+1, l))
        .peekable();

    let mut current_paragraph = None;

    let mut preprocessed = vec![];

    macro_rules! end_paragraph { {} => {
        if let Some((lno, content)) = current_paragraph.take() {
            preprocessed.push(Paragraph { lno, content });
        }
    } }

    while let Some((lno, l)) = lines.next() {
        let ii = if let Some((options,)) = mc!(l, "```(.*)") {
            let options = options.trim().to_owned();
            let mut content = String::new();
            loop {
                let (_, l) = lines.next().expect("EOF in blockquote");
                if m!(l, "```") {
                    drop(l);
                    break;
                }
                content += &l;
            }
            Some(BlockQuote { lno, options, content })
        } else if m!(l, r"^ \* ") {
            let mut bullet: String = l;
            loop {
                let (_, l) = match lines.peek() {
                    Some(l) => l,
                    None => break,
                };
                if !m!(l, r"^   +\S") {
                    break
                }
                bullet += &lines.next().unwrap().1;
            }
            Some(Bullet { lno, bullet })
        } else if let Some((hashes, text)) = mc!(l, r"^(\#+) (\S.*)") {
            Some(Heading {
                lno,
                depth: hashes.len(),
                text: text.trim_end().to_owned(),
            })
        } else if is_directive(&l) {
            let l = l.trim().strip_prefix("<!--##examples").unwrap();
            let l = l
                .strip_suffix("##-->")
                .unwrap_or_else(|| wrong(lno, "directive suffix wrong"));

            if m!(l, "^-ignore$") {
                while let Some((lno, l)) = lines.next() {
                    if m!(l, "^$") {
                        break
                    } else if is_directive(&l) {
                        wrong(lno, "directive in ignore section");
                    } else {
                        drop(l);
                    }
                }
                None
            } else if let Some((for_,)) = mc!(l, r"^-for (\S.*)$") {
                let for_ = for_.trim_end().to_owned();
                Some(ForDirective { lno, for_ })
            } else if let Some((toplevels,)) = mc!(
                l,
                r"^-for-toplevels-concat (\S.*)$",
            ) {
                let toplevels = toplevels
                    .split_ascii_whitespace()
                    .map(|s| s.to_owned())
                    .collect_vec();
                Some(ForToplevelsConcatDirective { lno, toplevels })
            } else if m!(l, "^-structs") {
                Some(StructsDirective { lno })
            } else {
                wrong(lno, format_args!("unrecgonised directive: {l:?}"))
            }
        } else if m!(l, "^$") {
            None
        } else {
            current_paragraph
                .get_or_insert((lno, String::new()))
                .1
                += &l;
            continue;
        };

        end_paragraph!();
        preprocessed.extend(ii);
    }

    end_paragraph!();
    preprocessed
}

fn extract_structs(input: &Preprocessed) -> Vec<syn::DeriveInput> {
    fn parse_content(lno: LineNo, content: &str) -> Vec<syn::DeriveInput> {
        let content = re!(r"^# ").replace_all(content, "");
        let items: Concatenated<syn::Item> = syn::parse_str(&content)
            .unwrap_or_else(|e| wrong(lno, format_args!("structs content parse failed: {e}, given {content}",)));
        items.0.into_iter().filter_map(|item| match item {
            syn::Item::Union(_) | syn::Item::Struct(_) | syn::Item::Enum(_) => {
                Some(
                    syn::parse2(item.into_token_stream()).expect("failed to reparse item as DeriveInput"))
            },
            _ => None,
        }).collect_vec()
    }
            
    input
        .iter()
        .enumerate()
        .filter_map(|(i, ii)| match ii {
            StructsDirective { lno } => Some((i, lno)),
            _ => None
        })
        .map(|(i, lno)| {
            match input.get(i+1) {
                Some(BlockQuote { lno, content, .. }) => {
                    parse_content(*lno, content).into_iter()
                }
                _ => wrong(
                    *lno,
                    "structs directive not followed by blockquote",
                ),
            }

        })
        .flatten()
        .collect()
}

#[test]
#[should_panic] // XXXX unfinished, bugs in docs and d-a, too
fn extract() {
    let iis = read_preprocess();

    for ii in &iis {
        println!("{:?}", ii);
    }
    let structs_ = extract_structs(&iis);
    println!("{:?}", structs_);
}
