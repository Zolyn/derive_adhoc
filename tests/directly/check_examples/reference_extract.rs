//! Extract examples from the reference manual

#![allow(dead_code)] // XXXX

use crate::*;
use crate::directly::*;

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::SeqCst;

type DocLoc = usize;

#[derive(Debug)]
enum InputItem {
    Bullet {
        loc: DocLoc,
        bullet: String,
    },
    BlockQuote {
        loc: DocLoc,
        options: String,
        content: String,
    },
    Heading {
        loc: DocLoc,
        /// number of hashes
        depth: usize,
        text: String,
    },
    Paragraph {
        loc: DocLoc,
        content: String,
    },
    Directive {
        used: DirectiveTrackUsed,
        loc: DocLoc,
        d: InputDirective,
    },
}

#[derive(Debug)]
enum InputDirective {
    For {
        for_: String,
    },
    Structs {
    },
    ForToplevelsConcat {
        toplevels: Vec<String>,
    },
}    

type Preprocessed = Vec<InputItem>;

use InputItem as II;
use InputDirective as ID;

const INPUT_FILE: &str = "doc/reference.md";

fn wrong(loc: DocLoc, msg: impl Display) -> ! {
    eprintln!("{INPUT_FILE}:{loc}: {msg}");
    panic!("unexpected content in documentation");
}

#[derive(Default, Debug)]
struct DirectiveTrackUsed(AtomicBool);

impl DirectiveTrackUsed {
    fn note(&self) {
        self.0.store(true, SeqCst);
    }

    fn is(&self) -> bool {
        self.0.load(SeqCst)
    }
}

fn read_preprocess() -> Preprocessed {
    let _ = dbg!(std::env::current_dir());

    let file = File::open(format!("../{INPUT_FILE}")).unwrap();

    let is_directive = |l: &str| m!(l, r"\s*<!--##examples\b");

    let mut lines = BufReader::new(file)
        .lines()
        .map(|l| l.expect("file read error"))
        .enumerate()
        .map(|(loc, l)| (loc+1, format!("{}\n", l.trim_end())))
        .peekable();

    let mut current_paragraph = None;

    let mut preprocessed = vec![];

    macro_rules! end_paragraph { {} => {
        if let Some((loc, content)) = current_paragraph.take() {
            preprocessed.push(II::Paragraph { loc, content });
        }
    } }

    while let Some((loc, l)) = lines.next() {
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
            Some(II::BlockQuote { loc, options, content })
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
            Some(II::Bullet { loc, bullet })
        } else if let Some((hashes, text)) = mc!(l, r"^(\#+) (\S.*)") {
            Some(II::Heading {
                loc,
                depth: hashes.len(),
                text: text.trim_end().to_owned(),
            })
        } else if is_directive(&l) {
            let l = l.trim().strip_prefix("<!--##examples").unwrap();
            let l = l
                .strip_suffix("##-->")
                .unwrap_or_else(|| wrong(loc, "directive suffix wrong"));
            let used = Default::default();

            let d = if m!(l, "^-ignore$") {
                while let Some((loc, l)) = lines.next() {
                    if m!(l, "^\n$") {
                        break
                    } else if is_directive(&l) {
                        wrong(loc, "directive in ignore section");
                    } else {
                        drop(l);
                    }
                }
                None
            } else if let Some((for_,)) = mc!(l, r"^-for (\S.*)$") {
                let for_ = for_.trim_end().to_owned();
                Some(ID::For { for_ })
            } else if let Some((toplevels,)) = mc!(
                l,
                r"^-for-toplevels-concat (\S.*)$",
            ) {
                let toplevels = toplevels
                    .split_ascii_whitespace()
                    .map(|s| s.to_owned())
                    .collect_vec();
                Some(ID::ForToplevelsConcat { toplevels })
            } else if m!(l, "^-structs") {
                Some(ID::Structs { })
            } else {
                wrong(loc, format_args!("unrecgonised directive: {l:?}"))
            };
            d.map(|d| II::Directive {
                used,
                loc,
                d
            })
        } else if m!(l, "^\n$") {
            None
        } else {
            current_paragraph
                .get_or_insert((loc, String::new()))
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
    fn parse_content(loc: DocLoc, content: &str) -> Vec<syn::DeriveInput> {
        let content = re!(r"(?m)^#(?: |$)").replace_all(content, "");
        let items: Concatenated<syn::Item> = syn::parse_str(&content)
            .unwrap_or_else(|e| wrong(loc, format_args!("structs content parse failed: {e}, given {content}",)));
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
            II::Directive {
                loc,
                used,
                d: ID::Structs { }
            } => {
                used.note();
                Some((i, loc))
            },
            _ => None
        })
        .map(|(i, loc)| {
            match input.get(i+1) {
                Some(II::BlockQuote { loc, content, .. }) => {
                    parse_content(*loc, content).into_iter()
                }
                _ => wrong(
                    *loc,
                    "structs directive not followed by blockquote",
                ),
            }

        })
        .flatten()
        .collect()
}

#[test]
#[should_panic] // XXXX incomplete
fn extract() {
    let iis = read_preprocess();

    for ii in &iis {
        println!("{:?}", ii);
    }
    let structs_ = extract_structs(&iis);
    println!("{:?}", structs_.iter().map(|s| &s.ident).collect_vec());

    for ii in &iis {
        match ii {
            II::Directive { used, loc, .. } if !used.is() => {
                wrong(*loc, "unused directive - out of place");
            }
            _ => {}
        }
    }
}
