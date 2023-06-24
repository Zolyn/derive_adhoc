//! Extract examples from the reference manual

use super::possibilities::PossibilitiesExample;
use super::*;

use std::cell::Cell;

#[derive(Debug)]
enum InputItem {
    Bullet {
        loc: DocLoc,
        bullet: String,
    },
    BlockQuote {
        loc: DocLoc,
        #[allow(dead_code)] // we ignore this for now
        options: String,
        content: String,
    },
    Heading {
        #[allow(dead_code)] // we don't syntax check our headings
        loc: DocLoc,
        /// number of hashes
        depth: usize,
        text: String,
    },
    #[allow(dead_code)] // paragraphs are just ignored
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
    Structs {},
    #[allow(dead_code)] // TODO EXTEST not yet implemented
    ForToplevelsConcat {
        toplevels: Vec<String>,
    },
    // TODO EXTEST need a way to test the tabular $vdefbody example
}

type Preprocessed = Vec<InputItem>;

use InputDirective as ID;
use InputItem as II;

#[derive(Default, Debug)]
struct DirectiveTrackUsed(Cell<bool>);

impl DirectiveTrackUsed {
    fn note(&self) {
        self.0.set(true);
    }

    fn is(&self) -> bool {
        self.0.get()
    }
}

fn read_preprocess(errs: &mut Errors) -> Preprocessed {
    let _ = dbg!(std::env::current_dir());

    let file = File::open(format!("../{INPUT_FILE}")).unwrap();

    let is_directive = |l: &str| m!(l, r"\s*<!--##examples\b");

    let mut lines = BufReader::new(file)
        .lines()
        .map(|l| l.expect("file read error"))
        .enumerate()
        .map(|(loc, l)| (loc + 1, format!("{}\n", l.trim_end())))
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
            Some(II::BlockQuote {
                loc,
                options,
                content,
            })
        } else if m!(l, r"^ \* ") {
            let mut bullet: String = l;
            loop {
                let (_, l) = match lines.peek() {
                    Some(l) => l,
                    None => break,
                };
                if !m!(l, r"^   +\S") {
                    break;
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
                .unwrap_or_else(|| bail(loc, "directive suffix wrong"));
            let used = Default::default();

            let d = if m!(l, "^-ignore$") {
                while let Some((loc, l)) = lines.next() {
                    if m!(l, "^\n$") {
                        break;
                    } else if is_directive(&l) {
                        errs.wrong(loc, "directive in ignore section");
                        break;
                    } else {
                        drop(l);
                    }
                }
                None
            } else if let Some((for_,)) = mc!(l, r"^-for (\S.*)$") {
                let for_ = for_.trim_end().to_owned();
                Some(ID::For { for_ })
            } else if let Some((toplevels,)) =
                mc!(l, r"^-for-toplevels-concat (\S.*)$",)
            {
                let toplevels = toplevels
                    .split_ascii_whitespace()
                    .map(|s| s.to_owned())
                    .collect_vec();
                Some(ID::ForToplevelsConcat { toplevels })
            } else if m!(l, "^-structs") {
                Some(ID::Structs {})
            } else {
                errs.wrong(loc, format_args!("unrecgonised directive: {l:?}"));
                continue;
            };
            d.map(|d| II::Directive { used, loc, d })
        } else if m!(l, "^\n$") {
            None
        } else {
            current_paragraph.get_or_insert((loc, String::new())).1 += &l;
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
            .unwrap_or_else(|e| {
                bail(
                    loc,
                    format_args!(
                        "structs content parse failed: {e}, given {content}",
                    ),
                )
            });
        items
            .0
            .into_iter()
            .filter_map(|item| match item {
                syn::Item::Union(_)
                | syn::Item::Struct(_)
                | syn::Item::Enum(_) => Some(
                    syn::parse2(item.into_token_stream())
                        .expect("failed to reparse item as DeriveInput"),
                ),
                _ => None,
            })
            .collect_vec()
    }

    input
        .iter()
        .enumerate()
        .filter_map(|(i, ii)| match ii {
            II::Directive {
                loc,
                used,
                d: ID::Structs {},
            } => {
                used.note();
                Some((i, loc))
            }
            _ => None,
        })
        .map(|(i, loc)| match input.get(i + 1) {
            Some(II::BlockQuote { loc, content, .. }) => {
                parse_content(*loc, content).into_iter()
            }
            _ => bail(*loc, "structs directive not followed by blockquote"),
        })
        .flatten()
        .collect()
}

#[derive(Default, Debug)]
struct SectionState<'i> {
    for_: Option<(DocLoc, &'i String, &'i DirectiveTrackUsed)>,
    t_limits: Vec<possibilities::Limit>,
}

fn parse_bullet(
    loc: DocLoc,
    bullet: &str,
    errs: &mut Errors,
    ss: &mut SectionState,
    examples_out: &mut Vec<Box<dyn Example>>,
) {
    let bullet = re!(r"\n   +").replace_all(bullet, " ");
    let (input, here_for, outputs) =
        match mc!(bullet, r#"(?m)^ \* `([^`]+)`(?: for ([^:]+))?: (.*)$"#) {
            Some(y) => y,
            None => {
                errs.wrong(
                    loc,
                    format_args!(
                        "failed to parse bullet point as example: {:?}",
                        bullet,
                    ),
                );
                return;
            }
        };

    let for_used_buf = Default::default();

    let for_ = ss.for_.or_else(|| {
        if here_for.is_empty() {
            None
        } else {
            Some((loc, &here_for, &for_used_buf))
        }
    });

    let mut all_must_match = false;
    let limit = match for_ {
        None => possibilities::Limit::True,
        Some((loc, for_, for_used)) => {
            use possibilities::Limit as L;
            for_used.note();
            if m!(for_, "^structs?$") {
                L::IsStruct
            } else if m!(for_, "^enum( variant)?s?$") {
                L::IsEnum
            } else if let Some((n,)) = mc!(for_, r"^`(\w+)`$") {
                L::Name(n.into())
            } else if let Some((f, n)) = mc!(for_, r"^`(\w+)` in `(\w+)`$") {
                all_must_match = true;
                L::Field { f, n }
            } else if m!(for_, "^others$") {
                all_must_match = true;
                L::Others(mem::take(&mut ss.t_limits))
            } else {
                errs.wrong(
                    loc,
                    format_args!(r#"unhandled for clause "{}""#, for_),
                );
                return;
            }
        }
    };
    ss.t_limits.push(limit.clone());

    let mut poss = |output: &str| {
        macro_rules! parse1 { { $v:ident } => {
            let $v: TokenStream = match syn::parse_str(&$v) {
                Ok(y) => y,
                Err(e) => {
                    errs.wrong(loc, format_args!(
                        r#"failed to parse {}: {:?}: {}"#,
                        stringify!($v),
                        $v,
                        e
                    ));
                    return;
                },
            };
        } }
        parse1!(input);
        parse1!(output);
        examples_out.push(Box::new(PossibilitiesExample {
            loc,
            input,
            limit: limit.clone(),
            all_must_match,
            output,
        }));
    };

    if m!(outputs, "^nothing$") {
        poss("");
    } else {
        let mut outputs = outputs;
        while !outputs.is_empty() {
            let (p, rest) = match mc!(outputs, "(?m)^`([^`]+)`(?:, (.*)|)$",) {
                Some(y) => y,
                None => {
                    errs.wrong(
                        loc,
                        format!(
                            r#"bad (tail of) bullet point examples "{}""#,
                            outputs,
                        ),
                    );
                    break;
                }
            };
            poss(&p);
            outputs = rest;
        }
    }
}

fn examples_section<'i>(
    input: impl Iterator<Item = &'i InputItem>,
    errs: &mut Errors,
    out: &mut Vec<Box<dyn Example>>,
) {
    let mut input = input.peekable();
    let mut ss = SectionState::default();

    while let Some(ii) = input.next() {
        match ii {
            II::Bullet { loc, bullet } => {
                parse_bullet(*loc, bullet, errs, &mut ss, out);
            }
            II::Directive { loc: d_loc, d, used } => match d {
                ID::For { for_: new } => ss.for_ = Some((*d_loc, new, used)),
                ID::ForToplevelsConcat { .. } => {
                    used.note();
                    if matches!(input.peek(), Some(II::Paragraph { .. })) {
                        _ = input.next();
                    }
                    eprintln!("FOR TOPLEVELS CONCAT NYI"); // TODO EXTEST
                }
                _ => {}
            },
            #[allow(unused_variables)] // TODO EXTEST
            II::BlockQuote { loc, options, content } => {
                // TODO EXTEST
            }
            II::Paragraph { loc, .. } => {
                errs.wrong(*loc, "unmarked text in examples section");
            }
            II::Heading { .. } => panic!("heading in subsection!"),
        }
    }
}

fn extract_examples(
    input: &Preprocessed,
    errs: &mut Errors,
) -> Vec<Box<dyn Example>> {
    let mut examples_out = vec![];

    for (ia, ib) in input
        .iter()
        .enumerate()
        .filter_map(|(i, ii)| match ii {
            II::Heading { depth, text, .. } if m!(text, r"^Examples?\b") => {
                Some((i, depth))
            }
            _ => None,
        })
        .map(|(ia, depth_a)| {
            let ib = input
                .iter()
                .enumerate()
                .skip(ia + 1)
                .find_map(|(ib, ii)| match ii {
                    II::Heading { depth: depth_b, .. }
                        if depth_b <= depth_a =>
                    {
                        Some(ib)
                    }
                    _ => None,
                })
                .unwrap_or(input.len());
            (ia, ib)
        })
    {
        let mut input = input[ia..ib].iter();
        loop {
            let is_heading = |ii: &InputItem| matches!(ii, II::Heading { .. });
            examples_section(
                input.take_while_ref(|ii| !is_heading(ii)),
                errs,
                &mut examples_out,
            );
            match input.next() {
                None => break,
                Some(ii) if is_heading(ii) => {}
                Some(ii) => panic!("shouldn't be {:?}", ii),
            }
        }
    }
    examples_out
}

pub fn extract(
    errs: &mut Errors,
) -> (Vec<syn::DeriveInput>, Vec<Box<dyn Example>>) {
    let iis = read_preprocess(errs);

    let structs = extract_structs(&iis);

    let examples = extract_examples(&iis, errs);

    for ii in &iis {
        match ii {
            II::Directive { used, loc, .. } if !used.is() => {
                errs.wrong(*loc, "unused directive - out of place");
            }
            _ => {}
        }
    }

    (structs, examples)
}
