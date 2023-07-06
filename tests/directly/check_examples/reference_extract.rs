//! Extract examples from the reference manual

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
    Paragraph {
        loc: DocLoc,
        #[allow(dead_code)] // paragraphs content is not currently used
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
    ForToplevelsConcat {
        toplevels: Vec<String>,
    },
    PossibilitiesBlockquote {
        heading_picture_loc: DocLoc,
        heading_picture: String,
    },
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
                let Some((_, l)) = lines.peek() else { break };
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
            } else if m!(l, "^-possibilities-blockquote") {
                (|| {
                    let (p_loc, l) = lines.next()?;
                    let (intro, field1, rest) = mc!(l, r"^(<!--)(.)(.*)-->")
                        .or_else(|| {
                            errs.wrong(loc, "possibilities-blockquote not followed by table picture");
                            None
                        })?;
                    Some(ID::PossibilitiesBlockquote {
                        heading_picture_loc: p_loc,
                        heading_picture: format!(
                            "{}{}{}",
                            field1.repeat(intro.len()),
                            field1,
                            rest,
                        ),
                    })
                })()
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

    blockquotes_after_directive(input, |d| match d {
        ID::Structs {} => Some(()),
        _ => None,
    })
    .map(|(_d_loc, (), bq_loc, content)| {
        parse_content(bq_loc, content).into_iter()
    })
    .flatten()
    .collect()
}

#[derive(Default, Debug)]
struct SectionState<'i> {
    for_: Option<(DocLoc, &'i String, &'i DirectiveTrackUsed)>,
    t_limits: Vec<Limit>,
    bq_input: Option<(DocLoc, &'i String)>,
}

fn parse_bullet(
    loc: DocLoc,
    bullet: &str,
    errs: &mut Errors,
    ss: &mut SectionState,
    examples_out: &mut Vec<Box<dyn Example>>,
) -> Result<(), String> {
    let bullet = re!(r"\n   +").replace_all(bullet, " ");
    let (input, here_for, outputs) =
        mc!(bullet, r#"(?m)^ \* `([^`]+)`(?: for ([^:]+))?: (.*)$"#)
            .ok_or_else(|| format!("syntax not as expected"))?;

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
        None => Limit::True,
        Some((loc, for_, for_used)) => {
            for_used.note();
            // Explicit error handling because we want to use the right loc:
            // it might be from an earlier directive.
            match Limit::parse(
                for_,
                &mut all_must_match,
                Some(&mut ss.t_limits),
            ) {
                Ok(y) => y,
                Err(m) => {
                    errs.wrong(loc, m);
                    return Ok(());
                }
            }
        }
    };

    let mut poss = |output: Result<&_, &_>| {
        Ok::<_, String>({
            examples_out.push(PossibilitiesExample::new(
                loc,
                &input,
                limit.clone(),
                all_must_match,
                output,
            )?);
        })
    };

    if let Some((mut rhs,)) = mc!(outputs, r"true for (.+)$") {
        if for_.is_some() {
            return Err(format!(
                "in condition example, `for ...` must be on RHS"
            ));
        }
        let mut true_contexts = vec![];
        while !rhs.is_empty() {
            let (for_, new_rhs) =
                mc!(rhs, r"([^,]+)(?:$|, and |, )(.*)")
                .ok_or_else(|| format!(
                    r#"bad True context syntax for condition example: "{}""#,
                    rhs,
                ))?;
            rhs = new_rhs;
            let lim = Limit::parse(&for_, &mut false, None)?;
            true_contexts.push((for_, lim));
        }
        examples_out.push(Box::new(ConditionExample::new(
            loc,
            input,
            true_contexts,
        )));
    } else if m!(outputs, "^nothing$") {
        poss(Ok(""))?;
    } else if let Some((msg,)) = mc!(outputs, r"error, ``(.*)``$") {
        poss(Err(msg.trim()))?;
    } else {
        let mut outputs = outputs;
        while !outputs.is_empty() {
            let (p, rest) =
                [
                    "(?m)^`([^`]+)`(?:, (.*)|)$",
                    "(?m)^``((?:[^`]+|`[^`])*)``(?:, (.*)|)$",
                ]
                .iter()
                .find_map(|re| mc!(outputs, re))
                .ok_or_else(|| {
                    format!(
                        r#"bad (tail of) bullet point examples "{}""#,
                        outputs,
                    )
                })?;
            poss(Ok(&p))?;
            outputs = rest;
        }
    }

    Ok(())
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
                match parse_bullet(*loc, bullet, errs, &mut ss, out) {
                    Ok(()) => {}
                    Err(e) => errs.wrong(
                        *loc,
                        format_args!(
                            "failed to parse bullet point: {:?}: {}",
                            bullet, e,
                        ),
                    ),
                }
            }
            II::Directive {
                loc: d_loc,
                d,
                used,
            } => match d {
                ID::For { for_: new } => ss.for_ = Some((*d_loc, new, used)),
                ID::ForToplevelsConcat { toplevels } => {
                    used.note();
                    if matches!(input.peek(), Some(II::Paragraph { .. })) {
                        _ = input.next();
                    }
                    let (bq_input_loc, bq_input) = match ss.bq_input.take() {
                        Some(y) => y,
                        None => {
                            errs.wrong(*d_loc,
 "for-toplevels-concat but no previous blockquote for input");
                            continue;
                        }
                    };
                    match input.next() {
                        Some(II::BlockQuote {
                            options: _,
                            content,
                            ..
                        }) => {
                            let example = ForToplevelsConcatExample {
                                loc: bq_input_loc,
                                input: (*bq_input).clone(),
                                toplevels: toplevels.clone(),
                                output: content.clone(),
                            };
                            out.push(Box::new(example));
                        }
                        _ => {
                            errs.wrong(*d_loc,
 "for-toplevels-concat not followed by output blockquote");
                            continue;
                        }
                    }
                }
                _ => {}
            },
            II::BlockQuote {
                loc,
                options,
                content,
            } => {
                if m!(options, r"^rust$") {
                    continue;
                }
                if let Some((prev, _)) = ss.bq_input {
                    errs.wrong(prev,
 "unused blockquote, simply followed by another - missing directive?");
                }
                ss.bq_input = Some((*loc, content));
            }
            II::Paragraph { loc, .. } => {
                errs.wrong(*loc, "unmarked text in examples section");
            }
            II::Heading { .. } => panic!("heading in subsection!"),
        }
    }
}

fn blockquotes_after_directive<'o, DD>(
    input: &'o Preprocessed,
    mut is_introducer: impl FnMut(&InputDirective) -> Option<DD> + 'o,
) -> impl Iterator<Item = (DocLoc, DD, DocLoc, &'o str)> + 'o {
    input
        .iter()
        .enumerate()
        .filter_map(move |(i, ii)| match ii {
            II::Directive {
                loc: d_loc,
                used,
                d,
            } => {
                let dd = is_introducer(d)?;
                used.note();
                Some((i, *d_loc, dd))
            }
            _ => None,
        })
        .map(move |(i, d_loc, dd)| match input.get(i + 1) {
            Some(II::BlockQuote {
                loc: bq_loc,
                content,
                ..
            }) => (d_loc, dd, *bq_loc, &**content),
            _ => bail(d_loc, "directive not followed by blockquote"),
        })
}

fn process_example_sections(
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

fn extract_by_picture<const N: usize>(
    chars: [char; N],
    picture_s: &str,
    data_s: &str,
) -> Result<[String; N], String> {
    let picture = picture_s.chars().collect_vec();
    let data: Vec<char> = format!("{:<len$}", data_s, len = picture.len())
        .chars()
        .collect();

    let mut used = vec![false; picture.len()];
    let mut error = Ok(());
    let output = chars.map(|c| {
        (|| {
            let (lhs, mid, _rhs) =
                mc!(picture_s, format!("([^{c}]*)([{c}]+)([^{c}]*)$"),)
                    .ok_or_else(|| {
                        format!(
                            "picture line has zero or several blocks of '{c}'"
                        )
                    })?;

            let a = lhs.len();
            let b = lhs.len() + mid.len();

            for used in &mut used[a..b] {
                assert!(!*used, "character '{}' repeated in requests!", c);
                *used = true;
            }
            let part =
                data[a..b].iter().collect::<String>().trim().to_string();

            Ok::<_, String>(part)
        })()
        .unwrap_or_else(|e: String| {
            if error.is_ok() {
                error = Err(e);
            }
            Default::default()
        })
    });

    (|| {
        error?;

        if let Some(wrong) = picture.iter().cloned().position(|c| {
            !(c.is_whitespace() || c == '#' || chars.iter().any(|tc| c == *tc))
        }) {
            return Err(format!(
                r"bad character in picture, not space of '#' or one of {chars:?}
picture: {picture_s}
   here: {nil:pad$}^",
                nil = "",
                pad = wrong,
            ));
        }

        for i in 0..data.len() {
            if data[i].is_whitespace() {
                continue;
            }
            if picture.get(i) == Some(&'#') {
                continue;
            }
            if used.get(i) == Some(&true) {
                continue;
            }
            return Err(format!(
                r"unexpected text, not in a column:
picture: {picture_s}
   data: {data_s}
   here: {nil:pad$}^",
                nil = "",
                pad = i,
            ));
        }
        Ok::<_, String>(output)
    })()
}

fn extract_possibilites_blockquotes(
    input: &Preprocessed,
    errs: &mut Errors,
    examples_out: &mut Vec<Box<dyn Example>>,
) {
    for (_d_loc, (p_loc, picture), bq_loc, content) in
        blockquotes_after_directive(input, |d| match d {
            ID::PossibilitiesBlockquote {
                heading_picture_loc: p_loc,
                heading_picture,
            } => Some((*p_loc, heading_picture.clone())),
            _ => None,
        })
    {
        let fields = ['i', 'f', 'o'];

        // Prechecking allows us to bail on the whole blockquote if
        // the picture is wrong, without requiring extract_by_picture
        // to distinguish bad pictures from bad data.
        match extract_by_picture(fields, &picture, "") {
            Err(m) => {
                errs.wrong(p_loc, format_args!("invalid picture line: {}", m));
                return;
            }
            Ok([..]) => {}
        }

        let mut t_limits = vec![];

        for (lno, l) in content.lines().enumerate() {
            let l_loc = bq_loc + lno + 1;

            match (|| {
                let columns = extract_by_picture(fields, &picture, l)?;
                for (c, data) in izip!(fields, &columns) {
                    if data.is_empty() {
                        return Err(format!(
                            "missing information for column '{c}'"
                        ));
                    }
                }
                let [input, for_, output] = columns;

                let for_ = re!("^for ").replace_all(&for_, "");

                let mut all_must_match = false;
                let limit = Limit::parse(
                    &for_,
                    &mut all_must_match,
                    Some(&mut t_limits),
                )?;

                PossibilitiesExample::new(
                    l_loc,
                    &input,
                    limit,
                    all_must_match,
                    Ok(&output),
                )
            })() {
                Ok(example) => examples_out.push(example),
                Err(m) => errs.wrong(l_loc, m),
            }
        }
    }
}

pub fn extract(
    errs: &mut Errors,
) -> (Vec<syn::DeriveInput>, Vec<Box<dyn Example>>) {
    let iis = read_preprocess(errs);

    let structs = extract_structs(&iis);

    let mut examples = process_example_sections(&iis, errs);

    extract_possibilites_blockquotes(&iis, errs, &mut examples);

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
