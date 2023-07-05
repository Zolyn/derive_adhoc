//! Examples involving searching over possible outputs for a particular input
//!
//! Where the input file doesn't specify
//! which driver (or part of driver) generates the output.
//! (Or maybe where it limits it according to `limit`).

use super::*;

pub struct ConditionExample {
    loc: DocLoc,
    /// A derive-adhoc condition
    input: String,
    /// Which contexts give true
    true_contexts: Vec<(String, Limit)>,
}

#[derive(Default)]
struct Tracker {
    ctxs: Vec<TrackerEntry>,
}

struct TrackerEntry {
    context_desc: String,
    limvals: Vec<Result<bool, ()>>,
    got: Result<bool, syn::Error>,
}

impl Example for ConditionExample {
    fn print_checking(&self) {
        println!("checking :{} {}", self.loc, &self.input,);
    }

    fn check(&self, errs: &mut Errors, drivers: &[syn::DeriveInput]) {
        let cond = match syn::parse_str::<Subst<BooleanContext>>(&self.input) {
            Ok(y) => y,
            Err(e) => {
                errs.wrong(self.loc, format_args!("failed to parse: {}", e));
                return;
            }
        };

        let mut tracker = Tracker::default();

        for_every_example_context(drivers, |ctx| {
            Ok::<_, Void>(self.check_one_context(&cond, &mut tracker, ctx))
        })
        .void_unwrap();

        let problems = self.problems(&tracker);
        if problems.is_empty() {
            return;
        }

        eprintln!();
        eprintln!("========================================");
        for problem in problems {
            errs.wrong(
                self.loc,
                format_args!("example mismatch: {}", problem),
            );
        }
        eprintln!("condition: {}", &self.input);

        let head_w = tracker
            .ctxs
            .iter()
            .map(|te| te.context_desc.len())
            .max()
            .unwrap();

        eprint!("{:l$} ", r#"context: \ exp. True for:"#, l = head_w);
        for (lim_s, _lim) in &self.true_contexts {
            eprint!(" {:1}", lim_s);
        }
        eprintln!(" Actual:");

        for te in &tracker.ctxs {
            eprint!(" {:l$} ", &te.context_desc, l = head_w);
            for ((lim_s, _lim), lv) in izip!(&self.true_contexts, &te.limvals)
            {
                eprint!(
                    " {:l$}",
                    match lv {
                        Ok(false) => '-',
                        Ok(true) => 'y',
                        Err(()) => 'E',
                    },
                    l = lim_s.len()
                );
            }
            eprintln!(
                " {}",
                match &te.got {
                    Ok(false) => "-".into(),
                    Ok(true) => "y".into(),
                    Err(e) => format!("error: {}", e),
                }
            );
        }
        eprintln!("========================================");
    }
}

impl ConditionExample {
    pub fn new(
        loc: DocLoc,
        input: String,
        true_contexts: Vec<(String, Limit)>,
    ) -> Self {
        ConditionExample {
            loc,
            input,
            true_contexts,
        }
    }

    fn check_one_context(
        &self,
        cond: &Subst<BooleanContext>,
        tracker: &mut Tracker,
        ctx: &Context<'_>,
    ) {
        let context_desc = ctx.desc_for_tests();
        let got = cond.eval_bool(ctx);

        let limvals = self
            .true_contexts
            .iter()
            .map(|(_lim_s, lim)| lim.matches(ctx))
            .collect_vec();

        tracker.ctxs.push(TrackerEntry {
            context_desc,
            got,
            limvals,
        })
    }

    //  for each Limit
    //
    //      must be at least one ctx matches (Ok(true))
    //      must be at least one ctx positively non-matching Ok(false)
    //
    //      for every ctx that matches
    //      evaluation must produce true (not err)
    //
    // for each ctx
    //
    //      if evaluation was true
    //      must be at least one Limit that matches
    //
    //      if evaluation was false
    //      must be at no Limit that positively matches
    //      must be at least one Limit that positively non-matches
    //
    //      if evaluation was error
    //      no Limit matches (handled above)
    fn problems(&self, tracker: &Tracker) -> Vec<String> {
        let mut problems = vec![];

        for (i, (lim_s, _lim)) in self.true_contexts.iter().enumerate() {
            if !tracker.ctxs.iter().any(|te| te.limvals[i] == Ok(true)) {
                problems.push(format!("{:?} matched no context", lim_s));
            }
            if !tracker.ctxs.iter().any(|te| te.limvals[i] == Ok(false)) {
                problems.push(format!("{:?} dismatched no context", lim_s));
            }
        }

        for te in &tracker.ctxs {
            let lim_results = izip!(&self.true_contexts, &te.limvals);

            match te.got {
                Ok(true) => {
                    if !te.limvals.iter().any(|l| matches!(l, Ok(true))) {
                        problems.push(format!(
 r#"for {}, evaluated to True, but not in list of "True for""#,
                        &te.context_desc,
                    ));
                    }
                }
                Ok(false) => {
                    if let Some(((lim_s, _), _)) = lim_results
                        .clone()
                        .find(|((_lim_s, _lim), lv)| matches!(lv, Ok(true)))
                    {
                        problems.push(format!(
 "for {}, evaluated to False, but listed as True for {:?}",
                            &te.context_desc,
                            lim_s,
                        ));
                    }
                    if !lim_results
                        .clone()
                        .any(|((_lim_s, _lim), lv)| matches!(lv, Ok(false)))
                    {
                        problems.push(format!(
 r#"for {}, evaluated to False, but none of the "True for" excluded it"#,
                            &te.context_desc,
                        ));
                    }
                }
                Err(_) => {
                    // handled above
                }
            }
        }

        problems
    }
}
