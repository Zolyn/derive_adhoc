use crate::framework::*;

use std::fmt::Error as E;
use std::fmt::Result as R;
use std::fmt::Write;

/// Type alias for our output text accumulator, in case we want to change it
type Out = String;

pub fn dump(ctx: &Context) {
    let w = (|| {
        let mut w = String::new();

        let description = ctx.expansion_description();
        writeln!(
            w,
            "---------- expansions dump from {} (start) ----------",
            description
        )?;

        dump_whole(&mut w, ctx)?;

        writeln!(
            w,
            "---------- expansions dump from {} (end) ----------",
            description
        )?;

        Ok::<_, E>(w)
    })()
    .expect("write to String failed");

    eprint!("{}", w);
}

fn template_result(ctx: &Context, templ: TokenStream) -> String {
    let parser = |input: &ParseBuffer<'_>| Template::parse(input, ());
    let templ: Template<TokenAccumulator> =
        parser.parse2(templ).expect("failed to parse own template");
    let result = (|| {
        let mut output = TokenAccumulator::new();
        templ.expand(ctx, &mut output);
        output.tokens()
    })();
    match result {
        Ok(result) => result.to_string(),
        Err(_) => "<error>".into(),
    }
}

fn dump_expand_one(w: &mut Out, ctx: &Context, templ: TokenStream) -> R {
    write!(w, "        {:16}", templ.to_string())?;
    writeln!(w, " =>   {}", template_result(ctx, templ))?;
    Ok(())
}

macro_rules! expand { { $w_ctx:expr, $($t:tt)* } => {
    dump_expand_one($w_ctx.0, $w_ctx.1, quote!{ $($t)* })?;
} }

fn dump_whole(w: &mut Out, ctx: &Context) -> R {
    writeln!(w, "top-level:")?;

    let c = (w, ctx);
    expand! { c, $tname }

    Ok(())
}
