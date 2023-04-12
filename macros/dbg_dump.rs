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
    let lh = format!("{:12} =>", templ.to_string());
    writeln!(w, "        {:16} {}", lh, template_result(ctx, templ))?;
    Ok(())
}

macro_rules! expand { { $w_ctx:expr, $($t:tt)* } => {
    dump_expand_one($w_ctx.0, $w_ctx.1, quote!{ $($t)* })?;
} }

fn dump_whole(mut w: &mut Out, ctx: &Context) -> R {
    writeln!(w, "top-level:")?;
    let c = (&mut w, ctx);

    expand! { c, $tname }
    expand! { c, $ttype }
    expand! { c, $tvis }
    expand! { c, $tgens }
    expand! { c, $tgnames }
    expand! { c, $twheres }
    expand! { c, $tdeftype }
    expand! { c, $tdefgens }
    expand! { c, $tdefkwd }
    expand! { c, ${tdefvariants VARIANTS} }

    WithinVariant::for_each(ctx, |ctx, wv| dump_variant(w, ctx, wv))?;

    Ok(())
}

fn variant_heading(w: &mut Out, wv: &WithinVariant) -> R {
    match wv.variant {
        None => write!(w, "value")?,
        Some(v) => write!(w, "variant {}", v.ident)?,
    };
    Ok(())
}

fn dump_variant(mut w: &mut Out, ctx: &Context, wv: &WithinVariant) -> R {
    variant_heading(w, wv)?;
    writeln!(w, ":")?;
    let c = (&mut w, ctx);

    expand! { c, $vname }
    expand! { c, $vtype }
    expand! { c, $vpat }
    expand! { c, ${vdefbody VNAME FIELDS..} }

    WithinField::for_each(ctx, |ctx, wf| dump_field(w, ctx, wf))?;

    Ok(())
}

fn dump_field(mut w: &mut Out, ctx: &Context, wf: &WithinField) -> R {
    variant_heading(w, ctx.variant.expect("in field but not variant!"))?;
    writeln!(w, ", field {}:", wf.fname(Span::call_site()).to_token_stream())?;
    let c = (&mut w, ctx);

    expand! { c, $fname }
    expand! { c, $ftype }
    expand! { c, $fvis }
    expand! { c, $fpatname }
    expand! { c, $fdefine }

    Ok(())
}
