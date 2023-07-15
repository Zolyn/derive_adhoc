use super::framework::*;

use std::fmt::Error as E;
use std::fmt::Result as R;
use std::fmt::Write;

/// Type alias for our output text accumulator, in case we want to change it
type Out = String;

pub fn dump(ctx: &Context) {
    let w = (|| {
        let mut w = String::new();

        let description =
            format!("derive-adhoc expansions dump for {}", ctx.top.ident);

        writeln!(w, "---------- {} (start) ----------", description)?;
        dump_whole(&mut w, ctx)?;
        writeln!(w, "---------- {} (end) ----------", description)?;

        Ok::<_, E>(w)
    })()
    .expect("write to String failed");

    eprint!("{}", w);
}

fn template_result(ctx: &Context, templ: TokenStream) -> String {
    let parser = |input: &ParseBuffer<'_>| Template::parse(input);
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

fn dump_any_one(
    w: &mut Out,
    ctx: &Context,
    show_templ: TokenStream,
    show_op: &str,
    make_real_templ: &dyn Fn(TokenStream) -> TokenStream,
) -> R {
    let lh = format!("{:12} {}", show_templ.to_string(), show_op);
    let templ = make_real_templ(show_templ);
    writeln!(w, "        {:16} {}", lh, template_result(ctx, templ))?;
    Ok(())
}

fn dump_expand_one(w: &mut Out, ctx: &Context, templ: TokenStream) -> R {
    dump_any_one(w, ctx, templ, "=>", &|t| t)
}

fn dump_bool_one(w: &mut Out, ctx: &Context, templ: TokenStream) -> R {
    let make_real = |templ| quote! { ${if #templ { true } else { false }} };
    dump_any_one(w, ctx, templ, "=", &make_real)
}

macro_rules! expand { { $w_ctx:expr, $($t:tt)* } => {
    dump_expand_one($w_ctx.0, $w_ctx.1, quote!{ $($t)* })?;
} }
macro_rules! bool { { $w_ctx:expr, $($t:tt)* } => {
    dump_bool_one($w_ctx.0, $w_ctx.1, quote!{ $($t)* })?;
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

    bool! { c, is_struct }
    bool! { c, is_enum }
    bool! { c, is_union }
    bool! { c, tvis }

    expand! { c, $tattrs }

    // Don't debug dump these.  But list them here, so that
    // check-keywords-documented is happy.  (That is nicer than
    // using the check-keywords-documented exception table.)
    if false {
        // TODO: search attributes and dump what would work
        expand! { c, $tmeta }
        expand! { c, $vmeta }
        expand! { c, $fmeta }
        bool! { c, tmeta }
        bool! { c, vmeta }
        bool! { c, fmeta }
        // Too complex to demonstrate
        expand! { c, $paste }
        // Too subtle to demonstrate
        expand! { c, $crate }
        // Recursive, would be silly
        expand! { c, $dbg_all_keywords }
        // Control flow, can't sensibly be dumped
        expand! { c, $when }
        expand! { c, $if }
        expand! { c, $select1 }
        expand! { c, $define }
        expand! { c, $defcond }
        bool! { c, not }
        bool! { c, all }
        bool! { c, any }
        // Vacuous
        bool! { c, true }
        bool! { c, false }
    }

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

    bool! { c, v_is_unit }
    bool! { c, v_is_tuple }
    bool! { c, v_is_named }

    expand! { c, $vattrs }

    WithinField::for_each(ctx, |ctx, wf| dump_field(w, ctx, wf))?;

    Ok(())
}

fn dump_field(mut w: &mut Out, ctx: &Context, wf: &WithinField) -> R {
    variant_heading(w, ctx.variant.expect("in field but not variant!"))?;
    let fname = wf.fname(Span::call_site()).to_token_stream();
    writeln!(w, ", field {}:", fname)?;
    let c = (&mut w, ctx);

    expand! { c, $fname }
    expand! { c, $ftype }
    expand! { c, $fvis }
    expand! { c, $fdefvis }
    expand! { c, $fpatname }
    expand! { c, $fdefine }

    bool! { c, fvis }
    bool! { c, fdefvis }

    expand! { c, $fattrs }

    Ok(())
}
