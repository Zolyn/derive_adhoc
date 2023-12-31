//! Implementation of the `expect` option

use crate::prelude::*;

/// Value for an `expect`
#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumString, Display)]
#[allow(non_camel_case_types)]
pub enum Target {
    items,
    expr,
}

/// Local context for a syntax check operation
struct Checking<'t> {
    ctx: &'t framework::Context<'t>,
    output: &'t mut TokenStream,
    target: DaOptVal<Target>,
}

/// Main entrypoint
///
/// Checks that `output` can be parsed as `target`.
///
/// If not, replaces `output` with something which will generate
/// compiler error(s) which the user will find helpful:
///  * A `compile_error!` invocation with the original error span
///  * include_file!` for a generated temporary file
///    containing the text of the output,
///    so that the compiler will point to the actual error.
pub fn check_expected_target_syntax(
    ctx: &framework::Context,
    output: &mut TokenStream,
    target: DaOptVal<Target>,
) {
    check::Checking {
        ctx,
        output,
        target,
    }
    .check();
}

impl Target {
    /// Checks if `ts` can parse as `self`, returning the error if not
    fn perform_check(self, ts: TokenStream) -> Option<syn::Error> {
        fn chk<T: Parse>(ts: TokenStream) -> Option<syn::Error> {
            syn::parse2::<Discard<T>>(ts).err()
        }

        use Target::*;
        match self {
            items => chk::<Concatenated<Discard<syn::Item>>>(ts),
            expr => chk::<syn::Expr>(ts),
        }
    }

    /// Tokens for `include!...` to include syntax element(s) like `self`
    fn include_syntax(self, file: &str) -> TokenStream {
        use Target::*;
        match self {
            items => quote! { include!{ #file } },
            expr => quote! { include!( #file ) },
        }
    }
}

impl Checking<'_> {
    /// Checks that `tokens` can be parsed as `T`
    ///
    /// Does the actual work of [`check_expected_target_syntax`]
    fn check(self) {
        let err = self.target.value.perform_check(self.output.clone());

        let err = match err {
            Some(err) => err,
            None => return,
        };

        let broken = mem::replace(self.output, err.into_compile_error());

        let expansion = expand_via_file(self.ctx, self.target.value, broken)
            .map_err(|e| {
                Span::call_site()
                    .error(format!(
 "derive-adhoc was unable to write out the expansion to a file for fuller syntax error reporting: {}",
                    e
                ))
                    .into_compile_error()
            });
        self.output.extend(expansion);
    }
}

/// Constructs an `include!` which includes the text for `broken`
///
/// Appends the `include` to `checking.output`.
///
/// If this can't be done, reports why not.
fn expand_via_file(
    ctx: &framework::Context,
    target: Target,
    broken: TokenStream,
) -> Result<TokenStream, String> {
    use sha3::{Digest as _, Sha3_256};
    use std::{fs, io, path::PathBuf};

    let text = format!("// {}:\n{}\n", ctx.expansion_description(), broken);

    let hash: String = {
        let mut hasher = Sha3_256::new();
        hasher.update(&text);
        let hash = hasher.finalize();
        hash[0..12].iter().map(|b| format!("{:02x}", b)).collect()
    };

    let dir: PathBuf = [env!("OUT_DIR"), "derive-adhoc~expansions~"]
        .iter()
        .collect();

    match fs::create_dir(&dir) {
        Ok(()) => {}
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => {}
        Err(e) => return Err(format!("create dir {:?}: {}", &dir, e)),
    };

    let leaf = format!("d-a-{}.rs", hash);
    let some_file = |leaf: &str| {
        let mut file = dir.clone();
        file.push(leaf);
        file
    };
    let file = some_file(&leaf);
    let file = file
        .to_str()
        .ok_or_else(|| format!("non UTF-8 path? from env var! {:?}", file))?;
    let file_tmp = some_file(&format!("{}~", leaf));

    fs::write(&file_tmp, &text)
        .map_err(|e| format!("create {:?}: {}", &file_tmp, e))?;
    fs::rename(&file_tmp, &file)
        .map_err(|e| format!("install new {:?}: {}", &file, e))?;

    Ok(target.include_syntax(file))
}
