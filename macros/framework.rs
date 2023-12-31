//! Core types and traits for parsing and expansion
//!
//! Also re-exports the names that the implementation wants.
//!
//! Should be included with `use super::framework::*`, not `crate::`,
//! so that it works with `tests/directly.rs` too.

pub use super::prelude::*;

pub use super::boolean::*;
pub use super::repeat::*;
pub use super::syntax::*;

pub(super) use super::paste;
pub(super) use super::paste::{IdentFrag, IdentFragInfallible};

/// Context during expansion
///
/// References the driver, and digested information about it.
/// Also represents where in the driver we are,
/// including repetition context.
#[derive(Debug, Clone)]
pub struct Context<'c> {
    pub top: &'c syn::DeriveInput,
    pub template_crate: &'c syn::Path,
    pub template_name: Option<&'c syn::Path>,
    pub tmetas: &'c PreprocessedMetas,
    pub variant: Option<&'c WithinVariant<'c>>,
    pub field: Option<&'c WithinField<'c>>,
    pub pvariants: &'c [PreprocessedVariant<'c>],
    pub definitions: Definitions<'c>,
    pub nesting_depth: u16,
    pub nesting_parent: Option<(&'c Context<'c>, &'c DefinitionName)>,
}

#[derive(Debug, Clone)]
pub struct PreprocessedVariant<'f> {
    pub fields: &'f syn::Fields,
    pub pmetas: PreprocessedMetas,
    pub pfields: Vec<PreprocessedField>,
}

#[derive(Debug, Clone)]
pub struct PreprocessedField {
    pub pmetas: PreprocessedMetas,
}

/// `#[adhoc(...)]` helper attributes
pub type PreprocessedMetas = Vec<PreprocessedMeta>;
pub type PreprocessedMeta = syn::Meta;

#[derive(Debug, Clone)]
pub struct WithinVariant<'c> {
    pub variant: Option<&'c syn::Variant>,
    pub fields: &'c syn::Fields,
    pub pmetas: &'c PreprocessedMetas,
    pub pfields: &'c [PreprocessedField],
}

#[derive(Debug, Clone)]
pub struct WithinField<'c> {
    pub field: &'c syn::Field,
    pub pfield: &'c PreprocessedField,
    pub index: u32,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Definitions<'c> {
    pub here: &'c [&'c Definition<DefinitionBody>],
    pub conds: &'c [&'c Definition<DefCondBody>],
    pub earlier: Option<&'c Definitions<'c>>,
}

/// Special processing instructions returned by
/// [`special_before_element_hook`](SubstParseContext::special_before_element_hook)
pub enum SpecialInstructions {
    /// This template is finished
    ///
    /// Stop parsing this `Template` though perhaps
    /// the surrounding `Group` is not finished.
    ///
    /// The parser for whatever called `Template::parse`
    /// will continue.
    EndOfTemplate,
}

/// Surrounding lexical context during parsing
///
/// This is the kind of lexical context a piece of a template appears in.
/// It is implemented for
///  * Types that represent an expansion output `ExpansionOutput`;
///    in this case, the lexical context is one where
///    the expansion is accumulated in this type.
///  * Places where template substitution syntax `${keyword }`
///    appears but where no output will be generated (eg, within
///    the condition of `${if }`.
///
/// The associated types are either `Void` or `()`.
/// They appears within the variants of `SubstDetails`,
/// causing inapplicable variants to be eliminated.
///
/// Because a variant is only inhabited if all of its fields are,
/// the conditions are effectively ANDed.
/// So the "default" value (for context that don't have an opnion)
/// is inhabitedness `()`.
///
/// Each type has an associated constructur,
/// used during parsing.
/// So this generates a parse error at parse time,
/// if a construct appears in the wrong place.
pub trait SubstParseContext: Sized {
    /// Uninhabited iff this lexical context is within `${paste }`
    type NotInPaste: Debug + Copy + Sized;
    /// Uninhabited iff this lexical context is within a condition.
    type NotInBool: Debug + Copy + Sized;
    /// Uninhabited unless this lexical context is within a condition.
    type BoolOnly: Debug + Copy + Sized;

    fn not_in_paste(span: &impl Spanned) -> syn::Result<Self::NotInPaste>;
    fn not_in_bool(span: &impl Spanned) -> syn::Result<Self::NotInBool>;

    fn bool_only(span: &impl Spanned) -> syn::Result<Self::BoolOnly> {
        Err(span.error(
            "derive-adhoc keyword is a condition - not valid as an expansion",
        ))
    }

    /// For communicating through `parse_special`
    type SpecialParseContext: Default;

    /// Handle any special syntax for a special kind of template context.
    ///
    /// This method is called only when parsing multi-element [`Template`]s,
    /// It's a hook, called before parsing each `TemplateElement`.
    ///
    /// It should consume any special syntax as appropriate,
    ///
    /// The default implementation is a no-op.
    /// The only non-default implementation is in `paste.rs`, for `$<...>` -
    /// see [`paste::AngleBrackets`].
    fn special_before_element_hook(
        _special: &mut Self::SpecialParseContext,
        _input: ParseStream,
    ) -> syn::Result<Option<SpecialInstructions>> {
        Ok(None)
    }
}

/// Expansion output accumulator, for a template lexical context
///
/// Each template lexical context has a distinct type which
///  * Represents the lexical context
///  * If that lexical context generates expansions,
///    accumulates the expansion.  That's what this trait is.
///
/// The methods are for accumulating various kinds of things
/// that can be found in templates, or result from template expansion.
///
/// The accumulating type (`Self` might be accumulating
/// tokens ([`TokenStream`]) or strings ([`paste::Items`]).
pub trait ExpansionOutput: SubstParseContext {
    /// Append something according to its `Display` impl
    ///
    /// This could be an `str` for example.
    /// This is *not* suitable for `TokenTree::Literal` or `syn::Lit`
    /// because their `Display` impls produce the version with `" "`.
    fn append_display<I: Display + Spanned + ToTokens>(&mut self, lit: &I);

    /// An identifier (or fragment of one)
    ///
    /// Uses the `IdentFragment` for identifier pasting,
    /// and the `ToTokens` for general expansion.
    fn append_identfrag_toks<I: IdentFrag>(
        &mut self,
        ident: &I,
    ) -> Result<(), I::BadIdent>;

    /// Append a Rust path (scoped identifier, perhaps with generics)
    ///
    /// To facilitate `${pawte }`, the path is provided as:
    ///  * some prefix tokens (e.g., a scoping path),
    ///  * the actual identifer,
    ///  * some suffix tokens (e.g. generics).
    ///
    /// `tspan` is the span of the part of the template
    /// which expanded into this path.
    ///
    /// This is a "more complex" expansion,
    /// in the terminology of the template reference:
    /// If a paste contains more than one, it is an error.
    fn append_idpath<A, B, I>(
        &mut self,
        template_entry_span: Span,
        pre: A,
        ident: &I,
        post: B,
    ) -> Result<(), I::BadIdent>
    where
        A: FnOnce(&mut TokenAccumulator),
        B: FnOnce(&mut TokenAccumulator),
        I: IdentFrag;

    /// Append a [`syn::LitStr`](struct@syn::LitStr)
    ///
    /// This is its own method because `syn::LitStr` is not `Display`,
    /// and we don't want to unconditionally turn it into a string
    /// before retokenising it.
    fn append_syn_litstr(&mut self, v: &syn::LitStr);

    /// Append a [`syn::Type`]
    ///
    /// This is a "more complex" expansion,
    /// in the terminology of the template reference:
    /// If a paste contains more than one, it is an error.
    fn append_syn_type(&mut self, te_span: Span, v: &syn::Type);

    /// Append using a function which generates tokens
    ///
    /// If you have an `impl `[`ToTokens`],
    /// use [`append_tokens`](ExpansionOutput::append_tokens) instead.
    ///
    /// Not supported within `${paste }`.
    /// The `NotInPaste` parameter makes this method unreachable
    /// when expanding within `${paste }`;
    /// or to put it another way,
    /// it ensures that such an attempt would have been rejected
    /// during template parsing.
    fn append_tokens_with(
        &mut self,
        np: &Self::NotInPaste,
        f: impl FnOnce(&mut TokenAccumulator) -> syn::Result<()>,
    ) -> syn::Result<()>;

    /// "Append" a substitution which can only be used within a boolean
    ///
    /// Such a thing cannot be expanded, so it cannot be appended,
    /// so this function must be unreachable.
    /// `expand_bool_only` is called (in expansion contexts)
    /// to handle uninhabited `SubstDetails` variants etc.
    ///
    /// Implementing it involves demonstrating that
    /// either `self`, or `Self::BoolOnly`, is uninhabited,
    /// with a call to [`void::unreachable`].
    fn append_bool_only(&mut self, bool_only: &Self::BoolOnly) -> !;

    /// Note that an error occurred
    ///
    /// This must arrange to
    /// (eventually) convert it using `into_compile_error`
    /// and emit it somewhere appropriate.
    fn record_error(&mut self, err: syn::Error);

    /// Convenience method for noting an error with span and message
    fn write_error<S: Spanned, M: Display>(&mut self, span: &S, message: M) {
        self.record_error(span.error(message));
    }

    /// Convenience method for writing a `ToTokens`
    ///
    /// Dispatches to
    /// [`append_tokens_with`](ExpansionOutput::append_tokens_with)
    /// Not supported within `${paste }`.
    //
    // I experimented with unifying this with `append_tokens_with`
    // using a `ToTokensFallible` trait, but it broke type inference
    // rather badly and had other warts.
    fn append_tokens(
        &mut self,
        np: &Self::NotInPaste,
        tokens: impl ToTokens,
    ) -> syn::Result<()> {
        self.append_tokens_with(np, |out| {
            out.append(tokens);
            Ok(())
        })
    }

    fn default_subst_meta_as() -> SubstMetaAs<Self>;
}

/// Convenience trait providing `item.expand()`
///
/// Implementations of this are often specific to the [`ExpansionOutput`].
///
/// Having this as a separate trait,
/// rather than hanging it off `ExpansionOutput`,
/// makes the expansion method more convenient to call.
///
/// It also avoids having to make all of these expansion methods
/// members of the `ExpansionOutput` trait.
pub trait Expand<O> {
    fn expand(&self, ctx: &Context, out: &mut O) -> syn::Result<()>;
}
/// Convenience trait providing `fn expand(self)`, infallible version
///
/// Some of our `expand` functions always record errors
/// within the output accumulator
/// and therefore do not need to return them.
pub trait ExpandInfallible<O> {
    fn expand(&self, ctx: &Context, out: &mut O);
}

/// Accumulates tokens, or errors
///
/// We collect all the errors, and if we get an error, don't write
/// anything out.
/// This is because `compile_error!` (from `into_compile_error`)
/// only works in certain places in Rust syntax (!)
#[derive(Debug)]
pub struct TokenAccumulator(Result<TokenStream, syn::Error>);

impl<'c> Context<'c> {
    pub fn is_enum(&self) -> bool {
        matches!(self.top.data, syn::Data::Enum(_))
    }

    /// Calls `f` with a top-level [`Context`] for a [`syn::DeriveInput`]
    ///
    /// `Context` has multiple levels of references to values created
    /// here, so we can't easily provide `Context::new()`.
    pub fn call<T>(
        driver: &syn::DeriveInput,
        template_crate: &syn::Path,
        template_name: Option<&syn::Path>,
        f: impl FnOnce(Context) -> syn::Result<T>,
    ) -> Result<T, syn::Error> {
        let tmetas = preprocess_attrs(&driver.attrs)?;

        let pvariants_one = |fields| {
            let pmetas = tmetas.clone(); // TODO Cow maybe?
            let pfields = preprocess_fields(fields)?;
            let pvariant = PreprocessedVariant {
                fields,
                pmetas,
                pfields,
            };
            syn::Result::Ok((Some(()), vec![pvariant]))
        };

        let union_fields;

        let (variant, pvariants) = match &driver.data {
            syn::Data::Struct(ds) => pvariants_one(&ds.fields)?,
            syn::Data::Union(du) => {
                union_fields = syn::Fields::Named(du.fields.clone());
                pvariants_one(&union_fields)?
            }
            syn::Data::Enum(de) => (
                None,
                de.variants
                    .iter()
                    .map(|variant| {
                        let fields = &variant.fields;
                        let pmetas = preprocess_attrs(&variant.attrs)?;
                        let pfields = preprocess_fields(&variant.fields)?;
                        Ok(PreprocessedVariant {
                            fields,
                            pmetas,
                            pfields,
                        })
                    })
                    .collect::<Result<Vec<_>, syn::Error>>()?,
            ),
        };

        // `variant` is None in enums; otherwise it's Some(())
        // and here we convert it to the real WithinVariant for the fields.
        let variant = variant.map(|()| WithinVariant {
            variant: None, // not actually a variant
            fields: pvariants[0].fields,
            pmetas: &pvariants[0].pmetas,
            pfields: &pvariants[0].pfields,
        });

        let ctx = Context {
            top: &driver,
            template_crate,
            template_name,
            tmetas: &tmetas,
            field: None,
            variant: variant.as_ref(),
            pvariants: &pvariants,
            definitions: Default::default(),
            nesting_depth: 0,
            nesting_parent: None,
        };

        f(ctx)
    }

    /// Description of the whole expansion, suitable for `dbg` option, etc.
    pub fn expansion_description(&self) -> impl Display {
        let ident = &self.top.ident;
        if let Some(templ) = &self.template_name {
            format!(
                "derive-adhoc expansion of {} for {}",
                templ.to_token_stream(),
                ident,
            )
        } else {
            format!("derive-adhoc expansion, for {}", ident,)
        }
    }
}

impl Default for TokenAccumulator {
    fn default() -> Self {
        TokenAccumulator(Ok(TokenStream::new()))
    }
}

impl TokenAccumulator {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn with_tokens<R>(
        &mut self,
        f: impl FnOnce(&mut TokenStream) -> R,
    ) -> Option<R> {
        self.0.as_mut().ok().map(f)
    }
    pub fn append(&mut self, t: impl ToTokens) {
        self.with_tokens(|out| t.to_tokens(out));
    }
    pub fn tokens(self) -> syn::Result<TokenStream> {
        self.0
    }
    /// Appends `val`, via [`ToTokensPunctComposable`] or [`ToTokens`]
    pub fn append_maybe_punct_composable(
        &mut self,
        val: &(impl ToTokens + ToTokensPunctComposable),
        composable: bool,
    ) {
        self.with_tokens(|out| {
            if composable {
                val.to_tokens_punct_composable(out);
            } else {
                val.to_tokens(out);
            }
        });
    }
}

impl SubstParseContext for TokenAccumulator {
    type NotInPaste = ();
    type NotInBool = ();
    fn not_in_bool(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn not_in_paste(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }

    type BoolOnly = Void;

    type SpecialParseContext = ();
}

impl ExpansionOutput for TokenAccumulator {
    fn append_display<L: Display + Spanned + ToTokens>(&mut self, lit: &L) {
        self.append(lit)
    }
    fn append_identfrag_toks<I: IdentFrag>(
        &mut self,
        ident: &I,
    ) -> Result<(), I::BadIdent> {
        self.with_tokens(
            |out| ident.frag_to_tokens(out), //
        )
        .unwrap_or(Ok(()))
    }
    fn append_idpath<A, B, I>(
        &mut self,
        _te_span: Span,
        pre: A,
        ident: &I,
        post: B,
    ) -> Result<(), I::BadIdent>
    where
        A: FnOnce(&mut TokenAccumulator),
        B: FnOnce(&mut TokenAccumulator),
        I: IdentFrag,
    {
        pre(self);
        self.append_identfrag_toks(ident)?;
        post(self);
        Ok(())
    }
    fn append_syn_litstr(&mut self, lit: &syn::LitStr) {
        self.append(lit);
    }
    fn append_syn_type(&mut self, _te_span: Span, ty: &syn::Type) {
        self.append(ty);
    }
    fn append_tokens_with(
        &mut self,
        _not_in_paste: &(),
        f: impl FnOnce(&mut TokenAccumulator) -> syn::Result<()>,
    ) -> syn::Result<()> {
        f(self)
    }

    fn append_bool_only(&mut self, bool_only: &Self::BoolOnly) -> ! {
        void::unreachable(*bool_only)
    }

    fn record_error(&mut self, err: syn::Error) {
        if let Err(before) = &mut self.0 {
            before.combine(err);
        } else {
            self.0 = Err(err)
        }
    }

    fn default_subst_meta_as() -> SubstMetaAs<Self> {
        SubstMetaAs::tokens((), ())
    }
}
