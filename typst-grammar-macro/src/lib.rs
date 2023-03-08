use proc_macro::TokenStream;
use quote::quote;

#[proc_macro]
pub fn typst_grammar(item: TokenStream) -> TokenStream {
    let input = item.into_iter();

    quote!().into()
}
