use rustler::Term;
use erlang_term::RawTerm;
use erlang_term::Term as OwnedTerm;

#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif]
fn raw_decode<'a>(term: Term<'a>) -> String {
    let raw_term = term.decode::<RawTerm>();

    format!("{:?}", raw_term)
}

#[rustler::nif]
fn decode<'a>(term: Term<'a>) -> String {
    let owned_term = term.decode::<OwnedTerm>();

    format!("{:?}", owned_term)
}


rustler::init!("Elixir.RustlerTest", [add, decode, raw_decode]);
