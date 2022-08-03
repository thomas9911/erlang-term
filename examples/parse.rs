use erlang_term::Term;

const HELP_TEXT: &'static str = r###"parse elixir binary to Term

example: 
    cargo run --example parse "$(elixir -e ":erlang.term_to_binary(%{{ok: 15}}) |> IO.inspect()")"

    cargo run --example parse "<<131, 97, 4>>"
"###;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    match print_help(args.get(0).unwrap_or(&String::from("help"))) {
        false => (),
        true => return,
    };

    let text = args.join(", ");

    let x: Vec<u8> = text
        .trim_start_matches("<<")
        .trim_end_matches(">>")
        .split(",")
        .map(|x| x.trim().parse().expect("input is not a elixir binary"))
        .collect();

    let term = Term::from_bytes(&x);

    println!("{:?}", term);
}

fn print_help(input: &str) -> bool {
    if ["help", "-h", "--help"].contains(&input) {
        println!("{}", HELP_TEXT);

        return true;
    }
    false
}
