use erlang_term::{RawTerm, Term};

// in Elixir:
// [
//     {0, true},
//     [:jR, 1.0],
//     [],
//     {#Reference<0.3700278564.2761949185.135793>},
//     %{
//         {#Reference<0.3700278564.2761949185.135794>} => %{
//         3 => #Reference<0.3700278564.2761949185.135795>,
//         :o => true
//         },
//         {2, :XdT8, true} => [-0.21875, false]
//     },
//     #Reference<0.3700278564.2761949185.135796>,
//     {%{-7 => #Reference<0.3700278564.2761949185.135797>}, [0.328125, ""]},
//     -0.984375,
//     [
//         [#Reference<0.3700278564.2761949185.135801>],
//         %{
//         #Reference<0.3700278564.2761949185.135798> => #Reference<0.3700278564.2761949185.135799>,
//         #Reference<0.3700278564.2761949185.135800> => false
//         },
//         %{9 => true}
//     ],
//     {912.4443359375, :RCJb5, <<87, 16, 45, 36, 10, 71, 200>>}
// ]
const FILE1: &[u8] = &[
    131, 108, 0, 0, 0, 10, 104, 2, 97, 0, 100, 0, 4, 116, 114, 117, 101, 108, 0, 0, 0, 2, 100, 0,
    2, 106, 82, 70, 63, 240, 0, 0, 0, 0, 0, 0, 106, 106, 104, 1, 90, 0, 3, 100, 0, 13, 110, 111,
    110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 0, 2, 18, 113, 164, 160, 0,
    1, 220, 141, 197, 36, 116, 0, 0, 0, 2, 104, 1, 90, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100,
    101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 0, 2, 18, 114, 164, 160, 0, 1, 220, 141,
    197, 36, 116, 0, 0, 0, 2, 97, 3, 90, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110,
    111, 104, 111, 115, 116, 0, 0, 0, 0, 0, 2, 18, 115, 164, 160, 0, 1, 220, 141, 197, 36, 100, 0,
    1, 111, 100, 0, 4, 116, 114, 117, 101, 104, 3, 97, 2, 100, 0, 4, 88, 100, 84, 56, 100, 0, 4,
    116, 114, 117, 101, 108, 0, 0, 0, 2, 70, 191, 204, 0, 0, 0, 0, 0, 0, 100, 0, 5, 102, 97, 108,
    115, 101, 106, 90, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115,
    116, 0, 0, 0, 0, 0, 2, 18, 116, 164, 160, 0, 1, 220, 141, 197, 36, 104, 2, 116, 0, 0, 0, 1, 98,
    255, 255, 255, 249, 90, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111,
    115, 116, 0, 0, 0, 0, 0, 2, 18, 117, 164, 160, 0, 1, 220, 141, 197, 36, 108, 0, 0, 0, 2, 70,
    63, 213, 0, 0, 0, 0, 0, 0, 109, 0, 0, 0, 0, 106, 70, 191, 239, 128, 0, 0, 0, 0, 0, 108, 0, 0,
    0, 3, 108, 0, 0, 0, 1, 90, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104,
    111, 115, 116, 0, 0, 0, 0, 0, 2, 18, 121, 164, 160, 0, 1, 220, 141, 197, 36, 106, 116, 0, 0, 0,
    2, 90, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0,
    0, 0, 0, 2, 18, 118, 164, 160, 0, 1, 220, 141, 197, 36, 90, 0, 3, 100, 0, 13, 110, 111, 110,
    111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 0, 2, 18, 119, 164, 160, 0, 1,
    220, 141, 197, 36, 90, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111,
    115, 116, 0, 0, 0, 0, 0, 2, 18, 120, 164, 160, 0, 1, 220, 141, 197, 36, 100, 0, 5, 102, 97,
    108, 115, 101, 116, 0, 0, 0, 1, 97, 9, 100, 0, 4, 116, 114, 117, 101, 106, 104, 3, 70, 64, 140,
    131, 142, 0, 0, 0, 0, 100, 0, 5, 82, 67, 74, 98, 53, 109, 0, 0, 0, 7, 87, 16, 45, 36, 10, 71,
    200, 106,
];

// in Elixir:
// [
//   [],
//   %{#Reference<0.3700278564.2761949185.136089> => 2},
//   [-2.125],
//   [:d9, 10.0625],
//   :o81f,
//   %{-7.953125 => :t, :BX => 1, #Reference<0.3700278564.2761949185.136090> => :_},
//   %{
//     {%{FRd5T: true}} => {},
//     {#Reference<0.3700278564.2761949185.136091>, true} => [:q]
//   },
//   %{{%{}, %{}} => [%{}]},
//   [],
//   %{
//     %{
//       6 => <<152, 194, 130, 3, 193, 5, 219, 93, 122>>,
//       #Reference<0.3700278564.2761949185.136092> => true
//     } => 289.8544921875
//   }
// ]
const FILE2: &[u8] = &[
    131, 108, 0, 0, 0, 10, 106, 116, 0, 0, 0, 1, 90, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100,
    101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 0, 2, 19, 153, 164, 160, 0, 1, 220, 141,
    197, 36, 97, 2, 108, 0, 0, 0, 1, 70, 192, 1, 0, 0, 0, 0, 0, 0, 106, 108, 0, 0, 0, 2, 100, 0, 2,
    100, 57, 70, 64, 36, 32, 0, 0, 0, 0, 0, 106, 100, 0, 4, 111, 56, 49, 102, 116, 0, 0, 0, 3, 70,
    192, 31, 208, 0, 0, 0, 0, 0, 100, 0, 1, 116, 100, 0, 2, 66, 88, 97, 1, 90, 0, 3, 100, 0, 13,
    110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 0, 2, 19, 154, 164,
    160, 0, 1, 220, 141, 197, 36, 100, 0, 1, 95, 116, 0, 0, 0, 2, 104, 1, 116, 0, 0, 0, 1, 100, 0,
    5, 70, 82, 100, 53, 84, 100, 0, 4, 116, 114, 117, 101, 104, 0, 104, 2, 90, 0, 3, 100, 0, 13,
    110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 0, 2, 19, 155, 164,
    160, 0, 1, 220, 141, 197, 36, 100, 0, 4, 116, 114, 117, 101, 108, 0, 0, 0, 1, 100, 0, 1, 113,
    106, 116, 0, 0, 0, 1, 104, 2, 116, 0, 0, 0, 0, 116, 0, 0, 0, 0, 108, 0, 0, 0, 1, 116, 0, 0, 0,
    0, 106, 106, 116, 0, 0, 0, 1, 116, 0, 0, 0, 2, 97, 6, 109, 0, 0, 0, 9, 152, 194, 130, 3, 193,
    5, 219, 93, 122, 90, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111,
    115, 116, 0, 0, 0, 0, 0, 2, 19, 156, 164, 160, 0, 1, 220, 141, 197, 36, 100, 0, 4, 116, 114,
    117, 101, 70, 64, 114, 29, 172, 0, 0, 0, 0, 106,
];

fn main() {
    if std::env::args().any(|x| x == "--help") {
        println!("small example how to filter etf binaries");
        println!("optionally you can give the '--json' flag to output in a more readable format");
        return;
    }

    let mut json_flag = false;
    if std::env::args().any(|x| x == "--json") {
        json_flag = true;
    }

    // load your files
    let files = &[FILE1, FILE2];

    for file in files {
        let loaded_term = RawTerm::from_bytes(file).expect("assume bytes are valid etf");

        // apply filter
        if let Some(filtered_data) = filter(loaded_term, json_flag) {
            if json_flag {
                // print as json
                let term = Term::from(filtered_data);
                print_json(term);
            } else {
                // store the bytes
                println!("{:?}", filtered_data.to_bytes());
            }
        }
    }
}

fn filter_vec(list: Vec<RawTerm>, json_flag: bool) -> Vec<RawTerm> {
    list.into_iter()
        .filter_map(|term| filter(term, json_flag))
        .collect()
}

fn replace_invalid(list: Vec<RawTerm>, json_flag: bool) -> Vec<RawTerm> {
    list.into_iter()
        .map(|term| {
            if let Some(filtered_term) = filter(term, json_flag) {
                filtered_term
            } else {
                // or put RawTerm::Nil
                RawTerm::SmallAtom("nil".to_string())
            }
        })
        .collect()
}

fn filter(term: RawTerm, json_flag: bool) -> Option<RawTerm> {
    match term {
        RawTerm::List(list) => Some(RawTerm::List(filter_vec(list, json_flag))),
        RawTerm::SmallTuple(list) => Some(RawTerm::SmallTuple(replace_invalid(list, json_flag))),
        RawTerm::LargeTuple(list) => Some(RawTerm::LargeTuple(replace_invalid(list, json_flag))),
        RawTerm::Map(tuple_list) => {
            let mut new_map = Vec::new();

            for (key, value) in tuple_list {
                let filtered_key = filter(key, json_flag);
                let filtered_value = filter(value, json_flag);
                if filtered_key.is_none() | filtered_value.is_none() {
                    continue;
                }
                let new_key = filtered_key.unwrap();
                if json_flag {
                    // json only allows string keys
                    if !new_key.is_string_like() {
                        continue;
                    }
                }

                new_map.push((new_key, filtered_value.unwrap()));
            }

            Some(RawTerm::Map(new_map))
        }
        RawTerm::SmallInt(_)
        | RawTerm::Int(_)
        | RawTerm::Atom(_)
        | RawTerm::SmallAtom(_)
        | RawTerm::AtomDeprecated(_)
        | RawTerm::SmallAtomDeprecated(_)
        | RawTerm::Float(_)
        | RawTerm::Binary(_)
        | RawTerm::String(_)
        | RawTerm::Nil => Some(term),
        _ => None,
    }
}

#[cfg(feature = "serde_impl")]
fn print_json(term: Term) {
    println!("{}", serde_json::to_string_pretty(&term).unwrap())
}

#[cfg(not(feature = "serde_impl"))]
fn print_json(term: Term) {
    println!("printing without serde_impl feature will just debug print the term.");
    println!("enable json printing by calling the example with `--features=serde_impl`");
    println!("{:#?}", term)
}
