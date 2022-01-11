//-------------------------------------------------------------------
// @author yangcancai

// Copyright (c) 2021 by yangcancai(yangcancai0112@gmail.com), All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//       https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

// @doc
//
// @end
// Created : 2022-01-11T07:29:43+00:00
//-------------------------------------------------------------------
#[macro_use]
extern crate bencher;
extern crate erlang_term;
// extern crate rand;
use bencher::Bencher;
use erlang_term::read_binary;
use erlang_term::RawTerm;

fn from_bytes(bench: &mut Bencher) {
    let large_tuple = read_binary("bins/large_tuple.bin").unwrap();
    let mut tuple = vec![];
    for i in 1..401 {
        if i < 256 {
            tuple.push(RawTerm::SmallInt(i as u8));
        } else {
            tuple.push(RawTerm::Int(i));
        }
    }
    let tuple = RawTerm::SmallTuple(vec![
        RawTerm::AtomDeprecated("row_data".into()),
        RawTerm::Binary(b"kkk".to_vec()),
        RawTerm::LargeTuple(tuple),
        RawTerm::SmallInt(10),
    ]);

    bench.iter(|| {
        let out = RawTerm::from_bytes(&large_tuple).unwrap();
        assert_eq!(tuple, out);
    })
}
benchmark_group!(benches, from_bytes);
benchmark_main!(benches);
