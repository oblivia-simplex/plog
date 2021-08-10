# yaml

A [YAML](http://yaml.org) parser.

## Installation

Using SWI-Prolog 7 or later.

    ?- pack_install('https://github.com/honnix/yaml.git').

Source code available and pull requests accepted
[here](https://github.com/honnix/yaml).

@author Hongxin Liang <hxliang1982@gmail.com>

@license Apache License Version 2.0

## Examples

    :- module(ex, []).

    :- use_module(library(yaml/parser)).
    :- use_module(library(yaml/util)).
    :- use_module(library(yaml/serializer)).

    ex1 :-
        read_yaml('ex1.yaml', YAML),
        parse(YAML, PL),
        print_term(PL, []),
        serialize(PL, YAML1),
        write(YAML1),
        write_yaml('ex1_gen.yaml', YAML1).

## License

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
