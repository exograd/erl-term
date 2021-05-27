%% Copyright (c) 2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(term_ecma48).

-export([sgr_sequence/1]).

-export_type([sgr_parameter/0]).

-type sgr_parameter() ::
        0..29
      | 30..37 | {38, 0..255} | {38, 0..255, 0..255, 0..255} | 39
      | 40..47 | {48, 0..255} | {48, 0..255, 0..255, 0..255} | 49
      | 50..65.

-spec sgr_sequence([sgr_parameter()]) -> term:text().
sgr_sequence(Parameters) ->
  ["\e[", lists:join($;, [sgr_parameter(P) || P <- Parameters]), $m].

-spec sgr_parameter(sgr_parameter()) -> term:text().
sgr_parameter(N) when is_integer(N) ->
  integer_to_list(N);
sgr_parameter({N, X}) ->
  [integer_to_list(N), ";5;", integer_to_list(X)];
sgr_parameter({N, R, G, B}) ->
  [integer_to_list(N), ";2",
   $;, integer_to_list(R),
   $;, integer_to_list(G),
   $;, integer_to_list(B)].
