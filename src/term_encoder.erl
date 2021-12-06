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

-module(term_encoder).

-export([encode/1]).

-spec encode(term:stream()) -> term:text().
encode(Stream) ->
  encode(Stream, []).

-spec encode(term:stream(), term:text()) -> term:text().
encode([], Acc) ->
  lists:reverse(Acc);
encode([{text, Text} | Stream], Acc) ->
  encode(Stream, [Text | Acc]);
encode([{sequence, Sequence} | Stream], Acc) ->
  encode(Stream, [encode_sequence(Sequence) | Acc]).

-spec encode_sequence(term:sequence()) -> term:text().
encode_sequence({sgr, Parameters}) ->
  sgr_sequence(Parameters).

-spec sgr_sequence([term:sgr_parameter()]) -> term:text().
sgr_sequence(Parameters) ->
  ["\e[", lists:join($;, [sgr_parameter(P) || P <- Parameters]), $m].

-spec sgr_parameter(term:sgr_parameter()) -> term:text().
sgr_parameter(N) when is_integer(N) ->
  integer_to_list(N);
sgr_parameter({N, X}) ->
  [integer_to_list(N), ";5;", integer_to_list(X)];
sgr_parameter({N, R, G, B}) ->
  [integer_to_list(N), ";2;",
   integer_to_list(R), $;,
   integer_to_list(G), $;,
   integer_to_list(B)].
