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

-spec encode(term:stream()) -> iodata().
encode(Stream) ->
  encode(Stream, []).

-spec encode(term:stream(), iodata()) -> iodata().
encode([], Acc) ->
  lists:reverse(Acc);
encode([{text, Text} | Stream], Acc) ->
  encode(Stream, [Text | Acc]);
encode([{sequence, Sequence} | Stream], Acc) ->
  encode(Stream, [encode_sequence(Sequence) | Acc]).

-spec encode_sequence(term:sequence()) -> iodata().
encode_sequence({sgr, Parameters}) ->
  sgr_sequence(Parameters).

-spec sgr_sequence([term:sgr_parameter()]) -> iodata().
sgr_sequence(Parameters) ->
  Codes = lists:flatten([sgr_parameter_codes(P) || P <- Parameters]),
  ["\e[", lists:join($;, [integer_to_list(C) || C <- Codes]), $m].

-spec sgr_parameter_codes(term:sgr_parameter()) -> 0..255 | [0..255].
sgr_parameter_codes(default) -> 0;
sgr_parameter_codes(bold) -> 1;
sgr_parameter_codes(italic) -> 3;
sgr_parameter_codes(underline) -> 4;
sgr_parameter_codes(reverse_video) -> 7;
sgr_parameter_codes(crossed_out) -> 9;
sgr_parameter_codes({foreground, black}) -> 30;
sgr_parameter_codes({foreground, red}) -> 31;
sgr_parameter_codes({foreground, green}) -> 32;
sgr_parameter_codes({foreground, yellow}) -> 33;
sgr_parameter_codes({foreground, blue}) -> 34;
sgr_parameter_codes({foreground, magenta}) -> 35;
sgr_parameter_codes({foreground, cyan}) -> 36;
sgr_parameter_codes({foreground, white}) -> 37;
sgr_parameter_codes({foreground, {rgb, R, G, B}}) -> [38, 2, R, G, B];
sgr_parameter_codes({foreground, {'8bit', N}}) -> [38, 5, N];
sgr_parameter_codes({background, black}) -> 40;
sgr_parameter_codes({background, red}) -> 41;
sgr_parameter_codes({background, green}) -> 42;
sgr_parameter_codes({background, yellow}) -> 43;
sgr_parameter_codes({background, blue}) -> 44;
sgr_parameter_codes({background, magenta}) -> 45;
sgr_parameter_codes({background, cyan}) -> 46;
sgr_parameter_codes({background, white}) -> 47;
sgr_parameter_codes({background, {rgb, R, G, B}}) -> [48, 2, R, G, B];
sgr_parameter_codes({background, {'8bit', N}}) -> [48, 5, N];
sgr_parameter_codes(Code) when is_integer(Code) -> Code.
