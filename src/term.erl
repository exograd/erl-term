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

-module(term).

-export([with_text_attributes/2]).

-export_type([text/0, color/0]).

-type text() :: unicode:chardata().

-type color() :: black | red | green | yellow | blue | magenta | cyan | white
               | {'8bit', 0..255}
               | {rgb, 0..255, 0..255, 0..255}.

-type text_attribute() ::
        bold | italic | underline | reverse_video | crossed_out
      | {foreground, color()}
      | {background, color()}.

-spec with_text_attributes(text(), [text_attribute()]) -> text().
with_text_attributes(Text, Attributes) ->
  Parameters = [text_attribute(A) || A <- Attributes],
  term_ecma48:encode([{sequence, {sgr, Parameters}},
                      {text, Text},
                      {sequence, {sgr, [0]}}]).

-spec text_attribute(text_attribute()) -> term_ecma48:sgr_parameter().
text_attribute(bold) -> 1;
text_attribute(italic) -> 3;
text_attribute(underline) -> 4;
text_attribute(reverse_video) -> 7;
text_attribute(crossed_out) -> 9;
text_attribute({foreground, black}) -> 30;
text_attribute({foreground, red}) -> 31;
text_attribute({foreground, green}) -> 32;
text_attribute({foreground, yellow}) -> 33;
text_attribute({foreground, blue}) -> 34;
text_attribute({foreground, magenta}) -> 35;
text_attribute({foreground, cyan}) -> 36;
text_attribute({foreground, white}) -> 37;
text_attribute({foreground, {'8bit', N}}) -> {38, N};
text_attribute({foreground, {rgb, R, G, B}}) -> {38, R, G, B};
text_attribute({background, black}) -> 40;
text_attribute({background, red}) -> 41;
text_attribute({background, green}) -> 42;
text_attribute({background, yellow}) -> 43;
text_attribute({background, blue}) -> 44;
text_attribute({background, magenta}) -> 45;
text_attribute({background, cyan}) -> 46;
text_attribute({background, white}) -> 47;
text_attribute({background, {'8bit', N}}) -> {48, N};
text_attribute({background, {rgb, R, G, B}}) -> {48, R, G, B}.
