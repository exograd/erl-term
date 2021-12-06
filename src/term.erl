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

-export([encode/1, decode/1]).

-export_type([text/0, stream/0, part/0, sequence/0,
              sgr_parameter/0, color/0]).

-type text() ::
        unicode:chardata().

-type stream() ::
        [part()].

-type part() ::
        {text, term:text()}
      | {sequence, sequence()}.

-type sequence() ::
        {sgr, [sgr_parameter()]}.

-type sgr_parameter() ::
        default
      | bold | italic | underline | reverse_video | crossed_out
      | {foreground, color()}
      | {background, color()}
      | 0..255. % only supported by the encoder

-type color() ::
        black | red | green | yellow | blue | magenta | cyan | white
      | {rgb, 0..255, 0..255, 0..255}
      | {'8bit', 0..255}.

-spec encode(stream()) -> term:text().
encode(Stream) ->
  term_encoder:encode(Stream).

-spec decode(binary()) -> {term:stream(), binary()}.
decode(Data) ->
  term_decoder:decode(Data).
