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

-module(term_decoder_tests).

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
  Decode = fun term_decoder:decode/1,
  [?_assertEqual({[], <<>>},
                 Decode(<<"">>)),
   ?_assertEqual({[{text, <<"foo">>}], <<>>},
                 Decode(<<"foo">>)),
   ?_assertEqual({[{sequence, {sgr, [bold]}},
                   {text, <<"foo">>},
                   {sequence, {sgr, [default]}}],
                  <<>>},
                 Decode(<<"\e[1mfoo\e[0m">>)),
   ?_assertEqual({[{text, <<"foo">>},
                   {sequence, {sgr, [crossed_out]}}],
                  <<"\e[">>},
                 Decode(<<"foo\e[9m\e[">>)),
   ?_assertEqual({[{sequence, {sgr, [italic, reverse_video,
                                     {foreground, red},
                                     {background, white}]}},
                   {text, <<"foo">>},
                   {sequence, {sgr, [default]}},
                   {text, <<" bar">>}],
                  <<>>},
                 Decode(<<"\e[3;7;31;47mfoo\e[0m bar">>)),
   ?_assertEqual({[{sequence, {sgr, [{foreground, {rgb, 20, 40, 80}}]}},
                   {text, <<"foo">>},
                   {sequence, {sgr, [{background, {'8bit', 123}}]}},
                   {text, <<"bar">>}],
                  <<>>},
                 Decode(<<"\e[38;2;20;40;80mfoo\e[48;5;123mbar">>))].
