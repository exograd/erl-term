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

-module(term_interpreter).

-export([init/0, update/2]).

-export_type([event/0, state/0]).

-type event() ::
        {text, binary()}.

-opaque state() ::
        #{buffer := binary(),
          sgr := sgr_state()}.

-type sgr_state() ::
        #{bold => boolean(),
          italic => boolean(),
          underline => boolean(),
          reverse_video => boolean(),
          crossed_out => boolean(),
          foreground_color => term:color(),
          background_color => term:color()}.

-spec init() -> state().
init() ->
  #{buffer => <<>>,
    sgr => #{}}.

-spec update(binary(), state()) -> {[event()], state()}.
update(Data, State = #{buffer := Buf}) ->
  {Stream, Rest} = term:decode(<<Buf/binary, Data/binary>>),
  interpret(Stream, [], State#{buffer => Rest}).

-spec interpret(term:stream(), [event()], state()) -> {[event()], state()}.
interpret([], Events, State) ->
  {lists:reverse(Events), State};
interpret([Part | Stream], Events, State) ->
  {PartEvents, State2} = interpret_part(Part, State),
  Events2 = lists:append(lists:reverse(PartEvents), Events),
  interpret(Stream, Events2, State2).

-spec interpret_part(term:part(), state()) -> {[event()], state()}.
interpret_part({text, Text}, State) ->
  {[{text, Text}], State};
%% TODO Other parts
interpret_part(_, State) ->
  {[], State}.
