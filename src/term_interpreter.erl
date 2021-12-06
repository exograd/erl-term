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

-export_type([event/0, graphic_state/0,
              state/0]).

-type event() ::
        {text, binary()}
      | {set_graphic_state, graphic_state()}
      | reset_graphic_state.

-type graphic_state() ::
        sgr_state().

-opaque state() ::
        #{buffer := binary(),
          sgr := sgr_state()}.

-type sgr_state() ::
        #{bold => boolean(),
          italic => boolean(),
          underline => boolean(),
          reverse_video => boolean(),
          crossed_out => boolean(),
          foreground => term:color(),
          background => term:color()}.

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
interpret_part({sequence, {sgr, Parameters}}, State = #{sgr := SGRState}) ->
  {Events, SGRState2} = update_sgr_state(SGRState, SGRState, Parameters, []),
  {Events, State#{sgr => SGRState2}};
interpret_part(_, State) ->
  {[], State}.

-spec update_sgr_state(sgr_state(), sgr_state(), [term:sgr_parameter()],
                       [event()]) ->
        {[event()], sgr_state()}.
update_sgr_state(State, State0, [], Events) when State =:= State0 ->
  {lists:reverse(Events), #{}};
update_sgr_state(State, _State0, [], Events) ->
  {lists:reverse([{set_graphic_state, State} | Events]), State};
update_sgr_state(State, State0, [default | Parameters], Events) when
    State =:= State0 ->
  update_sgr_state(#{}, #{}, Parameters, Events);
update_sgr_state(State, _State0, [default | Parameters], Events) ->
  update_sgr_state(#{}, #{}, Parameters,
                   [reset_graphic_state, {set_graphic_state, State} | Events]);
update_sgr_state(State, State0, [Parameter | Parameters], Events) ->
  State2 =
    case Parameter of
      bold -> State#{bold => true};
      italic -> State#{italic => true};
      underline -> State#{underline => true};
      reverse_video -> State#{reverse_video => true};
      crossed_out -> State#{crossed_out => true};
      {foreground, Color} -> State#{foreground => Color};
      {background, Color} -> State#{background => Color};
      _ -> State
    end,
  update_sgr_state(State2, State0, Parameters, Events).
