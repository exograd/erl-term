%% Copyright (c) 2021 Exograd SAS.
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

-export([run/1]).

-export_type([block/0]).

-type state() ::
        #{sgr := sgr_state()}.

-type sgr_state() ::
        #{bold => boolean(),
          italic => boolean(),
          underline => boolean(),
          reverse_video => boolean(),
          crossed_out => boolean(),
          foreground => term:color(),
          background => term:color()}.

-type block() ::
        #{text := binary(),
          bold => boolean(),
          italic => boolean(),
          underline => boolean(),
          reverse_video => boolean(),
          crossed_out => boolean(),
          foreground => term:color(),
          background => term:color()}.

-spec run(binary()) -> iodata().
run(Data) ->
  {Stream, _} = term:decode(Data),
  State = #{sgr => #{}},
  run(Stream, [], State).

-spec run(term:stream(), [block()], state()) -> [block()].
run([], Acc, _State) ->
  lists:reverse(Acc);
run([{text, Text} | Stream], Acc, State = #{sgr := SGRState}) ->
  Block = maps:merge(#{text => Text},
                     maps:with([bold, italic, underline, reverse_video,
                                crossed_out, foreground, background],
                               SGRState)),
  run(Stream, [Block | Acc], State);
run([{sequence, {sgr, Parameters}} | Stream], Acc,
    State = #{sgr := SGRState}) ->
  SGRState2 = apply_sgr_parameters(Parameters, SGRState),
  run(Stream, Acc, State#{sgr => SGRState2}).

-spec apply_sgr_parameters([term:sgr_parameter()], sgr_state()) -> sgr_state().
apply_sgr_parameters(Parameters, State) ->
  lists:foldl(fun apply_sgr_parameter/2, State, Parameters).

-spec apply_sgr_parameter(term:sgr_parameter(), sgr_state()) -> sgr_state().
apply_sgr_parameter(default, _State) ->
  #{};
apply_sgr_parameter(bold, State) ->
  State#{bold => true};
apply_sgr_parameter(italic, State) ->
  State#{italic => true};
apply_sgr_parameter(underline, State) ->
  State#{underline => true};
apply_sgr_parameter(reverse_video, State) ->
  State#{reverse_video => true};
apply_sgr_parameter(crossed_out, State) ->
  State#{crossed_out => true};
apply_sgr_parameter({foreground, Color}, State) ->
  State#{foreground => Color};
apply_sgr_parameter({background, Color}, State) ->
  State#{background => Color};
apply_sgr_parameter(_, State) ->
  State.
