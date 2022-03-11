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

-module(term_html).

-export([render/1, render/2]).

-export_type([options/0]).

-type options() ::
        #{return_binary => boolean()}.

-spec render(binary()) -> iodata().
render(Data) ->
  render(Data, #{}).

-spec render(binary(), options()) -> iodata().
render(Data, Options) ->
  Blocks = term_interpreter:run(Data),
  Output = render(Blocks, [], Options),
  case maps:get(return_binary, Options, false) of
    true ->
      iolist_to_binary(Output);
    false ->
      Output
  end.

-spec render([term_interpreter:block()], [iodata()], options()) -> [iodata()].
render([], Buf, _Options) ->
  lists:reverse(Buf);
render([Block = #{text := Text} | Blocks], Buf, Options) ->
  Attributes = block_attributes(Block),
  Elt =
    if
      map_size(Attributes) == 0 ->
        escape_text(Text);
      true ->
        elt(<<"span">>, Attributes, Text)
    end,
  render(Blocks, [Elt | Buf], Options).

-spec block_attributes(term_interpreter:block()) -> #{binary() := binary()}.
block_attributes(Block) ->
  maps:filter(fun (_, Value) -> byte_size(Value) > 0 end,
              #{<<"class">> => join($\s, block_css_classes(Block)),
                <<"style">> => join("; ", block_css_styles(Block))}).

-spec block_css_classes(term_interpreter:block()) -> [binary()].
block_css_classes(Block) ->
  F = fun
        (bold, true, Acc) ->
          [<<"tb">> | Acc];
        (italic, true, Acc) ->
          [<<"ti">> | Acc];
        (underline, true, Acc) ->
          [<<"tu">> | Acc];
        (reverse_video, true, Acc) ->
          [<<"trv">> | Acc];
        (crossed_out, true, Acc) ->
          [<<"tco">> | Acc];
        (foreground, Color, Acc) when is_atom(Color) ->
          [<<"tfg-", (atom_to_binary(Color))/binary>> | Acc];
        (background, Color, Acc) when is_atom(Color) ->
          [<<"tbg-", (atom_to_binary(Color))/binary>> | Acc];
        (_, _, Acc) ->
          Acc
      end,
  maps:fold(F, [], Block).

-spec block_css_styles(term_interpreter:block()) -> [binary()].
block_css_styles(Block) ->
  %% TODO 8 bit colors
  F = fun
        (foreground, {rgb, R, G, B}, Acc) ->
          Data = ["color: rgb(",
                  integer_to_binary(R), $,,
                  integer_to_binary(G), $,,
                  integer_to_binary(B),
                  $)],
          [iolist_to_binary(Data) | Acc];
        (background, {rgb, R, G, B}, Acc) ->
          Data = ["background-color: rgb(",
                  integer_to_binary(R), $,,
                  integer_to_binary(G), $,,
                  integer_to_binary(B),
                  $)],
          [iolist_to_binary(Data) | Acc];
        (_, _, Acc) ->
          Acc
      end,
  maps:fold(F, [], Block).

-spec elt(binary(), #{binary() := binary()}, binary()) -> iodata().
elt(Tag, Attributes, Text) ->
  AttributeData =
    if
      map_size(Attributes) == 0 ->
        [];
      true ->
        maps:fold(fun (K, V, Acc) ->
                      [[$\s, K, $=, $\", escape_attribute(V), $\"] | Acc]
                  end, [], Attributes)
    end,
  [$<, Tag, AttributeData, $>, escape_text(Text), $<, $/, Tag, $>].

-spec escape_text(binary()) -> binary().
escape_text(Data) ->
  escape_text(Data, <<>>).

-spec escape_text(binary(), binary()) -> binary().
escape_text(<<>>, Acc) ->
  Acc;
escape_text(<<$&, Data/binary>>, Acc) ->
  escape_text(Data, <<Acc/binary, "&amp;">>);
escape_text(<<$<, Data/binary>>, Acc) ->
  escape_text(Data, <<Acc/binary, "&lt;">>);
escape_text(<<$>, Data/binary>>, Acc) ->
  escape_text(Data, <<Acc/binary, "&gt;">>);
escape_text(<<C, Data/binary>>, Acc) ->
  escape_text(Data, <<Acc/binary, C>>).

-spec escape_attribute(binary()) -> binary().
escape_attribute(Data) ->
  escape_attribute(Data, <<>>).

-spec escape_attribute(binary(), binary()) -> binary().
escape_attribute(<<>>, Acc) ->
  Acc;
escape_attribute(<<$&, Data/binary>>, Acc) ->
  escape_attribute(Data, <<Acc/binary, "&amp;">>);
escape_attribute(<<$<, Data/binary>>, Acc) ->
  escape_attribute(Data, <<Acc/binary, "&lt;">>);
escape_attribute(<<$>, Data/binary>>, Acc) ->
  escape_attribute(Data, <<Acc/binary, "&gt;">>);
escape_attribute(<<$\", Data/binary>>, Acc) ->
  escape_attribute(Data, <<Acc/binary, "&quot;">>);
escape_attribute(<<$', Data/binary>>, Acc) ->
  escape_attribute(Data, <<Acc/binary, "&apos;">>);
escape_attribute(<<C, Data/binary>>, Acc) ->
  escape_attribute(Data, <<Acc/binary, C>>).

-spec join(iolist() | char(), iolist()) -> binary().
join(Separator, Data) ->
  iolist_to_binary(lists:join(Separator, Data)).
