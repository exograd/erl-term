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

-module(term_ecma48_decoder).

-export([decode/1]).

-spec decode(binary()) -> term_ecma48:document().
decode(Data) ->
  decode(Data, undefined, []).

-spec decode(binary(), undefined | binary(), term_ecma48:document()) ->
        term_ecma48:document().
decode(<<>>, _, Document) ->
  %% We ignore any started sequence data at the end of the document
  lists:reverse(Document);
decode(<<"\e[", Data/binary>>, undefined, Document) ->
  %% Start a new sequence
  decode(Data, <<"\e[">>, Document);
decode(Data = <<"\e[", _/binary>>, SequenceData, Document) ->
  %% SequenceData was never terminated, insert it as text
  decode(Data, undefined, [{text, SequenceData} | Document]);
decode(Data, undefined, Document) ->
  case binary:match(Data, <<"\e[">>) of
    {Start, _} ->
      Text = binary:part(Data, 0, Start),
      Data2 = binary:part(Data, Start, byte_size(Data) - Start),
      decode(Data2, undefined, [{text, Text} | Document]);
    nomatch ->
      %% No more sequence
      decode(<<>>, undefined, [{text, Data} | Document])
  end;
decode(<<"m", Data/binary>>, SequenceData, Document) ->
  %% End of an SGR sequence
  decode_sgr_sequence(SequenceData, Data, Document);
decode(<<C, Data/binary>>, SequenceData, Document) ->
  %% More data for the current sequence
  decode(Data, <<SequenceData/binary, C>>, Document).

-spec decode_sgr_sequence(binary(), binary(), term_ecma48:document()) ->
        term_ecma48:document().
decode_sgr_sequence(Data = <<"\e[", ParameterData/binary>>,
                    NextData, Document) ->
  case decode_sgr_parameter_values(ParameterData, []) of
    {ok, Values} ->
      case decode_sgr_parameters(Values, []) of
        {ok, Parameters} ->
          decode(NextData, undefined, [{sequence, {sgr, Parameters}} | Document]);
        error ->
          decode(NextData, undefined, [{text, Data} | Document])
      end;
    error ->
      decode(NextData, undefined, [{text, Data} | Document])
  end.

-spec decode_sgr_parameter_values(binary(), [non_neg_integer()]) ->
        {ok, [non_neg_integer()]} | error.
decode_sgr_parameter_values(<<>>, Codes) ->
  {ok, lists:reverse(Codes)};
decode_sgr_parameter_values(Data, Codes) ->
  [CodeString, Data2] =
    case binary:split(Data, <<";">>) of
      [CS, D2] -> [CS, D2];
      [CS] -> [CS, <<>>]
    end,
  try
    erlang:binary_to_integer(CodeString)
  of
    Code when Code >= 0, Code < 256 ->
      decode_sgr_parameter_values(Data2, [Code | Codes]);
    _ ->
      error
  catch
    error:_ ->
      error
  end.

-spec decode_sgr_parameters([non_neg_integer()],
                            [term_ecma48:sgr_parameter()]) ->
        {ok, [term_ecma48:sgr_parameter()]} | error.
decode_sgr_parameters([], Parameters) ->
  {ok, lists:reverse(Parameters)};
decode_sgr_parameters([Code | Codes], Parameters) when Code < 38 ->
  decode_sgr_parameters(Codes, [Code | Parameters]);
decode_sgr_parameters([38 | Codes], Parameters) ->
  case Codes of
    [2, R, G, B | Codes2] ->
      decode_sgr_parameters(Codes2, [{38, R, G, B} | Parameters]);
    [5, N | Codes2] ->
      decode_sgr_parameters(Codes2, [{38, N} | Parameters]);
    _ ->
      error
  end;
decode_sgr_parameters([Code | Codes], Parameters) when Code < 48 ->
  decode_sgr_parameters(Codes, [Code | Parameters]);
decode_sgr_parameters([Code | Codes], Parameters) when Code < 66 ->
  decode_sgr_parameters(Codes, [Code | Parameters]);
decode_sgr_parameters([_ | _], _) ->
  error.
