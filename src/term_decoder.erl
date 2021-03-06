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

-module(term_decoder).

-export([decode/1]).

-spec decode(binary()) -> {term:stream(), binary()}.
decode(Data) ->
  decode(Data, undefined, []).

-spec decode(binary(), undefined | binary(), term:stream()) ->
        {term:stream(), binary()}.
decode(<<>>, undefined, Stream) ->
  {lists:reverse(Stream), <<>>};
decode(<<>>, SequenceData, Stream) ->
  {lists:reverse(Stream), SequenceData};
decode(<<"\e[", Data/binary>>, undefined, Stream) ->
  %% Start a new sequence
  decode(Data, <<"\e[">>, Stream);
decode(Data = <<"\e[", _/binary>>, SequenceData, Stream) ->
  %% SequenceData was never terminated, insert it as text
  decode(Data, undefined, [{text, SequenceData} | Stream]);
decode(Data, undefined, Stream) ->
  case binary:match(Data, <<"\e[">>) of
    {Start, _} ->
      Text = binary:part(Data, 0, Start),
      Data2 = binary:part(Data, Start, byte_size(Data) - Start),
      decode(Data2, undefined, [{text, Text} | Stream]);
    nomatch ->
      %% No more sequence
      decode(<<>>, undefined, [{text, Data} | Stream])
  end;
decode(<<"m", Data/binary>>, SequenceData, Stream) ->
  %% End of an SGR sequence
  decode_sgr_sequence(SequenceData, Data, Stream);
decode(<<C, Data/binary>>, SequenceData, Stream) ->
  %% More data for the current sequence
  decode(Data, <<SequenceData/binary, C>>, Stream).

-spec decode_sgr_sequence(binary(), binary(), term:stream()) ->
        {term:stream(), binary()}.
decode_sgr_sequence(Data = <<"\e[", ParameterData/binary>>,
                    NextData, Stream) ->
  case decode_sgr_parameter_values(ParameterData, []) of
    {ok, Values} ->
      case decode_sgr_parameters(Values, []) of
        {ok, Parameters} ->
          decode(NextData, undefined,
                 [{sequence, {sgr, Parameters}} | Stream]);
        error ->
          decode(NextData, undefined, [{text, Data} | Stream])
      end;
    error ->
      decode(NextData, undefined, [{text, Data} | Stream])
  end.

-spec decode_sgr_parameter_values(binary(), [0..255]) ->
        {ok, [0..255]} | error.
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

-spec decode_sgr_parameters([0..255], [term:sgr_parameter()]) ->
        {ok, [term:sgr_parameter()]} | error.
decode_sgr_parameters([], Parameters) ->
  {ok, lists:reverse(Parameters)};
decode_sgr_parameters([Code | Codes], Parameters) when Code < 38 ->
  Parameter =
    case Code of
      0 -> default;
      1 -> bold;
      3 -> italic;
      4 -> underline;
      7 -> reverse_video;
      9 -> crossed_out;
      30 -> {foreground, black};
      31 -> {foreground, red};
      32 -> {foreground, green};
      33 -> {foreground, yellow};
      34 -> {foreground, blue};
      35 -> {foreground, magenta};
      36 -> {foreground, cyan};
      37 -> {foreground, white};
      _ -> Code
    end,
  decode_sgr_parameters(Codes, [Parameter | Parameters]);
decode_sgr_parameters([38 | Codes], Parameters) ->
  case Codes of
    [2, R, G, B | Codes2] ->
      decode_sgr_parameters(Codes2, [{foreground, {rgb, R, G, B}} | Parameters]);
    [5, N | Codes2] ->
      decode_sgr_parameters(Codes2, [{foreground, {'8bit', N}} | Parameters]);
    _ ->
      error
  end;
decode_sgr_parameters([Code | Codes], Parameters) when Code < 48 ->
  Parameter =
    case Code of
      40 -> {background, black};
      41 -> {background, red};
      42 -> {background, green};
      43 -> {background, yellow};
      44 -> {background, blue};
      45 -> {background, magenta};
      46 -> {background, cyan};
      47 -> {background, white};
      _ -> Code
    end,
  decode_sgr_parameters(Codes, [Parameter | Parameters]);
decode_sgr_parameters([48 | Codes], Parameters) ->
  case Codes of
    [2, R, G, B | Codes2] ->
      decode_sgr_parameters(Codes2, [{background, {rgb, R, G, B}} | Parameters]);
    [5, N | Codes2] ->
      decode_sgr_parameters(Codes2, [{background, {'8bit', N}} | Parameters]);
    _ ->
      error
  end;
decode_sgr_parameters([Code | Codes], Parameters) when Code < 66 ->
  decode_sgr_parameters(Codes, [Code | Parameters]);
decode_sgr_parameters([_ | _], _) ->
  error.
