%% -------------------------------------------------------------------
%%
%%
%% Copyright (c) 2012 Mobile Interactive Group a Velti company. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(util_gsm0338).

-export([init/0,
         encode/2,
         encode_parts/3,
         decode/2,
         special_pad_start/1]).

-include("util.hrl").
-include("util_gsm0338.hrl").

-record(tables, {char_to_bstr :: dict(),
                 escaped_gsm_to_char :: dict(),
                 gsm_to_char :: dict()}).


%% @doc Init the conversion tables.
%% @spec init() -> ok
-spec init() -> ok.
init() ->
    CharsBstrs = [{Char, <<GsmChar:7>>} || {Char, GsmChar} <- ?UNICODE_TO_GSM],
    % NOTE: The escaped bitstrings are reversed for quicker translation
    CharsEscapedBstrs = [{Char, <<GsmChar:7, ?GSM_ESCAPE:7>>} || {Char, GsmChar} <- ?UNICODE_TO_ESCAPED_GSM],
    CharToBstr = dict:from_list(CharsBstrs ++ CharsEscapedBstrs),
    EscapedGsmToChar = dict:from_list(?ESCAPED_GSM_TO_UNICODE),
    GsmToChar = dict:from_list(?GSM_TO_UNICODE),
    Tables = #tables{char_to_bstr = CharToBstr,
                escaped_gsm_to_char = EscapedGsmToChar,
                gsm_to_char = GsmToChar},
    application:set_env(util_gsm0338, encoding_tables, Tables).

%% @doc Convert a Unicode string to a GSM 03.38 binary, replacing unconvertable characters.
%%      Also returns the number of characters in the binary.
%%      Fallback must be a Unicode representation of a GSM 03.38 character.
%% @spec encode(string(), char()) -> {binary(), integer()}
-spec encode(string(), char()) -> {binary(), integer()}.
encode(Str, Fallback) ->
    {ok, Tables} = application:get_env(util_gsm0338, encoding_tables),
    pack(translate(Str, Fallback, Tables#tables.char_to_bstr)).

%% @doc Convert a Unicode string to a number of GSM 03.38 binaries, each no larger than MaxPartSize bytes.
%%      Also returns the number of characters in each binary.
%%      Fallback must be a Unicode representation of a GSM 03.38 character.
%% @spec encode_parts(string(), char(), integer()) -> [{binary(), integer()}]
-spec encode_parts(string(), char(), integer()) -> [{binary(), integer()}].
encode_parts(Str, Fallback, MaxPartSize) ->
    {ok, Tables} = application:get_env(util_gsm0338, encoding_tables),
    GsmBstrs = translate(Str, Fallback, Tables#tables.char_to_bstr),
    GsmBstrGroups = util:group_bitstrings(GsmBstrs, MaxPartSize),
    lists:map(fun pack/1, GsmBstrGroups).

%% @doc Convert a GSM 03.38 string or binary to a Unicode string, replacing unconvertable characters.
%%      Fallback must be a Unicode character.
%% @spec decode(string() | binary(), char()) -> string()

-spec decode(string() | binary(), char()) -> string().
decode(GsmBin, Fallback) when is_binary(GsmBin) ->
    decode(binary_to_list(GsmBin), Fallback);
decode(GsmStr, Fallback) ->
    decode(list_to_binary(lists:reverse(GsmStr)), Fallback, []).

-spec decode(bitstring(), char(), [string()]) -> string().
decode(TotalGsmBits, Fallback, Str) ->
    {ok, Tables} = application:get_env(util_gsm0338, encoding_tables),
    EscapedBitSize = bit_size(TotalGsmBits) - 14,
    BitSize = bit_size(TotalGsmBits) - 7,
    case TotalGsmBits of
        <<GsmBits:EscapedBitSize/bitstring, GsmChar:7, ?GSM_ESCAPE:7>> ->
            case dict:find(GsmChar, Tables#tables.escaped_gsm_to_char) of
                {ok, Char} ->
                    decode(GsmBits, Fallback, [Char | Str]);
                error ->
                    Char = util:dict_fetch(GsmChar, Tables#tables.gsm_to_char, Fallback),
                    decode(GsmBits, Fallback, [Char, ?UNICODE_NON_BREAKING_SPACE | Str])
            end;
        <<GsmBits:BitSize/bitstring, GsmChar:7>> ->
            Char = util:dict_fetch(GsmChar, Tables#tables.gsm_to_char, Fallback),
            decode(GsmBits, Fallback, [Char | Str]);
        _ ->
            lists:reverse(Str)
    end.


-spec translate(string(), char(), dict()) -> [binary()].
translate(Str, Fallback, Table) ->
    TranslatedFallback = dict:fetch(Fallback, Table),
    [util:dict_fetch(Char, Table, TranslatedFallback) || Char <- Str].

-spec pack([bitstring()]) -> {binary(), integer()}.
pack(GsmBstrs) ->
    GsmBstr = list_to_bitstring(lists:reverse(GsmBstrs)),
    GsmBin = special_pad_start(GsmBstr),
    GsmBytes = binary_to_list(GsmBin),
    {list_to_binary(lists:reverse(GsmBytes)), length(GsmBstrs)}.

-spec special_pad_start(bitstring()) -> binary().
special_pad_start(Bits) when is_binary(Bits) ->
    Bits;
special_pad_start(Bits) ->
    case 8 - bit_size(Bits) rem 8 of
        7 ->
            <<$ :7, Bits/bitstring>>;
        PadSize ->
            <<0:PadSize, Bits/bitstring>>
    end.
