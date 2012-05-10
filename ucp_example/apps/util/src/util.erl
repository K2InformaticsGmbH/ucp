-module(util).

-export([
         generate_guid/0,
         gs_now/0,
         group_binaries/2,
         group_bitstrings/2,
         term_to_string/1,
         dict_fetch/3
        ]).

-include("util.hrl").

%% @doc Returns a random (version 4) UUID
%% @spec generate_guid() -> string()
-spec generate_guid() -> string().
generate_guid() ->
    crypto:rand_uniform(1, round(math:pow(2, 128))).

-spec gs_now() -> integer().
gs_now() ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now())).

%% @doc Groups the binaries passed so that the combined byte size of each group does not exceed the max group byte size passed.
%% @spec group_binaries([binary()], integer()) -> [[binary()]]
-spec group_binaries([binary()], integer()) -> [[binary()]].
group_binaries(Bins, MaxGroupSize) ->
    group_bitstrings(Bins, MaxGroupSize).


%% @doc Groups the bitstrings passed so that the combined byte size of each group does not exceed the max group byte size passed.
%% @spec group_bitstrings([bitstring()], integer()) -> [[bitstring()]]
-spec group_bitstrings([bitstring()], integer()) -> [[bitstring()]].
group_bitstrings([], _MaxGroupSize) ->
    [];
group_bitstrings(Bstrs, MaxGroupSize) ->
    group_bitstrings(Bstrs, MaxGroupSize * 8, [], 0, []).

-spec group_bitstrings([bitstring()], integer(), [bitstring()], integer(), [[bitstring()]]) -> [[bitstring()]].
group_bitstrings([Bstr | _Bstrs], MaxGroupBits, _Group, _GroupBits, _Groups) when bit_size(Bstr) > MaxGroupBits ->
    erlang:error(badarg);
group_bitstrings([Bstr | Bstrs], MaxGroupBits, Group, GroupBits, Groups) when bit_size(Bstr) + GroupBits > MaxGroupBits ->
    group_bitstrings(Bstrs, MaxGroupBits, [Bstr], bit_size(Bstr), [lists:reverse(Group) | Groups]);
group_bitstrings([Bstr | Bstrs], MaxGroupBits, Group, GroupBits, Groups) ->
    group_bitstrings(Bstrs, MaxGroupBits, [Bstr | Group], bit_size(Bstr) + GroupBits, Groups);
group_bitstrings([], _MaxGroupBits, Group, _GroupBits, Groups) ->
    lists:reverse([lists:reverse(Group) | Groups]).

-spec term_to_string(term()) -> string().
term_to_string(Term) ->
    io_lib:format("~p", [Term]).

-spec dict_fetch(term(), dict(), term()) -> term().
dict_fetch(Key, Dict, DefaultValue) ->
    case dict:find(Key, Dict) of
        {ok, Value} ->
            Value;
        error ->
            DefaultValue
    end.