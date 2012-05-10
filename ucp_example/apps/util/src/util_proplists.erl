-module(util_proplists).

-export([
        find/2,
        fetch/2,
        fetch/3,
        store/3
    ]).

-include_lib("util/include/util.hrl").


%% @doc Returns the value for the given proplist key or undefined if it is not found.
%% @spec find(any(), proplist()) -> {ok, any()} | undefined
-spec find(any(), proplist()) -> {ok, any()} | undefined.
find(PropKey, PropList) ->
    case lists:keyfind(PropKey, 1, PropList) of
        {_PropKey, PropVal} ->
            {ok, PropVal};
        false ->
            undefined
    end.

%% @doc Returns the value for the given proplist key or throws an exception if it is not found.
%% @spec fetch(any(), proplist()) -> any()
-spec fetch(any(), proplist()) -> any().
fetch(PropKey, PropList) ->
    {ok, PropVal} = find(PropKey, PropList),
    PropVal.

%% @doc Returns the value for the given proplist key or the specified default if it is not found.
%% @spec fetch(any(), proplist(), any()) -> any()
-spec fetch(any(), proplist(), any()) -> any().
fetch(PropKey, PropList, DefaultPropVal) ->
    case find(PropKey, PropList) of
        undefined ->
            DefaultPropVal;
        {ok, PropVal} ->
            PropVal
    end.

-spec store(any(), any(), proplist()) -> proplist().
store(PropKey, PropVal, PropList) ->
    lists:keystore(PropKey, 1, PropList, {PropKey, PropVal}).
    