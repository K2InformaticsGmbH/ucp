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
    