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

-module(ucp_client_sup).

% Public functions
-export([start_link/0,
         start_session/7]).

% supervisor callbacks
-behaviour(supervisor).
-export([init/1]).

-include_lib("util/include/util.hrl").
-include("ucp_client.hrl").


% ----------------------------------------------------------------------------
% Public functions
% ----------------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_session(connection(), atom(), string(), integer(), string(), string(), integer()) -> {ok, pid()} | {error, term()}.
start_session(Connection, OperatorMod, Host, Port, Adc, Password, WindowSize) ->
    supervisor:start_child(?MODULE, [Connection, OperatorMod, Host, Port, Adc, Password, WindowSize]).


% ----------------------------------------------------------------------------
% supervisor callbacks
% ----------------------------------------------------------------------------

-spec init([]) -> {ok, term()}.
init([]) ->
    Client = {ucp_client, {ucp_client, start_link, []},
              transient, 60000, worker, [ucp_client]},
    {ok, {{simple_one_for_one, 3, 1}, [Client]}}.
