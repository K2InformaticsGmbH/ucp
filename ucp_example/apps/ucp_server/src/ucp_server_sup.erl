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

-module(ucp_server_sup).

% supervisor callbacks
-behaviour(supervisor).
-export([init/1]).

% Public functions
-export([start_link/1]).

-include_lib("util/include/util.hrl").
-include("ucp_server.hrl").


% ----------------------------------------------------------------------------
% Public functions
% ----------------------------------------------------------------------------

-spec start_link(integer()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Port).


% ----------------------------------------------------------------------------
% supervisor callbacks
% ----------------------------------------------------------------------------

-spec init(integer()) -> {ok, term()}.
init(Port) ->
    Server = {ucp_server, {ucp_server, start_link, [Port]},
               permanent, 60000, worker, [ucp_server]},
    {ok, {{one_for_one, 3, 1}, [Server]}}.
