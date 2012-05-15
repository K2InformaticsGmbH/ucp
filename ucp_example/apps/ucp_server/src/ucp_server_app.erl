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

-module(ucp_server_app).

-behaviour(application).

% Public functions
-export([set_variable/2,
         get_variable/1]).

% application callbacks
-export([start/2,
         stop/1]).

-include_lib("util/include/util.hrl").
-include("ucp_server.hrl").


% ----------------------------------------------------------------------------
% Public functions
% ----------------------------------------------------------------------------

-spec set_variable(server_variable(), term()) -> ok.
set_variable(Var, Val) ->
    {ok, Vars} = application:get_env(ucp_server, variables),
    NewVars = util_proplists:store(Var, Val, Vars),
    application:set_env(ucp_server, variables, NewVars),
    ok = ucp_server:set_variable(Var, Val).

-spec get_variable(server_variable()) -> term().
get_variable(Var) ->
    case application:get_env(ucp_server, variables) of
        {ok, Val} ->
            Val;
        _ ->
            Var
    end.

% ----------------------------------------------------------------------------
% application callbacks
% ----------------------------------------------------------------------------

-spec start(term(), list()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    util_log:open_log(?DR_REQUEST_LOG, "./log/"),
    util_log:open_log(?MO_REQUEST_LOG, "./log/"),
    util_log:open_log(?MT_REQUEST_LOG, "./log/"),
    util_gsm0338:init(),
    {ok, Port} = application:get_env(ucp_server, port),
    ucp_server_sup:start_link(Port).

-spec stop(term()) -> ok.
stop(_State) ->
    simple_logger:close(?DR_REQUEST_LOG),
    simple_logger:close(?MO_REQUEST_LOG),
    simple_logger:close(?MT_REQUEST_LOG),
    ucp_server:stop().
