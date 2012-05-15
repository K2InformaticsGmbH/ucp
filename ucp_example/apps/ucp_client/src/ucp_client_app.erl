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

-module(ucp_client_app).

% Public functions
-export([start_connection/2,
         start_connection/1,
         stop_connection/1,
         restart_connection/1,
         connections_status/0]).

% application callbacks
-behaviour(application).
-export([start/2,
         stop/1]).

-include_lib("util/include/util.hrl").
-include("ucp_client.hrl").

% ----------------------------------------------------------------------------
% Public functions
% ----------------------------------------------------------------------------

-spec start_connections() -> ok.
start_connections() ->
    util_log:log_info("Starting Connections~n", []),

    {ok, Connections} = application:get_env(ucp_client, connections),
    lists:foreach(fun({ConnectionName, Config}) ->
                          start_connection(ConnectionName, Config)
                  end,
                  Connections),

    ConnectionNames = lists:map(fun({ConnName, _}) -> ConnName end, Connections),
    util_log:log_info("Connections Started: ~p~n", [ConnectionNames]).

-spec start_connection(connection()) -> ok | {error, connection_not_found}.
start_connection(ConnName) ->
    Connections = application:get_env(ucp_client, connections),
     case util_proplists:find(ConnName, Connections) of
        {ok, Config} ->
            start_connection(ConnName, Config);
        undefined ->
            {error, connection_not_found}
     end.

-spec start_connection(connection(), term()) -> ok.
start_connection(ConnName, Config) ->
    util_log:log_info("Starting connection ~p~n", [ConnName]),
    OperatorMod = util_proplists:fetch(operator, Config),
    Host = util_proplists:fetch(host, Config),
    Port = util_proplists:fetch(port, Config),
    Adc = util_proplists:fetch(adc, Config),
    Password = util_proplists:fetch(password, Config),
    WindowSize = util_proplists:fetch(window_size, Config),
    {ok, _Pid} = ucp_client_sup:start_session(ConnName, OperatorMod, Host, Port, Adc, Password, WindowSize),
    MaxWaitSeconds = util_proplists:fetch(wait_for_login_timeout, Config),
    wait_for_login(ConnName, MaxWaitSeconds).

-spec stop_connection(connection()) -> ok.
stop_connection(ConnName) ->
    ucp_client:stop(ConnName).
    
-spec restart_connection(connection()) -> ok.
restart_connection(ConnName) ->
    case catch stop_connection(ConnName) of
        ok ->
            start_connection(ConnName);
        Error ->
            Error
    end.

-spec connections_status() -> proplist().
connections_status() ->
    {ok, Connections} = application:get_env(ucp_client, connections),
    lists:flatten(
        lists:map(
            fun({Name, _}) ->
                    Pid = self(),
                    spawn(fun() -> Status = ucp_client:is_logged_in(Name), Pid ! {done, Name, Status} end),
                    receive
                        {done, Name, Status} ->
                            ConnStatus = case Status of
                                true  -> up;
                                false -> down
                            end
                    after 100 ->
                            ConnStatus = timeout
                    end,
                    {Name, ConnStatus}
            end, Connections)
    ).

% ----------------------------------------------------------------------------
% application callbacks
% ----------------------------------------------------------------------------

-spec start(term(), list()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    util_log:open_log(?MT_REQUEST_LOG, "./log/"),
    util_log:open_log(?MO_REQUEST_LOG, "./log/"),
    util_log:open_log(?DR_REQUEST_LOG, "./log/"),
    ok = util_gsm0338:init(),
    {ok, Pid} = ucp_client_sup:start_link(),
    ?DUPLICATE_DELIVER_SM_KEY = ets:new(?DUPLICATE_DELIVER_SM_KEY, [set, public, named_table]),
    start_connections(),

    {ok, Pid}.

-spec stop(term()) -> ok.
stop(_State) ->
    simple_logger:close(?MT_REQUEST_LOG),
    simple_logger:close(?MO_REQUEST_LOG),
    simple_logger:close(?DR_REQUEST_LOG),
    ok.


% ----------------------------------------------------------------------------
% Private functions
% ----------------------------------------------------------------------------

-spec wait_for_login(connection(), integer()) -> ok.
wait_for_login(_Connection, 0) ->
    ok;
wait_for_login(Connection, MaxWaitSeconds) ->
    case ucp_client:is_logged_in(Connection) of
        true ->
            ok;
        false ->
            timer:sleep(1000),
            wait_for_login(Connection, MaxWaitSeconds -1)
    end.