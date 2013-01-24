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

%%% EUNIT tests for ucp_session.erl
%%% - Roland

-module(ucp_session_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

send_test() ->
    send().

send_again_test() ->
    timer:sleep(1000),
    send().

%%%


%% TODO: how to get this quiet ??
%% gen_server talks too much when terminating
send() ->
    {_, _} = ucp_test_server:start(),
    timer:sleep(100),
    {_, _} = ucp_test_client:start(c1),
    timer:sleep(100),
    {ok, _} = ucp_test_client:send(c1),
    timer:sleep(100),
    ok = ucp_test_client:stop(c1),
    timer:sleep(100),
    {_, ok, _, true} = ucp_test_server:stop().
