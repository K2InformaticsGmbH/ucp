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

%%% This is some help for doing manual testing
%%% - Roland

-module(t).

-compile(export_all).


%%% Loads all files needed

l() ->
    [c:l(t),
     c:l(ucp_syntax),
     c:l(ucp_arg_syntax),
     c:l(ucp_session),
     c:l(ucp_test_server),
     c:l(ucp_test_client)].

%%% Compiles all files needed in the test directory

c() ->
    [c:c(t),
     c:c(ucp_test_server),
     c:c(ucp_test_client)].

%%% (Re)starts the server

ss() ->
    ucp_test_server:start().


%%% (Re)starts the client

sc() ->
    [catch ucp_test_client:stop(foo),
     ucp_test_client:start(foo)].

%%% Sending a SM from the client to the server

send() ->
    ucp_test_client:send(foo).

%%% Deliver a SM from the server to the client
%%% NOTE: its probably called s1, but if you have
%%% connected from the client more than once, it
%%% can be called s2, s3, s4, ...

deliver(Session) ->
    ucp_test_server:deliver(Session).

deliver() ->
    deliver(s1).
