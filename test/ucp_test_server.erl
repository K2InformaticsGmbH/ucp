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

%%% /Roland Karlsson

%%% ----------------------------------------------------------
%%% MODULE: UCP_TEST_SERVER
%%%
%%% This is an example UCP server, a.k.a. an SMSC or MC.
%%% ----------------------------------------------------------

-module(ucp_test_server).

-behaviour(ucp_session).

-include_lib("ucperl/include/ucp_defines.hrl").
-include_lib("ucperl/include/ucp_errors.hrl").

-export([start/0, stop/0, stop/1]).
-export([send/1]).
-export([deliver/1, notify/1]).
-export([handle_incoming_operation/3,
         handle_incoming_result/3,
         handle_incoming_unknown_result/3,
         handle_outgoing_operation/3,
         handle_outgoing_result/3,
         handle_faulty_input/3,
         handle_timed_out_result/3,
         handle_stopped/2]).

-export([loop/4]).

%%% start

start() ->
    stop(),

    application:unload(ucperl),
    application:load(ucperl),
    case application:get_key(ucperl, vsn) of
        undefined ->
            io:format("starting~n", []);
        {ok, VSN} ->
            io:format("starting (VSN = ~p)~n", [VSN])
    end,

    Result = [{result_timeout, 2}],

    AppOpts = Result,

    SrvOpts = [],

    {ok, LS} = gen_tcp:listen(5000, [{active, false}]),

    PID = spawn(?MODULE, loop, [1, LS, AppOpts, SrvOpts]),

    catch ets:delete(ucp_info),
    ets:new(ucp_info, [named_table]),
    ets:insert(ucp_info, {proc, LS, PID}),

    {LS, PID}.

%%% loop

loop(Num, LS, AppOpts, SrvOpts) ->

    io:format("ACCEPT (wait)~n", []),
    {ok, Socket} = gen_tcp:accept(LS),
    io:format("ACCEPTED~p~n~n", [Socket]),

    Name = erlang:list_to_atom("s" ++ erlang:integer_to_list(Num)),
    AppOpts2 =  AppOpts ++ [{name, {local, Name}}],
    {ok, Pid} = ucp_session:start(?MODULE,
                                  self(),
                                  Socket,
                                  AppOpts2,
                                  []),

    io:format("CONNECTED: ~p~n~n", [{Name, Pid}]),

    loop(Num+1, LS, AppOpts, SrvOpts).

%%% stop

stop() ->
    io:format("stopping~n", []),

    Result =
        case catch ets:lookup(ucp_info, proc) of
            [{proc, LS, PID}] ->
                {LS, gen_tcp:close(LS), PID, exit(PID, quit)};
            Error ->
                Error
        end,
    Result.

stop(Ref) ->
    io:format("stopping ~p~n", [Ref]),

    %% The ucp sessions are called s1, s2, s3, ...
    ucp_session:stop(Ref).

%%% send

send(Ref) ->
    ADC = "1234",

    %% OADC and OTOA are needed for H3G UK
    OADC = "FOO\0BAR",                          % FOO@BAR
    OTOA = {otoa, ?UCP_OTOA_ALPHANUMERIC},

    ucp_session:submit_sm(Ref, ADC, OADC, "hello", [OTOA]).

deliver(Ref) ->
    ADC = "1234",

    %% OADC and OTOA are needed for H3G UK
    OADC = "FOO\0BAR",                          % FOO@BAR
    OTOA = {otoa, ?UCP_OTOA_ALPHANUMERIC},

    ucp_session:delivery_sm(Ref, ADC, OADC, "hello", [OTOA]).

notify(Ref) ->
    ADC = "1234",

    %% OADC and OTOA are needed for H3G UK
    OADC = "FOO\0BAR",                          % FOO@BAR
    OTOA = {otoa, ?UCP_OTOA_ALPHANUMERIC},

    ucp_session:notify_sm(Ref, ADC, OADC, [OTOA]).

%%% callbacks

handle_incoming_operation(ParentRef, Pid, {ok, UCP, Bytes, WinInfo}) ->
    io:format("Operation (~p > ~p): ~p [~p] ~p~n~n",
              [ParentRef, Pid, UCP, ucp_syntax:prettify(Bytes), WinInfo]),

    %% Simulate throttling - only accept max ten operation per second
    timer:sleep(100),

    case lists:keysearch(ot, 1, UCP) of
        {value, {ot,31}} ->
            ack;
        {value, {ot,51}} ->
            ack;
        {value, {ot,60}} ->
            ack;
        _ ->
            nack
    end.

handle_incoming_result(ParentRef, Pid, {ok, UCP, Bytes, WinInfo}) ->
    io:format("Result (~p > ~p): ~p [~p] ~p ~n~n",
              [ParentRef, Pid, UCP, ucp_syntax:prettify(Bytes), WinInfo]).

handle_incoming_unknown_result(ParentRef, Pid, {unknown_result, UCP, Bytes, WinInfo}) ->
    io:format("Unknown Result (~p > ~p): ~p [~p] ~p~n~n",
              [ParentRef, Pid, UCP, ucp_syntax:prettify(Bytes), WinInfo]).

handle_outgoing_operation(ParentRef, Pid, {ok, UCP, Bytes, WinInfo}) ->
    io:format("Outgoing Operation (~p > ~p): ~p [~p] ~p~n~n",
              [ParentRef, Pid, UCP, ucp_syntax:prettify(Bytes), WinInfo]).

handle_outgoing_result(ParentRef, Pid, {ok, UCP, Bytes, WinInfo}) ->
    io:format("Outgoing Result (~p > ~p): ~p [~p] ~p~n~n",
              [ParentRef, Pid, UCP, ucp_syntax:prettify(Bytes), WinInfo]).

handle_faulty_input(ParentRef, Pid, {Reason, Info, Bytes, WinInfo}) ->
    io:format("Error (~p > ~p): ~p, ~p [~p] ~p~n~n",
              [ParentRef, Pid, Reason, Info, ucp_syntax:prettify(Bytes), WinInfo]).

handle_timed_out_result(ParentRef, Pid, {timed_out, Trn, "", WinInfo}) ->
    io:format("Timeout (~p > ~p): ~p ~p~n~n~n",
              [ParentRef, Pid, Trn, WinInfo]).

handle_stopped(ParentRef, Pid) ->
    io:format("Stopped (~p > ~p):~n",
              [ParentRef, Pid]).
