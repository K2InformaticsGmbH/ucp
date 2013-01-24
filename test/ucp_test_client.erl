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
%%% MODULE: UCP_TEST_CLIENT
%%%
%%% This is an example UCP client, a.k.a. an ESME.
%%% ----------------------------------------------------------

-module(ucp_test_client).

-behaviour(ucp_session).

-include_lib("ucperl/include/ucp_defines.hrl").
-include_lib("ucperl/include/ucp_errors.hrl").

-export([start/0, start_link/0, start/1, start_link/1, stop/1]).
-export([send/1, send_binary/1, send_numeric/1, send_some/2]).
-export([send_large/1, send_large_binary/1]).
-export([handle_incoming_operation/3,
         handle_incoming_result/3,
         handle_incoming_unknown_result/3,
         handle_outgoing_operation/3,
         handle_outgoing_result/3,
         handle_faulty_input/3,
         handle_timed_out_result/3,
         handle_stopped/2]).

%%% start

start() ->
    start_private([]).

start_link() ->
    start_private([{link,true}]).

start(Name) ->
    start_private([{name,Name}]).

start_link(Name) ->
    start_private([{name,Name}, {link,true}]).


start_private(Opts) ->
    application:unload(ucperl),
    application:load(ucperl),
    case application:get_key(ucperl, vsn) of
        undefined ->
            io:format("starting~n", []);
        {ok, VSN} ->
            io:format("starting (VSN = ~p)~n", [VSN])
    end,

    MyOwnShortNumber = "1234",

    %% Three versions of keep alive period settings
    %% KAPeriod = [],
    KAPeriod = [{keep_alive_period, 10}],
    %% KAPeriod = [{keep_alive_period, 150}], %% H3G - UK/RoI

    KAADC = [{keep_alive_ot31_adc, MyOwnShortNumber}],
    %% TODO: according to logicaCMG this should be
    %% ?UCP_PID_PC_ABBREV_NUMBER. But according to the H3G UK spec it
    %% should be this:
    KAPID = [{keep_alive_ot31_pid, ?UCP_PID_PC_OVER_TCPIP}],

    KeepAlive = KAPeriod ++ KAADC ++ KAPID,

    %% Two versions of result timeout settings
    %% Result = [],
    Result = [{result_timeout, 2}],
    %% Result = [{result_timeout, 50}], %% H3G - UK/RoI

    %% Two versions of windowing settings
    %% Window = [],
    %% Window = [{win_size_out, 0}],
    %% Window = [{win_size_out, 1}],
    Window = [{win_size_out, 5}],

    Name =
        case (lists:keysearch(name, 1, Opts)) of
            {value, {_, N}} ->
                io:format("Name: ~p~n", [N]),
                [{name, {local, N}}];
            _ ->
                []
        end,

    AppOpts = KeepAlive ++ Result ++ Window ++ Name,

    {ok, Socket} = gen_tcp:connect("localhost", 5000, [{active, false}]),

    case (lists:keysearch(link, 1, Opts)) of
        {value,{_, true}} ->
            io:format("Link~n", []),
            {ok, Pid} = ucp_session:start_link(?MODULE,
                                               self(),
                                               Socket,
                                               AppOpts,
                                               []);
        _ ->
            {ok, Pid} = ucp_session:start(?MODULE,
                                          self(),
                                          Socket,
                                          AppOpts,
                                          [])
    end,
    io:format("login~n", []),

    %% OTON, ONPI and OPID are needed for H3G UK
    OTON = {oton, ?UCP_OTON_ABBREVIATED},
    ONPI = {onpi, ?UCP_ONPI_SMSC_SPECIFIC},
    OPID = {opid, ?UCP_OPID_PC},

    {ok, Trn} = ucp_session:login(Pid,
                                  MyOwnShortNumber,
                                  "secret",
                                  [OTON, ONPI, OPID]),
    io:format("done~n~n", []),
    {Pid, Trn}.

%%% stop

stop(Ref) ->
    ucp_session:stop(Ref).

%%% send

send(Ref) ->
    send(Ref, "hello", []).

send(Ref, Msg, Opt) ->
    send(Ref, Msg, Opt, []).

send(Ref, Msg, Opt, OptXs) ->
    ADC = "1234",

    %% OADC and OTOA are needed for H3G UK
    OADC = "FOO\0BAR",                          % FOO@BAR
    OTOA = {otoa, ?UCP_OTOA_ALPHANUMERIC},

    %% XSer is needed for H3G UK
    Xs =
        [{?UCP_XSER_SERVICE_GSM_DCS, [0]},
         {?UCP_XSER_SERVICE_BILLING_IDENTIFIER, "436PRM:150"}],
    XSer = {xser, Xs ++ OptXs},

    ucp_session:submit_sm(Ref, ADC, OADC, Msg, [OTOA, XSer] ++ Opt).

%%% NOTE - it does not matter if the data is binary or a list of char()
send_binary(Ref) ->
    MT = ?UCP_MT_TRANSPARENT_DATA,
    %% The number of bits in the message has to be provided.
    BITS = 20,
    send(Ref, <<7,8,9>>, [{mt,MT},{nb,BITS}]).

send_numeric(Ref) ->
    MT = ?UCP_MT_NUMERIC,
    send(Ref, "1984", [{mt, MT}]).

send_some(_,0) ->
    [];
send_some(Ref, N) when N > 0 ->
    M = "hello " ++ integer_to_list(N),
    %% Dont send more than 100 messages per second
    timer:sleep(10),
    [ send(Ref, M, []) | send_some(Ref, N - 1) ].

send_large(Ref) ->
    LargeMess = string:chars($a, 200),
    RefNr = 63,
    Xs = ucp_arg_syntax:split_sm(RefNr, LargeMess, []),
    [ send(Ref, S, [], [X]) || {X,S,_} <- Xs ].

%%% This example also shows how to add GSM ports - whatever that is
send_large_binary(Ref) ->
    LargeMess = string:chars($a, 200),
    %% The size of the transparent data is all bits minus three
    BinSize = length(LargeMess) * 8 - 3,
    RefNr = 67,
    Xs = ucp_arg_syntax:split_sm(RefNr, LargeMess, [{nb,BinSize},{ports,{240,230}}]),
    [ begin
          MT = ?UCP_MT_TRANSPARENT_DATA,
          send(Ref, S, [{mt,MT}, {nb,BITS}], [X])
      end || {X,S,BITS} <- Xs ].

%%% callbacks

handle_incoming_operation(ParentRef, Pid, {ok, UCP, Bytes, WinInfo}) ->
    io:format("Operation (~p > ~p): ~p [~p] ~p~n~n",
              [ParentRef, Pid, UCP, ucp_syntax:prettify(Bytes), WinInfo]),

    case lists:keysearch(ot, 1, UCP) of
        {value, {ot,52}} ->
            ack;
        {value, {ot,53}} ->
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
