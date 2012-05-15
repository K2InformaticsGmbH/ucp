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

-module(gen_ucp_client).

% Behaviour
-export([behaviour_info/1]).

% Public functions
-export([start/2,
         start/3,
         start_link/2,
         start_link/3,
         connect/4,
         close/1,
         login/5,
         send_mt/6]).

% gen_server callbacks
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

% ucp_session callbacks
-behaviour(ucp_session).
-export([handle_incoming_operation/3,
         handle_incoming_result/3,
         handle_incoming_unknown_result/3,
         handle_outgoing_operation/3,
         handle_outgoing_result/3,
         handle_faulty_input/3,
         handle_timed_out_result/3,
         handle_stopped/2]).

-include_lib("ucperl/include/ucp_defines.hrl").
-include_lib("util/include/util.hrl").
-include("ucp_client.hrl").

-define(CONNECT_TIMEOUT, 30000).

-record(st, {mod :: atom(),      % Callback module
             mod_st :: term(),   % Callback module's state
             session :: pid(),   % Session's pid
             trns :: dict()}).   % Trn to Ref mapping


% ----------------------------------------------------------------------------
% Behaviour
% ----------------------------------------------------------------------------

-spec behaviour_info(atom()) -> proplist() | undefined.
behaviour_info(callbacks) ->
    [{init, 1},
     {handle_call, 3},
     {handle_cast, 2},
     {handle_info, 2},
     {code_change, 3},
     {terminate, 2},
     {handle_request, 5},
     {handle_response, 4},
     {handle_message, 3},
     {handle_stop, 2}];
behaviour_info(_Other) ->
    undefined.


% ----------------------------------------------------------------------------
% Public functions
% ----------------------------------------------------------------------------

-spec start(atom(), term()) -> {ok, pid()} | {error, term()}.
start(Mod, Args) ->
    gen_server:start(?MODULE, {Mod, Args}, []).

-spec start(term(), atom(), term()) -> {ok, pid()} | {error, term()}.
start(SrvRef, Mod, Args) ->
    gen_server:start(SrvRef, ?MODULE, {Mod, Args}, []).

-spec start_link(atom(), term()) -> {ok, pid()} | {error, term()}.
start_link(Mod, Args) ->
    gen_server:start_link(?MODULE, {Mod, Args}, []).

-spec start_link(term(), atom(), term()) -> {ok, pid()} | {error, term()}.
start_link(SrvRef, Mod, Args) ->
    gen_server:start_link(SrvRef, ?MODULE, {Mod, Args}, []).

-spec connect(term(), string(), integer(), proplist()) -> ok | {error, term()}.
connect(SrvRef, Host, Port, Params) ->
    SrvPid = ref_to_pid(SrvRef),
    case gen_tcp:connect(Host, Port, [{active, false}], ?CONNECT_TIMEOUT) of
        {ok, Sock} ->
            case gen_tcp:controlling_process(Sock, SrvPid) of
                ok ->
                    gen_server:call(SrvRef, {connect, Sock, Params});
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec close(term()) -> ok.
close(SrvRef) ->
    gen_server:cast(SrvRef, close).

-spec login(term(), string(), string(), proplist(), list()) -> ok.
login(SrvRef, Oadc, Password, Params, Args) ->
    gen_server:cast(SrvRef, {{login, [Oadc, Password, Params]}, Args}).

-spec send_mt(term(), string(), string(), binary() | string(), proplist(), list()) -> ok.
send_mt(SrvRef, Adc, Oadc, Mt, Params, Args) ->
    gen_server:cast(SrvRef, {{send_mt, [Adc, Oadc, Mt, Params]}, Args}).


% ----------------------------------------------------------------------------
% gen_server callbacks
% ----------------------------------------------------------------------------

-spec init({atom(), list()}) -> {ok, #st{}}.
init({Mod, Args}) ->
    St = #st{mod = Mod},
    pack((St#st.mod):init(Args), St).

-spec handle_call(term(), term(), #st{}) -> term().
handle_call({connect, Sock, Params}, _From, #st{session = undefined} = St) ->
    {ok, Session} = ucp_session:start_link(?MODULE, self(), Sock, Params, []),
    {reply, ok, St#st{session = Session, trns = dict:new()}};
handle_call({connect, Sock, _Params}, _From, St) ->
    gen_tcp:close(Sock),
    {reply, {error, already_connected}, St};
handle_call({handle_message, _Session, Msg, Ucp}, _From, St) ->
    pack((St#st.mod):handle_message(Msg, Ucp, St#st.mod_st), St);
handle_call(Msg, From, St) ->
    pack((St#st.mod):handle_call(Msg, From, St#st.mod_st), St).

-spec handle_cast(term(), #st{}) -> term().
handle_cast(close, St) ->
    try
        true = is_process_alive(St#st.session),
        ok = ucp_session:stop(St#st.session)
    catch
        error:_NotAlive ->
            ok
    end,
    {noreply, St};
handle_cast({{ReqFunc, ReqArgs}, Args}, St) ->
    SrvPid = self(),
    spawn_link(fun () -> send_request(SrvPid, St#st.session, ReqFunc, ReqArgs, Args) end),
    {noreply, St};
handle_cast({handle_request, _Session, ReqFunc, ReqArgs, Args, {ref, Ref}}, St) ->
    pack((St#st.mod):handle_request(ReqFunc, ReqArgs, Ref, Args, St#st.mod_st), St);
handle_cast({handle_request, _Session, ReqFunc, ReqArgs, Args, {ref_and_trn, Ref, Trn}}, St) ->
    NewTrns = dict:store(Trn, Ref, St#st.trns),
    NewSt = St#st{trns = NewTrns},
    pack((St#st.mod):handle_request(ReqFunc, ReqArgs, Ref, Args, St#st.mod_st), NewSt);
handle_cast({handle_response, _Session, Resp, Ucp, {ref, Ref}}, St) ->
    pack((St#st.mod):handle_response(Resp, Ucp, Ref, St#st.mod_st), St);
handle_cast({handle_response, Session, Resp, Ucp, {trn, Trn}}, St) ->
    case dict:find(Trn, St#st.trns) of
        error ->
            gen_ucp_client_util:log_invalid_trn(self(), Session, {handle_response, {trn, Trn}, Resp, Ucp}),
            {noreply, St};
        {ok, Ref} ->
            NewTrns = dict:erase(Trn, St#st.trns),
            NewSt = St#st{trns = NewTrns},
            pack((St#st.mod):handle_response(Resp, Ucp, Ref, St#st.mod_st), NewSt)
    end;
handle_cast({handle_stop, _Session, Reason}, St) ->
    NewSt = flush_session(St),
    pack((St#st.mod):handle_stop(Reason, St#st.mod_st), NewSt);
handle_cast(Msg, St) ->
    pack((St#st.mod):handle_cast(Msg, St#st.mod_st), St).

-spec handle_info(term(), #st{}) -> term().
handle_info({'EXIT', Session, Reason}, #st{session = Session} = St) ->
    NewSt = flush_session(St),
    pack((St#st.mod):handle_stop(Reason, St#st.mod_st), NewSt);
handle_info(Info, St) ->
    pack((St#st.mod):handle_info(Info, St#st.mod_st), St).

-spec code_change(term(), #st{}, term()) -> {ok, #st{}}.
code_change(OldVsn, St, Extra) ->
    pack((St#st.mod):code_change(OldVsn, St#st.mod_st, Extra), St).

-spec terminate(term(), #st{}) -> term().
terminate(Reason, St) ->
    (St#st.mod):terminate(Reason, St#st.mod_st).


% ----------------------------------------------------------------------------
% ucp_session callbacks
% ----------------------------------------------------------------------------

% Quoted from ucp_session.erl:
%
% "The two callbacks handle_incoming_* are called for incoming messages of
% operation and result type.
%
% The handle_incoming_unknown_result is called whenever a result without
% matching operation arrives.
%
% The two callbacks handle_outgoing_* are called for outgoing messages of
% operation and result type.
%
% The callback handle_faulty_input is called whenever a faulty message is
% found in the input.
%
% The callback handle_timed_out_result is called whenever a result does not
% arrive on time."

-spec handle_incoming_operation(term(), pid(), {ok, proplist(), string(), term()}) -> ack | {ack, string()} | nack | {nack, integer() | {integer() | string()}} | nop.
handle_incoming_operation(SrvRef, Session, {ok, Ucp, _Bytes, _WinInfo}) ->
    case util_proplists:find(ot, Ucp) of
        {ok, ?UCP_OT_DELIVERY_SM} ->
            handle_message(SrvRef, Session, receive_mo, Ucp);
        {ok, ?UCP_OT_DELIVERY_SM_NOTIFICATION} ->
            handle_message(SrvRef, Session, receive_dr, Ucp);
        _ ->
            gen_ucp_client_util:log_unknown_operation(SrvRef, Session, Ucp),
            nack
    end.

-spec handle_incoming_result(term(), pid(), {ok, proplist(), string(), term()}) -> ok.
handle_incoming_result(SrvRef, Session, {ok, Ucp, _Bytes, _WinInfo}) ->
    case util_proplists:find(ot, Ucp) of
        {ok, Type} when Type =:= ?UCP_OT_SESSION_MANAGEMENT; Type =:= ?UCP_OT_SUBMIT_SM ->
            case util_proplists:find(trn, Ucp) of
                {ok, Trn} ->
                    handle_response(SrvRef, Session, ok, Ucp, {trn, Trn});
                undefined ->
                    gen_ucp_client_util:log_unknown_result(SrvRef, Session, Ucp)
            end;
        {ok, ?UCP_OT_MT_ALERT} ->
            case util_proplists:find(ack, Ucp) of
                {ok, _Ack} ->
                    ok;
                undefined ->
                    handle_stop(SrvRef, Session, keep_alive_nacked)
            end;
        _ ->
            gen_ucp_client_util:log_unknown_result(SrvRef, Session, Ucp)
    end.

-spec handle_timed_out_result(term(), pid(), {timed_out, integer(), string(), term()}) -> ok.
handle_timed_out_result(SrvRef, Session, {timed_out, Trn, [], _WinInfo}) ->
    handle_response(SrvRef, Session, {error, response_timed_out}, [], {trn, Trn}).

-spec handle_incoming_unknown_result(term(), pid(), {unknown_result, proplist(), string(), term()}) -> ok.
handle_incoming_unknown_result(SrvRef, Session, {unknown_result, Ucp, _Bytes, _WinInfo}) ->
    gen_ucp_client_util:log_unknown_result(SrvRef, Session, Ucp).

-spec handle_outgoing_operation(term(), pid(), {ok, proplist(), string(), term()}) -> ok.
handle_outgoing_operation(_SrvRef, _Session, {ok, _Ucp, _Bytes, _WinInfo}) ->
    ok.

-spec handle_outgoing_result(term(), pid(), {ok, proplist(), string(), term()}) -> ok.
handle_outgoing_result(_SrvRef, _Session, {ok, _Ucp, _Bytes, _WinInfo}) ->
    ok.

-spec handle_faulty_input(term(), pid(), {term(), term(), string(), term()}) -> ok.
handle_faulty_input(SrvRef, Session, {Reason, Info, Bytes, _WinInfo}) ->
    handle_stop(SrvRef, Session, {faulty_input_passed, Reason, Info, Bytes}).

-spec handle_stopped(term(), pid()) -> ok.
handle_stopped(SrvRef, Session) ->
    handle_stop(SrvRef, Session, session_stopped).


% ----------------------------------------------------------------------------

-spec handle_request(term(), pid(), atom(), list(), list(), {ref, reference()} | {ref_and_trn, reference(), integer()}) -> ok.
handle_request(SrvRef, Session, ReqFunc, ReqArgs, Args, RefOrRefAndTrn) ->
    gen_server:cast(SrvRef, {handle_request, Session, ReqFunc, ReqArgs, Args, RefOrRefAndTrn}).

-spec handle_response(term(), pid(), ok | {error, term()}, proplist(), {ref, reference()} | {trn, integer()}) -> ok.
handle_response(SrvRef, Session, Resp, Ucp, RefOrTrn) ->
    gen_server:cast(SrvRef, {handle_response, Session, Resp, Ucp, RefOrTrn}).

-spec handle_message(term(), pid(), receive_dr | receive_mo, proplist()) -> ack | {ack, string()} | nack | {nack, integer() | {integer() | string()}} | nop.
handle_message(SrvRef, Session, Msg, Ucp) ->
    gen_server:call(SrvRef, {handle_message, Session, Msg, Ucp}).

-spec handle_stop(term(), pid(), term()) -> ok.
handle_stop(SrvRef, Session, Reason) ->
    gen_server:cast(SrvRef, {handle_stop, Session, Reason}).


% ----------------------------------------------------------------------------
% Private functions
% ----------------------------------------------------------------------------

-spec ref_to_pid(term()) -> pid().
ref_to_pid(Ref) when is_pid(Ref) ->
    Ref;
ref_to_pid(Ref) when is_atom(Ref) ->
    whereis(Ref);
ref_to_pid({global, Name}) ->
    global:whereis_name(Name);
ref_to_pid({Name, Node}) ->
    rpc:call(Node, erlang, whereis, [Name]).

-spec pack(term(), #st{}) -> term().
pack({reply, Reply, ModSt}, St) ->
    {reply, Reply, St#st{mod_st = ModSt}};
pack({reply, Reply, ModSt, Timeout}, St) ->
    {reply, Reply, St#st{mod_st = ModSt}, Timeout};
pack({noreply, ModSt}, St) ->
    {noreply, St#st{mod_st = ModSt}};
pack({noreply, ModSt, Timeout}, St) ->
    {noreply, St#st{mod_st = ModSt}, Timeout};
pack({stop, Reason, Reply, ModSt}, St) ->
    {stop, Reason, Reply, St#st{mod_st = ModSt}};
pack({stop, Reason, ModSt}, St) ->
    {stop, Reason,  St#st{mod_st = ModSt}};
pack({ok, ModSt}, St) ->
    {ok, St#st{mod_st = ModSt}};
pack({ok, ModSt, Timeout}, St) ->
    {ok, St#st{mod_st = ModSt}, Timeout};
pack(Other, _St) ->
    Other.

-spec send_request(pid(), pid(), login | send_mt, list(), list()) -> ok.
send_request(SrvPid, Session, ReqFunc, ReqArgs, Args) ->
    Ref = make_ref(),
    try
        Result = case {ReqFunc, ReqArgs} of
            {login, [Adc, Password, Params]} ->
                ucp_session:login(Session, Adc, Password, Params);
            {send_mt, [Adc, Oadc, Mt, Params]} ->
                ucp_session:submit_sm(Session, Adc, Oadc, Mt, Params)
        end,
        case Result of
            {ok, Trn} ->
                handle_request(SrvPid, Session, ReqFunc, ReqArgs, Args, {ref_and_trn, Ref, Trn});
            {error, Reason} ->
                handle_request(SrvPid, Session, ReqFunc, ReqArgs, Args, {ref, Ref}),
                handle_response(SrvPid, Session, {error, {windowing_failed, Reason}}, [], {ref, Ref})
        end
    catch
        _Class:Reason1 ->
            handle_request(SrvPid, Session, ReqFunc, ReqArgs, Args, {ref, Ref}),
            handle_response(SrvPid, Session, {error, {request_failed, Reason1}}, [], {ref, Ref})
    end.

-spec flush_session(#st{}) -> #st{}.
flush_session(St) ->
    try
        Ref = erlang:monitor(process, St#st.session),
        exit(St#st.session, kill),
        erlang:demonitor(Ref, [flush])
    catch
        error:_NotAlive ->
            ok
    end,
    St#st{session = undefined, trns = undefined}.
