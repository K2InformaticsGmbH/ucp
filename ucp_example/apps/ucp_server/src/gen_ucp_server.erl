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

-module(gen_ucp_server).

% Behaviour
-export([behaviour_info/1]).

% Public functions
-export([start/2,
         start/3,
         start_link/2,
         start_link/3,
         listen/3,
         close/1,
         send_dr/6,
         send_mo/7]).

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

% TODO: Locate this in the std lib
-type socket() :: term().

-record(conn_st, {session :: pid(),      % Session's pid
                  trns :: dict()}).      % Trn to Ref mapping

-record(st, {listen_sock :: socket(),    % Listen socket
             start_params :: proplist(), % UCP params for the start_link call
             mod :: atom(),              % Callback module
             mod_st :: term(),           % Callback module's state
             conn_sts :: [#conn_st{}]}). % List of per-connection states


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
     {handle_request, 6},
     {handle_response, 5},
     {handle_message, 4},
     {handle_stop, 3}];
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

-spec listen(term(), integer(), proplist()) -> ok | {error, term()}.
listen(SrvRef, Port, Params) ->
    SrvPid = ref_to_pid(SrvRef),
    case gen_tcp:listen(Port, [{active, false}, {reuseaddr, true}]) of
        {ok, Sock} ->
            case gen_tcp:controlling_process(Sock, SrvPid) of
                ok ->
                    gen_server:call(SrvRef, {listen, Sock, Params});
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec close(term()) -> ok.
close(SrvRef) ->
    gen_server:call(SrvRef, close).

-spec send_dr(term(), pid(), string(), string(), proplist(), list()) -> ok.
send_dr(SrvRef, Session, Adc, Oadc, Params, Args) ->
    gen_server:cast(SrvRef, {{Session, send_dr, [Adc, Oadc, Params]}, Args}).

-spec send_mo(term(), pid(), string(), string(), binary(), proplist(), list()) -> ok.
send_mo(SrvRef, Session, Adc, Oadc, Mo, Params, Args) ->
    gen_server:cast(SrvRef, {{Session, send_mo, [Adc, Oadc, Mo, Params]}, Args}).


% ----------------------------------------------------------------------------
% gen_server callbacks
% ----------------------------------------------------------------------------

-spec init({atom(), list()}) -> {ok, #st{}}.
init({Mod, Args}) ->
    St = #st{mod = Mod, conn_sts = []},
    pack((St#st.mod):init(Args), St).

-spec handle_call(term(), term(), #st{}) -> term().
handle_call({listen, Sock, Params}, _From, #st{listen_sock = undefined} = St) ->
    SrvPid = self(),
    spawn_link(fun () -> accept(SrvPid, Sock) end),
    {reply, ok, St#st{listen_sock = Sock, start_params = Params}};
handle_call({listen, Sock, _Params}, _From, St) ->
    gen_tcp:close(Sock),
    {reply, {error, already_listening}, St};
handle_call({handle_message, Session, Msg, Ucp}, _From, St) ->
    pack((St#st.mod):handle_message(Session, Msg, Ucp, St#st.mod_st), St);
handle_call(Msg, From, St) ->
    pack((St#st.mod):handle_call(Msg, From, St#st.mod_st), St).

-spec handle_cast(term(), #st{}) -> term().
handle_cast({accept, {ok, Sock}}, St) ->
    {ok, Session} = ucp_session:start_link(?MODULE, self(), Sock, St#st.start_params, []),
    ConnSt = #conn_st{session = Session, trns = dict:new()},
    NewConnSts = [ConnSt | St#st.conn_sts],
    SrvPid = self(),
    spawn_link(fun () -> accept(SrvPid, St#st.listen_sock) end),
    {noreply, St#st{conn_sts = NewConnSts}};
handle_cast({accept, Error}, _St) ->
    % TODO: Should we really kill the server here?
    erlang:error({failed_to_accept, Error});
handle_cast(close, St) ->
    gen_tcp:close(St#st.listen_sock),
    lists:foreach(fun (ConnSt) ->
                          try
                              true = is_process_alive(ConnSt#conn_st.session),
                              ok = ucp_session:stop(ConnSt#conn_st.session)
                          catch
                              error:_NotAlive ->
                                  ok
                          end
                  end, St#st.conn_sts),
    {noreply, St#st{listen_sock = undefined, conn_sts = []}};
handle_cast({{Session, ReqFunc, ReqArgs}, Args}, St) ->
    SrvPid = self(),
    spawn_link(fun () -> send_request(SrvPid, Session, ReqFunc, ReqArgs, Args) end),
    {noreply, St};
handle_cast({handle_request, Session, ReqFunc, ReqArgs, Args, {ref, Ref}}, St) ->
    case lists:keyfind(Session, #conn_st.session, St#st.conn_sts) of
        false ->
            gen_ucp_server_util:log_invalid_session(self(), Session, {handle_request, {ref, Ref}, ReqFunc, ReqArgs, Args}),
            {noreply, St};
        _ConnSt ->
            pack((St#st.mod):handle_request(Session, ReqFunc, ReqArgs, Ref, Args, St#st.mod_st), St)
    end;
handle_cast({handle_request, Session, ReqFunc, ReqArgs, Args, {ref_and_trn, Ref, Trn}}, St) ->
    case lists:keyfind(Session, #conn_st.session, St#st.conn_sts) of
        false ->
            gen_ucp_server_util:log_invalid_session(self(), Session, {handle_request, {ref_and_trn, Ref, Trn}, ReqFunc, ReqArgs, Args}),
            {noreply, St};
        ConnSt ->
            NewTrns = dict:store(Trn, Ref, ConnSt#conn_st.trns),
            NewSt = update_trns(NewTrns, ConnSt, St),
            pack((St#st.mod):handle_request(Session, ReqFunc, ReqArgs, Ref, Args, St#st.mod_st), NewSt)
    end;
handle_cast({handle_response, Session, Resp, Ucp, {ref, Ref}}, St) ->
    case lists:keyfind(Session, #conn_st.session, St#st.conn_sts) of
        false ->
            gen_ucp_server_util:log_invalid_session(self(), Session, {handle_response, {ref, Ref}, Resp, Ucp}),
            {noreply, St};
        _ConnSt ->
            pack((St#st.mod):handle_response(Session, Resp, Ucp, Ref, St#st.mod_st), St)
    end;
handle_cast({handle_response, Session, Resp, Ucp, {trn, Trn}}, St) ->
    case lists:keyfind(Session, #conn_st.session, St#st.conn_sts) of
        false ->
            gen_ucp_server_util:log_invalid_session(self(), Session, {handle_response, {trn, Trn}, Resp, Ucp}),
            {noreply, St};
        ConnSt ->
            case dict:find(Trn, ConnSt#conn_st.trns) of
                error ->
                    gen_ucp_server_util:log_invalid_trn(self(), Session, {handle_response, {trn, Trn}, Resp, Ucp}),
                    {noreply, St};
                {ok, Ref} ->
                    NewTrns = dict:erase(Trn, ConnSt#conn_st.trns),
                    NewSt = update_trns(NewTrns, ConnSt, St),
                    pack((St#st.mod):handle_response(Session, Resp, Ucp, Ref, St#st.mod_st), NewSt)
            end
    end;
handle_cast({handle_stop, Session, Reason}, St) ->
    NewSt = flush_session(Session, St),
    pack((St#st.mod):handle_stop(Session, Reason, St#st.mod_st), NewSt);
handle_cast(Msg, St) ->
    pack((St#st.mod):handle_cast(Msg, St#st.mod_st), St).

-spec handle_info(term(), #st{}) -> term().
handle_info({'EXIT', Session, Reason} = Info, St) ->
    case lists:keymember(Session, #conn_st.session, St#st.conn_sts) of
        true ->
            NewSt = flush_session(Session, St),
            pack((St#st.mod):handle_stop(Session, Reason, St#st.mod_st), NewSt);
        false ->
            pack((St#st.mod):handle_info(Info, St#st.mod_st), St)
    end;
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
        {ok, ?UCP_OT_SESSION_MANAGEMENT} ->
            handle_message(SrvRef, Session, login, Ucp);
        {ok, ?UCP_OT_SUBMIT_SM} ->
            handle_message(SrvRef, Session, receive_mt, Ucp);
        {ok, ?UCP_OT_MT_ALERT} ->
            ack;
        _ ->
            gen_ucp_server_util:log_unknown_operation(SrvRef, Session, Ucp),
            nack
    end.

-spec handle_incoming_result(term(), pid(), {ok, proplist(), string(), term()}) -> ok.
handle_incoming_result(SrvRef, Session, {ok, Ucp, _Bytes, _WinInfo}) ->
    case util_proplists:find(ot, Ucp) of
        {ok, Type} when Type =:= ?UCP_OT_DELIVERY_SM; Type =:= ?UCP_OT_DELIVERY_SM_NOTIFICATION ->
            case util_proplists:find(trn, Ucp) of
                {ok, Trn} ->
                    handle_response(SrvRef, Session, ok, Ucp, {trn, Trn});
                undefined ->
                    gen_ucp_server_util:log_unknown_result(SrvRef, Session, Ucp)
            end;
        _ ->
            gen_ucp_server_util:log_unknown_result(SrvRef, Session, Ucp)
    end.

-spec handle_timed_out_result(term(), pid(), {timed_out, integer(), string(), term()}) -> ok.
handle_timed_out_result(SrvRef, Session, {timed_out, Trn, [], _WinInfo}) ->
    handle_response(SrvRef, Session, {error, response_timed_out}, [], {trn, Trn}).

-spec handle_incoming_unknown_result(term(), pid(), {unknown_result, proplist(), string(), term()}) -> ok.
handle_incoming_unknown_result(SrvRef, Session, {unknown_result, Ucp, _Bytes, _WinInfo}) ->
    gen_ucp_server_util:log_unknown_result(SrvRef, Session, Ucp).

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

-spec handle_message(term(), pid(), login | receive_mt, proplist()) -> ack | {ack, string()} | nack | {nack, integer() | {integer() | string()}} | nop.
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

-spec accept(pid(), socket()) -> ok.
accept(SrvPid, ListenSock) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Sock} ->
            case gen_tcp:controlling_process(Sock, SrvPid) of
                ok ->
                    gen_server:cast(SrvPid, {accept, {ok, Sock}});
                Error ->
                    gen_server:cast(SrvPid, {accept, Error})
            end;
        Error ->
            gen_server:cast(SrvPid, {accept, Error})
    end.

-spec send_request(pid(), pid(), send_dr | send_mo, list(), list()) -> ok.
send_request(SrvPid, Session, ReqFunc, ReqArgs, Args) ->
    Ref = make_ref(),
    try
        Result = case {ReqFunc, ReqArgs} of
            {send_dr, [Adc, Oadc, Params]} ->
                ucp_session:notify_sm(Session, Adc, Oadc, Params);
            {send_mo, [Adc, Oadc, Mo, Params]} ->
                ucp_session:delivery_sm(Session, Adc, Oadc, Mo, Params)
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

-spec flush_session(pid(), #st{}) -> #st{}.
flush_session(Session, St) ->
    try
        Ref = erlang:monitor(process, Session),
        exit(Session, kill),
        erlang:demonitor(Ref, [flush])
    catch
        error:_NotAlive ->
            ok
    end,
    NewConnSts = lists:keydelete(Session, #conn_st.session, St#st.conn_sts),
    St#st{conn_sts = NewConnSts}.

-spec update_trns(dict(), #conn_st{}, #st{}) -> #st{}.
update_trns(NewTrns, ConnSt, St) ->
    NewConnSt = ConnSt#conn_st{trns = NewTrns},
    NewConnSts = [NewConnSt | lists:keydelete(ConnSt#conn_st.session, #conn_st.session, St#st.conn_sts)],
    St#st{conn_sts = NewConnSts}.
