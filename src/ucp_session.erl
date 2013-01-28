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

%%% @author Roland Karlsson

%%% @doc
%%%  This module implements a behaviour for a UCP SMS session.
%%%
%%%  In this case we assume the session is on a computer talking
%%%  UCP over TCP/IP.
%%%
%%%  The behaviour spawns a gen_server for the session.
%%% @end

%%% @type opts() = [{Key::atom(),Value::term()}]
%%% @type ucp() = opts()
%%% @type state() = record()
%%% @type ip() = string()

-module(ucp_session).

-behaviour(gen_server).

-include_lib("ucp_defines.hrl").
-include_lib("ucp_errors.hrl").


%%% API EXPORTS

-export([start/5, start_link/5, stop/1]).
-export([login/4, submit_sm/5]).          %% Client
-export([reply_result/2]).
-export([delivery_sm/5, notify_sm/4]).    %% Server

%%% THIS BEHAVIOUR EXPORTS

-export([behaviour_info/1]).


%%% GEN_SERVER BEHAVIOUR EXPORTS

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


%%% TEST EXPORTS

-export([extract_messages/1]).


%%% PRIVATE EXPORTS


%%% CONSTANTS

-define(TRN_SIZE, 100).
-define(MIN_TIMEOUT, 1000).
-define(MS_PER_SECOND, 1000).
-define(CALL_TIMEOUT, 2000).

-define(TRN_CNT_POS, 1).
-define(TRN_POS, 2).
-define(TIME_POS, 3).

-define(STX, 2).
-define(ETX, 3).

-ifdef(debug).
-define(FORMAT(Format, Data), io:format(Format, Data)).
-else.
-define(FORMAT(_Format, _Data), ok).
-endif.



%%% MACROS


%%% RECORDS

-record(st,
        {mod, %% Module for callbacks
         parent_ref, %% Parent process reference for callbacks

         %% Transaction numbering and windowing
         trn_cnt,
         trn, %% trn_cnt modulo 100 (TRN_SIZE)
         win_size_out,
         %% win_size_in, %% TODO: Input windowing not implemented
         result_timeout,
         result_timers, %% This field is also used for windowing

         %% Buffer and socket
         socket,
         inbuffer,

         %% Fields used (by clients) for keep alive (partly OT 31)
         keep_alive_period,
         keep_alive_timer,
         keep_alive_ot31_adc,
         keep_alive_ot31_pid
        }).


%%% EXTERNAL API FUNCTIONS

%%% ----------------------------------------------------------
%%% START AND STOP
%%% ----------------------------------------------------------

%%% @doc Start the session process.
%%%  The (tcp) socket shall be {active,false}
%%% @end
%%% @spec start(Module, ParentRef, TcpSocket, Opts, SrvOpts) -> Res
%%%  Module = atom()
%%%  ParentRef = term()
%%%  TcpSocket = pid()
%%%  Opts = opts()
%%%  ServOpts = opts()
%%%  Res = {ok, pid()} | {error, string()}
%%% @end
start(Module, ParentRef, TcpSocket, Opts, SrvOpts) ->
    {ok, Pid} =
        case get_value(name, Opts) of
            undefined ->
                gen_server:start(?MODULE,
                                 [Module, ParentRef, TcpSocket, Opts], SrvOpts);
            Name ->
                gen_server:start(Name,
                                 ?MODULE,
                                 [Module, ParentRef, TcpSocket, Opts], SrvOpts)
        end,
    ok = gen_tcp:controlling_process(TcpSocket, Pid),
    {ok, Pid}.

%%% @doc Start the session process linked.
%%%  The (tcp) socket shall be {active,false}
%%% @end
%%% @spec start_link(Module, ParentRef, TcpSocket, Opts, SrvOpts) -> Res
%%%  Module = atom()
%%%  ParentRef = term()
%%%  TcpSocket = pid()
%%%  Opts = opts()
%%%  ServOpts = opts()
%%%  Res = {ok, pid()} | {error, string()}
%%% @end
start_link(Module, ParentRef, TcpSocket, Opts, SrvOpts) ->
    {ok, Pid} =
        case get_value(name, Opts) of
            undefined ->
                gen_server:start_link(?MODULE,
                                 [Module, ParentRef, TcpSocket, Opts], SrvOpts);
            Name ->
                gen_server:start_link(Name,
                                 ?MODULE,
                                 [Module, ParentRef, TcpSocket, Opts], SrvOpts)
        end,
    ok = gen_tcp:controlling_process(TcpSocket, Pid),
    {ok, Pid}.

%%% @doc Stop the session process.
%%% @spec stop(Ref) -> ok
%%%  Ref = atom() | pid()
%%% @end
stop(Ref) ->
    gen_server:cast(ref_to_pid(Ref), stop).

%%% ----------------------------------------------------------
%%% SENDING
%%% ----------------------------------------------------------

%%% @doc Send a login via the session.
%%% @spec login(Ref, OAddr, PWD, Opt) -> Res
%%%  Ref = atom() | pid()
%%%  PWD = string()
%%%  Opt = opt()
%%%  Res = {ok, TRN} | {error, overlapping_trn | window_full}
%%%  TRN = integer()
%%% @end
login(Ref, OAddr, PWD, Opt) ->
    gen_server:call(ref_to_pid(Ref), {login, OAddr, PWD, Opt}, ?CALL_TIMEOUT).

%%% @doc Send a short message submit via the session.
%%% @spec submit_sm(Ref, Addr, OrigAddr, SM, Opt) -> Res
%%%  Opt = opt()
%%%  Ref = atom() | pid()
%%%  Addr = string()
%%%  OrigAddr = string()
%%%  SM = binary() | string()
%%%  Res = {ok, TRN} | {error, overlapping_trn | window_full}
%%%  TRN = integer()
%%% @end
submit_sm(Ref, Addr, OrigAddr, SM, Opt) ->
    gen_server:call(ref_to_pid(Ref),
                    {submit_sm, Addr, OrigAddr, SM, Opt}, ?CALL_TIMEOUT).

%%% @doc Send a short message delivery via the session.
%%% @spec delivery_sm(Ref, Addr, OrigAddr, SM, Opt) -> Res
%%%  Opt = opt()
%%%  Ref = atom() | pid()
%%%  Addr = string()
%%%  OrigAddr = string()
%%%  SM = string()
%%%  Res = {ok, TRN} | {error, overlapping_trn | window_full}
%%%  TRN = integer()
%%% @end
delivery_sm(Ref, Addr, OrigAddr, SM, Opt) ->
    gen_server:call(ref_to_pid(Ref),
                    {delivery_sm, Addr, OrigAddr, SM, Opt}, ?CALL_TIMEOUT).

%%% @doc Send a short message notification via the session.
%%% @spec notify_sm(Ref, Addr, OrigAddr, Opt) -> Res
%%%  Opt = opt()
%%%  Ref = atom() | pid()
%%%  Addr = string()
%%%  OrigAddr = string()
%%%  Res = {ok, TRN} | {error, overlapping_trn | window_full}
%%%  TRN = integer()
%%% @end
notify_sm(Ref, Addr, OrigAddr, Opt) ->
    gen_server:call(ref_to_pid(Ref),
                    {notify_sm, Addr, OrigAddr, Opt}, ?CALL_TIMEOUT).


%%% @doc Send a result message via the session.
%%% @spec reply_result(Ref, Message) -> ok
%%%  Ref = atom() | pid()
%%%  Message = string()
%%% @end
reply_result(Ref, Message) ->
    gen_server:call(ref_to_pid(Ref),
                    {reply, Message}, ?CALL_TIMEOUT).

%%% THIS BEHAVIOUR DEFINITION FUNCTION

%%% @doc Defines the callbacks for the behaviour.
%%%```
%%%* handle_incoming_operation(term(), pid(), OK_MESS) -> ack|nack|{nack, Reason}|nop
%%%* handle_incoming_result(term(), pid(), OK_MESS)
%%%* handle_incoming_unknown_result(term(), pid(), OK_MESS)
%%%* handle_outgoing_operation(term(), pid(), OK_MESS)
%%%* handle_outgoing_result(term(), pid(), OK_MESS)
%%%* handle_faulty_input(term(), pid(), ERR_MESS)
%%%* handle_timed_out_result(term(), pid(), TO_MESS)
%%%    OK_MESS  = {ok, ucp(), Mess::string(), WinInfo}
%%%    ERR_MESS = {Result::atom(), Info::term(), Mess::string(), WinInfo}
%%%    TO_MESS  = {timed_out, Trn, "", WinInfo}
%%%    WinInfo  = undefined | {in | out, Free::integer()}
%%%'''
%%%* handle_stopped(term(), pid())
%%%
%%% The two callbacks handle_incoming_*
%%% are called for incoming messages of operation and
%%% result type.
%%%
%%% The handle_incoming_unknown_result is acalled
%%% whenever a result without matching operation
%%% arrives.
%%%
%%% The two callbacks handle_outgoing_* are called for outgoing
%%% messages of operation and result type.
%%%
%%% The callback handle_faulty_input is called whenever
%%% a faulty message is found in the input.
%%%
%%% The callback handle_timed_out_result is called whenever
%%% a result does not arrive on time.
%%% @end

%%% TODO: Do we need more info in timed_out ??

%%% TODO: Input windowing not implemented

%%% @spec behaviour_info(Type::atom()) -> term()
behaviour_info(callbacks) ->
    [{handle_incoming_operation, 3},
     {handle_incoming_result, 3},
     {handle_incoming_unknown_result, 3},
     {handle_outgoing_operation, 3},
     {handle_outgoing_result, 3},
     {handle_faulty_input, 3},
     {handle_timed_out_result, 3},
     {handle_stopped, 2}];
behaviour_info(_Other) ->
    undefined.


%%% GEN_SERVER BEHAVIOUR CALLBACK FUNCTIONS

%%% ----------------------------------------------------------
%%% INIT
%%% ----------------------------------------------------------

%%% @private
init([Module, ParentRef, Socket, Opts]) ->
    format("init([~w, ~w, ~w, ~w])~n",
           [Module, ParentRef, Socket, Opts]),

    Rtimeout =
        case get_value(result_timeout, Opts) of
            undefined ->
                infinity;
            RT ->
                RT
        end,

    KAperiod =
        case get_value(keep_alive_period, Opts) of
            undefined ->
                infinity;
            KAP ->
                KAP
        end,
    KAADC = get_value(keep_alive_ot31_adc, Opts),
    KAPID = get_value(keep_alive_ot31_pid, Opts),

    WinSizeOut =
        case get_value(win_size_out, Opts) of
            undefined ->
                ?TRN_SIZE;
            W ->
                W
        end,

    State =
        #st {mod = Module,
             parent_ref = ParentRef,
             trn_cnt = 0,
             trn = 0,
             win_size_out = WinSizeOut,
             %% TODO: Input windowing not implemented
             %% win_size_in = WinSizeIn,
             keep_alive_period = KAperiod,
             keep_alive_timer = timestamp(KAperiod),
             keep_alive_ot31_adc = KAADC,
             keep_alive_ot31_pid = KAPID,
             result_timeout = Rtimeout,
             result_timers = [],
             socket = Socket,
             inbuffer = []
            },

    ok = inet:setopts(Socket, [{active, once}]),

    {ok, State, get_timeout(State)}.

%%% ----------------------------------------------------------
%%% HANDLE_CALL
%%% ----------------------------------------------------------

%%% @private
handle_call({login, OAddr, PWD, Opt}, From, State) ->
    format("handle_call( {login, ~w, ~w, ~w}, ~w, ~w)~n",
           [OAddr, PWD, Opt, From, State]),

    MakeFun =
        fun(Trn) ->
                ucp_syntax:make_session_management(Trn,
                                                   OAddr,
                                                   ?UCP_STYP_OPEN_SESSION,
                                                   PWD,
                                                   Opt)
        end,
    NewState = try_to_send_message(MakeFun, State, From),
    {noreply, NewState, get_timeout(NewState)};

handle_call({submit_sm, Addr, OrigAddr, SM, Opt}, From, State) ->
    format("handle_call( {submit_sm, ~w, ~w, ~w}, ~w, ~w)~n",
           [Addr, OrigAddr, SM, From, State]),

    MakeFun =
        fun(Trn) ->
                ucp_syntax:make_submit_sm(Trn,
                                          Addr,
                                          OrigAddr,
                                          SM,
                                          Opt)
        end,
    NewState = try_to_send_message(MakeFun, State, From),
    {noreply, NewState, get_timeout(NewState)};

%%% TODO - copy and paste of submit_sm
handle_call({delivery_sm, Addr, OrigAddr, SM, Opt}, From, State) ->
    format("handle_call( {delivery_sm, ~w, ~w, ~w}, ~w, ~w)~n",
           [Addr, OrigAddr, SM, From, State]),

    MakeFun =
        fun(Trn) ->
                ucp_syntax:make_delivery_sm(Trn,
                                            Addr,
                                            OrigAddr,
                                            SM,
                                            Opt)
        end,
    NewState = try_to_send_message(MakeFun, State, From),
    {noreply, NewState, get_timeout(NewState)};

%%% TODO - copy and paste of submit_sm
handle_call({notify_sm, Addr, OrigAddr, Opt}, From, State) ->
    format("handle_call( {notify_sm, ~w, ~w}, ~w, ~w)~n",
           [Addr, OrigAddr, From, State]),

    MakeFun =
        fun(Trn) ->
                ucp_syntax:make_delivery_notification(Trn,
                                                      Addr,
                                                      OrigAddr,
                                                      Opt)
        end,
    NewState = try_to_send_message(MakeFun, State, From),
    {noreply, NewState, get_timeout(NewState)};

handle_call({reply, Message}, From, State) ->
    format("handle_call( {reply, ~w}, ~w, ~w)~n",
           [Message, From, State]),
    NewState = send_result(Message, State),
    {reply, ok, NewState};


handle_call(Request, _From, State) ->
    error_logger:warning_msg("Unexpected Call ~w~n", [Request]),

    {reply, ok, State, get_timeout(State)}.

%%% ----------------------------------------------------------
%%% HANDLE_CAST
%%% ----------------------------------------------------------

%%% @private
handle_cast(stop, State) ->
    format("handle_cast( stop, ~w)~n", [State]),

    Module = State#st.mod,
    Module:handle_stopped(State#st.parent_ref, self()),
    NewState = close_socket(State),

    {stop, normal, NewState};

handle_cast(Request, State) ->
    error_logger:warning_msg("Unexpected Cast ~w~n", [Request]),

    {noreply, State, get_timeout(State)}.

%%% ----------------------------------------------------------
%%% HANDLE_INFO
%%% ----------------------------------------------------------

%%% @private
handle_info(timeout, State) ->
    format("handle_info( timeout, ~w)~n", [State]),

    {KeepAliveTA, ResultTA} = get_timed_out(State),

    TmpState =
        if
            KeepAliveTA ->
                send_keep_alive(State);
            true ->
                State
        end,
    NewState =
        if
            ResultTA ->
                time_out_results(TmpState);
            true ->
                TmpState
        end,

    {noreply, NewState, get_timeout(NewState)};

handle_info({tcp, Socket, Data}, State) ->
    format("handle_info( {tcp, ~w, ~p}, ~w)~n",
           [Socket, ucp_syntax:prettify(Data), State]),

    Bytes = State#st.inbuffer ++ Data,
    {Messages, BytesLeft} = extract_messages(Bytes),
    TmpState =
        lists:foldl(fun(M, S) -> notify_message(M, S) end, State, Messages),

    ok = inet:setopts(Socket, [{active, once}]),

    NewState = TmpState#st{inbuffer = BytesLeft},
    {noreply, NewState, get_timeout(NewState)};

handle_info({tcp_closed, Socket}, State) ->
    format("handle_info( {tcp_closed, ~w}, ~w)~n", [Socket, State]),

    Module = State#st.mod,

    case State#st.inbuffer of
        "" ->
            ok;
        Bytes ->
            ERR_MESS = {garbage, undefined, Bytes, undefined},
            Module:handle_faulty_input(State#st.parent_ref, self(), ERR_MESS)
    end,

    Module:handle_stopped(State#st.parent_ref, self()),

    NewState = State#st{socket = undefined, inbuffer = ""},
    {stop, normal, NewState};

handle_info({tcp_error, _Socket, Reason}, State) ->
    error_logger:warning_msg("Unexpected TCP error ~w~n", [Reason]),

    {noreply, State, get_timeout(State)};

handle_info(Info, State) ->
    error_logger:warning_msg("Unexpected Info ~w~n", [Info]),

    {noreply, State, get_timeout(State)}.

%%% ----------------------------------------------------------
%%% TERMINATE
%%% ----------------------------------------------------------

%%% @private
terminate(Reason, State) ->
    format("terminate( ~w, ~w)~n", [Reason, State]),

    close_socket(State),

    void.

%%% ----------------------------------------------------------
%%% CODE_CHANGE
%%% ----------------------------------------------------------

%%% @private
code_change(OldVsn, State, Extra) ->
    format("code_change( ~w, ~w, ~w)~n", [OldVsn, State, Extra]),

    {ok, State}.


%%% PRIVATE FUNCTIONS

%%% ----------------------------------------------------------
%%% REF_TO_PID
%%%
%%% Converts a ref/pid to its pid
%%% ----------------------------------------------------------

ref_to_pid(Ref) when is_pid(Ref) ->
    Ref;
ref_to_pid(Ref) when is_atom(Ref) ->
    whereis(Ref);
ref_to_pid({global, Name}) ->
    global:whereis_name(Name);
ref_to_pid({Name, Node}) ->
    rpc:call(Node, erlang, whereis, [Name]).

%%% ----------------------------------------------------------
%%% GET_VALUE
%%%
%%% Fetches a value from a key-list.  This function has the same
%%% semantics as the fields in a record, i.e. undefined is a special
%%% value
%%% ----------------------------------------------------------

get_value(Key, UCP) ->
    case lists:keysearch(Key, 1, UCP) of
        {value, {_,Value}} ->
            Value;
        false -> undefined
    end.

%%% ----------------------------------------------------------
%%% TRY_TO_SEND_MESSAGE
%%%
%%% A wrapper function for send_message. It tests if sending is
%%% allowed and it also builds the message, using a provided fun.
%%%
%%% IMPORTANT: this function have to call gen_server:reply !!
%%%            if the From field is not 'undefined'
%%% ----------------------------------------------------------

try_to_send_message(MakeFun, State, From) ->
    format("Try to send message = ~p ~p~n", [MakeFun, State]),

    %% Build the login message and send it (to the SMSC).

    Trn = State#st.trn,
    Rtimers = State#st.result_timers,

    case lists:keymember(Trn, ?TRN_POS, Rtimers) of
        true ->
            error_logger:warning_msg("Next TRN (~w) in use~n", [Trn]),
            reply(From, {error, overlapping_trn}),
            State;
        false ->
            NextTrnCnt = State#st.trn_cnt,
            Rtimers = State#st.result_timers,
            WinSize = State#st.win_size_out,
            Free = compute_free_window(Rtimers, NextTrnCnt, WinSize),
            if
                Free < 1 ->
                    error_logger:warning_msg("Output window full~n", []),
                    reply(From, {error, window_full}),
                    State;
                true ->
                try

                    reply(From, {ok, Trn}),
                    Message = MakeFun(Trn),
                    NewState = send_message(Message, State),
                    NewState
                catch _:_ ->
                    io:format("~n~n ERROR: ~p", [erlang:get_stacktrace()])
                end
            end
    end.

%%% Help function

reply(undefined, _) ->
    ok;
reply(From, Message) ->
    gen_server:reply(From, Message).

%%% ----------------------------------------------------------
%%% SEND_MESSAGE
%%%
%%% A generic function that sends messages
%%% ----------------------------------------------------------

send_message(Message, State) ->
    format("Message = ~p~n", [ucp_syntax:prettify(Message)]),

    Socket = State#st.socket,
    ok = gen_tcp:send(Socket, Message),

    TrnCnt = State#st.trn_cnt,
    Trn = State#st.trn,
    KAtimer = timestamp(State#st.keep_alive_period),
    Rtimer = timestamp(State#st.result_timeout),

    %% Insert the new Trn last in time order
    %% This assures the first in the list is the next timeout
    Rtimers = State#st.result_timers,
    NewRtimers = Rtimers ++ [{TrnCnt, Trn, Rtimer}],

    %% TODO: is it a good choice to parse all outgoing messages?
    %% Its only made for the user's convenience
    UCP= ucp_syntax:parse(Message),

    Module = State#st.mod,
    WinSize = State#st.win_size_out,
    NextTrnCnt = TrnCnt + 1,
    WinInfoOut = {out, compute_free_window(NewRtimers, NextTrnCnt, WinSize)},
    OK_MESS = {ok, UCP, Message, WinInfoOut},
    Module:handle_outgoing_operation(State#st.parent_ref, self(), OK_MESS),

    State#st{trn_cnt = NextTrnCnt,
             trn = (Trn + 1) rem ?TRN_SIZE,
             keep_alive_timer = KAtimer,
             result_timers = NewRtimers}.

%%% ----------------------------------------------------------
%%% SEND_RESULT
%%%
%%% A generic function that sends results
%%% ----------------------------------------------------------

send_result(Message, State) ->
    format("Result = ~p~n", [ucp_syntax:prettify(Message)]),

    Socket = State#st.socket,
    ok = gen_tcp:send(Socket, Message),

    %% TODO: is it a good choice to parse all outgoing results?
    %% Its only made for the user's convenience
    UCP = ucp_syntax:parse(Message),

    Module = State#st.mod,

    %% TODO: Input windowing not implemented
    WinInfoIn = undefined,

    OK_MESS = {ok, UCP, Message, WinInfoIn},
    Module:handle_outgoing_result(State#st.parent_ref, self(), OK_MESS),

    State#st{keep_alive_timer = timestamp(State#st.keep_alive_period)}.

%%% ----------------------------------------------------------
%%% CLOSE_SOCKET
%%%
%%% Closes the socket if its open
%%% ----------------------------------------------------------

close_socket(State) ->
    case State#st.socket of
        undefined ->
            State;
        Socket ->
            catch gen_tcp:close(Socket),
            State#st{socket = undefined}
    end.

%%% ----------------------------------------------------------
%%% EXTRACT_MESSAGES
%%%
%%% Extract messages from a list of bytes from a TCP stream.
%%% A message starts with STX (2) and ends with STX (3).
%%% A well behaved stream looks like this |2...32...32...|
%%% where in this example the STX of the last packet not yet
%%% has arrived. There are some error cases:
%%% 1. Waiting for STX you get something else
%%%    => skip it
%%% 2. Waiting for ETX you get STX
%%%    => restart waiting for ETX
%%% 3. Waiting for ETX you reach end of buffer
%%%    => save what you got in next inbuffer
%%%
%%% TODO: 4 and 5 are not implemented yet
%%%
%%% 4. No correct messages arrives for MAX chars
%%%    => take down line
%%% 5. Waiting for ETX it does not arrive for MAX time
%%%    => take down line
%%% ----------------------------------------------------------

%%% @private
extract_messages(Bytes) ->
    search_for_start(Bytes, [], []).

%%% Search for STX (i.e. start of message)
search_for_start([?STX|Bytes], [], Msgs) ->
    search_for_end(Bytes, [?STX], Msgs);
search_for_start([?STX|Bytes], Acc, Msgs) ->
    search_for_end(Bytes, [?STX], Msgs ++ [{garbage, Acc}]);
search_for_start([B|Bytes], Acc, Msgs) ->
    search_for_start(Bytes, Acc ++ [B], Msgs);
search_for_start([], [], Msgs) ->
    {Msgs, []};
search_for_start([], Acc, Msgs) ->
    {Msgs ++ [{garbage, Acc}], []}.

%%% Search for ETX (i.e. end of message)
search_for_end([?STX|_]=Bytes, Acc, Msgs) ->
    search_for_start(Bytes, Acc, Msgs);
search_for_end([?ETX|Bytes], Acc, Msgs) ->
    search_for_start(Bytes, [], Msgs ++ [{ok, Acc ++ [?ETX]}]);
search_for_end([B|Bytes], Acc, Msgs) ->
    search_for_end(Bytes, Acc ++ [B], Msgs);
search_for_end([], Acc, Msgs) ->
    {Msgs, Acc}.


%%% ----------------------------------------------------------
%%% NOTIFY_MESSAGE, NOTIFY_OPERATION, NOTIFY_RESULT
%%%
%%% Calls appropriate callback function for every message block,
%%% faulty or not, found by extract_messages/1.
%%% ----------------------------------------------------------

notify_message({ok, Message}, State) ->
    format("Notify ok message = ~p~n",
           [ucp_syntax:prettify(Message)]),

    NewState =
        case catch ucp_syntax:parse(Message) of
            {'EXIT', Reason} ->
                notify_parse_error(Reason, Message, State);
            UCP ->
                case get_value(type, UCP) of
                    "O" ->
                        notify_operation(UCP, Message, State);
                    "R" ->
                        notify_result(UCP, Message, State)
                end
        end,
    NewState;
notify_message({Result, Message}, State) ->
    format("Notify ~w message = ~p~n",
           [Result, ucp_syntax:prettify(Message)]),

    Module = State#st.mod,
    ERR_MESS = {Result, undefined, Message, undefined},
    Module:handle_faulty_input(State#st.parent_ref, self(), ERR_MESS),
    State.

notify_parse_error(Reason, Message, State) ->
    format("  parse error~n", []),

    Module = State#st.mod,
    ERR_MESS = {parse_error, Reason, Message, undefined},
    Module:handle_faulty_input(State#st.parent_ref, self(), ERR_MESS),
    State.

notify_operation(UCP, Message, State) ->
    format("  operation~n", []),

    Module = State#st.mod,

    %% TODO: Input windowing not implemented
    WinInfoIn = undefined,

    OK_MESS = {ok, UCP, Message, WinInfoIn},
    Result =
        case Module:handle_incoming_operation(State#st.parent_ref, self(), OK_MESS) of
            ack ->
                ucp_syntax:make_ack(UCP);
            {ack, SM} ->
                ucp_syntax:make_ack(UCP, SM);
            nack ->
                Reason = ?UCP_NACK_OPERATION_NOT_SUPPORTED,
                ucp_syntax:make_nack(UCP, Reason);
            {nack, Reason} ->
                ucp_syntax:make_nack(UCP, Reason);
            _ ->
                undefined
        end,

    case Result of
        undefined ->
            State;
        Result ->
            format("    result=~p~n", [ucp_syntax:prettify(Result)]),
            send_result(Result, State)
    end.

notify_result(UCP, Message, State) ->
    format("  result~n", []),

    Trn = get_value(trn, UCP),

    Module = State#st.mod,
    Rtimers = State#st.result_timers,

    case lists:keymember(Trn, ?TRN_POS, Rtimers) of
        true ->
            NewRtimers = lists:keydelete(Trn, 2, Rtimers),
            WinSize = State#st.win_size_out,
            NextTrnCnt = State#st.trn_cnt,
            WinInfoOut =
                {out, compute_free_window(NewRtimers, NextTrnCnt, WinSize)},
            OK_MESS = {ok, UCP, Message, WinInfoOut},
            Module:handle_incoming_result(State#st.parent_ref, self(), OK_MESS),
            State#st{result_timers = NewRtimers};
        false ->
            ERR_MESS = {unknown_result, UCP, Message, undefined},
            Module:handle_incoming_unknown_result(State#st.parent_ref, self(), ERR_MESS),
            State
    end.

%%% ----------------------------------------------------------
%%% TIMESTAMP
%%%
%%% Returns a timestamt T seconds in the future
%%% ----------------------------------------------------------

timestamp(infinity) ->
    infinity;
timestamp(T) ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + T.

%%% ----------------------------------------------------------
%%% GET_TIMEOUT
%%%
%%% Computes the next timeout, given state
%%% Never returns a timeout less than a second
%%% ----------------------------------------------------------

get_timeout(State) ->
    {KAtimeout, Rtimeout} = get_timeouts(State),

    Timeout =
        case {KAtimeout, Rtimeout} of
            {infinity, T} ->
                T;
            {T, infinity} ->
                T;
            {T1, T2} ->
                if T1 < T2 -> T1 ; true -> T2 end
        end,

    case Timeout of
        infinity ->
            infinity;
        _ ->
            if
                Timeout < ?MIN_TIMEOUT ->
                    format("WARNING: low timeout ~w~n", [Timeout]),
                    ?MIN_TIMEOUT;
                true ->
                    Timeout
            end
    end.

%%% ----------------------------------------------------------
%%% GET_TIMED_OUT
%%%
%%% Return a pair of boolean telling whether keep alive and result
%%% have timed out.
%%% ----------------------------------------------------------

get_timed_out(State) ->
    {KAtimeout, Rtimeout} = get_timeouts(State),
    {gto(KAtimeout), gto(Rtimeout)}.

%%% ----------------------------------------------------------
%%% GET_TIMEOUTS
%%%
%%% Return a pair of timeout values for keep alive and result timers
%%% ----------------------------------------------------------

get_timeouts(State) ->
    KAtimeout = compute_timeout(State#st.keep_alive_timer),
    Rtimeout =
        case State#st.result_timers of
            [] ->
                infinity;
            [{_,_,Timer}|_] ->
                compute_timeout(Timer)
        end,
    {KAtimeout, Rtimeout}.

%%% ----------------------------------------------------------
%%% COMPUTE_TIMEOUT
%%%
%%% given the value of a timer - compute the timeout value
%%% ----------------------------------------------------------

compute_timeout(Timer) ->
    case Timer of
        infinity ->
            infinity;
        T ->
            (T - timestamp(0)) * ?MS_PER_SECOND
    end.

%%% Help function

gto(infinity) ->
    false;
gto(Time) when Time < ?MIN_TIMEOUT ->
    true;
gto(_) ->
    false.

%%% ----------------------------------------------------------
%%% SEND_KEEP_ALIVE
%%%
%%% Send a keep alive message
%%% ----------------------------------------------------------

send_keep_alive(State) ->
    format("send_keep_alive(~w)~n", [State]),

    %% At a timeout, i.e. whan nothing have happened for a certain
    %% time, build a keep alive message and send it (to the SMSC).

    MakeFun =
        fun(Trn) ->
                ucp_syntax:make_mt_alert(Trn,
                                         State#st.keep_alive_ot31_adc,
                                         State#st.keep_alive_ot31_pid)
        end,
    NewState = try_to_send_message(MakeFun, State, undefined),
    NewState.

%%% ----------------------------------------------------------
%%% TIME_OUT_RESULTS
%%%
%%% Traverse the result timer list and time out results
%%% ----------------------------------------------------------

time_out_results(State) ->
    format("time_out_results(~w)~n", [State]),

    Timers = State#st.result_timers,
    NewTimers = tors(Timers, State, timestamp(0), []),
    State#st{result_timers = NewTimers}.

tors([], _, _, NewTs) ->
    NewTs;
tors([T = {_,_,infinity}|Ts], State, Timestamp, NewTs) ->
    tors(Ts, State, Timestamp, NewTs ++ [T]);
tors([T = {_,_,Time}|Ts], State, Timestamp, NewTs) when Time > Timestamp ->
    tors(Ts, State, Timestamp, NewTs ++ [T]);
tors([{_,Trn, _}|Ts], State, Timestamp, NewTs) ->
    Module = State#st.mod,
    WinSize = State#st.win_size_out,
    NextTrnCnt = State#st.trn_cnt,
    NewRtimers = NewTs ++ Ts, %% All but so far removed timers
    WinInfoOut = {out, compute_free_window(NewRtimers, NextTrnCnt, WinSize)},
    TO_MESS = {timed_out, Trn, "", WinInfoOut},
    Module:handle_timed_out_result(State#st.parent_ref, self(), TO_MESS),
    tors(Ts, State, Timestamp, NewTs).

%%% ----------------------------------------------------------
%%% COMPUTE_FREE_WINDOW and COMPUTE_USED_WINDOW
%%%
%%% Computes how large part of the window that is free or used.
%%%
%%% Assumes not wrap around time (i.e. like trn_cnt) and timers.
%%%
%%% Assumes NextTrnCnt is the next ID that is going to be allocated.
%%% The actual ID allocated is NextTrnCnt rem 100.
%%%
%%% Assumes Rtimers sorted - oldest fist
%%% ----------------------------------------------------------

compute_free_window(Rtimers, NextTrnCnt, WinSize) ->
    WinSize - compute_used_window(Rtimers, NextTrnCnt).

compute_used_window(Rtimers, NextTrnCnt) ->
    case Rtimers of
        [] ->
            0;
        [{OldestTrnCnt,_,_}|_] ->
            NextTrnCnt - OldestTrnCnt
    end.

%%% ----------------------------------------------------------
%%% FORMAT
%%%
%%% Just a temporary thing that makes it easy to turn off logging
%%% ----------------------------------------------------------

format(Format, Data) ->
    ?FORMAT(Format, Data).
