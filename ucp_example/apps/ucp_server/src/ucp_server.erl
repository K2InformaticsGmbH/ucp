-module(ucp_server).

% Public functions
-export([start_link/1,
         stop/0,
         send_dr/4,
         send_fake_dr/6,
         send_mo/3,
         send_concat_mo/3,
         set_variable/2,
         get_variable/1,
         get_mts/0]).

% gen_ucp_server callbacks
-behaviour(gen_ucp_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2,
         handle_request/6,
         handle_response/5,
         handle_message/4,
         handle_stop/3]).

-include_lib("ucperl/include/ucp_defines.hrl").
-include_lib("util/include/util.hrl").
-include("ucp_server.hrl").

-define(DEFAULT_LOGIN_RESPONSE, ack).
-define(DEFAULT_RECEIVE_MT_RESPONSE, ack).
-define(DEFAULT_AUTO_DRS, true).
-define(DEFAULT_AUTO_DR_DELAY, 0).
-define(DEFAULT_AUTO_DR_DELIVERY_STATUS, 0).
-define(DEFAULT_AUTO_DR_ERROR_CODE, 0).

-define(PART_MO_MAX_BYTE_BODY_LEN, 134).

-define(GSM_DCS_DEFAULT, 0).

-define(MSG_ID_LENGTH, 12).

-record(simple_mt, {source :: string(),      % Also known as OADC
                    destination :: string(), % Also known as ADC
                    body :: string(),        % Self-explanatory
                    msg_id :: string(),      % message ID
                    timestamp :: datetime(), % The date and time when the Mt was sent
                    dr_pid :: pid()}).       % The PID of the automatic DR process

-record(st, {srv_ref :: atom(),      % Cached server reference
             port :: integer(),      % Listen port
             sessions :: [pid()],    % List of currently active sessions
             mts :: [#simple_mt{}],  % List of MTs received
             vars :: dict(),         % Current values of variables
             session_reqs :: dict()}).


% ----------------------------------------------------------------------------
% Public functions
% ----------------------------------------------------------------------------

-spec start_link(integer()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    SrvRef = ?MODULE,
    ucp_server_util:log_start(SrvRef),
    gen_ucp_server:start_link({local, SrvRef}, ?MODULE, Port).

-spec stop() -> ok.
stop() ->
    SrvRef = ?MODULE,
    case whereis(SrvRef) of
        undefined ->
            ok;
        Pid ->
            exit(Pid, shutdown)
    end.

-spec send_dr(string(), datetime(), integer(), integer()) -> ok | {error, term()}.
send_dr(MsgId, Timestamp, DeliveryStatus, ErrorCode) ->
    SrvRef = ?MODULE,
    gen_server:call(SrvRef, {send_dr, MsgId, Timestamp, DeliveryStatus, ErrorCode}).

-spec send_fake_dr(string(), string(), string(), datetime(), integer(), integer()) -> ok | {error, term()}.
send_fake_dr(Destination, Source, MsgId, Timestamp, DeliveryStatus, ErrorCode) ->
    SrvRef = ?MODULE,
    gen_server:call(SrvRef, {send_fake_dr, Destination, Source, MsgId, Timestamp, DeliveryStatus, ErrorCode}).

-spec send_mo(string(), string(), string()) -> ok | {error, term()}.
send_mo(Destination, Source, Mo) ->
    SrvRef = ?MODULE,
    gen_server:call(SrvRef, {send_mo, Destination, Source, Mo}).

-spec send_concat_mo(string(), string(), string()) -> ok | {error, term()}.
send_concat_mo(Destination, Source, Mo) ->
    SrvRef = ?MODULE,
    gen_server:call(SrvRef, {send_concat_mo, Destination, Source, Mo}).

-spec set_variable(server_variable(), term()) -> ok | {error, term()}.
set_variable(Var, Val) ->
    SrvRef = ?MODULE,
    gen_server:call(SrvRef, {set_variable, Var, Val}).

-spec get_variable(server_variable()) -> {ok, term()} | {error, term()}.
get_variable(Var) ->
    SrvRef = ?MODULE,
    gen_server:call(SrvRef, {get_variable, Var}).

-spec get_mts() -> [{string(), string(), string(), string(), datetime()}].
get_mts() ->
    SrvRef = ?MODULE,
    gen_server:call(SrvRef, get_mts).


% ----------------------------------------------------------------------------
% gen_ucp_server callbacks
% ----------------------------------------------------------------------------

-spec init(integer()) -> {ok, #st{}}.
init(Port) ->
    random:seed(now()),
    {ok, Vars} = application:get_env(ucp_server, variables),
    LoginResponse = util_proplists:fetch(login_response, Vars, ?DEFAULT_LOGIN_RESPONSE),
    ReceiveMtResponse = util_proplists:fetch(receive_mt_response, Vars, ?DEFAULT_RECEIVE_MT_RESPONSE),
    AutoDrs = util_proplists:fetch(auto_drs, Vars, ?DEFAULT_AUTO_DRS),
    AutoDrDelay = util_proplists:fetch(auto_dr_delay, Vars, ?DEFAULT_AUTO_DR_DELAY),
    AutoDrDeliveryStatus = util_proplists:fetch(auto_dr_delivery_status, Vars, ?DEFAULT_AUTO_DR_DELIVERY_STATUS),
    AutoDrErrorCode = util_proplists:fetch(auto_dr_error_code, Vars, ?DEFAULT_AUTO_DR_ERROR_CODE),
    Vars = [{login_response, LoginResponse},
            {receive_mt_response, ReceiveMtResponse},
            {auto_drs, AutoDrs},
            {auto_dr_delay, AutoDrDelay},
            {auto_dr_delivery_status, AutoDrDeliveryStatus},
            {auto_dr_error_code, AutoDrErrorCode}],
    SrvRef = ?MODULE,
    St = #st{srv_ref = SrvRef, port = Port, sessions = [], mts = [], vars = dict:from_list(Vars), session_reqs = dict:new()},
    spawn_link(fun () -> listen(St) end),
    {ok, St}.

-spec handle_call(term(), term(), #st{}) -> term().
handle_call({send_dr, MsgId, Timestamp, DeliveryStatus, ErrorCode}, _From, St) ->
    case lists:keyfind(MsgId, #simple_mt.msg_id, St#st.mts) of
        #simple_mt{} = Mt ->
            case random_session(St) of
                {ok, Session} ->
                    Params = make_send_dr_params(Mt#simple_mt.destination, Mt#simple_mt.msg_id, Timestamp, DeliveryStatus, ErrorCode),
                    ucp_server_util:log_sending_dr(St#st.srv_ref, Session, Mt#simple_mt.msg_id),
                    ucp_server_util:log_dr_request(St#st.srv_ref, Session, Params),
                    gen_ucp_server:send_dr(St#st.srv_ref, Session, Mt#simple_mt.source, Mt#simple_mt.destination, Params, [Mt#simple_mt.msg_id]),
                    NewMts = lists:keydelete(MsgId, #simple_mt.msg_id, St#st.mts),
                    {reply, ok, St#st{mts = NewMts}};
                undefined ->
                    {reply, {error, session_not_found}, St}
            end;
        false ->
            {reply, {error, mt_not_found}, St}
    end;
handle_call({send_fake_dr, Destination, Source, MsgId, Timestamp, DeliveryStatus, ErrorCode}, _From, St) ->
    case random_session(St) of
        {ok, Session} ->
            Params = make_send_dr_params(Destination, MsgId, Timestamp, DeliveryStatus, ErrorCode),
            ucp_server_util:log_sending_dr(St#st.srv_ref, Session, "fake"),
            ucp_server_util:log_dr_request(St#st.srv_ref, Session, Params),
            gen_ucp_server:send_dr(St#st.srv_ref, Session, Source, Destination, Params, ["fake"]),
            {reply, ok, St};
        undefined ->
            {reply, {error, session_not_found}, St}
    end;
handle_call({send_concat_mo, Destination, Source, Mo}, _From, St) ->
    case random_session(St) of
        {ok, Session} ->
            MultiGsm = util_gsm0338:encode_parts(Mo, $?, ?PART_MO_MAX_BYTE_BODY_LEN),
            TotalPart = length(MultiGsm),
            PartReference = random:uniform(255),
            lists:foldl(fun({EncodedMo, CharNum}, {PrevLen, PartNumber}) ->
                            Params = make_send_concat_mo_params(PartReference, TotalPart, PartNumber),
                            %PartBody = lists:sublist(Mo, PrevLen + 1, CharNum),
                            %% TODO: add log with mo?
                            ucp_server_util:log_sending_mo(St#st.srv_ref, Session),
                            ucp_server_util:log_mo_request(St#st.srv_ref, Session, Params),
                            gen_ucp_server:send_mo(St#st.srv_ref, Session, Destination, Source, EncodedMo, Params, []),
                            {PrevLen + CharNum, PartNumber + 1}
                        end, {0, 1}, MultiGsm),
            {reply, ok, St};
        undefined ->
            {reply, {error, session_not_found}, St}
    end;
handle_call({send_mo, Destination, Source, Mo}, _From, St) ->
    case random_session(St) of
        {ok, Session} ->
            Params = make_send_mo_params(),
            ucp_server_util:log_sending_mo(St#st.srv_ref, Session),
            ucp_server_util:log_mo_request(St#st.srv_ref, Session, Params),
            {EncodedMo, _} = util_gsm0338:encode(Mo, $?),
            gen_ucp_server:send_mo(St#st.srv_ref, Session, Destination, Source, EncodedMo, Params, []),
            {reply, ok, St};
        undefined ->
            {reply, {error, session_not_found}, St}
    end;
handle_call({set_variable, Var, Val}, _From, St) ->
    case dict:is_key(Var, St#st.vars) of
        true ->
            NewVars = dict:store(Var, Val, St#st.vars),
            case {Var, Val} of
                {auto_drs, AutoDrs} ->
                    NewMts = lists:map(switch_auto_drs_fun(AutoDrs, St#st.vars), St#st.mts),
                    {reply, ok, St#st{vars = NewVars, mts = NewMts}};
                _ ->
                    {reply, ok, St#st{vars = NewVars}}
            end;
        false ->
            {reply, {error, var_not_found}, St}
    end;
handle_call({get_variable, Var}, _From, St) ->
    case dict:is_key(Var, St#st.vars) of
        true ->
            Val = dict:fetch(Var, St#st.vars),
            {reply, {ok, Val}, St};
        false ->
            {reply, {error, var_not_found}, St}
    end;
handle_call(get_mts, _From, St) ->
    % TODO: Update the element order here to reflect the order in the updated simple_mt record
    Mts = lists:map(fun (Mt) ->
                            {Mt#simple_mt.msg_id, Mt#simple_mt.source, Mt#simple_mt.destination, Mt#simple_mt.body, Mt#simple_mt.timestamp}
                    end, St#st.mts),
    {reply, Mts, St};
handle_call(_Msg, _From, St) ->
    {reply, ok, St}.

-spec handle_cast(term(), #st{}) -> term().
handle_cast(_Msg, St) ->
    {noreply, St}.

-spec handle_info(term(), #st{}) -> {noreply, #st{}}.
handle_info(_Info, St) ->
    {noreply, St}.

-spec code_change(term(), #st{}, term()) -> {ok, #st{}}.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

-spec terminate(term(), #st{}) -> ok.
terminate(_Reason, _St) ->
    ok.

-spec handle_request(pid(), atom(), list(), reference(), list(), #st{}) -> {noreply, #st{}}.
handle_request(Session, send_dr, [_Destination, _Source, _Params], Ref, [MsgId], St) ->
    ucp_server_util:log_send_dr_request(St#st.srv_ref, Session, MsgId, Ref),
    Reqs = dict:fetch(Session, St#st.session_reqs),
    NewReqs = dict:store(Ref, {send_dr, MsgId}, Reqs),
    NewSessionReqs = dict:store(Session, NewReqs, St#st.session_reqs),
    {noreply, St#st{session_reqs = NewSessionReqs}};
handle_request(Session, send_mo, [_Destination, _Source, _Mo, _Params], Ref, [], St) ->
    ucp_server_util:log_send_mo_request(St#st.srv_ref, Session, Ref),
    Reqs = dict:fetch(Session, St#st.session_reqs),
    NewReqs = dict:store(Ref, send_mo, Reqs),
    NewSessionReqs = dict:store(Session, NewReqs, St#st.session_reqs),
    {noreply, St#st{session_reqs = NewSessionReqs}}.

-spec handle_response(pid(), ok | {error, term()}, proplist(), reference(), #st{}) -> {noreply, #st{}}.
handle_response(Session, Resp, Ucp, Ref, St) ->
    Reqs = dict:fetch(Session, St#st.session_reqs),
    case dict:find(Ref, Reqs) of
        {ok, {send_dr, MsgId}} ->
            case Resp of
                ok ->
                    case util_proplists:find(ack, Ucp) of
                        {ok, _Ack} ->
                            ucp_server_util:log_send_dr_success(St#st.srv_ref, Session, MsgId, Ref);
                        undefined ->
                            ErrorCode = util_proplists:fetch(ec, Ucp, -1),
                            Description = util_proplists:fetch(sm, Ucp, []),
                            ucp_server_util:log_send_dr_failure(St#st.srv_ref, Session, MsgId, Ref, {request_nacked, ErrorCode, Description})
                    end;
                {error, Reason} ->
                    ucp_server_util:log_send_dr_failure(St#st.srv_ref, Session, MsgId, Ref, Reason)
            end,
            NewReqs = dict:erase(Ref, Reqs),
            NewSessionReqs = dict:store(Session, NewReqs, St#st.session_reqs),
            {noreply, St#st{session_reqs = NewSessionReqs}};
        {ok, send_mo} ->
            case Resp of
                ok ->
                    case util_proplists:find(ack, Ucp) of
                        {ok, _Ack} ->
                            ucp_server_util:log_send_mo_success(St#st.srv_ref, Session, Ref);
                        undefined ->
                            ErrorCode = util_proplists:fetch(ec, Ucp, -1),
                            Description = util_proplists:fetch(sm, Ucp, []),
                            ucp_server_util:log_send_mo_failure(St#st.srv_ref, Session, Ref, {request_nacked, ErrorCode, Description})
                    end;
                {error, Reason} ->
                    ucp_server_util:log_send_mo_failure(St#st.srv_ref, Session, Ref, Reason)
            end,
            NewReqs = dict:erase(Ref, Reqs),
            NewSessionReqs = dict:store(Session, NewReqs, St#st.session_reqs),
            {noreply, St#st{session_reqs = NewSessionReqs}};
        error ->
            ucp_server_util:log_unknown_response(St#st.srv_ref, Session, Ref, Resp, Ucp),
            {noreply, St}
    end.

-spec handle_message(pid(), login | receive_mt, proplist(), #st{}) -> {reply, ack | {ack, string()} | nack | {nack, integer() | {integer() | string()}} | nop, #st{}}.
handle_message(Session, login, _Ucp, St) ->
    ucp_server_util:log_login(St#st.srv_ref, Session),
    case dict:fetch(login_response, St#st.vars) of
        ack ->
            NewSessions = [Session | St#st.sessions],
            NewSessionReqs = dict:store(Session, dict:new(), St#st.session_reqs),
            {reply, ack, St#st{sessions = NewSessions, session_reqs = NewSessionReqs}};
        Nack ->
            {reply, Nack, St}
    end;
handle_message(Session, receive_mt, Ucp, St) ->
    MsgId = make_msg_id(),
    Source = util_proplists:fetch(oadc, Ucp),
    Destination = util_proplists:fetch(adc, Ucp),
    Body = util_proplists:fetch(msg, Ucp),
    Timestamp = calendar:universal_time(),
    ucp_server_util:log_receive_mt(St#st.srv_ref, Session, MsgId, Ucp),
    case dict:fetch(receive_mt_response, St#st.vars) of
        ack ->
            Mt = #simple_mt{source = Source, destination = Destination, msg_id = MsgId, body = Body, timestamp = Timestamp},
            NewMts = case dict:fetch(auto_drs, St#st.vars) of
                true ->
                    DrPid = spawn_link(fun () -> queue_auto_dr(Mt, St#st.vars) end),
                    [Mt#simple_mt{dr_pid = DrPid} | St#st.mts];
                false ->
                    [Mt | St#st.mts]
            end,
            {reply, {ack, MsgId}, St#st{mts = NewMts}};
        Nack ->
            {reply, Nack, St}
    end;
handle_message(Session, Msg, Ucp, St) ->
    ucp_server_util:log_unknown_message(St#st.srv_ref, Session, Msg, Ucp),
    {reply, nack, St}.

-spec handle_stop(pid(), term(), #st{}) -> {noreply, #st{}}.
handle_stop(Session, Reason, St) ->
    ucp_server_util:log_stop(St#st.srv_ref, Session, Reason),
    NewSt = shut_down_session(Session, Reason, St),
    {noreply, NewSt}.


% ----------------------------------------------------------------------------
% Private functions
% ----------------------------------------------------------------------------

-spec shut_down_session(pid(), term(), #st{}) -> #st{}.
shut_down_session(Session, Reason, St) ->
    NewSessions = lists:delete(Session, St#st.sessions),
    Reqs = dict:fetch(Session, St#st.session_reqs),
    lists:foreach(fun ({Ref, {send_dr, MsgId}}) ->
                          ucp_server_util:log_send_dr_failure(St#st.srv_ref, Session, MsgId, Ref, Reason);
                      ({Ref, send_mo}) ->
                          ucp_server_util:log_send_mo_failure(St#st.srv_ref, Session, Ref, Reason)
                  end, dict:to_list(Reqs)),
    NewSessionReqs = dict:erase(Session, St#st.session_reqs),
    St#st{sessions = NewSessions, session_reqs = NewSessionReqs}.

-spec listen(#st{}) -> ok.
listen(St) ->
    ListenParams = make_listen_params(),
    ok = gen_ucp_server:listen(St#st.srv_ref, St#st.port, ListenParams).

-spec random_session(#st{}) -> {ok, pid()} | undefined.
random_session(#st{sessions = []}) ->
    undefined;
random_session(St) ->
    RandomN = random:uniform(length(St#st.sessions)),
    {ok, lists:nth(RandomN, St#st.sessions)}.

-spec switch_auto_drs_fun(boolean(), dict()) -> fun().
switch_auto_drs_fun(true, Vars) ->
    fun (#simple_mt{dr_pid = undefined} = Mt) ->
            DrPid = spawn_link(fun () -> queue_auto_dr(Mt, Vars) end),
            Mt#simple_mt{dr_pid = DrPid};
        (Mt) ->
            Mt
    end;
switch_auto_drs_fun(false, _Vars) ->
    fun (#simple_mt{dr_pid = undefined} = Mt) ->
            Mt;
        (Mt) ->
            exit(Mt#simple_mt.dr_pid, kill),
            Mt#simple_mt{dr_pid = undefined}
    end.

-spec queue_auto_dr(#simple_mt{}, dict()) -> ok | {error, term()}.
queue_auto_dr(Mt, Vars) ->
    DrDelay = dict:fetch(auto_dr_delay, Vars),
    timer:sleep(DrDelay div 2),
    Timestamp = calendar:universal_time(),
    timer:sleep(DrDelay div 2),
    DeliveryStatus = dict:fetch(auto_dr_delivery_status, Vars),
    ErrorCode = dict:fetch(auto_dr_error_code, Vars),
    send_dr(Mt#simple_mt.msg_id, Timestamp, DeliveryStatus, ErrorCode).

-spec make_msg_id() -> string().
make_msg_id() ->
    [crypto:rand_uniform($0, $9) || _ <- lists:seq(1, ?MSG_ID_LENGTH)].

-spec make_dr_msg(string(), string(), datetime(), integer()) -> string().
% TODO: This currently emulates Three
make_dr_msg(Destination, MsgId, RawTimestamp, DeliveryStatus) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = RawTimestamp,
    Description = case DeliveryStatus of
        0 ->
            lists:flatten(io_lib:format(" has been delivered on ~4.10.0B-~2.10.0B-~2.10.0B at ~2.10.0B:~2.10.0B:~2.10.0B.", [Year, Month, Day, Hour, Minute, Second]));
        1 ->
            " has been buffered";
        2 ->
            " could not be delivered, because we are useless"
    end,
    lists:concat([MsgId, ":", " Message for ", Destination, ", ", Description]).

-spec make_listen_params() -> proplist().
make_listen_params() ->
    [].

-spec make_send_dr_params(string(), string(), datetime(), integer(), integer()) -> proplist().
make_send_dr_params(Destination, MsgId, Timestamp, DeliveryStatus, ErrorCode) ->
    [{msg, make_dr_msg(Destination, MsgId, Timestamp, DeliveryStatus)},
     {dscts, Timestamp},
     {dst, DeliveryStatus},
     {rsn, ErrorCode}].

-spec make_send_mo_params() -> proplist().
make_send_mo_params() ->
    [{xser, [{?UCP_XSER_SERVICE_GSM_DCS, [?GSM_DCS_DEFAULT]}]}].

-spec make_send_concat_mo_params(integer(), integer(), integer()) -> proplist().
make_send_concat_mo_params(PartReference, TotalPart, PartNumber) ->
    [{xser, [{?UCP_XSER_SERVICE_GSM_UDH, [5, 0, 3, PartReference, TotalPart, PartNumber]}, {?UCP_XSER_SERVICE_GSM_DCS, [?GSM_DCS_DEFAULT]}]}].
