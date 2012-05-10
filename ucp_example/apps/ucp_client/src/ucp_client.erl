-module(ucp_client).

% Public functions
-export([start_link/7,
         stop/1,
         send_mt/1,
         is_logged_in/1]).

% gen_ucp_client callbacks
-behaviour(gen_ucp_client).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2,
         handle_request/5,
         handle_response/4,
         handle_message/3,
         handle_stop/2]).

-include_lib("ucperl/include/ucp_defines.hrl").
-include_lib("util/include/util.hrl").
-include("ucp_client.hrl").

-define(GSM_DCS_DEFAULT, 0).
-define(GSM_DCS_8BIT, 4).

-define(UCP_EC_NO_ERROR, 0).

-define(DEFAULT_OPERATOR_REQUEST_TIMEOUT, 120000).

-record(st, {connection :: connection(), % TODO
             srv_ref :: atom(),
             operator_mod :: atom(),
             host :: string(),
             port :: integer(),
             adc :: string(),
             password :: string(),
             window_size :: integer(),
             login_req :: reference(),
             reqs :: dict(),
             connector :: pid(),         % Subprocess retrying connect/1
             requestors :: [pid()]}).    % Request processes calling send_mt/2

-ifdef(TEST).
-export([send_mt_request_st/2,
         login_response_st/2,
         send_mt_response_st/4,
         receive_dr_or_mo_message_st/2,
         stop_st/7,
         stop_st_connector/8]).

send_mt_request_st(Connection, OperatorMod) ->
    SrvRef = list_to_atom(Connection),
    #st{srv_ref = SrvRef, operator_mod = OperatorMod, reqs = not_undefined}.

login_response_st(Connection, ReqRef) ->
    SrvRef = list_to_atom(Connection),
    #st{connection = Connection, srv_ref = SrvRef, login_req = ReqRef}.

send_mt_response_st(Connection, OperatorMod, ReqRef, Mt) ->
    SrvRef = list_to_atom(Connection),
    Reqs = dict:store(ReqRef, Mt, dict:new()),
    #st{connection = Connection, srv_ref = SrvRef, operator_mod = OperatorMod, reqs = Reqs, requestors = []}.

receive_dr_or_mo_message_st(Connection, OperatorMod) ->
    SrvRef = list_to_atom(Connection),
    #st{connection = Connection, srv_ref = SrvRef, operator_mod = OperatorMod}.

stop_st(Connection, OperatorMod, Host, Port, Adc, Password, WindowSize) ->
    SrvRef = list_to_atom(Connection),
    #st{connection = Connection, srv_ref = SrvRef, operator_mod = OperatorMod, host = Host, port = Port, adc = Adc, password = Password, window_size = WindowSize}.

stop_st_connector(Connection, OperatorMod, Host, Port, Adc, Password, WindowSize, Connector) ->
    SrvRef = list_to_atom(Connection),
    #st{connection = Connection, srv_ref = SrvRef, operator_mod = OperatorMod, host = Host, port = Port, adc = Adc, password = Password, window_size = WindowSize, connector = Connector}.
-endif.


% ----------------------------------------------------------------------------
% Public functions
% ----------------------------------------------------------------------------

-spec start_link(connection(), atom(), string(), integer(), string(), string(), integer()) -> {ok, pid()} | {error, term()}.
start_link(Connection, OperatorMod, Host, Port, Adc, Password, WindowSize) ->
    SrvRef = list_to_atom(Connection),
    ucp_client_util:log_start(SrvRef),
    gen_ucp_client:start_link({local, SrvRef}, ?MODULE, {Connection, OperatorMod, Host, Port, Adc, Password, WindowSize}).

-spec stop(connection()) -> ok.
stop(Connection) ->
    SrvRef = list_to_atom(Connection),
    case whereis(SrvRef) of
        undefined ->
            ok;
        Pid ->
            exit(Pid, shutdown),
            ok
    end.

-spec send_mt(#mt{}) -> ok | {error, term()}.
send_mt(Mt) ->
    SrvRef = list_to_atom(Mt#mt.connection),
    case gen_server:call(SrvRef, {send_mt, Mt}) of
        ok ->
            receive
                release ->
                    ok
            after ?DEFAULT_OPERATOR_REQUEST_TIMEOUT ->
                ok
            end;
        Error ->
            Error
    end.

-spec is_logged_in(connection()) -> boolean().
is_logged_in(Connection) ->
    SrvRef = list_to_atom(Connection),
    gen_server:call(SrvRef, is_logged_in).


% ----------------------------------------------------------------------------
% gen_ucp_client callbacks
% ----------------------------------------------------------------------------

-spec init({connection(), atom(), string(), integer(), string(), string(), integer()}) -> {ok, #st{}}.
init({Connection, OperatorMod, Host, Port, Adc, Password, WindowSize}) ->
    SrvRef = list_to_atom(Connection),
    Connector = spawn_link(ucp_client_connector, connect, [SrvRef, OperatorMod, Host, Port, Adc, Password, WindowSize]),
    St = #st{connection = Connection, srv_ref = SrvRef, operator_mod = OperatorMod, host = Host, port = Port, adc = Adc, password = Password, window_size = WindowSize, connector = Connector},
    {ok, St}.

-spec handle_call(term(), term(), #st{}) -> term().
handle_call({send_mt, Mt}, _From, #st{reqs = undefined} = St) ->
    ucp_client_util:log_failed_to_send_mt(Mt, not_logged_in, St#st.operator_mod),
    {reply, {error, not_logged_in}, St};
handle_call({send_mt, #mt{part_data = {text_part, GsmText, _Body}} = Mt}, {Requestor, _Tag}, St) ->
    OriginatorFormat = Mt#mt.originator_format,
    OpParams = (St#st.operator_mod):get_default_params(send_mt, OriginatorFormat),
    BillingCode = get_billing_code(Mt, St),
    Params = [{vp, calendar:gregorian_seconds_to_datetime(Mt#mt.expires_at)},
              {xser, [{?UCP_XSER_SERVICE_GSM_DCS, [?GSM_DCS_DEFAULT]},
                      {?UCP_XSER_SERVICE_BILLING_IDENTIFIER, BillingCode}]} | OpParams],
    ucp_client_util:log_mt_request(Mt, Params),
    ucp_client_util:log_sending_mt(Mt),
    gen_ucp_client:send_mt(St#st.srv_ref, Mt#mt.msisdn, Mt#mt.originator, GsmText, Params, [Mt, Requestor]),
    {reply, ok, St};
handle_call({send_mt, #mt{part_data = {header_text_part, Header, GsmText, _Body}} = Mt}, {Requestor, _Tag}, St) ->
    OriginatorFormat = Mt#mt.originator_format,
    OpParams = (St#st.operator_mod):get_default_params(send_mt, OriginatorFormat),
    BillingCode = get_billing_code(Mt, St),
    Params = [{vp, calendar:gregorian_seconds_to_datetime(Mt#mt.expires_at)},
              {xser, [{?UCP_XSER_SERVICE_GSM_DCS, [?GSM_DCS_DEFAULT]},
                      {?UCP_XSER_SERVICE_BILLING_IDENTIFIER, BillingCode},
                      {?UCP_XSER_SERVICE_GSM_UDH, Header}]} | OpParams],
    ucp_client_util:log_mt_request(Mt, Params),
    ucp_client_util:log_sending_mt(Mt),
    gen_ucp_client:send_mt(St#st.srv_ref, Mt#mt.msisdn, Mt#mt.originator, GsmText, Params, [Mt, Requestor]),
    {reply, ok, St};
handle_call(is_logged_in, _From, #st{reqs = undefined} = St) ->
    {reply, false, St};
handle_call(is_logged_in, _From, St) ->
    {reply, true, St};
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

-spec handle_request(atom(), list(), reference(), list(), #st{}) -> {noreply, #st{}}.
handle_request(login, [_Adc, _Password, _Params], Ref, _Args, St) ->
    ucp_client_util:log_login_request(St#st.srv_ref, Ref),
    {noreply, St#st{login_req = Ref}};
handle_request(send_mt, [_Adc, _Oadc, _MtBody, _Params], Ref, [Mt, Requestor], St) ->
    ucp_client_util:log_send_mt_request(St#st.srv_ref, Mt#mt.key, Ref),
    NewReqs = dict:store(Ref, Mt, St#st.reqs),
    NewRequestors = [Requestor | St#st.requestors],
    {noreply, St#st{reqs = NewReqs, requestors = NewRequestors}}.

-spec handle_response(ok | {error, term()}, proplist(), reference(), #st{}) -> {noreply, #st{}}.
handle_response(ok, Ucp, Ref, #st{login_req = Ref} = St) ->
    case util_proplists:find(ack, Ucp) of
        {ok, _Ack} ->
            ucp_client_util:log_login_success(St#st.srv_ref, Ref),
            {noreply, St#st{login_req = undefined, reqs = dict:new(), connector = undefined, requestors = []}};
        undefined ->
            ErrorCode = util_proplists:fetch(ec, Ucp, ?UCP_EC_NO_ERROR),
            Description = util_proplists:fetch(sm, Ucp, []),
            ucp_client_util:log_login_failure(St#st.srv_ref, Ref, {request_nacked, ErrorCode, Description}),
            gen_ucp_client:close(St#st.srv_ref),
            {noreply, St#st{login_req = undefined, connector = undefined}}
    end;
handle_response({error, Reason}, _Ucp, Ref, #st{login_req = Ref} = St) ->
    ucp_client_util:log_login_failure(St#st.srv_ref, Ref, Reason),
    gen_ucp_client:close(St#st.srv_ref),
    {noreply, St#st{login_req = undefined, connector = undefined}};
handle_response(Resp, Ucp, Ref, St) ->
    case dict:find(Ref, St#st.reqs) of
        {ok, Mt} ->
            case Resp of
                ok ->
                    case util_proplists:find(ack, Ucp) of
                        {ok, _Ack} ->
                            TransactionId = util_proplists:fetch(sm, Ucp),
                            ucp_client_util:log_send_mt_success(St#st.srv_ref, Mt#mt.key, Ref, TransactionId);
                        undefined ->
                            ErrorCode = util_proplists:fetch(ec, Ucp, ?UCP_EC_NO_ERROR),
                            Description = util_proplists:fetch(sm, Ucp, []),
                            ucp_client_util:log_send_mt_failure(St#st.srv_ref, Mt#mt.key, Ref, {request_nacked, ErrorCode, Description})
                    end;
                {error, Reason} ->
                    ucp_client_util:log_send_mt_failure(St#st.srv_ref, Mt#mt.key, Ref, Reason)
            end,
            NewReqs = dict:erase(Ref, St#st.reqs),
            NewRequestors = case dict:size(NewReqs) of
                0 ->
                    [Requestor ! release || Requestor <- St#st.requestors],
                    [];
                _ ->
                    St#st.requestors
            end,
            {noreply, St#st{reqs = NewReqs, requestors = NewRequestors}};
        error ->
            ucp_client_util:log_unknown_response(St#st.srv_ref, Ref, Resp, Ucp),
            {noreply, St}
    end.

-spec handle_message(receive_dr | receive_mo, proplist(), #st{}) -> {reply, ack | {ack, string()} | nack | {nack, integer() | {integer() | string()}} | nop, #st{}}.
handle_message(receive_dr, Ucp, St) ->
    ucp_client_util:log_receive_dr(St#st.srv_ref),
    ucp_client_util:log_dr_request(Ucp, St#st.connection),
    spawn(fun() -> (St#st.operator_mod):handle_raw_dr(Ucp, St#st.connection) end),
    {reply, ack, St};
handle_message(receive_mo, Ucp, St) ->
    ucp_client_util:log_receive_mo(St#st.srv_ref),

    Connection = St#st.connection,
    {ok, Ot} = util_proplists:find(ot, Ucp),
    {ok, Oadc} = util_proplists:find(oadc, Ucp),
    {ok, Adc} = util_proplists:find(adc, Ucp),
    {ok, Trn} = util_proplists:find(trn, Ucp),

    {Duplicate, DuplicateKey} = duplicate_deliver_sm(Ot, Oadc, Adc, Connection, Trn),

    case Duplicate of
        false ->
            UTCGregorianSeconds = util:gs_now(),
            ets:insert(?DUPLICATE_DELIVER_SM_KEY, {DuplicateKey, UTCGregorianSeconds}),
            ucp_client_util:log_mo_request(Ucp, St#st.connection),
            spawn(fun() -> (St#st.operator_mod):handle_raw_mo(Ucp, St#st.connection) end);
        true ->
            util_log:log_error("Detected duplicate MO ot:~p oadc:~p adc:~p trn:~p Connection:~p~n", [Ot, Oadc, Adc, Trn, Connection])
    end,

    {reply, ack, St};
handle_message(Msg, Ucp, St) ->
    ucp_client_util:log_unknown_message(St#st.srv_ref, Msg, Ucp),
    {reply, nack, St}.

-spec handle_stop(term(), #st{}) -> {noreply, #st{}}.
handle_stop(Reason, St) ->
    ucp_client_util:log_stop(St#st.srv_ref, Reason),
    NewSt = shut_down_everything(Reason, St),
    Connector = spawn_link(ucp_client_connector, sleep_and_connect, [NewSt#st.srv_ref, NewSt#st.operator_mod, NewSt#st.host, NewSt#st.port, NewSt#st.adc, NewSt#st.password, NewSt#st.window_size]),
    {noreply, NewSt#st{connector = Connector}}.


% ----------------------------------------------------------------------------
% Private functions
% ----------------------------------------------------------------------------

-spec shut_down_everything(term(), #st{}) -> #st{}.
shut_down_everything(Reason, St) ->
    case St#st.login_req of
        undefined ->
            ok;
        _ ->
            ucp_client_util:log_login_failure(St#st.srv_ref, St#st.login_req, Reason)
    end,
    case St#st.reqs of
        undefined ->
            ok;
        _ ->
            lists:foreach(fun ({Ref, Mt}) ->
                                  ucp_client_util:log_send_mt_failure(St#st.srv_ref, Mt#mt.key, Ref, Reason)
                          end, dict:to_list(St#st.reqs))
    end,
    case St#st.connector of
        undefined ->
            ok;
        _ ->
            unlink(St#st.connector),
            exit(St#st.connector, kill)
    end,
    case St#st.requestors of
        undefined ->
            ok;
        _ ->
            [Requestor ! release || Requestor <- St#st.requestors]
    end,
    St#st{login_req = undefined, reqs = undefined, connector = undefined, requestors = undefined}.

-spec get_billing_code(#mt{}, #st{}) -> string().
get_billing_code(#mt{billing_code = []}, St) ->
    (St#st.operator_mod):get_default_billing_code();
get_billing_code(Mt, _St) ->
    Mt#mt.billing_code.

duplicate_deliver_sm(Ot, Oadc, Adc, Connection, Trn) ->
    DuplicateDetectionKey = {Ot, Oadc, Adc, Connection, Trn},

    Duplicate = case ets:lookup(?DUPLICATE_DELIVER_SM_KEY, DuplicateDetectionKey) of
                    [{DuplicateDetectionKey, Timestamp}] ->
                        UTCGregorianSeconds = util:gs_now(),
                        TimeDifference = case application:get_env(ucp_client, mo_duplicate_time_difference) of
                                             {ok, Val} ->
                                                  Val;
                                              _ ->
                                                  5
                                         end,
                        LastProcessed = UTCGregorianSeconds - Timestamp,
                        case LastProcessed < TimeDifference of
                             true -> true;
                            false -> false
                        end;
                    _ ->
                        false
                end,
    {Duplicate, DuplicateDetectionKey}.