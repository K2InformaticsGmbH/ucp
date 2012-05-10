-module(ucp_server_util).

-export([log_start/1,
         log_sending_dr/3,
         log_dr_request/3,
         log_send_dr_request/4,
         log_send_dr_success/4,
         log_send_dr_failure/5,
         log_sending_mo/2,
         log_mo_request/3,
         log_send_mo_request/3,
         log_send_mo_success/3,
         log_send_mo_failure/4,
         log_unknown_response/5,
         log_login/2,
         log_receive_mt/4,
         log_unknown_message/4,
         log_stop/3]).

-include_lib("util/include/util.hrl").
-include("ucp_server.hrl").


-spec log_start(atom()) -> ok.
log_start(SrvRef) ->
    ok = util_log:log_event([ucp_server_start, SrvRef]).


-spec log_sending_dr(atom(), pid(), string()) -> ok.
log_sending_dr(SrvRef, Session, MsgId) ->
    ok = util_log:log_event([ucp_server_sending_dr, SrvRef, util:term_to_string(Session), MsgId]).

-spec log_dr_request(atom(), pid(), term()) -> ok.
log_dr_request(ServRef, Session, Params) ->
    ok = util_log:log(?DR_REQUEST_LOG, "dr request: ServRef: ~p, Session: ~p~nRequest:~n~p~n", [ServRef, Session, Params]).

-spec log_send_dr_request(atom(), pid(), string(), reference()) -> ok.
log_send_dr_request(SrvRef, Session, MsgId, Ref) ->
    ok = util_log:log_event([ucp_server_send_dr_request, SrvRef, util:term_to_string(Session), MsgId, util:term_to_string(Ref)]).

-spec log_send_dr_success(atom(), pid(), string(), reference()) -> ok.
log_send_dr_success(SrvRef, Session, MsgId, Ref) ->
    ok = util_log:log_event([ucp_server_send_dr_success, SrvRef, util:term_to_string(Session), MsgId, util:term_to_string(Ref)]).

-spec log_send_dr_failure(atom(), pid(), string(), reference(), term()) -> ok.
log_send_dr_failure(SrvRef, Session, MsgId, Ref, Reason) ->
    ok = util_log:log_event([ucp_server_send_dr_failure, SrvRef, util:term_to_string(Session), MsgId, util:term_to_string(Ref), util:term_to_string(Reason)]),
    ok = util_log:log_error("UCP server send_dr failure - srv_ref: ~p, session: ~p, msg_ref: ~p, ref: ~p, reason:~n~p~n", [SrvRef, Session, MsgId, Ref, Reason]).


-spec log_sending_mo(atom(), pid()) -> ok.
log_sending_mo(SrvRef, Session) ->
    ok = util_log:log_event([ucp_server_send_mo_attempt, SrvRef, util:term_to_string(Session)]).

-spec log_mo_request(atom(), pid(), proplist()) -> ok.
log_mo_request(SrvRef, Session, Params) ->
    ok = util_log:log(?MO_REQUEST_LOG, "mo request: ServRef: ~p, Session: ~p~nRequest:~n~p~n", [SrvRef, Session, Params]).    

-spec log_send_mo_request(atom(), pid(), reference()) -> ok.
log_send_mo_request(SrvRef, Session, Ref) ->
    ok = util_log:log_event([ucp_server_sending_mo, SrvRef, util:term_to_string(Session), util:term_to_string(Ref)]).

-spec log_send_mo_success(atom(), pid(), reference()) -> ok.
log_send_mo_success(SrvRef, Session, Ref) ->
    ok = util_log:log_event([ucp_server_send_mo_success, SrvRef, util:term_to_string(Session), util:term_to_string(Ref)]).

-spec log_send_mo_failure(atom(), pid(), reference(), term()) -> ok.
log_send_mo_failure(SrvRef, Session, Ref, Reason) ->
    ok = util_log:log_event([ucp_server_send_mo_failure, SrvRef, util:term_to_string(Session), util:term_to_string(Ref), util:term_to_string(Reason)]),
    ok = util_log:log_error("UCP server send_mo failure - srv_ref: ~p, session: ~p, ref: ~p, reason:~n~p~n", [SrvRef, Session, Ref, Reason]).


-spec log_unknown_response(atom(), pid(), reference(), term(), term()) -> ok.
log_unknown_response(SrvRef, Session, Ref, Resp, Ucp) ->
    ok = util_log:log_event([ucp_server_unknown_response, SrvRef, util:term_to_string(Session), util:term_to_string(Ref), util:term_to_string(Resp), util:term_to_string(Ucp)]),
    ok = util_log:log_error("UCP server unknown response - srv_ref: ~p, session: ~p, ref: ~p, resp:~n~p, ucp:~n~p~n", [SrvRef, Session, Ref, Resp, Ucp]).


-spec log_login(atom(), pid()) -> ok.
log_login(SrvRef, Session) ->
    ok = util_log:log_event([ucp_server_login, SrvRef, util:term_to_string(Session)]).

-spec log_receive_mt(atom(), pid(), string(), term()) -> ok.
log_receive_mt(SrvRef, Session, MsgId, Ucp) ->
    ok = util_log:log_event([ucp_server_receive_mt, SrvRef, util:term_to_string(Session), MsgId]),
    ok = util_log:log(?MT_REQUEST_LOG, "~s mt request~nServRef:~p, Session:~p, MsgId:~p~nMT Request:~n~p~n",[util_log:stamp(), SrvRef, Session, MsgId, Ucp]).

-spec log_unknown_message(atom(), pid(), term(), term()) -> ok.
log_unknown_message(SrvRef, Session, Msg, Ucp) ->
    ok = util_log:log_event([ucp_client_unknown_message, SrvRef, util:term_to_string(Session), util:term_to_string(Msg), util:term_to_string(Ucp)]),
    ok = util_log:log_error("UCP client unknown message - srv_ref: ~p, session: ~p, msg:~n~p, ucp:~n~p~n", [SrvRef, Session, Msg, Ucp]).


-spec log_stop(atom(), pid(), term()) -> ok.
log_stop(SrvRef, Session, Reason) ->
    ok = util_log:log_event([ucp_server_stop, SrvRef, util:term_to_string(Session), util:term_to_string(Reason)]),
    ok = util_log:log_error("UCP server stop - srv_ref: ~p, session: ~p, reason:~n~p~n", [SrvRef, Session, Reason]).
