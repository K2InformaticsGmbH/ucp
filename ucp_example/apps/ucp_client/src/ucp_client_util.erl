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

-module(ucp_client_util).

-export([log_start/1,
         log_connect_attempt/3,
         log_connect_success/1,
         log_connect_failure/2,
         log_login_attempt/3,
         log_login_request/2,
         log_login_success/2,
         log_login_failure/3,
         log_send_mt_attempt/2,
         log_send_mt_request/3,
         log_send_mt_success/4,
         log_send_mt_failure/4,
         log_unknown_response/4,
         log_receive_dr/1,
         log_handle_raw_dr_success/4,
         log_handle_raw_dr_failure/3,
         log_receive_mo/1,
         log_handle_raw_mo_success/5,
         log_handle_raw_mo_failure/3,
         log_unknown_message/3,
         log_stop/2,
         log_failed_to_send_mt/3,
         log_sending_mt/1,
         log_mt_request/2,
         log_dr_request/2,
         log_mo_request/2]).

-include_lib("util/include/util.hrl").
-include("ucp_client.hrl").

-spec log_start(atom()) -> ok.
log_start(SrvRef) ->
    ok = util_log:log_event([ucp_client_start, SrvRef]).


-spec log_connect_attempt(atom(), string(), integer()) -> ok.
log_connect_attempt(SrvRef, Host, Port) ->
    ok = util_log:log_event([ucp_client_connect_attempt, SrvRef, Host, Port]).

-spec log_connect_success(atom()) -> ok.
log_connect_success(SrvRef) ->
    ok = util_log:log_event([ucp_client_connect_success, SrvRef]).

-spec log_connect_failure(atom(), term()) -> ok.
log_connect_failure(SrvRef, Reason) ->
    ok = util_log:log_event([ucp_client_connect_failure, SrvRef, Reason]),
    ok = util_log:log_error("UCP client connect failure - srv_ref: ~p, reason:~n~p~n", [SrvRef, Reason]).


-spec log_login_attempt(atom(), string(), string()) -> ok.
log_login_attempt(SrvRef, Adc, Password) ->
    ok = util_log:log_event([ucp_client_login_attempt, SrvRef, Adc, Password]).

-spec log_login_request(atom(), reference()) -> ok.
log_login_request(SrvRef, Ref) ->
    ok = util_log:log_event([ucp_client_login_request, SrvRef, util:term_to_string(Ref)]).

-spec log_login_success(atom(), reference()) -> ok.
log_login_success(SrvRef, Ref) ->
    ok = util_log:log_event([ucp_client_login_success, SrvRef, util:term_to_string(Ref)]).

-spec log_login_failure(atom(), reference(), term()) -> ok.
log_login_failure(SrvRef, Ref, Reason) ->
    ok = util_log:log_event([ucp_client_login_failure, SrvRef, util:term_to_string(Ref), util:term_to_string(Reason)]),
    ok = util_log:log_error("UCP client login failure - srv_ref: ~p, ref: ~p, reason:~n~p~n", [SrvRef, Ref, Reason]).


-spec log_send_mt_attempt(atom(), string()) -> ok.
log_send_mt_attempt(SrvRef, Key) ->
    ok = util_log:log_event([ucp_client_send_mt_attempt, SrvRef, Key]).

-spec log_send_mt_request(atom(), string(), reference()) -> ok.
log_send_mt_request(SrvRef, Key, Ref) ->
    ok = util_log:log_event([ucp_client_send_mt_request, SrvRef, Key, util:term_to_string(Ref)]).

-spec log_send_mt_success(atom(), string(), reference(), string()) -> ok.
log_send_mt_success(SrvRef, Key, Ref, TransactionId) ->
    ok = util_log:log_event([ucp_client_send_mt_success, SrvRef, Key, util:term_to_string(Ref), TransactionId]).

-spec log_send_mt_failure(atom(), string(), reference(), term()) -> ok.
log_send_mt_failure(SrvRef, Key, Ref, Reason) ->
    ok = util_log:log_event([ucp_client_send_mt_failure, SrvRef, Key, util:term_to_string(Ref), util:term_to_string(Reason)]).


-spec log_unknown_response(atom(), reference(), term(), term()) -> ok.
log_unknown_response(SrvRef, Ref, Resp, Ucp) ->
    ok = util_log:log_event([ucp_client_unknown_response, SrvRef, util:term_to_string(Ref), util:term_to_string(Resp), util:term_to_string(Ucp)]),
    ok = util_log:log_error("UCP client unknown response - srv_ref: ~p, ref: ~p, resp:~n~p, ucp:~n~p~n", [SrvRef, Ref, Resp, Ucp]).


-spec log_receive_dr(atom()) -> ok.
log_receive_dr(SrvRef) ->
    ok = util_log:log_event([ucp_client_receive_dr, SrvRef]).

-spec log_handle_raw_dr_success(connection(), string(), atom(), string()) -> ok.
log_handle_raw_dr_success(Connection, TransactionId, Status, ErrorCode) ->
    ok = util_log:log_event([ucp_client_handle_raw_dr_success, Connection, TransactionId, Status, ErrorCode]).

-spec log_handle_raw_dr_failure(connection(), term(), term()) -> ok.
log_handle_raw_dr_failure(Connection, Ucp, Error) ->
    ok = util_log:log_event([ucp_client_handle_raw_dr_failure, Connection, util:term_to_string(Ucp), util:term_to_string(Error)]),
    ok = util_log:log_error("UCP client handle_raw_dr failure - connection: ~p, ucp:~n~p, error:~n~p~n", [Connection, Ucp, Error]).


-spec log_receive_mo(atom()) -> ok.
log_receive_mo(SrvRef) ->
    ok = util_log:log_event([ucp_client_receive_mo, SrvRef]).

-spec log_handle_raw_mo_success(connection(), string(), string(), binary(), string()) -> ok.
log_handle_raw_mo_success(Connection, Source, Destination, Body, Header) ->
    ok = util_log:log_event([ucp_client_handle_raw_mo_success, Connection, Source, Destination, Body, Header]).

-spec log_handle_raw_mo_failure(connection(), term(), term()) -> ok.
log_handle_raw_mo_failure(Connection, Ucp, Error) ->
    ok = util_log:log_event([ucp_client_handle_raw_mo_failure, Connection, util:term_to_string(Ucp), util:term_to_string(Error)]),
    ok = util_log:log_error("UCP client handle_raw_mo failure - connection: ~p, ucp:~n~p, error:~n~p~n", [Connection, Ucp, Error]).


-spec log_unknown_message(atom(), term(), term()) -> ok.
log_unknown_message(SrvRef, Msg, Ucp) ->
    ok = util_log:log_event([ucp_client_unknown_message, SrvRef, util:term_to_string(Msg), util:term_to_string(Ucp)]),
    ok = util_log:log_error("UCP client unknown message - srv_ref: ~p, msg:~n~p, ucp:~n~p~n", [SrvRef, Msg, Ucp]).


-spec log_stop(atom(), term()) -> ok.
log_stop(SrvRef, Reason) ->
    ok = util_log:log_event([ucp_client_stop, SrvRef, util:term_to_string(Reason)]),
    ok = util_log:log_error("UCP client stop - srv_ref: ~p, reason:~n~p~n", [SrvRef, Reason]).

-spec log_failed_to_send_mt(#mt{}, atom(), atom()) -> ok.
log_failed_to_send_mt(Mt, Error, Operator) ->
    ok = util_log:log_event([failed_to_send_mt, Mt#mt.key, Mt#mt.msisdn, Mt#mt.originator, Mt#mt.connection, Operator, Error]).

-spec log_sending_mt(#mt{}) -> ok.
log_sending_mt(Mt) ->
    ok = util_log:log_event([sending_mt, Mt#mt.key, Mt#mt.msisdn, Mt#mt.originator, Mt#mt.connection, Mt#mt.operator]).

-spec log_mt_request(#mt{}, term()) -> ok.
log_mt_request(Mt, Request) ->
    ok = util_log:log(?MT_REQUEST_LOG, "mt request: ~p~nKey:~p~nMsisdn: ~p Originator: ~p~nOperator:~p~nConnection:~p~nRequest:~n~p~n",[Mt#mt.part_data, Mt#mt.key, Mt#mt.msisdn, Mt#mt.originator, Mt#mt.operator, Mt#mt.connection, Request]).

-spec log_dr_request(term(), string()) -> ok.
log_dr_request(RawDr, Connection) ->
    ok = util_log:log(?DR_REQUEST_LOG, "dr request~nConnection:~p~nDR Request:~n~p~n",[Connection, RawDr]).

-spec log_mo_request(term(), string()) -> ok.
log_mo_request(RawMo, Connection) ->
    ok = util_log:log(?MO_REQUEST_LOG, "mo request~nConnection:~p~nMO Request:~n~p~n",[Connection, RawMo]).