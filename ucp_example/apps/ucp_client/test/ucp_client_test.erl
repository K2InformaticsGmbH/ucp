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

-module(ucp_client_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("util/include/util.hrl").

-define(OPERATOR_MOD, test_operator_mod).
-define(HOST, "test-host").
-define(PORT, 7357).
-define(ADC, "test-adc").
-define(PASSWORD, "test-password").
-define(WINDOW_SIZE, 7).
-define(REQ_REF, "test-req-ref").
-define(TRANSACTION_ID, "test-transaction-id").
-define(DESCRIPTION, "test-description").
-define(KEY, "test-key").
-define(CONNECTION, "test-connection").
-define(REASON, "test-reason").
-define(DESTINATION, "440000000000").
-define(SOURCE, "test-source").
-define(INTERNATIONAL_SOURCE, "440000000000").
-define(BILLING_CODE, "test-billing-code").
-define(ERROR_CODE, 7357).
-define(TEXT, "test-text").
-define(GSM_TEXT, <<244,242,156,222,162,151,241,116>>).
-define(HEADER, <<"test-header">>).
-define(DATA, <<"test-data">>).


connect_ok_test() ->
    meck:new(ucp_client_util, [passthrough]),
    meck:new(gen_ucp_client),
    meck:new(?OPERATOR_MOD),
    meck:expect(ucp_client_util, log_connect_attempt, 3, ok),
    meck:expect(ucp_client_util, log_connect_success, 1, ok),
    meck:expect(ucp_client_util, log_login_attempt, 3, ok),
    meck:expect(?OPERATOR_MOD, get_default_params, 1, []),
    meck:expect(?OPERATOR_MOD, get_default_params, 2, []),
    meck:expect(gen_ucp_client, connect, 4, ok),
    meck:expect(gen_ucp_client, login, 5, ok),
    SrvRef = list_to_atom(?CONNECTION),
    ?assertMatch({ok, _Pid}, ucp_client:init({?CONNECTION, ?OPERATOR_MOD, ?HOST, ?PORT, ?ADC, ?PASSWORD, ?WINDOW_SIZE})),
    timer:sleep(100),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_connect_attempt, [SrvRef, ?HOST, ?PORT])),
    ?assertEqual(1, meck:num_calls(test_operator_mod, get_default_params, [connect])),
    % TODO: Test connect params
    ?assertEqual(1, meck:num_calls(gen_ucp_client, connect, [SrvRef, ?HOST, ?PORT, '_'])),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_connect_success, [SrvRef])),
    ?assertEqual(1, meck:num_calls(test_operator_mod, get_default_params, [login])),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_login_attempt, [SrvRef, ?ADC, ?PASSWORD])),
    % TODO: Test login params
    ?assertEqual(1, meck:num_calls(gen_ucp_client, login, [SrvRef, ?ADC, ?PASSWORD, '_', []])),
    ?assert(meck:validate([ucp_client_util, ?OPERATOR_MOD, gen_ucp_client])),
    meck:unload().

connect_error_test() ->
    meck:new([ucp_client_connector, ucp_client_util], [passthrough]),
    meck:new([gen_ucp_client, ?OPERATOR_MOD]),
    meck:expect(?OPERATOR_MOD, get_default_params, 1, []),
    meck:expect(?OPERATOR_MOD, get_default_params, 2, []),
    meck:expect(ucp_client_connector, sleep_and_connect, 7, ok),
    meck:expect(ucp_client_util, log_connect_attempt, 3, ok),
    meck:expect(ucp_client_util, log_connect_failure, 2, ok),
    meck:expect(gen_ucp_client, connect, 4, {error, ?REASON}),
    SrvRef = list_to_atom(?CONNECTION),
    ?assertMatch({ok, _Pid}, ucp_client:init({?CONNECTION, ?OPERATOR_MOD, ?HOST, ?PORT, ?ADC, ?PASSWORD, ?WINDOW_SIZE})),
    timer:sleep(100),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_connect_attempt, [SrvRef, ?HOST, ?PORT])),
    ?assertEqual(1, meck:num_calls(?OPERATOR_MOD, get_default_params, [connect])),
    % TODO: Test connect params
    ?assertEqual(1, meck:num_calls(gen_ucp_client, connect, [SrvRef, ?HOST, ?PORT, '_'])),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_connect_failure, [SrvRef, ?REASON])),
    ?assertEqual(1, meck:num_calls(ucp_client_connector, sleep_and_connect, [SrvRef, ?OPERATOR_MOD, ?HOST, ?PORT, ?ADC, ?PASSWORD, ?WINDOW_SIZE])),
    ?assert(meck:validate([ucp_client_connector, ucp_client_util, ?OPERATOR_MOD, gen_ucp_client])),
    meck:unload().

send_mt_request(ContentPart, Msg, Originator, OriginatorFormat, UcpParams) ->
    meck:new([ucp_client_util], [passthrough]),
    meck:new([gen_ucp_client]),
    meck:new(?OPERATOR_MOD, [passthrough]),
    meck:expect(ucp_client_util, log_sending_mt, 1, ok),
    meck:expect(gen_ucp_client, send_mt, 6, ok),
    meck:expect(ucp_client_util, log_mt_request, 2, ok),
    meck:expect(?OPERATOR_MOD, get_default_params, 1, []),

    SendMtParam = [{nrq, 1},
                   {nt, 7},
                   {mcls, 1}],
    AlphaOTOA = {otoa, 5039},
    InternationalOTOA = {otoa, 1139},

    meck:expect(?OPERATOR_MOD, get_default_params, fun(send_mt, alphanumeric) ->
                                                            [AlphaOTOA | SendMtParam];
                                                      (send_mt, international_msisdn) ->
                                                            [InternationalOTOA | SendMtParam];
                                                      (_, _) ->
                                                            []
                                                   end),
    Mt = #mt{key = ?KEY,
             connection = ?CONNECTION,
             msisdn = ?DESTINATION,
             originator = Originator,
             originator_format = OriginatorFormat,
             part_data = ContentPart,
             billing_code = ?BILLING_CODE,
             expires_at = 0},
    Args = [Mt, self()],
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:send_mt_request_st(?CONNECTION, ?OPERATOR_MOD),
    ?assertMatch({reply, ok, _}, ucp_client:handle_call({send_mt, Mt}, {self(), test_tag}, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_sending_mt, [Mt])),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_mt_request, [Mt, UcpParams])),
    % TODO: Test send_mt params
    ?assertEqual(1, meck:num_calls(gen_ucp_client, send_mt, [SrvRef, ?DESTINATION, Originator, Msg, UcpParams, Args])),
    ?assert(meck:validate([ucp_client_util, gen_ucp_client])),
    meck:unload().

send_mt_request_text_part_test() ->
    Originator = ?SOURCE,
    OriginatorFormat = alphanumeric,
    OTOA = 5039,
    OpParams = [{otoa, OTOA},
                {nrq, 1},
                {nt, 7},
                {mcls, 1}],
    UcpParams = [{vp, {{0,1,1},{0,0,0}}},
                 {xser, [{16#02, [0]},
                         {16#0C, ?BILLING_CODE}]} | OpParams],
    send_mt_request({text_part, ?GSM_TEXT, ?TEXT}, ?GSM_TEXT, Originator, OriginatorFormat, UcpParams).

send_mt_request_header_text_part_test() ->
    Originator = ?SOURCE,
    OriginatorFormat = alphanumeric,
    OTOA = 5039,
    OpParams = [{otoa, OTOA},
                {nrq, 1},
                {nt, 7},
                {mcls, 1}],
    Header = <<(byte_size(?HEADER)):8, ?HEADER/binary>>,
    UcpParams = [{vp, {{0,1,1},{0,0,0}}},
                 {xser, [{16#02, [0]},
                         {16#0C, ?BILLING_CODE},
                         {1, Header}]} | OpParams],
    send_mt_request({header_text_part, Header, ?GSM_TEXT, ?TEXT}, ?GSM_TEXT, Originator, OriginatorFormat, UcpParams).

send_mt_request_text_international_shortcode_test() ->
    Originator = ?INTERNATIONAL_SOURCE,
    OriginatorFormat = international_msisdn,
    OTOA = 1139,
    OpParams = [{otoa, OTOA},
                {nrq, 1},
                {nt, 7},
                {mcls, 1}],
    UcpParams = [{vp, {{0,1,1},{0,0,0}}},
                 {xser, [{16#02, [0]},
                         {16#0C, ?BILLING_CODE}]} | OpParams],
    send_mt_request({text_part, ?GSM_TEXT, ?TEXT}, ?GSM_TEXT, Originator, OriginatorFormat, UcpParams).

send_mt_request_header_text_part_international_shortcode_test() ->
    Originator = ?INTERNATIONAL_SOURCE,
    OriginatorFormat = international_msisdn,
    OTOA = 1139,
    OpParams = [{otoa, OTOA},
                {nrq, 1},
                {nt, 7},
                {mcls, 1}],
    Header = <<(byte_size(?HEADER)):8, ?HEADER/binary>>,
    UcpParams = [{vp, {{0,1,1},{0,0,0}}},
                 {xser, [{16#02, [0]},
                         {16#0C, ?BILLING_CODE},
                         {1, Header}]} | OpParams],
    send_mt_request({header_text_part, Header, ?GSM_TEXT, ?TEXT}, ?GSM_TEXT, Originator, OriginatorFormat, UcpParams).

login_response_ack_test() ->
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_login_success, 2, ok),
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:login_response_st(?CONNECTION, ?REQ_REF),
    Ucp = [{ack, "A"}],
    ?assertMatch({noreply, _}, ucp_client:handle_response(ok, Ucp, ?REQ_REF, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_login_success, [SrvRef, ?REQ_REF])),
    ?assert(meck:validate(ucp_client_util)),
    meck:unload().

log_response_nack_test() ->
    meck:new(ucp_client_util, [passthrough]),
    meck:new(gen_ucp_client),
    meck:expect(ucp_client_util, log_login_failure, 3, ok),
    meck:expect(gen_ucp_client, close, 1, ok),
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:login_response_st(?CONNECTION, ?REQ_REF),
    Ucp = [{nack, "N"},
           {ec, ?ERROR_CODE},
           {sm, ?DESCRIPTION}],
    ?assertMatch({noreply, _}, ucp_client:handle_response(ok, Ucp, ?REQ_REF, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_login_failure, [SrvRef, ?REQ_REF, {request_nacked, ?ERROR_CODE, ?DESCRIPTION}])),
    ?assertEqual(1, meck:num_calls(gen_ucp_client, close, [SrvRef])),
    ?assert(meck:validate([ucp_client_util, gen_ucp_client])),
    meck:unload().

log_response_nack_undef_ec_test() ->
    meck:new(ucp_client_util, [passthrough]),
    meck:new(gen_ucp_client),
    meck:expect(ucp_client_util, log_login_failure, 3, ok),
    meck:expect(gen_ucp_client, close, 1, ok),
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:login_response_st(?CONNECTION, ?REQ_REF),
    Ucp = [{nack, "N"},
           {sm, ?DESCRIPTION}],
    ?assertMatch({noreply, _}, ucp_client:handle_response(ok, Ucp, ?REQ_REF, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_login_failure, [SrvRef, ?REQ_REF, {request_nacked, 0, ?DESCRIPTION}])),
    ?assertEqual(1, meck:num_calls(gen_ucp_client, close, [SrvRef])),
    ?assert(meck:validate([ucp_client_util, gen_ucp_client])),
    meck:unload().

log_response_error_test() ->
    meck:new(ucp_client_util, [passthrough]),
    meck:new(gen_ucp_client),
    meck:expect(ucp_client_util, log_login_failure, 3, ok),
    meck:expect(gen_ucp_client, close, 1, ok),
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:login_response_st(?CONNECTION, ?REQ_REF),
    Ucp = [],
    ?assertMatch({noreply, _}, ucp_client:handle_response({error, ?REASON}, Ucp, ?REQ_REF, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_login_failure, [SrvRef, ?REQ_REF, ?REASON])),
    ?assertEqual(1, meck:num_calls(gen_ucp_client, close, [SrvRef])),
    ?assert(meck:validate([ucp_client_util, gen_ucp_client])),
    meck:unload().

send_mt_response_ack_test() ->
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_send_mt_success, 4, ok),
    Mt = #mt{key = ?KEY,
             msisdn = ?DESTINATION,
             originator = ?SOURCE},
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:send_mt_response_st(?CONNECTION, ?OPERATOR_MOD, ?REQ_REF, Mt),
    Ucp = [{ack, "A"},
           {sm, ?TRANSACTION_ID}],
    ?assertMatch({noreply, _}, ucp_client:handle_response(ok, Ucp, ?REQ_REF, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_send_mt_success, [SrvRef, ?KEY, ?REQ_REF, ?TRANSACTION_ID])),
    ?assert(meck:validate([ucp_client_util])),
    meck:unload().

send_mt_response_nack_test() ->
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_send_mt_failure, 4, ok),
    Mt = #mt{key = ?KEY,
             msisdn = ?DESTINATION,
             originator = ?SOURCE},
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:send_mt_response_st(?CONNECTION, ?OPERATOR_MOD, ?REQ_REF, Mt),
    Ucp = [{nack, "N"},
           {ec, ?ERROR_CODE},
           {sm, ?DESCRIPTION}],
    ?assertMatch({noreply, _}, ucp_client:handle_response(ok, Ucp, ?REQ_REF, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_send_mt_failure, [SrvRef, ?KEY, ?REQ_REF, {request_nacked, ?ERROR_CODE, ?DESCRIPTION}])),
    ?assert(meck:validate([ucp_client_util])),
    meck:unload().

send_mt_response_nack_undef_ec_test() ->
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_send_mt_failure, 4, ok),
    Mt = #mt{key = ?KEY,
             msisdn = ?DESTINATION,
             originator = ?SOURCE},
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:send_mt_response_st(?CONNECTION, ?OPERATOR_MOD, ?REQ_REF, Mt),
    Ucp = [{nack, "N"},
           {sm, ?DESCRIPTION}],
    ?assertMatch({noreply, _}, ucp_client:handle_response(ok, Ucp, ?REQ_REF, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_send_mt_failure, [SrvRef, ?KEY, ?REQ_REF, {request_nacked, 0, ?DESCRIPTION}])),
    ?assert(meck:validate([ucp_client_util])),
    meck:unload().

send_mt_response_error_test() ->
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_send_mt_failure, 4, ok),
    Mt = #mt{key = ?KEY},
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:send_mt_response_st(?CONNECTION, ?OPERATOR_MOD, ?REQ_REF, Mt),
    Ucp = [],
    ?assertMatch({noreply, _}, ucp_client:handle_response({error, ?REASON}, Ucp, ?REQ_REF, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_send_mt_failure, [SrvRef, ?KEY, ?REQ_REF, ?REASON])),
    ?assert(meck:validate([ucp_client_util])),
    meck:unload().

receive_dr_or_mo_message(Msg, LogReceiveFunc, LogRequestFunc, ProcessFunc) ->
    ?DUPLICATE_DELIVER_SM_KEY = ets:new(?DUPLICATE_DELIVER_SM_KEY, [set, public, named_table]),
    meck:new(ucp_client_util, [passthrough]),
    meck:new(?OPERATOR_MOD),
    meck:expect(ucp_client_util, LogReceiveFunc, 1, ok),
    meck:expect(ucp_client_util, LogRequestFunc, 2, ok),
    meck:expect(?OPERATOR_MOD, ProcessFunc, 2, ok),
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:receive_dr_or_mo_message_st(?CONNECTION, ?OPERATOR_MOD),
    Ucp = [{ot,52}, {oadc, "07530640497"}, {adc, "80988"}, {trn, 30}],
    ?assertMatch({reply, ack, _}, ucp_client:handle_message(Msg, Ucp, St)),
    timer:sleep(1000),
    ?assertEqual(1, meck:num_calls(ucp_client_util, LogReceiveFunc, [SrvRef])),
    ?assertEqual(1, meck:num_calls(ucp_client_util, LogRequestFunc, [Ucp, ?CONNECTION])),
    ?assertEqual(1, meck:num_calls(?OPERATOR_MOD, ProcessFunc, [Ucp, ?CONNECTION])),
    ?assert(meck:validate([ucp_client_util, ?OPERATOR_MOD])),
    meck:unload(),
    ets:delete(?DUPLICATE_DELIVER_SM_KEY).

receive_dr_message_test() ->
    receive_dr_or_mo_message(receive_dr, log_receive_dr, log_dr_request, handle_raw_dr).

receive_mo_message_test() ->
    receive_dr_or_mo_message(receive_mo, log_receive_mo, log_mo_request, handle_raw_mo).

stop_test() ->
    meck:new([ucp_client_connector, ucp_client_util], [passthrough]),
    meck:expect(ucp_client_connector, sleep_and_connect, 7, ok),
    meck:expect(ucp_client_util, log_stop, 2, ok),
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:stop_st(?CONNECTION, ?OPERATOR_MOD, ?HOST, ?PORT, ?ADC, ?PASSWORD, ?WINDOW_SIZE),
    ?assertMatch({noreply, _}, ucp_client:handle_stop(?REASON, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_stop, [SrvRef, ?REASON])),
    timer:sleep(100),
    ?assertEqual(1, meck:num_calls(ucp_client_connector, sleep_and_connect, [SrvRef, ?OPERATOR_MOD, ?HOST, ?PORT, ?ADC, ?PASSWORD, ?WINDOW_SIZE])),
    ?assert(meck:validate([ucp_client_connector, ucp_client_util])),
    meck:unload().

stop_2_test() ->
    meck:new([ucp_client_connector, ucp_client_util], [passthrough]),
    meck:expect(ucp_client_connector, sleep_and_connect, 7, ok),
    meck:expect(ucp_client_util, log_stop, 2, ok),
    SrvRef = list_to_atom(?CONNECTION),
    Pid = spawn(fun() -> receive _ -> ok end end),
    PrevProcesses = length(erlang:processes()),
    St = ucp_client:stop_st_connector(?CONNECTION, ?OPERATOR_MOD, ?HOST, ?PORT, ?ADC, ?PASSWORD, ?WINDOW_SIZE, Pid),
    ?assertMatch({noreply, _}, ucp_client:handle_stop(?REASON, St)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_stop, [SrvRef, ?REASON])),
    timer:sleep(100),
    ?assertEqual(PrevProcesses -1, length(erlang:processes())),
    ?assertEqual(1, meck:num_calls(ucp_client_connector, sleep_and_connect, [SrvRef, ?OPERATOR_MOD, ?HOST, ?PORT, ?ADC, ?PASSWORD, ?WINDOW_SIZE])),
    ?assert(meck:validate([ucp_client_connector, ucp_client_util])),
    meck:unload().

smpp_client_generator_test_() ->
    {foreach,
        fun per_test_setup/0,
        fun per_test_cleanup/1,
        [
         {timeout, 10, ?_test(duplicate_mo_is_not_ignored_after_5_seconds())}
        ]
    }.

duplicate_mo_is_ignored_test() ->
    ?DUPLICATE_DELIVER_SM_KEY = ets:new(?DUPLICATE_DELIVER_SM_KEY, [set, public, named_table]),
    meck:new(ucp_client_util, [passthrough]),
    meck:new(?OPERATOR_MOD),
    meck:expect(ucp_client_util, log_receive_mo, 1, ok),
    meck:expect(ucp_client_util, log_mo_request, 2, ok),
    meck:expect(?OPERATOR_MOD, handle_raw_mo, 2, ok),
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:receive_dr_or_mo_message_st(?CONNECTION, ?OPERATOR_MOD),
    Ucp = [{ot,52}, {oadc, "07530640497"}, {adc, "80988"}, {trn, 30}],
    ?assertMatch({reply, ack, _}, ucp_client:handle_message(receive_mo, Ucp, St)),
    ?assertMatch({reply, ack, _}, ucp_client:handle_message(receive_mo, Ucp, St)),

    %% The second mo should be ignored
    ?assertEqual(1, meck:num_calls(?OPERATOR_MOD, handle_raw_mo, [Ucp, ?CONNECTION])),
    ?assert(meck:validate([ucp_client_util, ?OPERATOR_MOD])),
    meck:unload(),
    ets:delete(?DUPLICATE_DELIVER_SM_KEY).

duplicate_mo_is_not_ignored_after_5_seconds() ->
    ?DUPLICATE_DELIVER_SM_KEY = ets:new(?DUPLICATE_DELIVER_SM_KEY, [set, public, named_table]),
    meck:new(ucp_client_util, [passthrough]),
    meck:new(?OPERATOR_MOD),
    meck:expect(ucp_client_util, log_receive_mo, 1, ok),
    meck:expect(ucp_client_util, log_mo_request, 2, ok),
    meck:expect(?OPERATOR_MOD, handle_raw_mo, 2, ok),
    SrvRef = list_to_atom(?CONNECTION),
    St = ucp_client:receive_dr_or_mo_message_st(?CONNECTION, ?OPERATOR_MOD),
    Ucp = [{ot,52}, {oadc, "07530640497"}, {adc, "80988"}, {trn, 30}],
    Ucp2 = [{ot,52}, {oadc, "07530640497"}, {adc, "80988"}, {trn, 30}],

    ?assertMatch({reply, ack, _}, ucp_client:handle_message(receive_mo, Ucp, St)),
    timer:sleep(500),

    ?assertEqual(1, meck:num_calls(?OPERATOR_MOD, handle_raw_mo, [Ucp, ?CONNECTION])),

    timer:sleep(6000),
    ?assertMatch({reply, ack, _}, ucp_client:handle_message(receive_mo, Ucp2, St)),
    timer:sleep(500),

    ?assertEqual(2, meck:num_calls(?OPERATOR_MOD, handle_raw_mo, [Ucp2, ?CONNECTION])),

    ?assert(meck:validate([ucp_client_util, ?OPERATOR_MOD])),
    meck:unload(),
    ets:delete(?DUPLICATE_DELIVER_SM_KEY).

per_test_setup() ->
    ok.

per_test_cleanup(_) ->
    ok.
