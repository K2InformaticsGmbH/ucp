-module(default_operator_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ucperl/include/ucp_defines.hrl").
-include_lib("util/include/util.hrl").

-define(GUID, "123456789").
-define(SHORTCODE, "12345").
-define(MSISDN, "447400438622").
-define(MSG_ID, "100831140436").
-define(SWAPPED_MSG_ID, "310810140436").
-define(CONNECTION, "test-connection").
-define(ERROR_CODE, 7357).
-define(UTF8_SHORT_MESSAGE, <<195,165,195,164,195,166,195,160,195,167,206,147,195,168,195,169,206,148,206,166,195,172,206,155,195,177,195,182,195,184,195,178,195,159,206,160,206,163,195,188,195,185>>).
-define(GSM0338_SHORT_MESSAGE, [143,125,231,159,152,16,10,16,201,129,210,231,51,16,30,11,198,111,0]).
-define(GSM0338_DCS, 0).
-define(UCS2_SHORT_MESSAGE, [0,229,0,228,0,230,0,224,0,231,3,147,0,232,0,233,3,148,3,166,0,236,3,155,0,241,0,246,0,248,0,242,0,223,3,160,3,163,0,252,0,249]).
-define(UCS2_DCS, 1).
-define(KEEP_ALIVE_ADC, "test-keep-alive-adc").
-define(WINDOW_SIZE, "test-window-size").
-define(BILLING_ID, "test-billing-mt").
-define(DCS, 0).
-define(HEADER1, [5,0,3,255,2,1]).
-define(HEADER2, [5,0,3,255,2,2]).
-define(HEADER, [5,0,3,255,1,1]).
-define(HEX_HEADER, "050003FF0101").
-define(TEXT, lists:duplicate(160, $a)).
-define(GSM_TEXT, <<225,112,56,28,14,135,195,225,112,56,28,14,135,195,225,
                    112,56,28,14,135,195,225,112,56,28,14,135,195,225,112,
                    56,28,14,135,195,225,112,56,28,14,135,195,225,112,56,
                    28,14,135,195,225,112,56,28,14,135,195,225,112,56,28,
                    14,135,195,225,112,56,28,14,135,195,225,112,56,28,14,
                    135,195,225,112,56,28,14,135,195,225,112,56,28,14,135,
                    195,225,112,56,28,14,135,195,225,112,56,28,14,135,195,
                    225,112,56,28,14,135,195,225,112,56,28,14,135,195,225,
                    112,56,28,14,135,195,225,112,56,28,14,135,195,225,112,
                    56,28,14,135,195>>).
-define(MULTI_TEXT, lists:duplicate(161, $a)).
-define(MULTI_TEXT_PART1, lists:duplicate(153, $a)).
-define(MULTI_TEXT_PART2, lists:duplicate(8, $a)).
-define(MULTI_GSM_TEXT_PART1,  <<225,112,56,28,14,135,195,225,112,56,28,14,135,195,225,
                                112,56,28,14,135,195,225,112,56,28,14,135,195,225,112,
                                56,28,14,135,195,225,112,56,28,14,135,195,225,112,56,
                                28,14,135,195,225,112,56,28,14,135,195,225,112,56,28,
                                14,135,195,225,112,56,28,14,135,195,225,112,56,28,14,
                                135,195,225,112,56,28,14,135,195,225,112,56,28,14,135,
                                195,225,112,56,28,14,135,195,225,112,56,28,14,135,195,
                                225,112,56,28,14,135,195,225,112,56,28,14,135,195,225,
                                112,56,28,14,135,195,225,112,56,28,14,135,195,97>>).
-define(MULTI_GSM_TEXT_PART2, <<225,112,56,28,14,135,195>>).

send_text_mt_multipart_test() ->
    util_gsm0338:init(),
    code:unstick_mod(random),
    meck:new([ucp_client, random]),
    meck:new(util, [passthrough]),
    meck:expect(ucp_client, send_mt, 1, ok),
    meck:expect(util, generate_guid, 0, ?GUID),
    meck:expect(random, uniform, 1, 255),
    meck:expect(util, gs_now, 0, 0),
    PartData1 = {header_text_part, ?HEADER1, ?MULTI_GSM_TEXT_PART1, ?MULTI_TEXT_PART1},
    PartData2 = {header_text_part, ?HEADER2, ?MULTI_GSM_TEXT_PART2, ?MULTI_TEXT_PART2},
    Mt1 = generate_mt(?SHORTCODE, alphanumeric, ?MSISDN, ?BILLING_ID, PartData1),
    Mt2 = generate_mt(?SHORTCODE, alphanumeric, ?MSISDN, ?BILLING_ID, PartData2),
    ?assertMatch(ok, default_operator:send_text_mt(?SHORTCODE, alphanumeric, ?MSISDN, ?CONNECTION, ?MULTI_TEXT, ?BILLING_ID)),
    ?assertEqual(1, meck:num_calls(ucp_client, send_mt, [Mt1])),
    ?assertEqual(1, meck:num_calls(ucp_client, send_mt, [Mt2])),
    meck:validate(ucp_client),
    code:stick_mod(random),
    meck:unload().

generate_mt(Originator, OriginatorFormat, Msisdn, BillingCode, PartData) ->
    Mt = #mt{
      key = ?GUID,
      operator = default_operator,
      msisdn = ?MSISDN,
      originator = ?SHORTCODE,
      originator_format = alphanumeric,
      connection = ?CONNECTION,
      billing_code = ?BILLING_ID,
      part_data = PartData,
      created_at = 0,
      expires_at = 86400 * 3
     }.

handle_raw_dr_ok_success_test() ->
    Datetime = {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Timestamp = calendar:datetime_to_gregorian_seconds(Datetime),
    % "Message for 07400438622, with identification 100811140436 has been delivered on 2010-08-24 at 14:05:23."
    Description = lists:flatten(io_lib:format("~s: Message for ~s, has been delivered on ~4.10.0B-~2.10.0B-~2.10.0B at ~2.10.0B:~2.10.0B:~2.10.0B.", [?MSG_ID, ?MSISDN, Year, Month, Day, Hour, Minute, Second])),
    RawDr = [{oadc, ?MSISDN},
             {adc, ?SHORTCODE},
             {dscts, Datetime},
             {dst, 0},
             {rsn, ?ERROR_CODE},
             {msg, Description}],
    ErrorCode = integer_to_list(?ERROR_CODE),
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_handle_raw_dr_success, 4, ok),
    ?assertMatch(ok, default_operator:handle_raw_dr(RawDr, ?CONNECTION)),
    ?assertMatch(1, meck:num_calls(ucp_client_util, log_handle_raw_dr_success, [?CONNECTION, ?MSG_ID, success, ErrorCode])),
    ?assert(meck:validate(ucp_client_util)),
    meck:unload().

handle_raw_dr_ok_operator_retry_test() ->
    Datetime = {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Timestamp = calendar:datetime_to_gregorian_seconds(Datetime),
    % "Message for 07400438622, with identification 100811140436 has been delivered on 2010-08-24 at 14:05:23."
    Description = lists:flatten(io_lib:format("~s: Message for ~s, has been delivered on ~4.10.0B-~2.10.0B-~2.10.0B at ~2.10.0B:~2.10.0B:~2.10.0B.", [?MSG_ID, ?MSISDN, Year, Month, Day, Hour, Minute, Second])),
    RawDr = [{oadc, ?MSISDN},
             {adc, ?SHORTCODE},
             {dscts, Datetime},
             {dst, 1},
             {rsn, ?ERROR_CODE},
             {msg, Description}],
    ErrorCode = integer_to_list(?ERROR_CODE),
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_handle_raw_dr_success, 4, ok),
    ?assertMatch(ok, default_operator:handle_raw_dr(RawDr, ?CONNECTION)),
    ?assertMatch(1, meck:num_calls(ucp_client_util, log_handle_raw_dr_success, [?CONNECTION, ?MSG_ID, operator_retry, ErrorCode])),
    ?assert(meck:validate(ucp_client_util)),
    meck:unload().

handle_raw_dr_ok_error_test() ->
    Datetime = {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Timestamp = calendar:datetime_to_gregorian_seconds(Datetime),
    % "Message for 07400438622, with identification 100811140436 has been delivered on 2010-08-24 at 14:05:23."
    Description = lists:flatten(io_lib:format("~s: Message for ~s, has been delivered on ~4.10.0B-~2.10.0B-~2.10.0B at ~2.10.0B:~2.10.0B:~2.10.0B.", [?MSG_ID, ?MSISDN, Year, Month, Day, Hour, Minute, Second])),
    RawDr = [{oadc, ?MSISDN},
             {adc, ?SHORTCODE},
             {dscts, Datetime},
             {dst, 2},
             {rsn, ?ERROR_CODE},
             {msg, Description}],
    ErrorCode = integer_to_list(?ERROR_CODE),
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_handle_raw_dr_success, 4, ok),
    ?assertMatch(ok, default_operator:handle_raw_dr(RawDr, ?CONNECTION)),
    ?assertMatch(1, meck:num_calls(ucp_client_util, log_handle_raw_dr_success, [?CONNECTION, ?MSG_ID, error, ErrorCode])),
    ?assert(meck:validate(ucp_client_util)),
    meck:unload().

handle_raw_dr_error_test() ->
    RawDr = [],
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_handle_raw_dr_failure, 3, ok),
    ?assertMatch(ok, default_operator:handle_raw_dr(RawDr, ?CONNECTION)),
    ?assertMatch(1, meck:num_calls(ucp_client_util, log_handle_raw_dr_failure, [?CONNECTION, RawDr, '_'])),
    ?assert(meck:validate(ucp_client_util)),
    meck:unload().

handle_raw_mo_gsm0338_msg_dcs_ok_test() ->
    util_gsm0338:init(),
    meck:new(util, [passthrough]),
    meck:expect(util, generate_guid, fun() -> "123" end),
    DateTime = calendar:universal_time(),
    Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),

    RawMo = [{msg, ?GSM0338_SHORT_MESSAGE},
             {dcs, ?GSM0338_DCS},
             {oadc, ?MSISDN},
             {adc, ?SHORTCODE},
             {scts, DateTime}],

    meck:new([ucp_client_util], [passthrough]),
    meck:expect(ucp_client_util, log_handle_raw_mo_success, 5, ok),
    ?assertMatch(ok, default_operator:handle_raw_mo(RawMo, ?CONNECTION)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_handle_raw_mo_success, [?CONNECTION, ?MSISDN, ?SHORTCODE, ?UTF8_SHORT_MESSAGE, []])),
    ?assert(meck:validate([ucp_client_util])),
    meck:unload().

handle_raw_mo_gsm0338_msg_no_dcs_ok_test() ->
    util_gsm0338:init(),
    meck:new(util, [passthrough]),
    meck:expect(util, generate_guid, fun() -> "123" end),
    DateTime = calendar:universal_time(),
    Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
    RawMo = [{msg, ?GSM0338_SHORT_MESSAGE},
             {oadc, ?MSISDN},
             {adc, ?SHORTCODE},
             {scts, DateTime}],
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_handle_raw_mo_success, 5, ok),
    ?assertMatch(ok, default_operator:handle_raw_mo(RawMo, ?CONNECTION)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_handle_raw_mo_success, [?CONNECTION, ?MSISDN, ?SHORTCODE, ?UTF8_SHORT_MESSAGE, []])),
    ?assert(meck:validate([ucp_client_util])),
    meck:unload().

handle_raw_mo_ucs2_msg_ok_test() ->
    util_gsm0338:init(),
    meck:new(util, [passthrough]),
    meck:expect(util, generate_guid, fun() -> "123" end),
    DateTime = calendar:universal_time(),
    Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
    RawMo = [{msg, ?UCS2_SHORT_MESSAGE},
             {dcs, ?UCS2_DCS},
             {oadc, ?MSISDN},
             {adc, ?SHORTCODE},
             {scts, DateTime}],
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_handle_raw_mo_success, 5, ok),
    ?assertMatch(ok, default_operator:handle_raw_mo(RawMo, ?CONNECTION)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_handle_raw_mo_success, [?CONNECTION, ?MSISDN, ?SHORTCODE, ?UTF8_SHORT_MESSAGE, []])),
    ?assert(meck:validate(ucp_client_util)),
    meck:unload().

handle_raw_mo_no_msg_ok_test() ->
    meck:new(util, [passthrough]),
    meck:expect(util, generate_guid, fun() -> "123" end),
    DateTime = calendar:universal_time(),
    Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
    RawMo = [{oadc, ?MSISDN},
             {adc, ?SHORTCODE},
             {scts, DateTime}],
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_handle_raw_mo_success, 5, ok),
    ?assertMatch(ok, default_operator:handle_raw_mo(RawMo, ?CONNECTION)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_handle_raw_mo_success, [?CONNECTION, ?MSISDN, ?SHORTCODE, <<>>, []])),
    ?assert(meck:validate(ucp_client_util)),
    meck:unload().

handle_raw_mo_header_ok_test() ->
    util_gsm0338:init(),
    meck:new(util, [passthrough]),
    meck:expect(util, generate_guid, fun() -> "123" end),
    DateTime = calendar:universal_time(),
    Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
    RawMo = [{msg, ?GSM0338_SHORT_MESSAGE},
             {oadc, ?MSISDN},
             {adc, ?SHORTCODE},
             {scts, DateTime},
             {xser, [{?UCP_XSER_SERVICE_GSM_UDH, ?HEADER}]}],
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_handle_raw_mo_success, 5, ok),
    ?assertMatch(ok, default_operator:handle_raw_mo(RawMo, ?CONNECTION)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_handle_raw_mo_success, [?CONNECTION, ?MSISDN, ?SHORTCODE, ?UTF8_SHORT_MESSAGE, ?HEX_HEADER])),
    ?assert(meck:validate(ucp_client_util)),
    meck:unload().

handle_raw_mo_error_test() ->
    RawMo = [],
    meck:new(ucp_client_util, [passthrough]),
    meck:expect(ucp_client_util, log_handle_raw_mo_failure, 3, ok),
    ?assertMatch(ok, default_operator:handle_raw_mo(RawMo, ?CONNECTION)),
    ?assertEqual(1, meck:num_calls(ucp_client_util, log_handle_raw_mo_failure, [?CONNECTION, RawMo, '_'])),
    ?assert(meck:validate(ucp_client_util)),
    meck:unload().
