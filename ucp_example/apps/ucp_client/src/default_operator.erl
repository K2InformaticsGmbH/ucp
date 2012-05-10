-module(default_operator).

% gen_ucp_operator callbacks
-export([get_default_params/1,
         get_default_params/2,
         get_default_billing_code/0]).

-export([
        start_connection/2,
        send_text_mt/3,
        send_text_mt/6,
        send_mt/1,
        handle_raw_dr/2,
        handle_raw_mo/2
    ]).

-include_lib("ucperl/include/ucp_defines.hrl").
-include_lib("util/include/util.hrl").

-define(DEFAULT_BILLING_CODE, "000000:0000").
-define(MAX_MSG_REF, 8191).

-define(MT_MAX_BYTE_BODY_LEN, 140).
-define(PART_MT_MAX_BYTE_BODY_LEN, 134).

-define(KEEP_ALIVE_PERIOD, 150).
-define(RESULT_TIMEOUT, 50).

-define(UCP_NRQ_ON, 1).
-define(UCP_NT_ALL, 7).
-define(UCP_MCLS_1, 1).

-spec start_connection(string(), proplist()) -> ok.
start_connection(ConnName, Config) ->
    {ok, Connections} = application:get_env(ucp_client, connections),
    NewConnections = lists:umerge(Connections, [{ConnName, Config}]),
    application:set_env(ucp_client, connections, NewConnections),
    ucp_client_app:start_connection(ConnName, Config).

-spec send_text_mt(string(), string(), string()) -> ok.
send_text_mt(Originator, MSISDN, Body) ->
    {ok, Connections} = application:get_env(ucp_client, connections),
    RandomN = random:uniform(length(Connections)),
    {ConnName, _} = lists:nth(RandomN, Connections),
    BillingCode = ?DEFAULT_BILLING_CODE,
    send_text_mt(Originator, alphanumeric, MSISDN, ConnName, Body, BillingCode).

-spec send_text_mt(string(), alphanumeric | international_msisdn, string(), string(), string(), string()) -> ok.
send_text_mt(Originator, OriginatorFormat, MSISDN, Connection, Body, BillingCode) ->
    case util_gsm0338:encode_parts(Body, $?, ?MT_MAX_BYTE_BODY_LEN) of
        [{GsmText, _CharNum}] ->
            send_mt(Originator, OriginatorFormat, MSISDN, Connection, {text_part, GsmText, Body}, BillingCode);
        _ ->
            MultiGsm = util_gsm0338:encode_parts(Body, $?, ?PART_MT_MAX_BYTE_BODY_LEN),
            TotalPart = length(MultiGsm),
            PartReference = random:uniform(255),
            lists:foldl(fun({Gsm, CharNum}, {PrevLen, PartNumber}) ->
                            Header = create_multipart_header(PartReference, TotalPart, PartNumber),
                            PartBody = lists:sublist(Body, PrevLen + 1, CharNum),
                            send_mt(Originator, OriginatorFormat, MSISDN, Connection, {header_text_part, Header, Gsm, PartBody}, BillingCode),
                            {PrevLen + CharNum, PartNumber + 1}
                        end, {0, 1}, MultiGsm),
            ok
    end.

send_mt(Originator, OriginatorFormat, MSISDN, Connection, PartData, BillingCode) ->
    Operator = ?MODULE,
    ExpireTime = 86400 * 3,
    CreatedAt = util:gs_now(),
    ExpiresAt = CreatedAt + ExpireTime,

    Mt = #mt{
      key = util:generate_guid(),
      operator = Operator,
      msisdn = MSISDN,
      originator = Originator,
      originator_format = OriginatorFormat,
      connection = Connection,
      billing_code = BillingCode,
      part_data = PartData,
      created_at = CreatedAt,
      expires_at = ExpiresAt
     },
    send_mt(Mt).


-spec send_mt(#mt{}) -> ok | {error, term()}.
send_mt(Mt) ->
    ucp_client:send_mt(Mt).


% ----------------------------------------------------------------------------
% ucp_client calls
% ----------------------------------------------------------------------------

-spec handle_raw_dr(term(), connection()) -> ok.
handle_raw_dr(RawDr, Connection) ->
    case process_raw_dr(RawDr) of
        {ok, [{Status, TransactionId, _Destination, _Timestamp, ErrorCode}]} ->
            ucp_client_util:log_handle_raw_dr_success(Connection, TransactionId, Status, ErrorCode);
        {error, Error} ->
            ucp_client_util:log_handle_raw_dr_failure(Connection, RawDr, Error)
    end.

-spec handle_raw_mo(term(), connection()) -> ok.
handle_raw_mo(RawMo, Connection) ->
    case process_raw_mo(RawMo) of
        {ok, [{ShortMessage, Source, Destination, _Timestamp, Header}]} ->
            ucp_client_util:log_handle_raw_mo_success(Connection, Source, Destination, ShortMessage, Header);
        {error, Error} ->
            ucp_client_util:log_handle_raw_mo_failure(Connection, RawMo, Error)
    end.

-spec get_default_params(connect | login | send_mt) -> proplist().
get_default_params(connect) ->
    [{keep_alive_period, ?KEEP_ALIVE_PERIOD},
     {keep_alive_ot31_pid, ?UCP_PID_PC_OVER_TCPIP},
     {result_timeout, ?RESULT_TIMEOUT}];
get_default_params(login) ->
    [{oton, ?UCP_OTON_ABBREVIATED},
     {onpi, ?UCP_ONPI_SMSC_SPECIFIC},
     {opid, ?UCP_OPID_PC}];
get_default_params(send_mt) ->
    [{otoa, ?UCP_OTOA_ALPHANUMERIC},
     {nrq, ?UCP_NRQ_ON},
     {nt, ?UCP_NT_ALL},
     {mcls, ?UCP_MCLS_1}].

-spec get_default_params(send_mt, atom()) -> proplist().
get_default_params(send_mt, alphanumeric) ->
    [{otoa, ?UCP_OTOA_ALPHANUMERIC},
     {nrq, ?UCP_NRQ_ON},
     {nt, ?UCP_NT_ALL},
     {mcls, ?UCP_MCLS_1}];
get_default_params(send_mt, international_msisdn) ->
    [{otoa, ?UCP_OTOA_TON_AND_NPI},
     {nrq, ?UCP_NRQ_ON},
     {nt, ?UCP_NT_ALL},
     {mcls, ?UCP_MCLS_1}].

-spec get_default_billing_code() -> string().
get_default_billing_code() ->
    ?DEFAULT_BILLING_CODE.

% ----------------------------------------------------------------------------
% Private functions
% ----------------------------------------------------------------------------



create_multipart_header(PartReference, TotalPart, PartNumber) ->
    [5, 0, 3, PartReference, TotalPart, PartNumber].
% NOTE: Each element of the list to be returned as {ok, List} will be:
%       {success | operator_retry | error, TransactionId, Destination, Timestamp, ErrorCode}
-spec process_raw_dr(term()) -> {ok, [{success | operator_retry | error, string(), string(), integer(), string()}]} | {error, term()}.
process_raw_dr(RawDr) ->
    try
        % Quoted from ucp_syntax.erl:
        % {dst,num,1},                 %% Delivery status
        % {oadc,chrstr,22},            %% Address of originator : special treatment
        % {adc,numstr,16},             %% Address of recipient
        % {dscts,time,12},             %% Delivery time stamp
        % {rsn,num,3},                 %% Reason code
        DeliveryStatus = util_proplists:fetch(dst, RawDr),
        % Dst field stands for delivery status and the possible values you would expect are:
        % 0 - Delivered
        % 1 - Buffered (the message has not been delivered just yet, but it is in our
        %     short term storage system (buffer) and we are re-attempting delivery.)
        % 2 - Not delivered
        Status = case DeliveryStatus of
            0 ->
                success;
            1 ->
                operator_retry;
            _ ->
                error
        end,
        Destination = util_proplists:fetch(oadc, RawDr),
        Timestamp = calendar:datetime_to_gregorian_seconds(util_proplists:fetch(dscts, RawDr)),
        RawErrorCode = util_proplists:fetch(rsn, RawDr),
        ErrorCode = integer_to_list(RawErrorCode),
        {ok, RawMsg} =  util_proplists:find(msg, RawDr),
        [TransactionId | _] = string:tokens(RawMsg, ":"),
        {ok, [{Status, TransactionId, Destination, Timestamp, ErrorCode}]}
    catch
        _Class:Error ->
            {error, Error}
    end.

% NOTE: Each element of the list to be returned as {ok, List} will be:
%       {ShortMessage, Source, Destination, Timestamp, []} or
%       {ShortMessage, Source, Destination, Timestamp, Header}
-spec process_raw_mo(term()) -> {ok, [{binary(), string(), string(), integer(), string()}]} | {error, term()}.
process_raw_mo(RawMo) ->
    try
        % Quoted from ucp_syntax.erl:
        % {msg,chrstr,640},            %% Message : Special treatment
        % {oadc,chrstr,22},            %% Address of originator : special treatment
        % {adc,numstr,16},             %% Address of recipient
        % {scts,time,12},              %% Service center time stamp
        RawShortMessage = util_proplists:fetch(msg, RawMo, []),
        ShortMessage = case util_proplists:fetch(dcs, RawMo, 0) of
            0 ->
                UnicodeShortMessage = util_gsm0338:decode(RawShortMessage, $?),
                unicode:characters_to_binary(UnicodeShortMessage, utf32, utf8);
            1 ->
                RSM = case RawShortMessage of
                           List when is_list(List) ->
                              list_to_binary(RawShortMessage);
                           _ ->
                              RawShortMessage
                      end,
                unicode:characters_to_binary(unicode:characters_to_list(RSM, utf16), utf8)
        end,
        Source = util_proplists:fetch(oadc, RawMo),
        Destination = util_proplists:fetch(adc, RawMo),
        Timestamp =  calendar:datetime_to_gregorian_seconds(util_proplists:fetch(scts, RawMo)),
        ExtraServices = util_proplists:fetch(xser, RawMo, []),

        Result = case util_proplists:find(?UCP_XSER_SERVICE_GSM_UDH, ExtraServices) of
            {ok, Header} ->
                HexStrHeader = lists:concat(lists:map(fun(X) ->
                                                        case X < 16 of
                                                            true -> 
                                                                "0" ++ erlang:integer_to_list(X, 16);
                                                            _ ->
                                                                erlang:integer_to_list(X, 16)
                                                        end
                                                    end, Header)),
                [{ShortMessage, Source, Destination, Timestamp, HexStrHeader}];
            undefined ->
                [{ShortMessage, Source, Destination, Timestamp, ""}]
        end,
        {ok, Result}
    catch
        _Class:Error ->
            {error, Error}
    end.
