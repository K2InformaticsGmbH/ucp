-module(ucp_client_connector).

-export([connect/7,
         sleep_and_connect/7]).

-define(CONNECT_RETRY_DELAY, 60000).


-spec connect(atom(), atom(), string(), integer(), string(), string(), integer()) -> ok.
connect(SrvRef, OperatorMod, Host, Port, Adc, Password, WindowSize) ->
    ucp_client_util:log_connect_attempt(SrvRef, Host, Port),
    OpConnectParams = OperatorMod:get_default_params(connect),
    ConnectParams = [{keep_alive_ot31_adc, Adc},
                     {win_size_out, WindowSize} | OpConnectParams],
    case gen_ucp_client:connect(SrvRef, Host, Port, ConnectParams) of
        ok ->
            ucp_client_util:log_connect_success(SrvRef),
            LoginParams = OperatorMod:get_default_params(login),
            ucp_client_util:log_login_attempt(SrvRef, Adc, Password),
            gen_ucp_client:login(SrvRef, Adc, Password, LoginParams, []);
        {error, Reason} ->
            ucp_client_util:log_connect_failure(SrvRef, Reason),
            ?MODULE:sleep_and_connect(SrvRef, OperatorMod, Host, Port, Adc, Password, WindowSize)
    end.

-spec sleep_and_connect(atom(), atom(), string(), integer(), string(), string(), integer()) -> ok.
sleep_and_connect(SrvRef, OperatorMod, Host, Port, Adc, Password, WindowSize) ->
    timer:sleep(?CONNECT_RETRY_DELAY),
    ?MODULE:connect(SrvRef, OperatorMod, Host, Port, Adc, Password, WindowSize).
