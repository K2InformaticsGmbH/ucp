%%% EUNIT tests for ucp_session.erl
%%% - Roland

-module(ucp_session_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

send_test() ->
    send().

send_again_test() ->
    timer:sleep(1000),
    send().

%%%


%% TODO: how to get this quiet ??
%% gen_server talks too much when terminating
send() ->
    {_, _} = ucp_test_server:start(),
    timer:sleep(100),
    {_, _} = ucp_test_client:start(c1),
    timer:sleep(100),
    {ok, _} = ucp_test_client:send(c1),
    timer:sleep(100),
    ok = ucp_test_client:stop(c1),
    timer:sleep(100),
    {_, ok, _, true} = ucp_test_server:stop().
