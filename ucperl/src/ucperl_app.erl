-module(ucperl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% =============================================================================
%% Application callbacks
%% =============================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, terms) ->
    {ok, pid()} | ignore | {error, term()}.
start(_StartType, _StartArgs) ->
    ucperl_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
