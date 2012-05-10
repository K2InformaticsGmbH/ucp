-module(util_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include("util.hrl").

%% =============================================================================
%% Application callbacks
%% =============================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, terms) ->
    {ok, pid()}.
start(_StartType, _StartArgs) ->
    util_log:open_log(?EVENT_LOG, "./log/"),
    util_log:open_log(?ERROR_LOG, "./log/"),
    util_log:open_log(?INFO_LOG, "./log/"),
    {ok, self()}.

-spec stop(term()) -> ok.
stop(_State) ->
    simple_logger:close(?EVENT_LOG),
    simple_logger:close(?ERROR_LOG),
    simple_logger:close(?INFO_LOG),
    ok.
