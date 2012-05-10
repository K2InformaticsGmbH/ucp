-module(simple_logger_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(term(), list()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    simple_logger_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
