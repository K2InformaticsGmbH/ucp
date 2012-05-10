
-module(simple_logger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_child(atom(), string()) -> {ok, pid()} | {error, term()}.
start_child(FileName, FilePath) ->
    supervisor:start_child(?MODULE, [FileName, FilePath]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Logger = ?CHILD(simple_logger, worker),
    {ok, { {simple_one_for_one, 5, 1}, [Logger]} }.

