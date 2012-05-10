-module(ucp_client_sup).

% Public functions
-export([start_link/0,
         start_session/7]).

% supervisor callbacks
-behaviour(supervisor).
-export([init/1]).

-include_lib("util/include/util.hrl").
-include("ucp_client.hrl").


% ----------------------------------------------------------------------------
% Public functions
% ----------------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_session(connection(), atom(), string(), integer(), string(), string(), integer()) -> {ok, pid()} | {error, term()}.
start_session(Connection, OperatorMod, Host, Port, Adc, Password, WindowSize) ->
    supervisor:start_child(?MODULE, [Connection, OperatorMod, Host, Port, Adc, Password, WindowSize]).


% ----------------------------------------------------------------------------
% supervisor callbacks
% ----------------------------------------------------------------------------

-spec init([]) -> {ok, term()}.
init([]) ->
    Client = {ucp_client, {ucp_client, start_link, []},
              transient, 60000, worker, [ucp_client]},
    {ok, {{simple_one_for_one, 3, 1}, [Client]}}.
