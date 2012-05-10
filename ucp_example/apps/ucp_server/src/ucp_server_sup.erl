-module(ucp_server_sup).

% supervisor callbacks
-behaviour(supervisor).
-export([init/1]).

% Public functions
-export([start_link/1]).

-include_lib("util/include/util.hrl").
-include("ucp_server.hrl").


% ----------------------------------------------------------------------------
% Public functions
% ----------------------------------------------------------------------------

-spec start_link(integer()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Port).


% ----------------------------------------------------------------------------
% supervisor callbacks
% ----------------------------------------------------------------------------

-spec init(integer()) -> {ok, term()}.
init(Port) ->
    Server = {ucp_server, {ucp_server, start_link, [Port]},
               permanent, 60000, worker, [ucp_server]},
    {ok, {{one_for_one, 3, 1}, [Server]}}.
