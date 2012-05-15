%% -------------------------------------------------------------------
%%
%%
%% Copyright (c) 2012 Mobile Interactive Group a Velti company. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

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
