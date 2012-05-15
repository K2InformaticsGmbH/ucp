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

-module(gen_ucp_client_util).

-export([log_unknown_operation/3,
         log_unknown_result/3,
         log_invalid_trn/3]).


-spec log_unknown_operation(term(), pid(), term()) -> ok.
log_unknown_operation(SrvRef, Session, Ucp) ->
    ok = util_log:log_event([gen_ucp_client_unknown_operation, util:term_to_string(SrvRef), util:term_to_string(Session), util:term_to_string(Ucp)]),
    ok = util_log:log_error("General UCP client unknown operation - srv_ref: ~p, session: ~p, ucp:~n~p~n", [SrvRef, Session, Ucp]).

-spec log_unknown_result(term(), pid(), term()) -> ok.
log_unknown_result(SrvRef, Session, Ucp) ->
    ok = util_log:log_event([gen_ucp_client_unknown_result, util:term_to_string(SrvRef), util:term_to_string(Session), util:term_to_string(Ucp)]),
    ok = util_log:log_error("General UCP client unknown result - srv_ref: ~p, session: ~p, ucp:~n~p~n", [SrvRef, Session, Ucp]).

-spec log_invalid_trn(term(), pid(), term()) -> ok.
log_invalid_trn(SrvRef, Session, Info) ->
    ok = util_log:log_event([gen_ucp_server_invalid_trn, util:term_to_string(SrvRef), util:term_to_string(Session), util:term_to_string(Info)]),
    ok = util_log:log_error("General UCP server invalid trn - srv_ref: ~p, session: ~p, info:~n~p~n", [SrvRef, Session, Info]).
