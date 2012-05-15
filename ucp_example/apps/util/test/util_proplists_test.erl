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

-module(util_proplists_test).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_PROPLIST, [{?TEST_PROPKEY, ?TEST_PROPVAL}]).
-define(TEST_PROPKEY, test_propkey).
-define(TEST_PROPVAL, test_propval).


find_ok_test() ->
    ?assertMatch({ok, ?TEST_PROPVAL}, util_proplists:find(?TEST_PROPKEY, ?TEST_PROPLIST)).

find_undefined_test() ->
    ?assertMatch(undefined, util_proplists:find(invalid_propkey, ?TEST_PROPLIST)).


fetch_ok_test() ->
    ?assertMatch(?TEST_PROPVAL, util_proplists:fetch(?TEST_PROPKEY, ?TEST_PROPLIST)).

fetch_badmatch_test() ->
    ?assertError({badmatch, undefined}, util_proplists:fetch(invalid_propkey, ?TEST_PROPLIST)).

fetch_ok_not_default_test() ->
    ?assertMatch(?TEST_PROPVAL, util_proplists:fetch(?TEST_PROPKEY, ?TEST_PROPLIST, default_propval)).

fetch_ok_default_test() ->
    ?assertMatch(default_propval, util_proplists:fetch(invalid_propkey, ?TEST_PROPLIST, default_propval)).


store_test() ->
    ?assertMatch([{?TEST_PROPKEY, new_propval}], util_proplists:store(?TEST_PROPKEY, new_propval, ?TEST_PROPLIST)),
    ?assertEqual(?TEST_PROPLIST ++ [{new_propkey, new_propval}], util_proplists:store(new_propkey, new_propval, ?TEST_PROPLIST)).
