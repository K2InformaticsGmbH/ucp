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

%%% EUNIT tests for ucp_syntax.erl
%%% - Roland

-module(ucp_syntax_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

parse(Mess) ->
    M = [2] ++ Mess ++ [3],
    io:format("~s~n", [M]),
    ucp_syntax:parse(M).

p30_test() ->
    parse("01/00045/O/30/66677789///1//////68656C6C6F/CE").

p30a_test() ->
    %% Parse 30 reply not implemented
    ?assertError({badmatch,_},
                  parse("01/00041/R/30/A//66677789:180594141236/F3")).

%%% 31 strings taken from 4.0 specification of EMI/UCP
p31_test() ->
    parse("02/00035/O/31/0234765439845/0139/A0").

p31a_test() ->
    parse("04/00024/R/31/A/0003/2E").

p31n_test() ->
    parse("00/00022/R/31/N/06//07").

%%% 51 strings taken from 4.0 specification of EMI/UCP
p51_1_test() ->
    parse("18/00113/O/51/012345/09876//1/1920870340125000/4/0539//////3012961212//////3//4D657373616765203531/////////////CD").

p51_2_test() ->
    parse("39/00099/O/51/0657467/078769//1//7//1/0545765/0122/1/0808971800///////4/32/F5AA34DE////1/////////65").

p51a_test() ->
    parse("00/00039/R/51/A//012234:090996101010/68").

p51n_test() ->
    parse("00/00022/R/51/N/31//07").

%%% 52 strings taken from 4.0 specification of EMI/UCP
p52_test() ->
    parse("00/00120/O/52/076523578/07686745/////////////120396111055////3//43616C6C20796F75206261636B206C617465722E///0//////////A3").

p52a_test() ->
    parse("00/00039/R/52/A//076567:010196010101/6C").

p52n_test() ->
    parse("00/00022/R/52/N/01//05").

%%% 53 strings taken from 4.0 specification of EMI/UCP
p53_test() ->
    parse("00/00234/O/53/1299998/3155555/////////////090196161057/1/108/090196161105/3//4D65737361676520666F7220333135353535352C2077697468206964656E74696669636174696F6E2039363031303931363130353720686173206265656E206275666665726564/////////////1F").

p53a_test() ->
    parse("00/00032/R/53/A//020296020202/F2").

p53n_test() ->
    parse("00/00022/R/53/N/02//07").

%%% 60 strings taken from 4.0 specification of EMI/UCP
p60_test() ->
    parse("02/00059/O/60/07656765/2/1/1/50617373776F7264//0100//////61").

p60a_test() ->
    parse("00/00019/R/60/A//6D").

p60n_test() ->
    parse("00/00022/R/60/N/01//04").

m_test() ->
    S = ucp_syntax:make_31(12, "08842857", 0139),
    io:format("~s~n", [S]),
    S.

p_test() ->
    ucp_syntax:parse(m_test()).

r_test() ->
    S = ucp_syntax:make_result(p_test()),
    io:format("~s~n", [S]),
    S.

a_test() ->
    S = ucp_syntax:make_ack(p_test()),
    io:format("~s~n", [S]),
    S.

n_test() ->
    S = ucp_syntax:make_nack(p_test(), {1, "I dont think so!"}),
    io:format("~s~n", [S]),
    S.

pa_test() ->
    ucp_syntax:parse(a_test()).

pn_test() ->
    ucp_syntax:parse(n_test()).

pr_test() ->
    ucp_syntax:parse(r_test()).

pp30_test() -> p(30).
pa30_test() -> %% Parse 30 reply not implemented
    ?assertError({badmatch,_}, pa(30)).
pn30_test() -> pn(30).
pr30_test() -> %% Parse 30 reply not implemented
    ?assertError({badmatch,_}, pr(30)).

pp31_test() -> p(31).
pa31_test() -> pa(31).
pn31_test() -> pn(31).
pr31_test() -> pr(31).

pp311_test() -> p(311).
pa311_test() -> pa(311).
pn311_test() -> pn(311).
pr311_test() -> pr(311).

pp51_test() -> p(51).
pa51_test() -> pa(51).
pn51_test() -> pn(51).
pr51_test() -> pr(51).

pp52_test() -> p(52).
pa52_test() -> pa(52).
pn52_test() -> pn(52).
pr52_test() -> pr(52).

pp53_test() -> p(53).
pa53_test() -> pa(53).
pn53_test() -> pn(53).
pr53_test() -> pr(53).

pp60_test() -> p(60).
pa60_test() -> pa(60).
pn60_test() -> pn(60).
pr60_test() -> pr(60).

m(N) ->
    S = case N of
            30 ->
                ucp_syntax:make_30(11,
                                   "12700000000180");
            31 ->
                ucp_syntax:make_31(11,
                                   "12700000000180",
                                   0539);
            311 ->
                ucp_syntax:make_31(11,
                                   "127.0.0.1:81",
                                   0539);
            51 ->
                ucp_syntax:make_51(31,
                                   "08842857",
                                   "08842858",
                                   "hello", []);
            52 ->
                ucp_syntax:make_52(32,
                                   "08842857",
                                   "08842858",
                                   "hello", []);
            53 ->
                ucp_syntax:make_53(33,
                                   "08842857",
                                   "08842858", []);
            60 ->
                ucp_syntax:make_60(40,
                                   "08842857",
                                   1,
                                   "123", [])
        end,
    io:format("~s~n", [S]),
    S.

p(N) ->
    ucp_syntax:parse(m(N)).

r(N) ->
    S = ucp_syntax:make_result(p(N)),
    io:format("~s~n", [S]),
    S.

a(N) ->
    S = ucp_syntax:make_ack(p(N)),
    io:format("~s~n", [S]),
    S.

n(N) ->
    S = ucp_syntax:make_nack(p(N), 1),
    io:format("~s~n", [S]),
    S.

pa(N) ->
    ucp_syntax:parse(a(N)).

pn(N) ->
    ucp_syntax:parse(n(N)).

pr(N) ->
    ucp_syntax:parse(r(N)).
