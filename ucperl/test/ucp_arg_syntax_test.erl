%%% EUNIT tests for ucp_arg_syntax.erl
%%% - Roland

-module(ucp_arg_syntax_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


x1_test() ->
    Xsers = [{1, "abcde"}],
    ?assertEqual(Xsers,
                 ucp_arg_syntax:parse_xsers(ucp_arg_syntax:make_xsers(Xsers))).

x2_test() ->
    Xsers = [{2, "abcde"}],
    ?assertEqual(Xsers,
                 ucp_arg_syntax:parse_xsers(ucp_arg_syntax:make_xsers(Xsers))).

%%% TODO: this is an anomaly. A flat string() data is created
%%% as if it is recursive. Maybe this should not be possible.
x4_test() ->
    Xsers = [{2, [{32,"......"}]}],
    Xsers2 = [{2,[32,6,46,46,46,46,46,46]}],
    ?assertEqual(Xsers2,
                 ucp_arg_syntax:parse_xsers(ucp_arg_syntax:make_xsers(Xsers))).

%%% NOTE: this deals with Three IE systems.
x5_test() ->
    Xsers = [{2, [0]}],
    ?assertEqual(Xsers, ucp_arg_syntax:parse_xsers("020100")),
    ?assertEqual(Xsers, ucp_arg_syntax:parse_xsers("02010")).

p7_1_test() ->
    Unpacked = <<1,1,1,1,1,1,1,1>>,
    Packed = <<129,64,32,16,8,4,2>>,
    ?assertEqual(Packed, ucp_arg_syntax:pack7(Unpacked)).

%%% TODO p7_2 and p7_3 - coding so that e.g. @ is \0 ????
p7_2_test() ->
    Unpacked = <<"ALPHA\0NUM">>,
    Packed = <<16#41,16#26,16#14,16#19,16#04,16#38,16#AB,16#4D>>,
    ?assertEqual(Packed, ucp_arg_syntax:pack7(Unpacked)).

%%% ERROR - @ should be coded as \0
p7_3_test() ->
    Unpacked = <<"ALPHA@NUM">>,
    Packed = <<16#41,16#26,16#14,16#19,16#04,16#38,16#AB,16#4D>>,
    ?assertError({badmatch,_},
                  Packed = ucp_arg_syntax:pack7(Unpacked)).

u7_1_test() ->
    Unpacked = <<1,1,1,1,1,1,1,1>>,
    Packed = <<129,64,32,16,8,4,2>>,
    ?assertEqual(Packed, ucp_arg_syntax:pack7(Unpacked)).
