-module(util_gsm0338_test).

-include_lib("eunit/include/eunit.hrl").

-define(BONUS_UNICODE, [229,228,230,224,231,915,232,233,916,934,236,923,241,246,248,242,223,928,931,252,249]).
-define(BONUS_BIN, <<143,125,231,159,152,16,10,16,201,129,210,231,51,16,30,11,198,111,0>>).


encode_test() ->
    ok = util_gsm0338:init(),
    ?assertMatch({<<"a">>, 1}, util_gsm0338:encode("a", $?)),
    ?assertMatch({<<97, 241, 24>>, 3}, util_gsm0338:encode("abc", $?)),
    ?assertMatch({<<"?">>, 1}, util_gsm0338:encode([255], $?)).

encode_parts_test() ->
    ok = util_gsm0338:init(),
    ?assertMatch([{<<"a">>, 1}, {<<"b">>, 1}], util_gsm0338:encode_parts("ab", $?, 1)),
    ?assertMatch([{<<97, 241, 24>>, 3}, {<<228, 178, 25>>, 3}], util_gsm0338:encode_parts("abcdef", $?, 3)),
    ?assertMatch([{<<"?">>, 1}, {<<"?">>, 1}], util_gsm0338:encode_parts([255, 255], $?, 1)).


bonus_encode_test() ->
    ok = util_gsm0338:init(),
    Size = length(?BONUS_UNICODE),
    ?assertMatch({?BONUS_BIN, Size}, util_gsm0338:encode(?BONUS_UNICODE, $?)).

decode_test() ->
    ok = util_gsm0338:init(),
    ?assertMatch("a", util_gsm0338:decode("a", $?)),
    ?assertMatch("abc", util_gsm0338:decode([97, 241, 24], $?)).

bonus_decode_test() ->
    ok = util_gsm0338:init(),
    ?assertMatch(?BONUS_UNICODE, util_gsm0338:decode(?BONUS_BIN, $?)).


special_pad_start_test() ->
    ?assertMatch(<<>>, util_gsm0338:special_pad_start(<<>>)),
    ?assertMatch(<<$ :7, 1:1>>, util_gsm0338:special_pad_start(<<1:1>>)),
    ?assertMatch(<<2:8>>, util_gsm0338:special_pad_start(<<2:2>>)),
    ?assertMatch(<<255:8>>, util_gsm0338:special_pad_start(<<255:8>>)),
    ?assertMatch(<<$ :7, 1:1, 0:8>>, util_gsm0338:special_pad_start(<<1:1, 0:8>>)),
    ?assertMatch(<<2:8, 0:8>>, util_gsm0338:special_pad_start(<<2:2, 0:8>>)).

encode_ambiguous_extra_at_test() ->
    Tables = util_gsm0338:init(),
    ?assertMatch({<<0,0,0,0,0,0,64>>, 7}, util_gsm0338:encode("@@@@@@@", $?)),
    ?assertMatch({<<0,0,0,0,0,0,0>>, 8}, util_gsm0338:encode("@@@@@@@@", $?)),
    ?assertMatch({<<0,0,0,0,0,0,0,0,0,0,0,0,0,64>>, 15}, util_gsm0338:encode("@@@@@@@@@@@@@@@", $?)),
    ?assertMatch({<<0,0,0,0,0,0,0,0,0,0,0,0,0,0>>, 16}, util_gsm0338:encode("@@@@@@@@@@@@@@@@", $?)).

decode_ambiguous_extra_at_test() ->
    Tables = util_gsm0338:init(),
    ?assertMatch("@@@@@@@ ", util_gsm0338:decode(<<0,0,0,0,0,0,64>>, $?)),
    ?assertMatch("@@@@@@@@", util_gsm0338:decode(<<0,0,0,0,0,0,0>>, $?)),
    ?assertMatch("@@@@@@@@@@@@@@@ ", util_gsm0338:decode(<<0,0,0,0,0,0,0,0,0,0,0,0,0,64>>, $?)),
    ?assertMatch("@@@@@@@@@@@@@@@@", util_gsm0338:decode(<<0,0,0,0,0,0,0,0,0,0,0,0,0,0>>, $?)).
