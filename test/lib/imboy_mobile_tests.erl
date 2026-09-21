-module(imboy_mobile_tests).
-include_lib("eunit/include/eunit.hrl").

%%% 手机号归一化与脱敏（GZAPP-06 / D11 PII 口径）
%%% 纯函数单测：normalize / mask / valid。

normalize_test_() ->
    [
        {"+86 前缀剥离", fun() ->
            ?assertEqual({ok, <<"13800138000">>}, imboy_mobile:normalize(<<"+8613800138000">>))
        end},
        {"空白与分隔符剥离", fun() ->
            ?assertEqual({ok, <<"13800138000">>}, imboy_mobile:normalize(<<" 138-0013 (8000) ">>))
        end},
        {"国际长号保留（5-20 位数字）", fun() ->
            ?assertEqual(
                {ok, <<"85212345678">>}, imboy_mobile:normalize(<<"+852 1234 5678">>)
            )
        end},
        {"非数字 → invalid", fun() ->
            ?assertEqual({error, invalid}, imboy_mobile:normalize(<<"138abc0000">>))
        end},
        {"过短（<5 位）→ invalid", fun() ->
            ?assertEqual({error, invalid}, imboy_mobile:normalize(<<"1234">>))
        end},
        {"过长（>20 位）→ invalid", fun() ->
            ?assertEqual(
                {error, invalid}, imboy_mobile:normalize(<<"123456789012345678901">>)
            )
        end},
        {"非 binary → invalid", fun() ->
            ?assertEqual({error, invalid}, imboy_mobile:normalize(13800138000))
        end}
    ].

mask_test_() ->
    [
        {"11 位国内号：前3后4", fun() ->
            ?assertEqual(<<"138****8000">>, imboy_mobile:mask(<<"13800138000">>))
        end},
        {"8 位边界：前3后4", fun() ->
            ?assertEqual(<<"123****5678">>, imboy_mobile:mask(<<"12345678">>))
        end},
        {"短号（<8 位）全掩码", fun() ->
            ?assertEqual(<<"****">>, imboy_mobile:mask(<<"1234567">>)),
            ?assertEqual(<<"****">>, imboy_mobile:mask(<<>>))
        end}
    ].

valid_test_() ->
    [
        {"合法形状", fun() ->
            ?assert(imboy_mobile:valid(<<"13800138000">>))
        end},
        {"非法形状", fun() ->
            ?assertNot(imboy_mobile:valid(<<"13800x8000">>)),
            ?assertNot(imboy_mobile:valid(<<"123">>))
        end}
    ].
