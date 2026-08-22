%%% @doc user_agg 纯校验策略 eunit 测试（零 mock）。
%%% 验证资料字段更新校验与 user_logic:update/3 现有语义逐字对齐：
%%% gender/allow_search 枚举、email 格式、字段白名单为纯决策；
%%% email 占用查询属 I/O，留 logic 外壳（T3.2），本层只裁决格式。
-module(user_agg_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- email（格式纯决策；占用查询留外壳）----

%% 合法邮箱格式 → {ok, {check_email, Val}}（占用/绑定由外壳处理）。
email_valid_format_test() ->
    ?assertEqual(
        {ok, {check_email, <<"a@b.com">>}},
        user_agg:validate_update(<<"email">>, <<"a@b.com">>)
    ).

%% 非法邮箱格式 → {error, bad_email_format}。
email_bad_format_test() ->
    ?assertEqual(
        {error, bad_email_format},
        user_agg:validate_update(<<"email">>, <<"not-an-email">>)
    ).

%% 非 binary 邮箱值 → 落白名单兜底（unsupported_field），镜像原 is_binary 守卫。
email_non_binary_falls_through_test() ->
    ?assertEqual(
        {error, unsupported_field},
        user_agg:validate_update(<<"email">>, 123)
    ).

%% 邮箱解绑值 → {ok, {set_field, <<"email">>, <<>>}}
email_unbind_test() ->
    ?assertEqual(
        {ok, {set_field, <<"email">>, <<>>}},
        user_agg:validate_update(<<"email">>, <<>>)
    ).

%% 手机号与支付宝测试
mobile_and_alipay_test() ->
    ?assertEqual(
        {ok, {set_field, <<"mobile">>, <<>>}},
        user_agg:validate_update(<<"mobile">>, <<>>)
    ),
    ?assertEqual(
        {ok, {set_field, <<"mobile">>, <<"+8613812345678">>}},
        user_agg:validate_update(<<"mobile">>, <<"+8613812345678">>)
    ),
    ?assertEqual(
        {ok, {set_setting, <<"alipay">>, <<"alipay-account">>}},
        user_agg:validate_update(<<"alipay">>, <<"alipay-account">>)
    ).

%% ---- 透传字段（无校验，原样落库）----

passthrough_fields_test() ->
    [
        ?assertEqual(
            {ok, {set_field, F, <<"v">>}},
            user_agg:validate_update(F, <<"v">>)
        )
     || F <- [
            <<"sign">>,
            <<"nickname">>,
            <<"avatar">>,
            <<"region">>,
            <<"birthday">>
        ]
    ].

%% ---- gender 枚举（1 男 / 2 女 / 3 保密）----

gender_valid_test() ->
    ?assertEqual({ok, {set_gender, 1}}, user_agg:validate_update(<<"gender">>, <<"1">>)),
    ?assertEqual({ok, {set_gender, 2}}, user_agg:validate_update(<<"gender">>, <<"2">>)),
    ?assertEqual({ok, {set_gender, 3}}, user_agg:validate_update(<<"gender">>, <<"3">>)).

gender_invalid_test() ->
    ?assertEqual({error, bad_gender}, user_agg:validate_update(<<"gender">>, <<"0">>)),
    ?assertEqual({error, bad_gender}, user_agg:validate_update(<<"gender">>, <<"4">>)),
    ?assertEqual({error, bad_gender}, user_agg:validate_update(<<"gender">>, <<"x">>)).

%% ---- allow_search 枚举（1 / 2）----

allow_search_valid_test() ->
    ?assertEqual(
        {ok, {set_allow_search, 1}},
        user_agg:validate_update(<<"allow_search">>, <<"1">>)
    ),
    ?assertEqual(
        {ok, {set_allow_search, 2}},
        user_agg:validate_update(<<"allow_search">>, <<"2">>)
    ).

allow_search_invalid_test() ->
    ?assertEqual(
        {error, bad_allow_search},
        user_agg:validate_update(<<"allow_search">>, <<"3">>)
    ),
    ?assertEqual(
        {error, bad_allow_search},
        user_agg:validate_update(<<"allow_search">>, <<"x">>)
    ).

%% ---- 未支持字段 → 兜底拒绝 ----

unsupported_field_test() ->
    ?assertEqual(
        {error, unsupported_field},
        user_agg:validate_update(<<"unknown_col">>, <<"v">>)
    ).

%% ---- 辅助谓词 ----

is_valid_email_predicate_test() ->
    ?assert(user_agg:is_valid_email(<<"a@b.com">>)),
    ?assertNot(user_agg:is_valid_email(<<"nope">>)).

validate_gender_predicate_test() ->
    ?assertEqual({ok, 2}, user_agg:validate_gender(<<"2">>)),
    ?assertEqual(error, user_agg:validate_gender(<<"9">>)).

validate_allow_search_predicate_test() ->
    ?assertEqual({ok, 1}, user_agg:validate_allow_search(<<"1">>)),
    ?assertEqual(error, user_agg:validate_allow_search(<<"9">>)).

%% ---- 隐私布尔开关（QA #19）----

privacy_bool_switch_test() ->
    ?assertEqual(
        {ok, {set_setting, <<"allow_add_by_phone">>, true}},
        user_agg:validate_update(<<"allow_add_by_phone">>, <<"true">>)
    ),
    ?assertEqual(
        {ok, {set_setting, <<"allow_add_by_qr">>, false}},
        user_agg:validate_update(<<"allow_add_by_qr">>, <<"false">>)
    ),
    ?assertEqual(
        {ok, {set_online_visibility, false}},
        user_agg:validate_update(<<"show_online_status">>, <<"false">>)
    ),
    ?assertEqual(
        {ok, {set_nearby_visible, true}},
        user_agg:validate_update(<<"allow_nearby_visible">>, <<"true">>)
    ).

privacy_bool_switch_invalid_test() ->
    ?assertEqual(
        {error, bad_bool},
        user_agg:validate_update(<<"allow_add_by_phone">>, <<"1">>)
    ),
    ?assertEqual(
        {error, bad_bool},
        user_agg:validate_update(<<"show_online_status">>, <<"yes">>)
    ).

validate_bool_predicate_test() ->
    ?assertEqual({ok, true}, user_agg:validate_bool(<<"true">>)),
    ?assertEqual({ok, false}, user_agg:validate_bool(<<"false">>)),
    ?assertEqual({ok, true}, user_agg:validate_bool(true)),
    ?assertEqual(error, user_agg:validate_bool(<<"0">>)).
