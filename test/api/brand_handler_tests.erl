-module(brand_handler_tests).
%%%===================================================================
%%% @doc brand_handler 白标品牌配置 EUnit 测试（C0-BRAND-01）
%%%
%%% 覆盖：默认 fixture、白标 fixture、逐字段非法值回退、URL scheme 白名单、
%%% 未知键丢弃、config 键名前缀契约。全部为纯函数测试，不触库、不 mock。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% fixture
%%%===================================================================

%% 默认 fixture：不配置任何 brand_* 时的开源 imboy 品牌
default_fixture_test() ->
    D = brand_handler:defaults(),
    ?assertEqual(<<"imboy"/utf8>>, maps:get(<<"site_name">>, D)),
    ?assertEqual(<<"#2474E5">>, maps:get(<<"primary_color">>, D)),
    ?assertEqual(<<"light">>, maps:get(<<"theme">>, D)),
    %% 客服/隐私等对外联系方式默认必须为空，代码不得预置
    ?assertEqual(<<>>, maps:get(<<"support_url">>, D)),
    ?assertEqual(<<>>, maps:get(<<"privacy_url">>, D)),
    %% 默认值自身必须是合法配置（normalize 幂等）
    ?assertEqual(D, brand_handler:normalize(D)).

%% 计划要求的字段全部存在：应用名 / Logo / 启动页 / 主题主色 / 客服 / 隐私
required_fields_present_test() ->
    Keys = maps:keys(brand_handler:defaults()),
    lists:foreach(
        fun(K) -> ?assert(lists:member(K, Keys)) end,
        [
            <<"site_name">>,
            <<"logo_url">>,
            <<"splash_url">>,
            <<"primary_color">>,
            <<"support_url">>,
            <<"privacy_url">>
        ]
    ).

%% 白标 fixture：完整合法配置必须原样透出
whitelabel_fixture_test() ->
    Brand = #{
        <<"site_name">> => <<"某企业IM"/utf8>>,
        <<"logo_url">> => <<"https://cdn.example.com/logo.png">>,
        <<"splash_url">> => <<"https://cdn.example.com/splash.png">>,
        <<"primary_color">> => <<"#1A73E8">>,
        <<"accent_color">> => <<"#FF6D00">>,
        <<"theme">> => <<"dark">>,
        <<"slogan">> => <<"高效协作"/utf8>>,
        <<"copyright">> => <<"© 2026 某企业"/utf8>>,
        <<"company">> => <<"某企业股份有限公司"/utf8>>,
        <<"support_url">> => <<"https://support.example.com">>,
        <<"privacy_url">> => <<"https://example.com/privacy">>
    },
    ?assertEqual(Brand, brand_handler:normalize(Brand)).

%%%===================================================================
%%% 非法值逐字段回退
%%%===================================================================

invalid_color_falls_back_test() ->
    D = brand_handler:defaults(),
    Default = maps:get(<<"primary_color">>, D),
    lists:foreach(
        fun(Bad) ->
            Got = brand_handler:normalize(#{<<"primary_color">> => Bad}),
            ?assertEqual(Default, maps:get(<<"primary_color">>, Got))
        end,
        [<<"07C160">>, <<"#07C16">>, <<"#GGGGGG">>, <<"red">>, <<>>, 123, undefined]
    ),
    %% 合法色值（大小写十六进制）保留
    ?assertEqual(
        <<"#aAbBcC">>,
        maps:get(
            <<"primary_color">>, brand_handler:normalize(#{<<"primary_color">> => <<"#aAbBcC">>})
        )
    ).

%% URL 只允许 http(s) 绝对地址或空，挡掉可注入前端的 scheme
invalid_url_scheme_falls_back_test() ->
    lists:foreach(
        fun(Field) ->
            lists:foreach(
                fun(Bad) ->
                    Got = brand_handler:normalize(#{Field => Bad}),
                    ?assertEqual(<<>>, maps:get(Field, Got))
                end,
                [
                    <<"javascript:alert(1)">>,
                    <<"data:text/html,<script>">>,
                    <<"file:///etc/passwd">>,
                    <<"//evil.example.com">>,
                    <<"/relative/path.png">>,
                    <<"https://">>,
                    <<"http://">>,
                    42
                ]
            ),
            %% 合法 http(s) 保留
            ?assertEqual(
                <<"https://ok.example.com/a.png">>,
                maps:get(
                    Field, brand_handler:normalize(#{Field => <<"https://ok.example.com/a.png">>})
                )
            )
        end,
        [<<"logo_url">>, <<"splash_url">>, <<"support_url">>, <<"privacy_url">>]
    ).

invalid_theme_falls_back_test() ->
    lists:foreach(
        fun(Bad) ->
            Got = brand_handler:normalize(#{<<"theme">> => Bad}),
            ?assertEqual(<<"light">>, maps:get(<<"theme">>, Got))
        end,
        [<<"Dark">>, <<"auto">>, <<>>, 1]
    ),
    ?assertEqual(
        <<"dark">>, maps:get(<<"theme">>, brand_handler:normalize(#{<<"theme">> => <<"dark">>}))
    ).

empty_site_name_falls_back_test() ->
    lists:foreach(
        fun(Bad) ->
            Got = brand_handler:normalize(#{<<"site_name">> => Bad}),
            ?assertEqual(<<"imboy"/utf8>>, maps:get(<<"site_name">>, Got))
        end,
        [<<>>, undefined, 0]
    ).

%% 单个坏字段不得废掉整套品牌：其余合法字段必须保留
one_bad_field_does_not_poison_others_test() ->
    Got = brand_handler:normalize(#{
        <<"site_name">> => <<"某企业IM"/utf8>>,
        <<"primary_color">> => <<"not-a-color">>,
        <<"logo_url">> => <<"https://cdn.example.com/logo.png">>
    }),
    ?assertEqual(<<"某企业IM"/utf8>>, maps:get(<<"site_name">>, Got)),
    ?assertEqual(<<"https://cdn.example.com/logo.png">>, maps:get(<<"logo_url">>, Got)),
    ?assertEqual(<<"#2474E5">>, maps:get(<<"primary_color">>, Got)).

%% 缺失字段补默认；未知键丢弃（不透传到客户端）
partial_config_and_unknown_keys_test() ->
    Got = brand_handler:normalize(#{
        <<"site_name">> => <<"X">>, <<"secret_token">> => <<"leak">>
    }),
    ?assertEqual(lists:sort(maps:keys(brand_handler:defaults())), lists:sort(maps:keys(Got))),
    ?assertEqual(false, maps:is_key(<<"secret_token">>, Got)),
    ?assertEqual(<<"light">>, maps:get(<<"theme">>, Got)).

normalize_non_map_falls_back_test() ->
    ?assertEqual(brand_handler:defaults(), brand_handler:normalize(undefined)),
    ?assertEqual(brand_handler:defaults(), brand_handler:normalize(<<"junk">>)).

%%%===================================================================
%%% config 键名契约（管理端写入 brand_* 键）
%%%===================================================================

config_key_prefix_test() ->
    ?assertEqual(<<"brand_site_name">>, brand_handler:config_key(<<"site_name">>)),
    ?assertEqual(<<"brand_splash_url">>, brand_handler:config_key(<<"splash_url">>)),
    %% 每个字段都有对应 config 键
    lists:foreach(
        fun(F) -> ?assertMatch(<<"brand_", _/binary>>, brand_handler:config_key(F)) end,
        maps:keys(brand_handler:defaults())
    ).
