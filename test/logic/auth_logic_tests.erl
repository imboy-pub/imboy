-module(auth_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% auth_logic 模块的 EUnit 测试
%%%
%%% 目标：验证认证逻辑功能
%%% 覆盖：资源访问权限验证、Token 验证、边界条件
%%%===================================================================

%% ===================================================================
%% verify_for_open/3 测试
%% ===================================================================

verify_for_open_with_all_params_success_test_() ->
    ?WITH_MECK(auth_ds, [
        {'get_token', 3, fun(assets, <<"open">>, _V) -> <<"valid_token">> end}
    ], fun() ->
        Path = <<"/resources/file123">>,
        Tk = <<"valid_token">>,
        Val = <<"1234567890">>,

        Result = auth_logic:verify_for_open(Path, Tk, Val),
        ?assertEqual(<<"ok">>, Result)
    end).

verify_for_open_with_undefined_path_returns_fail_test_() ->
    ?TEST_SIMPLE(fun() ->
        Path = undefined,
        Tk = <<"valid_token">>,
        Val = <<"1234567890">>,

        Result = auth_logic:verify_for_open(Path, Tk, Val),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_open_with_undefined_tk_returns_fail_test_() ->
    ?TEST_SIMPLE(fun() ->
        Path = <<"/resources/file123">>,
        Tk = undefined,
        Val = <<"1234567890">>,

        Result = auth_logic:verify_for_open(Path, Tk, Val),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_open_with_undefined_val_returns_fail_test_() ->
    ?TEST_SIMPLE(fun() ->
        Path = <<"/resources/file123">>,
        Tk = <<"valid_token">>,
        Val = undefined,

        Result = auth_logic:verify_for_open(Path, Tk, Val),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_open_with_invalid_token_returns_fail_test_() ->
    ?WITH_MECK(auth_ds, [
        {'get_token', 3, fun(assets, <<"open">>, _V) -> <<"different_token">> end}
    ], fun() ->
        Path = <<"/resources/file123">>,
        Tk = <<"invalid_token">>,
        Val = <<"1234567890">>,

        Result = auth_logic:verify_for_open(Path, Tk, Val),
        ?assertEqual(<<"fail">>, Result)
    end).

%% ===================================================================
%% verify_for_assets/4 测试
%% ===================================================================

verify_for_assets_with_all_params_success_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'get_token', 3, fun(assets, scene, _V) -> <<"valid_asset_token">> end}
        ]},
        {elib_dt, [
            {'utc', 1, fun(_Unit) -> 10000 end}
        ]}
    ], fun() ->
        Scene = <<"attachment">>,
        Tk = <<"valid_asset_token">>,
        V = 10000,
        Path = <<"/files/image.jpg">>,

        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"ok">>, Result)
    end).

verify_for_assets_with_undefined_scene_returns_fail_test_() ->
    ?TEST_SIMPLE(fun() ->
        Scene = undefined,
        Tk = <<"valid_token">>,
        V = 10000,
        Path = <<"/files/image.jpg">>,

        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_assets_with_undefined_tk_returns_fail_test_() ->
    ?TEST_SIMPLE(fun() ->
        Scene = <<"attachment">>,
        Tk = undefined,
        V = 10000,
        Path = <<"/files/image.jpg">>,

        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_assets_with_error_v_returns_fail_test_() ->
    ?TEST_SIMPLE(fun() ->
        Scene = <<"attachment">>,
        Tk = <<"valid_token">>,
        V = error,
        Path = <<"/files/image.jpg">>,

        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_assets_with_expired_token_returns_fail_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'get_token', 3, fun(assets, scene, _V) -> <<"valid_asset_token">> end}
        ]},
        {elib_dt, [
            {'utc', 1, fun(_Unit) -> 20000 end}
        ]}
    ], fun() ->
        Scene = <<"attachment">>,
        Tk = <<"valid_asset_token">>,
        V = 10000,
        Path = <<"/files/image.jpg">>,

        % 当前时间 20000，V + 7200 = 17200，20000 > 17200，token 已过期
        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_assets_with_invalid_token_returns_fail_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'get_token', 3, fun(assets, scene, _V) -> <<"different_token">> end}
        ]},
        {elib_dt, [
            {'utc', 1, fun(_Unit) -> 10000 end}
        ]}
    ], fun() ->
        Scene = <<"attachment">>,
        Tk = <<"invalid_token">>,
        V = 10000,
        Path = <<"/files/image.jpg">>,

        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_assets_with_binary_v_in_range_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'get_token', 3, fun(assets, scene, _V) -> <<"valid_asset_token">> end}
        ]},
        {elib_dt, [
            {'utc', 1, fun(_Unit) -> 10000 end}
        ]}
    ], fun() ->
        Scene = <<"attachment">>,
        Tk = <<"valid_asset_token">>,
        V = <<"10000">>,
        Path = <<"/files/image.jpg">>,

        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"ok">>, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

verify_for_open_with_empty_path_test_() ->
    ?TEST_SIMPLE(fun() ->
        Path = <<>>,
        Tk = <<"valid_token">>,
        Val = <<"1234567890">>,

        Result = auth_logic:verify_for_open(Path, Tk, Val),
        % 空路径应该能处理（虽然可能返回 fail）
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_assets_with_empty_scene_test_() ->
    ?TEST_SIMPLE(fun() ->
        Scene = <<>>,
        Tk = <<"valid_token">>,
        V = 10000,
        Path = <<"/files/image.jpg">>,

        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_assets_with_empty_token_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'get_token', 3, fun(assets, scene, _V) -> <<"stored_token">> end}
        ]},
        {elib_dt, [
            {'utc', 1, fun(_Unit) -> 10000 end}
        ]}
    ], fun() ->
        Scene = <<"attachment">>,
        Tk = <<>>,
        V = 10000,
        Path = <<"/files/image.jpg">>,

        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

%% ===================================================================
%% 时间窗口测试
%% ===================================================================

verify_for_assets_within_time_window_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'get_token', 3, fun(assets, scene, _V) -> <<"valid_asset_token">> end}
        ]},
        {elib_dt, [
            {'utc', 1, fun(_Unit) -> 10000 end}
        ]}
    ], fun() ->
        Scene = <<"attachment">>,
        Tk = <<"valid_asset_token">>,
        V = 5000,
        Path = <<"/files/image.jpg">>,

        % V + 7200 = 12200，当前时间 10000 < 12200，在有效期内
        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"ok">>, Result)
    end).

verify_for_assets_exceeds_time_window_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'get_token', 3, fun(assets, scene, _V) -> <<"valid_asset_token">> end}
        ]},
        {elib_dt, [
            {'utc', 1, fun(_Unit) -> 20000 end}
        ]}
    ], fun() ->
        Scene = <<"attachment">>,
        Tk = <<"valid_asset_token">>,
        V = 5000,
        Path = <<"/files/image.jpg">>,

        % V + 7200 = 12200，当前时间 20000 > 12200，超出有效期
        Result = auth_logic:verify_for_assets(Scene, Tk, V, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

%% ===================================================================
%% Token 匹配测试
%% ===================================================================

verify_for_open_token_match_test_() ->
    ?WITH_MECK(auth_ds, [
        {'get_token', 3, fun(assets, <<"open">>, _V) -> <<"expected_token">> end}
    ], fun() ->
        Path = <<"/resources/file123?expected_token">>,
        Tk = <<"expected_token">>,
        Val = <<"1234567890">>,

        Result = auth_logic:verify_for_open(Path, Tk, Val),
        ?assertEqual(<<"ok">>, Result)
    end).

verify_for_open_token_mismatch_test_() ->
    ?WITH_MECK(auth_ds, [
        {'get_token', 3, fun(assets, <<"open">>, _V) -> <<"stored_token">> end}
    ], fun() ->
        Path = <<"/resources/file123?different_token">>,
        Tk = <<"different_token">>,
        Val = <<"1234567890">>,

        Result = auth_logic:verify_for_open(Path, Tk, Val),
        ?assertEqual(<<"fail">>, Result)
    end).
