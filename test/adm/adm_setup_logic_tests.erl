-module(adm_setup_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% adm_setup_logic 模块的 EUnit 测试（P0-5 首启初始化向导）
%%%
%%% 目标：验证首启初始化检测与超级管理员创建流程
%%% 覆盖：
%%%   - is_initialized/0: 空库 / 已置 flag / legacy (有用户无 flag)
%%%   - do_init/1:        正常路径 / 已完成 / 弱密码 / 竞态
%%%
%%% 策略：RED 阶段 — 运行 `make eunit` 期望编译期报 undef
%%%       (adm_setup_logic 尚未实现)。批次 2 实现后应全绿。
%%%===================================================================

%% ===================================================================
%% is_initialized/0 测试
%% ===================================================================

%% @doc 空库且无 flag：视为未初始化
is_initialized_empty_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'count', 0, fun() -> {ok, 0} end}
        ]},
        {config_ds, [
            {'get', 1, fun(<<"adm.setup.completed_at">>) -> <<>> end}
        ]}
    ], fun() ->
        ?assertEqual(false, adm_setup_logic:is_initialized())
    end).

%% @doc 已写入 flag：视为已初始化
is_initialized_with_flag_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'count', 0, fun() -> {ok, 1} end}
        ]},
        {config_ds, [
            {'get', 1, fun(<<"adm.setup.completed_at">>) -> <<"2026-04-10T12:00:00Z">> end}
        ]}
    ], fun() ->
        ?assertEqual(true, adm_setup_logic:is_initialized())
    end).

%% @doc Legacy：库里已有 adm_user 但无 flag（升级场景）
%%      仍视为已初始化，避免向后兼容问题导致重置风险
is_initialized_legacy_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'count', 0, fun() -> {ok, 3} end}
        ]},
        {config_ds, [
            {'get', 1, fun(<<"adm.setup.completed_at">>) -> <<>> end}
        ]}
    ], fun() ->
        ?assertEqual(true, adm_setup_logic:is_initialized())
    end).

%% ===================================================================
%% do_init/1 测试
%% ===================================================================

%% @doc 正常路径：空库 + 合法手机号 + 强密码 → 创建管理员 + 写 flag
do_init_happy_path_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'count', 0, fun() -> {ok, 0} end},
            {'save', 1, fun(_Row) -> {ok, 1} end}
        ]},
        {config_ds, [
            {'get', 1, fun(<<"adm.setup.completed_at">>) -> <<>> end},
            {'set', 2, fun(<<"adm.setup.completed_at">>, _Ts) -> ok end}
        ]},
        {elib_type, [
            {'is_mobile', 1, fun(<<"13800138000">>) -> true end},
            {'is_email', 1, fun(_) -> false end}
        ]}
    ], fun() ->
        Params = #{
            <<"account">> => <<"13800138000">>,
            <<"password">> => <<"Admin@12345">>,
            <<"nickname">> => <<"超级管理员"/utf8>>
        },
        Result = adm_setup_logic:do_init(Params),
        ?assertMatch({ok, _}, Result)
    end).

%% @doc 已完成初始化后再次调用 → 拒绝
do_init_already_done_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'count', 0, fun() -> {ok, 1} end}
        ]},
        {config_ds, [
            {'get', 1, fun(<<"adm.setup.completed_at">>) -> <<"2026-04-10T12:00:00Z">> end}
        ]}
    ], fun() ->
        Params = #{
            <<"account">> => <<"13800138000">>,
            <<"password">> => <<"Admin@12345">>,
            <<"nickname">> => <<"超级管理员"/utf8>>
        },
        Result = adm_setup_logic:do_init(Params),
        ?assertEqual({error, ?ERR_SETUP_ALREADY_COMPLETED}, Result)
    end).

%% @doc 弱密码：纯数字 / 长度不足 → 拒绝
do_init_weak_password_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'count', 0, fun() -> {ok, 0} end}
        ]},
        {config_ds, [
            {'get', 1, fun(<<"adm.setup.completed_at">>) -> <<>> end}
        ]},
        {elib_type, [
            {'is_mobile', 1, fun(<<"13800138000">>) -> true end},
            {'is_email', 1, fun(_) -> false end}
        ]}
    ], fun() ->
        ParamsShort = #{
            <<"account">> => <<"13800138000">>,
            <<"password">> => <<"abc12">>,
            <<"nickname">> => <<"Admin"/utf8>>
        },
        ?assertEqual({error, ?ERR_SETUP_INVALID_PARAMS},
                     adm_setup_logic:do_init(ParamsShort)),

        ParamsDigits = #{
            <<"account">> => <<"13800138000">>,
            <<"password">> => <<"12345678">>,
            <<"nickname">> => <<"Admin"/utf8>>
        },
        ?assertEqual({error, ?ERR_SETUP_INVALID_PARAMS},
                     adm_setup_logic:do_init(ParamsDigits))
    end).

%% @doc 无效账号：非手机号非邮箱 → 拒绝
do_init_invalid_account_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'count', 0, fun() -> {ok, 0} end}
        ]},
        {config_ds, [
            {'get', 1, fun(<<"adm.setup.completed_at">>) -> <<>> end}
        ]},
        {elib_type, [
            {'is_mobile', 1, fun(_) -> false end},
            {'is_email', 1, fun(_) -> false end}
        ]}
    ], fun() ->
        Params = #{
            <<"account">> => <<"not-a-valid-account">>,
            <<"password">> => <<"Admin@12345">>,
            <<"nickname">> => <<"Admin"/utf8>>
        },
        ?assertEqual({error, ?ERR_SETUP_INVALID_PARAMS},
                     adm_setup_logic:do_init(Params))
    end).

%% @doc 竞态：第一次检查通过，但保存前 count 又变 → 仍需幂等拒绝
%%      （实现通过写后再次确认 flag 或 unique 约束兜底）
do_init_race_condition_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'count', 0, fun() -> {ok, 0} end},
            {'save', 1, fun(_Row) ->
                {error, {conflict, <<"duplicate account">>}}
            end}
        ]},
        {config_ds, [
            {'get', 1, fun(<<"adm.setup.completed_at">>) -> <<>> end}
        ]},
        {elib_type, [
            {'is_mobile', 1, fun(_) -> true end},
            {'is_email', 1, fun(_) -> false end}
        ]}
    ], fun() ->
        Params = #{
            <<"account">> => <<"13800138000">>,
            <<"password">> => <<"Admin@12345">>,
            <<"nickname">> => <<"Admin"/utf8>>
        },
        Result = adm_setup_logic:do_init(Params),
        ?assertMatch({error, _}, Result)
    end).
