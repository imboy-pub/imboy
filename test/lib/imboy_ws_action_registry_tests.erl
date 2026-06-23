-module(imboy_ws_action_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% WS action 注册表测试（数据驱动路由的基石）
%% 验证：内置 action 注册、查表、动态注册（插件扩展）、unknown 兜底。

%% @doc 确保注册表 gen_server 已启动（容忍已在运行）
ensure_started() ->
    case imboy_ws_action_registry:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

%% @doc init_builtin 后内置 C2C/C2G action 可查
builtin_actions_test_() ->
    ?TEST_SIMPLE(fun() ->
        ensure_started(),
        ok = imboy_ws_action_registry:init_builtin(),
        %% C2C 内置 action
        ?assertEqual(
            {ok, {msg_c2c_logic, c2c_revoke}},
            imboy_ws_action_registry:lookup(<<"c2c">>, <<"message_revoke">>)
        ),
        ?assertEqual(
            {ok, {msg_c2c_logic, c2c_edit_ack}},
            imboy_ws_action_registry:lookup(<<"c2c">>, <<"message_edit_ack">>)
        ),
        ?assertEqual(
            {ok, {msg_c2c_logic, c2c_read}},
            imboy_ws_action_registry:lookup(<<"c2c">>, <<"message_read">>)
        ),
        %% C2G 内置 action
        ?assertEqual(
            {ok, {msg_c2g_logic, c2g_revoke}},
            imboy_ws_action_registry:lookup(<<"c2g">>, <<"message_revoke">>)
        ),
        ?assertEqual(
            {ok, {msg_c2g_logic, c2g_edit}},
            imboy_ws_action_registry:lookup(<<"c2g">>, <<"message_edit">>)
        )
    end).

%% @doc 未注册的 action 返回 undefined
unknown_action_test_() ->
    ?TEST_SIMPLE(fun() ->
        ensure_started(),
        ok = imboy_ws_action_registry:init_builtin(),
        ?assertEqual(
            undefined,
            imboy_ws_action_registry:lookup(<<"c2c">>, <<"no_such_action_xyz">>)
        ),
        ?assertEqual(
            undefined,
            imboy_ws_action_registry:lookup(<<"c2x">>, <<"message_revoke">>)
        )
    end).

%% @doc 动态 register（插件扩展场景）后可查，且不影响内置
dynamic_register_test_() ->
    ?TEST_SIMPLE(fun() ->
        ensure_started(),
        ok = imboy_ws_action_registry:init_builtin(),
        ok = imboy_ws_action_registry:register(
            <<"c2c">>, <<"plugin_custom_action">>, {my_plugin_logic, handle}
        ),
        ?assertEqual(
            {ok, {my_plugin_logic, handle}},
            imboy_ws_action_registry:lookup(<<"c2c">>, <<"plugin_custom_action">>)
        ),
        %% 内置 action 仍在
        ?assertEqual(
            {ok, {msg_c2c_logic, c2c_revoke}},
            imboy_ws_action_registry:lookup(<<"c2c">>, <<"message_revoke">>)
        )
    end).

%% @doc unregister 后查不到
unregister_test_() ->
    ?TEST_SIMPLE(fun() ->
        ensure_started(),
        ok = imboy_ws_action_registry:init_builtin(),
        ok = imboy_ws_action_registry:register(
            <<"c2c">>, <<"temp_action">>, {temp_mod, temp_fn}
        ),
        ?assertEqual(
            {ok, {temp_mod, temp_fn}},
            imboy_ws_action_registry:lookup(<<"c2c">>, <<"temp_action">>)
        ),
        ok = imboy_ws_action_registry:unregister(<<"c2c">>, <<"temp_action">>),
        ?assertEqual(
            undefined,
            imboy_ws_action_registry:lookup(<<"c2c">>, <<"temp_action">>)
        )
    end).
