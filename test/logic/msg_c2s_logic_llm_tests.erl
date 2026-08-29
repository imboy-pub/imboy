-module(msg_c2s_logic_llm_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc msg_c2s_logic LLM 分派「架构迁移守卫」（CI-00 重写）
%%%
%%% 背景：437601b0（2026-08-21 Discovery & Agent/Bot 架构 Phase 1-7）将
%%% msg_c2s_logic 的 llm_callback/2、llm_callback/4 与 bot_qian_fan 注册表
%%% 分派整体移除（bot_* 前缀废弃 → bot_prefix_deprecated 引导，Agent 走
%%% C2C），provider 解析收敛到 imboy_llm_registry:lookup/1，外呼编排迁至
%%% ai_agent_reply（maybe_dispatch/run_and_reply，复用 c2s_to_external/5
%%% 的 ApiCallback 注入骨架）。原 7 个用例测的是已删除内部函数（undef），
%%% 其行为语义已由重构时同步更新的 msg_c2s_logic_tests（c2s_unsupported
%%% 等分派用例）、bot_logic_tests、agent_rate_limiter_tests、
%%% llm_stream_tests 覆盖。
%%%
%%% 本模块保留为迁移守卫：锁定现行为契约 + 旧路径不复活——
%%% 防止有人把 provider 硬编码或 llm_callback 桥接加回 msg_c2s_logic
%%% 形成双路径。
%%%===================================================================

%% ===================================================================
%% 旧路径守卫：llm_callback/2、/4 不得复活在 msg_c2s_logic
%% ===================================================================

llm_callback_bridge_not_resurrected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertNot(erlang:function_exported(msg_c2s_logic, llm_callback, 2)),
        ?assertNot(erlang:function_exported(msg_c2s_logic, llm_callback, 4))
    end).

%% ===================================================================
%% c2s/3 现行为契约
%% ===================================================================

c2s_bot_prefix_deprecated_test_() ->
    %% bot_* 前缀已废弃（migrations/00000071_bot_prefix_to_agent）：
    %% 返回 bot_prefix_deprecated 引导，而非注册表分派。
    ?TEST_SIMPLE(fun() ->
        Data = #{
            <<"to">> => <<"bot_qian_fan">>,
            <<"payload">> => #{<<"text">> => <<"你好"/utf8>>},
            <<"created_at">> => 1751990400000
        },
        {reply, Resp} = msg_c2s_logic:c2s(<<"mid1">>, 7, Data),
        ?assertEqual(<<"bot_prefix_deprecated">>, maps:get(<<"action">>, Resp)),
        ?assertEqual(<<"mid1">>, maps:get(<<"id">>, Resp))
    end).

c2s_unsupported_for_unknown_to_test_() ->
    %% 非 sync、非 bot_* 的 to：c2s_unsupported 降级（message_ds 组装 S2C）。
    ?WITH_MECKS(
        [
            {message_ds, [
                {'assemble_s2c', 3, fun(_MsgId, Code, _To) -> #{<<"code">> => Code} end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"external_unknown">>,
                <<"payload">> => #{<<"text">> => <<"hi">>},
                <<"created_at">> => 1751990400000
            },
            {reply, Resp} = msg_c2s_logic:c2s(<<"mid2">>, 7, Data),
            ?assertEqual(<<"c2s_unsupported">>, maps:get(<<"code">>, Resp))
        end
    ).

%% ===================================================================
%% 新架构入口守卫：回调注入契约与 provider 解析都在
%% ===================================================================

c2s_to_external_api_callback_entry_exists_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% c2s_to_external/5：ApiCallback 由调用方注入（BYO-LLM 适配层契约）
        ?assert(erlang:function_exported(msg_c2s_logic, c2s_to_external, 5))
    end).

llm_registry_lookup_contract_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% lookup/1 返回 {ok, #{module => M, opts => Opts}}（chat/3 契约）；
        %% qianfan 为内置 provider（向后兼容名保留）。
        {ok, #{module := Mod}} = imboy_llm_registry:lookup(<<"qianfan">>),
        ?assertEqual(imboy_llm_qianfan, Mod),
        %% 未注册名称返回 undefined（由上层降级处理）
        ?assertEqual(undefined, imboy_llm_registry:lookup(<<"no_such_provider_ci00">>))
    end).

ai_agent_reply_dispatch_entry_exists_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% Agent/Bot C2S 外呼编排入口（maybe_dispatch/3 + run_and_reply/5）；
        %% function_exported 只查已加载模块，先 ensure_loaded。
        {module, ai_agent_reply} = code:ensure_loaded(ai_agent_reply),
        ?assert(erlang:function_exported(ai_agent_reply, maybe_dispatch, 3)),
        ?assert(erlang:function_exported(ai_agent_reply, run_and_reply, 5))
    end).
