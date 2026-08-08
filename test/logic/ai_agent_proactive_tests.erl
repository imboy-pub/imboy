-module(ai_agent_proactive_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc ai_agent_proactive EUnit 测试（AI 冷启动 M1）
%%% 覆盖：send_text 投递骨架（payload 双键/空文本短路）；
%%%       send_welcome 模板路（默认，零 LLM 成本）；
%%%       send_welcome LLM 路（rate limiter 闸门 + chat + 失败/限流回退模板）。
%%%===================================================================

%% 让 elib_async:async 同步执行闭包，便于断言副作用（本模块未用，备用）
-define(SYNC_ASYNC,
    {elib_async, [
        {'async', 1, fun(F) ->
            F(),
            ok
        end}
    ]}
).

%% send_text 持久化侧（离线可达）：stage 写 staging + enqueue 触发 worker。
%% eunit 无 msg_store gen_server 且 msg_store TSID 生成器未注册，统一 meck。
-define(MSG_STORE_MECK,
    {msg_store_ds, [
        {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
        {'enqueue', 3, fun(_, _, _) -> ok end}
    ]}
).

%% ===================================================================
%% send_text/3：投递骨架
%% ===================================================================

send_text_delivers_c2c_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
            {elib_retry_config, [{'intervals', 1, fun(<<"c2c">>) -> [0, 3000] end}]},
            {message_ds, [{'send_next', 4, fun(_ToUid, _MsgId, _Json, _MsLi) -> ok end}]},
            ?MSG_STORE_MECK
        ],
        fun() ->
            ?assertEqual(ok, ai_agent_proactive:send_text(42, 7, <<"你好"/utf8>>)),
            %% 投递给 human(7)，MsgId 为 tsid 字符串，重试节奏取 c2c 档
            ?assert(meck:called(message_ds, send_next, [7, <<"8888">>, '_', [0, 3000]])),
            [{_, {_, send_next, [7, _, Json, _]}, _} | _] = meck:history(message_ds),
            Msg = jsone:decode(Json),
            %% C2C 帧：agent(42) → human(7)
            ?assertEqual(<<"C2C">>, maps:get(<<"type">>, Msg)),
            ?assertEqual(<<"42">>, maps:get(<<"from">>, Msg)),
            ?assertEqual(<<"7">>, maps:get(<<"to">>, Msg)),
            ?assertEqual(<<"text">>, maps:get(<<"msg_type">>, Msg)),
            Payload = maps:get(<<"payload">>, Msg),
            %% text/content 双键兼容不同渲染消费者
            ?assertEqual(<<"你好"/utf8>>, maps:get(<<"text">>, Payload)),
            ?assertEqual(<<"你好"/utf8>>, maps:get(<<"content">>, Payload))
        end
    ).

send_text_empty_text_no_op_test_() ->
    ?WITH_MECKS(
        [{message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]}],
        fun() ->
            ?assertEqual(ok, ai_agent_proactive:send_text(42, 7, <<>>)),
            ?assertNot(meck:called(message_ds, send_next, '_'))
        end
    ).

%% ===================================================================
%% send_welcome/4：模板路（默认，零 LLM）
%% ===================================================================

send_welcome_template_path_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
            {elib_retry_config, [{'intervals', 1, fun(_) -> [0] end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            {imboy_llm_registry, [{'lookup', 1, fun(_) -> {ok, #{module => x, opts => #{}}} end}]},
            ?MSG_STORE_MECK
        ],
        fun() ->
            Cfg = #{
                welcome_template => <<"嗨 {{nickname}}，我是 AI 助手"/utf8>>,
                welcome_llm_enabled => false
            },
            ?assertEqual(ok, ai_agent_proactive:send_welcome(42, 7, <<"小明"/utf8>>, Cfg)),
            ?assert(meck:called(message_ds, send_next, [7, '_', '_', '_'])),
            [{_, {_, send_next, [7, _, Json, _]}, _} | _] = meck:history(message_ds),
            Msg = jsone:decode(Json),
            Payload = maps:get(<<"payload">>, Msg),
            %% {{nickname}} 占位替换生效
            ?assertEqual(<<"嗨 小明，我是 AI 助手"/utf8>>, maps:get(<<"content">>, Payload)),
            %% 模板路零 LLM
            ?assertNot(meck:called(imboy_llm_registry, lookup, '_'))
        end
    ).

send_welcome_template_default_when_missing_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
            {elib_retry_config, [{'intervals', 1, fun(_) -> [0] end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?MSG_STORE_MECK
        ],
        fun() ->
            %% Cfg 无 template 键 → 用内置默认文案
            ?assertEqual(ok, ai_agent_proactive:send_welcome(42, 7, <<>>, #{})),
            [{_, {_, send_next, [7, _, Json, _]}, _} | _] = meck:history(message_ds),
            Msg = jsone:decode(Json),
            Payload = maps:get(<<"payload">>, Msg),
            Content = maps:get(<<"content">>, Payload),
            %% 默认文案非空且不含未替换占位符；空昵称用兜底称呼
            ?assert(byte_size(Content) > 0),
            ?assertEqual(nomatch, binary:match(Content, <<"{{nickname}}">>))
        end
    ).

send_welcome_disabled_agent_does_not_deliver_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_ds, [
                {'is_agent', 1, fun(42) -> false end}
            ]},
            {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
            {elib_retry_config, [{'intervals', 1, fun(_) -> [0] end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?MSG_STORE_MECK
        ],
        fun() ->
            ?assertEqual(ok, ai_agent_proactive:send_welcome(42, 7, <<"小明">>, #{})),
            ?assertNot(meck:called(message_ds, send_next, '_'))
        end
    ).

%% ===================================================================
%% send_welcome/4：LLM 路（闸门 + 回退）
%% ===================================================================

send_welcome_llm_path_test_() ->
    ?WITH_MECKS(
        [
            {agent_rate_limiter, [{'allow', 2, fun(_, _) -> allow end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(42) ->
                    {true, #{
                        <<"provider">> => <<"openai">>,
                        <<"system_prompt">> => <<"你是助手"/utf8>>
                    }}
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(<<"openai">>) ->
                    {ok, #{module => imboy_llm_openai, opts => #{}}}
                end}
            ]},
            {imboy_llm_openai, [
                {'chat', 3, fun(42, _Msgs, _Opts) ->
                    {ok, #{<<"result">> => <<"欢迎新朋友！"/utf8>>}}
                end}
            ]},
            {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
            {elib_retry_config, [{'intervals', 1, fun(_) -> [0] end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?MSG_STORE_MECK
        ],
        fun() ->
            Cfg = #{welcome_llm_enabled => true},
            ?assertEqual(ok, ai_agent_proactive:send_welcome(42, 7, <<"小明"/utf8>>, Cfg)),
            %% 限流闸门在 LLM 之前，以 AgentUid 为 Scope、ToUid 为 FromUid
            ?assert(meck:called(agent_rate_limiter, allow, [42, 7])),
            ?assert(meck:called(imboy_llm_openai, chat, '_')),
            [{_, {_, send_next, [7, _, Json, _]}, _} | _] = meck:history(message_ds),
            Msg = jsone:decode(Json),
            Payload = maps:get(<<"payload">>, Msg),
            ?assertEqual(<<"欢迎新朋友！"/utf8>>, maps:get(<<"content">>, Payload))
        end
    ).

send_welcome_llm_error_falls_back_to_template_test_() ->
    ?WITH_MECKS(
        [
            {agent_rate_limiter, [{'allow', 2, fun(_, _) -> allow end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(42) -> {true, #{<<"provider">> => <<"openai">>}} end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(_) -> {ok, #{module => imboy_llm_openai, opts => #{}}} end}
            ]},
            {imboy_llm_openai, [{'chat', 3, fun(_, _, _) -> {error, timeout} end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]},
            {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
            {elib_retry_config, [{'intervals', 1, fun(_) -> [0] end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?MSG_STORE_MECK
        ],
        fun() ->
            Cfg = #{
                welcome_llm_enabled => true,
                welcome_template => <<"模板兜底 {{nickname}}"/utf8>>
            },
            ?assertEqual(ok, ai_agent_proactive:send_welcome(42, 7, <<"小明"/utf8>>, Cfg)),
            %% LLM 失败仍投递：回退模板文案（欢迎是产品功能，须可达）
            [{_, {_, send_next, [7, _, Json, _]}, _} | _] = meck:history(message_ds),
            Msg = jsone:decode(Json),
            Payload = maps:get(<<"payload">>, Msg),
            ?assertEqual(<<"模板兜底 小明"/utf8>>, maps:get(<<"content">>, Payload))
        end
    ).

send_welcome_rate_limited_falls_back_to_template_test_() ->
    ?WITH_MECKS(
        [
            {agent_rate_limiter, [{'allow', 2, fun(_, _) -> {deny, agent_rate} end}]},
            {imboy_llm_registry, [{'lookup', 1, fun(_) -> {ok, #{module => x, opts => #{}}} end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]},
            {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
            {elib_retry_config, [{'intervals', 1, fun(_) -> [0] end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?MSG_STORE_MECK
        ],
        fun() ->
            Cfg = #{welcome_llm_enabled => true},
            ?assertEqual(ok, ai_agent_proactive:send_welcome(42, 7, <<"小明"/utf8>>, Cfg)),
            %% 限流 deny：零 LLM 调用（成本闸住），回退模板（功能可达）
            ?assertNot(meck:called(imboy_llm_registry, lookup, '_')),
            ?assert(meck:called(message_ds, send_next, [7, '_', '_', '_']))
        end
    ).

%% ===================================================================
%% send_text/3：部署级 E2EE 明文拒收门
%%
%% 本模块直写 msg_store_ds:stage/enqueue，不经 msg_c2c_logic:stage_and_send_c2c，
%% 所以必须自带同款门。只桩 config_ds（部署配置边界），imboy_policy 判定真实执行。
%% ===================================================================

policy_config_meck(Caps) ->
    {config_ds, [
        {'get', 2, fun(_Key, Default) -> Default end},
        {'env', 2, fun
            (product_profile, community) -> community;
            (capabilities, #{}) -> Caps;
            (_Key, Default) -> Default
        end}
    ]}.

send_text_gate_mecks(Caps) ->
    [
        policy_config_meck(Caps),
        {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
        {elib_retry_config, [{'intervals', 1, fun(<<"c2c">>) -> [0, 3000] end}]},
        {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
        ?MSG_STORE_MECK
    ].

%% agent 无设备私钥、只能发明文；required 部署下必须拒发，
%% 不得把明文写进 staging/msg_c2c，也不得实时推送。
send_text_blocked_when_e2ee_required_test_() ->
    ?WITH_MECKS(
        send_text_gate_mecks(#{e2ee_mode => required}),
        fun() ->
            %% 对调用方仍是恒 ok（fire-and-forget 语义不变）
            ?assertEqual(ok, ai_agent_proactive:send_text(42, 7, <<"你好"/utf8>>)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 10)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3)),
            ?assertEqual(0, meck:num_calls(message_ds, send_next, 4))
        end
    ).

%% storage_mode=secure_e2ee 同样拒发
send_text_blocked_when_storage_mode_secure_e2ee_test_() ->
    ?WITH_MECKS(
        send_text_gate_mecks(#{storage_mode => secure_e2ee}),
        fun() ->
            ?assertEqual(ok, ai_agent_proactive:send_text(42, 7, <<"你好"/utf8>>)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 10)),
            ?assertEqual(0, meck:num_calls(message_ds, send_next, 4))
        end
    ).

%% 【防误伤】明文部署（community 档 e2ee_mode=optional）下欢迎消息照常发出
send_text_allowed_when_deployment_does_not_require_e2ee_test_() ->
    ?WITH_MECKS(
        send_text_gate_mecks(#{}),
        fun() ->
            ?assertEqual(ok, ai_agent_proactive:send_text(42, 7, <<"你好"/utf8>>)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 10)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, enqueue, 3)),
            ?assertEqual(1, meck:num_calls(message_ds, send_next, 4))
        end
    ).

%% 【防误伤】e2ee_mode=disabled（enterprise 档形态）同样放行
send_text_allowed_when_e2ee_disabled_test_() ->
    ?WITH_MECKS(
        send_text_gate_mecks(#{storage_mode => archived, e2ee_mode => disabled}),
        fun() ->
            ?assertEqual(ok, ai_agent_proactive:send_text(42, 7, <<"你好"/utf8>>)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 10))
        end
    ).

%% send_welcome 走 send_text，required 部署下整条欢迎链一并拒发（不留旁路）
send_welcome_blocked_when_e2ee_required_test_() ->
    ?WITH_MECKS(
        send_text_gate_mecks(#{e2ee_mode => required}),
        fun() ->
            ?assertEqual(ok, ai_agent_proactive:send_welcome(42, 7, <<"小明"/utf8>>, #{})),
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 10)),
            ?assertEqual(0, meck:num_calls(message_ds, send_next, 4))
        end
    ).
