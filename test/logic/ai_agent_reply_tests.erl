-module(ai_agent_reply_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc ai_agent_reply EUnit 测试（Phase 1 T1.4）
%%% 覆盖：agent DM → LLM → 回投 agent→human；E2EE 红线跳过；
%%%       非 agent/非文本无副作用；provider 缺失/LLM 失败不投递。
%%%===================================================================

%% 让 elib_async:async 同步执行闭包，便于断言副作用
-define(SYNC_ASYNC,
    {elib_async, [
        {'async', 1, fun(F) ->
            F(),
            ok
        end}
    ]}
).

%% B-30：回复必须先 stage/enqueue 落库再推在线设备，故这两个边界要打桩。
%% 不打桩会穿透打真库 —— 这三条用例此前正是因此变红。
-define(STAGE_STUB,
    {msg_store_ds, [
        {'stage', 10, fun(_T, _Id, _MT, _E, _E2, _P, _From, _To, _C, _S) -> ok end},
        {'enqueue', 3, fun(_T, _Id, _Meta) -> ok end}
    ]}
).

%% E2EE 部署级门放行（本模块只测投递编排，门本身有独立测试）
-define(POLICY_OK,
    {imboy_policy, [
        {'validate_message_write', 5, fun(_T, _MT, _E, _E2, _P) -> ok end}
    ]}
).

%% 限流器放行（隔离 agent_rate_limiter，本模块只测 dispatch 编排，不测限流内部）
-define(ALLOW_RL,
    {agent_rate_limiter, [
        {'allow', 2, fun(_Scope, _FromUid) -> allow end}
    ]}
).

%% ===================================================================
%% Happy path：agent 私聊 → LLM → 回投
%% ===================================================================

agent_dm_triggers_llm_and_delivers_reply_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
            ?ALLOW_RL,
            {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(42) ->
                    {true, #{
                        <<"provider">> => <<"openai">>,
                        <<"model">> => <<"deepseek-chat">>,
                        <<"system_prompt">> => <<"你是助手"/utf8>>
                    }}
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(<<"openai">>) ->
                    {ok, #{module => imboy_llm_openai, opts => #{base_url => <<"u">>}}}
                end}
            ]},
            {imboy_llm_openai, [
                {'chat', 3, fun(42, _Messages, _Opts) ->
                    {ok, #{<<"result">> => <<"你好，我能帮你"/utf8>>}}
                end}
            ]},
            {message_ds, [
                {'send_next', 4, fun(_ToUid, _MsgId, _Json, _MsLi) -> ok end}
            ]},
            ?STAGE_STUB,
            ?POLICY_OK
        ],
        fun() ->
            Data = #{
                <<"payload">> => #{<<"content">> => <<"在吗"/utf8>>},
                <<"msg_type">> => <<"text">>
            },
            ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
            %% 回投给 human(7)，且 provider chat 被调
            ?assert(meck:called(imboy_llm_openai, chat, '_')),
            ?assert(meck:called(message_ds, send_next, [7, '_', '_', '_'])),
            %% system_prompt 注入为开场 + model 覆盖透传
            [{_, {_, chat, [42, Messages, Opts]}, _} | _] =
                meck:history(imboy_llm_openai),
            ?assertMatch([#{<<"role">> := <<"system">>} | _], Messages),
            ?assertEqual(<<"deepseek-chat">>, maps:get(model, Opts))
        end
    ).

%% ===================================================================
%% 金钱 DoS 闸门：限流 deny 时不触发 LLM、不投递，仍恒返回 ok
%% ===================================================================

rate_limited_denies_no_llm_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
            {agent_rate_limiter, [{'allow', 2, fun(_, _) -> {deny, requester_rate} end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(42) -> {true, #{<<"provider">> => <<"openai">>}} end}
            ]},
            {imboy_llm_registry, [{'lookup', 1, fun(_) -> {ok, #{module => x, opts => #{}}} end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?STAGE_STUB,
            ?POLICY_OK
        ],
        fun() ->
            Data = #{
                <<"payload">> => #{<<"content">> => <<"在吗"/utf8>>},
                <<"msg_type">> => <<"text">>
            },
            ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
            %% 限流拦截在 LLM 之前：registry/lookup 与投递均不发生
            ?assertNot(meck:called(imboy_llm_registry, lookup, '_')),
            ?assertNot(meck:called(message_ds, send_next, '_'))
        end
    ).

%% ===================================================================
%% E2EE 红线：绝不触发
%% ===================================================================

e2ee_message_never_triggers_test_() ->
    ?WITH_MECKS(
        [{ai_agent_ds, [{'is_agent', 1, fun(_) -> {true, #{}} end}]}],
        fun() ->
            Data = #{
                <<"msg_type">> => <<"e2ee">>,
                <<"e2ee">> => #{<<"e2ee">> => true},
                <<"payload">> => #{<<"content">> => <<"ciphertext">>}
            },
            ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
            %% E2EE 短路在 is_agent 之前
            ?assertNot(meck:called(ai_agent_ds, is_agent, '_'))
        end
    ).

%% ===================================================================
%% 非 agent / 非文本 无副作用
%% ===================================================================

non_agent_recipient_no_op_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_ds, [{'is_agent', 1, fun(_) -> false end}]},
            {imboy_llm_registry, [{'lookup', 1, fun(_) -> {ok, #{module => x, opts => #{}}} end}]}
        ],
        fun() ->
            Data = #{<<"payload">> => #{<<"content">> => <<"hi">>}},
            ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
            ?assertNot(meck:called(imboy_llm_registry, lookup, '_'))
        end
    ).

non_text_message_no_op_test_() ->
    ?WITH_MECKS(
        [{ai_agent_ds, [{'is_agent', 1, fun(_) -> {true, #{}} end}]}],
        fun() ->
            %% 图片消息 payload 无 content/text
            Data = #{
                <<"msg_type">> => <<"image">>,
                <<"payload">> => #{<<"url">> => <<"http://x/a.png">>}
            },
            ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
            ?assertNot(meck:called(ai_agent_ds, is_agent, '_'))
        end
    ).

%% ===================================================================
%% provider 缺失 / LLM 失败 → 不投递（不崩）
%% ===================================================================

provider_missing_no_delivery_test_() ->
    ?WITH_MECKS(
        [
            ?ALLOW_RL,
            {ai_agent_ds, [
                {'is_agent', 1, fun(_) -> {true, #{<<"provider">> => <<"ghost">>}} end}
            ]},
            {imboy_llm_registry, [{'lookup', 1, fun(<<"ghost">>) -> undefined end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?STAGE_STUB,
            ?POLICY_OK
        ],
        fun() ->
            Data = #{<<"payload">> => #{<<"content">> => <<"hi">>}},
            ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
            ?assertNot(meck:called(message_ds, send_next, '_'))
        end
    ).

llm_error_no_delivery_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
            ?ALLOW_RL,
            {ai_agent_ds, [
                {'is_agent', 1, fun(_) -> {true, #{<<"provider">> => <<"openai">>}} end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(_) -> {ok, #{module => imboy_llm_openai, opts => #{}}} end}
            ]},
            {imboy_llm_openai, [{'chat', 3, fun(_, _, _) -> {error, timeout} end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?STAGE_STUB,
            ?POLICY_OK
        ],
        fun() ->
            Data = #{<<"payload">> => #{<<"content">> => <<"hi">>}},
            ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
            ?assertNot(meck:called(message_ds, send_next, '_'))
        end
    ).

%% ===================================================================
%% 流式回复（Phase 2 T2.2）：llm_stream_enabled=true + provider.stream
%% ===================================================================

%% 成功流式：逐 delta 节流后 publish stream_delta 帧，结束用同 id 定稿落库
agent_dm_streams_deltas_then_finalizes_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
            ?ALLOW_RL,
            {elib_tsid, [{'generate', 0, fun() -> 12345 end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(42) -> {true, #{<<"provider">> => <<"openai">>}} end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(<<"openai">>) ->
                    {ok, #{module => imboy_llm_openai, opts => #{}}}
                end}
            ]},
            {imboy_llm_openai, [
                {'capabilities', 0, fun() ->
                    #{stream => true, vision => false, tools => false}
                end},
                {'chat_stream', 4, fun(42, _Msgs, _Opts, StreamFun) ->
                    %% "你好"(6B) 缓冲；+"世界"(6B)=12B 达阈值 flush 一帧；"！"(3B) 缓冲
                    StreamFun(<<"你好"/utf8>>),
                    StreamFun(<<"世界"/utf8>>),
                    StreamFun(<<"！"/utf8>>),
                    {ok, #{<<"result">> => <<"你好世界！"/utf8>>}}
                end}
            ]},
            {imboy_syn, [{'publish', 2, fun(_Uid, _Json) -> {ok, 1} end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?STAGE_STUB,
            ?POLICY_OK
        ],
        fun() ->
            application:set_env(imboy, llm_stream_enabled, true),
            try
                Data = #{
                    <<"payload">> => #{<<"content">> => <<"在吗"/utf8>>},
                    <<"msg_type">> => <<"text">>
                },
                ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
                %% 两帧 stream_delta：达阈值的 "你好世界" + 尾块 "！"
                Pubs = meck:history(imboy_syn),
                ?assertEqual(2, length(Pubs)),
                [{_, {_, publish, [7, Json0]}, _} | _] = Pubs,
                D0 = jsone:decode(Json0),
                ?assertEqual(<<"stream_delta">>, maps:get(<<"msg_type">>, D0)),
                ?assertEqual(<<"12345">>, maps:get(<<"id">>, D0)),
                P0 = maps:get(<<"payload">>, D0),
                ?assertEqual(<<"你好世界"/utf8>>, maps:get(<<"delta">>, P0)),
                ?assertEqual(0, maps:get(<<"index">>, P0)),
                ?assertEqual(<<"12345">>, maps:get(<<"stream_id">>, P0)),
                %% 定稿走 send_next，复用同一 StreamId（前端主键去重替换气泡）
                ?assert(meck:called(message_ds, send_next, [7, <<"12345">>, '_', '_']))
            after
                application:unset_env(imboy, llm_stream_enabled)
            end
        end
    ).

%% 流式失败：已累积部分作为定稿发出（同 id），前端流式气泡不会永不定稿
stream_error_finalizes_partial_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
            ?ALLOW_RL,
            {elib_tsid, [{'generate', 0, fun() -> 999 end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(42) -> {true, #{<<"provider">> => <<"openai">>}} end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(<<"openai">>) ->
                    {ok, #{module => imboy_llm_openai, opts => #{}}}
                end}
            ]},
            {imboy_llm_openai, [
                {'capabilities', 0, fun() ->
                    #{stream => true, vision => false, tools => false}
                end},
                {'chat_stream', 4, fun(42, _Msgs, _Opts, StreamFun) ->
                    %% "部分回复"(12B) 达阈值即 flush，随后 provider 报错
                    StreamFun(<<"部分回复"/utf8>>),
                    {error, timeout}
                end}
            ]},
            {imboy_syn, [{'publish', 2, fun(_, _) -> {ok, 1} end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?STAGE_STUB,
            ?POLICY_OK
        ],
        fun() ->
            application:set_env(imboy, llm_stream_enabled, true),
            try
                Data = #{
                    <<"payload">> => #{<<"content">> => <<"hi">>},
                    <<"msg_type">> => <<"text">>
                },
                ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
                %% 兜底定稿：已累积 "部分回复" 走 send_next，同一 id
                ?assert(meck:called(message_ds, send_next, [7, <<"999">>, '_', '_']))
            after
                application:unset_env(imboy, llm_stream_enabled)
            end
        end
    ).

%% ===================================================================
%% B-30：agent 的 C2C 回复必须落库，用户离线也不丢
%%
%% 此前 deliver_reply 只调 message_ds:send_next —— 那只推 imboy_syn 里的
%% **在线**设备，不写 staging/msg_c2c/archive。用户切后台或断网时回复永久丢失，
%% 离线 pull 拉不到、历史里也没有。演示能过、生产必挂。
%% ===================================================================

%% 判据本体：回复先 stage 再 enqueue，最后才推在线设备
reply_is_persisted_before_push_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
            ?ALLOW_RL,
            {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(42) ->
                    {true, #{<<"provider">> => <<"openai">>, <<"model">> => <<"m">>}}
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(<<"openai">>) ->
                    {ok, #{module => imboy_llm_openai, opts => #{}}}
                end}
            ]},
            {imboy_llm_openai, [
                {'chat', 3, fun(42, _M, _O) -> {ok, #{<<"result">> => <<"hi">>}} end}
            ]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?STAGE_STUB,
            ?POLICY_OK
        ],
        fun() ->
            Data = #{
                <<"payload">> => #{<<"content">> => <<"?">>},
                <<"msg_type">> => <<"text">>
            },
            ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, '_')),
            ?assertEqual(1, meck:num_calls(msg_store_ds, enqueue, '_')),
            %% 方向必须是 agent(42) → human(7)。本模块的 FromUid 是收消息的人类、
            %% ToId 是 agent，照抄变量名就会把方向写反。
            [StageArgs] = [
                A
             || {_P, {msg_store_ds, stage, A}, _R} <- meck:history(msg_store_ds)
            ],
            ?assertEqual(42, lists:nth(7, StageArgs)),
            ?assertEqual(7, lists:nth(8, StageArgs)),
            [_T, _Id, Meta] = hd([
                A
             || {_P, {msg_store_ds, enqueue, A}, _R} <- meck:history(msg_store_ds)
            ]),
            ?assertEqual(42, maps:get(from_id, Meta)),
            ?assertEqual(7, maps:get(to_id, Meta))
        end
    ).

%% E2EE required 部署下必须**拒发而非降级**：agent 无设备私钥，没有"加密后再发"
%% 这条路。不挡则 agent 明文从这条旁路灌进 msg_c2c。
reply_blocked_by_policy_gate_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
            ?ALLOW_RL,
            {elib_tsid, [{'generate', 0, fun() -> 8888 end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(42) ->
                    {true, #{<<"provider">> => <<"openai">>, <<"model">> => <<"m">>}}
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(<<"openai">>) ->
                    {ok, #{module => imboy_llm_openai, opts => #{}}}
                end}
            ]},
            {imboy_llm_openai, [
                {'chat', 3, fun(42, _M, _O) -> {ok, #{<<"result">> => <<"hi">>}} end}
            ]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]},
            ?STAGE_STUB,
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) -> {error, e2ee_required} end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"payload">> => #{<<"content">> => <<"?">>},
                <<"msg_type">> => <<"text">>
            },
            ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
            %% 一个都不许发生：不落库、不推送
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, '_')),
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, '_')),
            ?assertEqual(0, meck:num_calls(message_ds, send_next, '_'))
        end
    ).
