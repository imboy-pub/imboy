-module(msg_c2s_logic_llm_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2s_logic LLM 注册表分派的 EUnit 测试（Phase 0 T0.5）
%%%
%%% 目标：验证 bot_* 分派改为查 imboy_llm_registry 后的行为
%%% 覆盖：llm_callback/2 契约桥接（{ok,_}/{error,_} → 裸 map）、
%%%        bot_qian_fan → qianfan 向后兼容映射、未注册 bot 降级
%%%===================================================================

%% ===================================================================
%% llm_callback/2 — imboy_llm:chat 契约 → c2s_to_external 回调契约
%% ===================================================================

llm_callback_ok_unwraps_to_bare_map_test_() ->
    ?WITH_MECKS(
        [
            {imboy_llm_openai, [
                {'chat', 3, fun(_Uid, _Messages, _Opts) ->
                    {ok, #{<<"result">> => <<"AI 回复"/utf8>>}}
                end}
            ]}
        ],
        fun() ->
            Opts = #{model => <<"m1">>},
            Callback = msg_c2s_logic:llm_callback(imboy_llm_openai, Opts),
            Resp = Callback(7, <<"你好"/utf8>>, []),
            % {ok, RespMap} → 裸 RespMap（c2s_to_external 的 ApiCallback 契约）
            ?assertEqual(#{<<"result">> => <<"AI 回复"/utf8>>}, Resp),
            % Text 包装为单条 user 消息，registry opts 透传给 chat/3
            ?assert(
                meck:called(imboy_llm_openai, chat, [
                    7,
                    [#{<<"role">> => <<"user">>, <<"content">> => <<"你好"/utf8>>}],
                    Opts
                ])
            )
        end
    ).

llm_callback_error_raises_to_trigger_retry_test_() ->
    ?WITH_MECKS(
        [
            {imboy_llm_openai, [
                {'chat', 3, fun(_, _, _) -> {error, timeout} end}
            ]},
            {elib_log, [
                {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            Callback = msg_c2s_logic:llm_callback(imboy_llm_openai, #{}),
            % {error, _} → 抛异常，让 c2s_to_external 的 async_retry 重试
            % （不能吞成空 result，否则 Fun 不抛 → 不重试 → 用户收到空气泡）
            ?assertError({llm_failed, timeout}, Callback(7, <<"hi">>, []))
        end
    ).

%% ===================================================================
%% c2s/3 分派 — 查注册表
%% ===================================================================

c2s_bot_qian_fan_maps_to_qianfan_provider_test_() ->
    ?WITH_MECKS(
        [
            {imboy_llm_registry, [
                {'lookup', 1, fun(<<"qianfan">>) ->
                    {ok, #{module => imboy_llm_openai, opts => #{}}}
                end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 1, fun(_) -> <<"2026-07-09T00:00:00Z">> end}
            ]},
            {elib_async, [
                {'async_retry', 1, fun(_Fun) -> ok end}
            ]},
            {msg_store_ds, [
                % stage 返回 error 短路后续 enqueue/异步 API，浅层验证分派已进入骨架
                {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> error end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(_MsgId, Code, _To) -> #{<<"code">> => Code} end}
            ]},
            {elib_log, [
                {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"bot_qian_fan">>,
                <<"payload">> => #{<<"text">> => <<"你好"/utf8>>},
                <<"created_at">> => 1751990400000
            },
            Result = msg_c2s_logic:c2s(<<"mid1">>, 7, Data),
            % 向后兼容：bot_qian_fan → 注册表键 <<"qianfan">>
            ?assert(meck:called(imboy_llm_registry, lookup, [<<"qianfan">>])),
            % 已进入 c2s_to_external 骨架（stage 被调用）
            ?assert(meck:called(msg_store_ds, stage, '_')),
            ?assertEqual({reply, #{<<"code">> => <<"internal_error">>}}, Result)
        end
    ).

c2s_unregistered_bot_replies_unsupported_test_() ->
    ?WITH_MECKS(
        [
            {imboy_llm_registry, [
                {'lookup', 1, fun(_) -> undefined end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(_MsgId, Code, _To) -> #{<<"code">> => Code} end}
            ]}
        ],
        fun() ->
            Data = #{<<"to">> => <<"bot_nope">>},
            Result = msg_c2s_logic:c2s(<<"mid2">>, 7, Data),
            ?assert(meck:called(imboy_llm_registry, lookup, [<<"nope">>])),
            ?assertEqual({reply, #{<<"code">> => <<"c2s_unsupported">>}}, Result)
        end
    ).

%% ===================================================================
%% llm_callback/4 — 流式感知（Phase 2 T2.2）
%% ===================================================================

%% 开关开 + provider 支持流式：逐帧直推 stream_delta（同定稿 id）+ 返回完整 RespMap
llm_callback4_streams_when_capable_test_() ->
    ?WITH_MECKS(
        [
            {imboy_llm_openai, [
                {'capabilities', 0, fun() ->
                    #{stream => true, vision => false, tools => false}
                end},
                {'chat_stream', 4, fun(7, _Msgs, _Opts, StreamFun) ->
                    StreamFun(<<"部分一二三四"/utf8>>),
                    {ok, #{<<"result">> => <<"完整回复"/utf8>>}}
                end}
            ]},
            {imboy_syn, [{'publish', 2, fun(_, _) -> {ok, 1} end}]}
        ],
        fun() ->
            application:set_env(imboy, llm_stream_enabled, true),
            try
                Cb = msg_c2s_logic:llm_callback(imboy_llm_openai, #{}, <<"bot_x">>, <<"mid9">>),
                Resp = Cb(7, <<"hi">>, []),
                %% 返回完整 RespMap 供 c2s_to_external 定稿
                ?assertEqual(#{<<"result">> => <<"完整回复"/utf8>>}, Resp),
                %% 帧：id=bot_response+MsgId，type=C2S，from=bot 标识
                Pubs = meck:history(imboy_syn),
                ?assert(length(Pubs) >= 1),
                [{_, {_, publish, [7, Json]}, _} | _] = Pubs,
                D = jsone:decode(Json),
                ?assertEqual(<<"stream_delta">>, maps:get(<<"msg_type">>, D)),
                ?assertEqual(<<"bot_responsemid9">>, maps:get(<<"id">>, D)),
                ?assertEqual(<<"C2S">>, maps:get(<<"type">>, D)),
                ?assertEqual(<<"bot_x">>, maps:get(<<"from">>, D))
            after
                application:unset_env(imboy, llm_stream_enabled)
            end
        end
    ).

%% 开关关：llm_callback/4 委托 /2 一次性桥接，不推流式帧
llm_callback4_falls_back_to_sync_test_() ->
    ?WITH_MECKS(
        [
            {imboy_llm_openai, [
                {'chat', 3, fun(_, _, _) -> {ok, #{<<"result">> => <<"sync">>}} end}
            ]},
            {imboy_syn, [{'publish', 2, fun(_, _) -> {ok, 1} end}]}
        ],
        fun() ->
            application:unset_env(imboy, llm_stream_enabled),
            Cb = msg_c2s_logic:llm_callback(imboy_llm_openai, #{}, <<"bot_x">>, <<"mid">>),
            Resp = Cb(7, <<"hi">>, []),
            ?assertEqual(#{<<"result">> => <<"sync">>}, Resp),
            ?assertEqual(0, length(meck:history(imboy_syn))),
            ?assert(meck:called(imboy_llm_openai, chat, '_'))
        end
    ).
