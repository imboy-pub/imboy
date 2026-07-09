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

%% ===================================================================
%% Happy path：agent 私聊 → LLM → 回投
%% ===================================================================

agent_dm_triggers_llm_and_delivers_reply_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
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
            ]}
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
            {ai_agent_ds, [
                {'is_agent', 1, fun(_) -> {true, #{<<"provider">> => <<"ghost">>}} end}
            ]},
            {imboy_llm_registry, [{'lookup', 1, fun(<<"ghost">>) -> undefined end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]}
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
            {ai_agent_ds, [
                {'is_agent', 1, fun(_) -> {true, #{<<"provider">> => <<"openai">>}} end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(_) -> {ok, #{module => imboy_llm_openai, opts => #{}}} end}
            ]},
            {imboy_llm_openai, [{'chat', 3, fun(_, _, _) -> {error, timeout} end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]},
            {message_ds, [{'send_next', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            Data = #{<<"payload">> => #{<<"content">> => <<"hi">>}},
            ?assertEqual(ok, ai_agent_reply:maybe_dispatch(7, 42, Data)),
            ?assertNot(meck:called(message_ds, send_next, '_'))
        end
    ).
