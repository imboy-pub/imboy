-module(ai_agent_group_reply_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% Phase 4 T4.2 群触发：ai_agent_group_reply 触发决策核心 单测。
% 覆盖：@agent 门控放行 → LLM → 回群；E2EE 跳过；agent 发送者防环；限流拦截；
%       非 agent @ 不触发。
%%%

-define(SYNC_ASYNC,
    {elib_async, [
        {'async', 1, fun(F) ->
            F(),
            ok
        end}
    ]}
).
-define(ALLOW_RL,
    {agent_rate_limiter, [{'allow', 2, fun(_, _) -> allow end}]}
).

group_data(Mentions, Text) ->
    #{
        <<"to">> => <<"100">>,
        <<"msg_type">> => <<"text">>,
        <<"payload">> => #{<<"content">> => Text, <<"mentions">> => Mentions}
    }.

%% @agent + 策略放行 + 限流放行 → LLM 触发，定稿经 msg_c2g_logic:c2g 回群（from=agent）
mention_agent_triggers_and_delivers_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
            ?ALLOW_RL,
            {elib_tsid, [{'generate', 0, fun() -> 7777 end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun
                    (42) -> {true, #{<<"provider">> => <<"openai">>, <<"trigger_policy">> => #{}}};
                    (_) -> false
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(<<"openai">>) ->
                    {ok, #{module => imboy_llm_openai, opts => #{}}}
                end}
            ]},
            {imboy_llm_openai, [
                {'chat', 3, fun(42, _Msgs, _Opts) -> {ok, #{<<"result">> => <<"群里你好"/utf8>>}} end}
            ]},
            {msg_c2g_logic, [{'c2g', 3, fun(_MsgId, _From, _Data) -> ok end}]}
        ],
        fun() ->
            Data = group_data([42], <<"@bot 在吗"/utf8>>),
            ?assertEqual(ok, ai_agent_group_reply:maybe_dispatch(7, 100, Data, [7, 42, 8])),
            ?assert(meck:called(imboy_llm_openai, chat, '_')),
            %% 定稿以 agent(42) 身份回群 100
            ?assert(meck:called(msg_c2g_logic, c2g, ['_', 42, '_']))
        end
    ).

%% E2EE 群消息 → 绝不触发
e2ee_group_never_triggers_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_ds, [{'is_agent', 1, fun(_) -> {true, #{}} end}]}
        ],
        fun() ->
            Data = (group_data([42], <<"x">>))#{
                <<"msg_type">> => <<"e2ee">>,
                <<"e2ee">> => #{<<"e2ee">> => true}
            },
            ?assertEqual(ok, ai_agent_group_reply:maybe_dispatch(7, 100, Data, [7, 42])),
            %% E2EE 在 is_agent 之前短路（连 mentioned_agents 都不进）
            ?assertNot(meck:called(ai_agent_ds, is_agent, '_'))
        end
    ).

%% 发送者本身是 agent → 不触发（防 agent↔agent 环）
agent_sender_no_trigger_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_ds, [
                {'is_agent', 1, fun
                    (99) -> {true, #{}};
                    (_) -> false
                end}
            ]},
            {imboy_llm_registry, [{'lookup', 1, fun(_) -> undefined end}]}
        ],
        fun() ->
            Data = group_data([42], <<"hi">>),
            %% FromUid=99 是 agent
            ?assertEqual(ok, ai_agent_group_reply:maybe_dispatch(99, 100, Data, [42])),
            ?assertNot(meck:called(imboy_llm_registry, lookup, '_'))
        end
    ).

%% 限流拦截 → 不调 LLM、不回群
rate_limited_no_llm_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
            {agent_rate_limiter, [{'allow', 2, fun(_, _) -> {deny, agent_rate} end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun
                    (42) -> {true, #{<<"provider">> => <<"openai">>, <<"trigger_policy">> => #{}}};
                    (_) -> false
                end}
            ]},
            {imboy_llm_registry, [{'lookup', 1, fun(_) -> {ok, #{module => x, opts => #{}}} end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]}
        ],
        fun() ->
            Data = group_data([42], <<"@bot 在吗"/utf8>>),
            ?assertEqual(ok, ai_agent_group_reply:maybe_dispatch(7, 100, Data, [7, 42])),
            ?assertNot(meck:called(imboy_llm_registry, lookup, '_')),
            ?assertNot(meck:called(msg_c2g_logic, c2g, '_'))
        end
    ).

%% HIGH-1 回归：mentions 重复同一 agent（[42,42,42]）→ 只触发一次（去重）
duplicate_mentions_trigger_once_test_() ->
    ?WITH_MECKS(
        [
            ?SYNC_ASYNC,
            ?ALLOW_RL,
            {elib_tsid, [{'generate', 0, fun() -> 7777 end}]},
            {ai_agent_ds, [
                {'is_agent', 1, fun
                    (42) -> {true, #{<<"provider">> => <<"openai">>, <<"trigger_policy">> => #{}}};
                    (_) -> false
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(<<"openai">>) ->
                    {ok, #{module => imboy_llm_openai, opts => #{}}}
                end}
            ]},
            {imboy_llm_openai, [
                {'chat', 3, fun(42, _, _) -> {ok, #{<<"result">> => <<"hi">>}} end}
            ]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]}
        ],
        fun() ->
            Data = group_data([42, 42, 42], <<"@bot @bot @bot"/utf8>>),
            ?assertEqual(ok, ai_agent_group_reply:maybe_dispatch(7, 100, Data, [7, 42])),
            %% 去重后只调一次 LLM、只回一条
            ?assertEqual(1, meck:num_calls(imboy_llm_openai, chat, '_')),
            ?assertEqual(1, meck:num_calls(msg_c2g_logic, c2g, '_'))
        end
    ).

%% 被 @ 的是普通成员（非 agent）→ 不触发
non_agent_mention_no_trigger_test_() ->
    ?WITH_MECKS(
        [
            ?ALLOW_RL,
            {ai_agent_ds, [{'is_agent', 1, fun(_) -> false end}]},
            {imboy_llm_registry, [{'lookup', 1, fun(_) -> {ok, #{module => x, opts => #{}}} end}]}
        ],
        fun() ->
            Data = group_data([8], <<"@alice hi">>),
            ?assertEqual(ok, ai_agent_group_reply:maybe_dispatch(7, 100, Data, [7, 8])),
            ?assertNot(meck:called(imboy_llm_registry, lookup, '_'))
        end
    ).
