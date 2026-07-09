-module(agent_trigger_policy_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc agent_trigger_policy 纯函数测试（Phase 1 T1.5）
%%% 核心验收：群里 @agent 才回，普通消息不回（安全默认）。
%%%===================================================================

%% 空策略 = 仅 @ 触发（安全默认）
default_mention_only_replies_when_mentioned_test() ->
    ?assert(agent_trigger_policy:should_trigger(#{}, #{mentioned => true, text => <<"你好"/utf8>>})).

default_mention_only_ignores_plain_message_test() ->
    ?assertNot(
        agent_trigger_policy:should_trigger(#{}, #{mentioned => false, text => <<"随便聊聊"/utf8>>})
    ).

%% mention 显式关闭 → 被 @ 也不因 mention 触发
mention_disabled_ignores_at_test() ->
    ?assertNot(
        agent_trigger_policy:should_trigger(
            #{<<"mention">> => false},
            #{mentioned => true, text => <<"hi">>}
        )
    ).

%% suffix_q：疑问句结尾触发（半角 + 全角）
suffix_q_half_width_triggers_test() ->
    ?assert(
        agent_trigger_policy:should_trigger(
            #{<<"suffix_q">> => true},
            #{mentioned => false, text => <<"今天天气如何?"/utf8>>}
        )
    ).

suffix_q_full_width_triggers_test() ->
    ?assert(
        agent_trigger_policy:should_trigger(
            #{<<"suffix_q">> => true},
            #{mentioned => false, text => <<"今天天气如何？"/utf8>>}
        )
    ).

suffix_q_trailing_space_still_triggers_test() ->
    ?assert(
        agent_trigger_policy:should_trigger(
            #{<<"suffix_q">> => true},
            #{mentioned => false, text => <<"在吗?  ">>}
        )
    ).

suffix_q_non_question_ignored_test() ->
    ?assertNot(
        agent_trigger_policy:should_trigger(
            #{<<"suffix_q">> => true},
            #{mentioned => false, text => <<"就这样吧"/utf8>>}
        )
    ).

%% keywords：命中任一关键词触发
keyword_hit_triggers_test() ->
    ?assert(
        agent_trigger_policy:should_trigger(
            #{<<"keywords">> => [<<"帮助"/utf8>>, <<"help">>]},
            #{mentioned => false, text => <<"我需要帮助一下"/utf8>>}
        )
    ).

keyword_miss_ignored_test() ->
    ?assertNot(
        agent_trigger_policy:should_trigger(
            #{<<"keywords">> => [<<"help">>]},
            #{mentioned => false, text => <<"闲聊"/utf8>>}
        )
    ).

%% group_allowlist：非空时仅命中群放行
allowlist_blocks_other_group_test() ->
    ?assertNot(
        agent_trigger_policy:should_trigger(
            #{<<"group_allowlist">> => [100, 200]},
            #{mentioned => true, text => <<"hi">>, group_id => 300}
        )
    ).

allowlist_allows_listed_group_test() ->
    ?assert(
        agent_trigger_policy:should_trigger(
            #{<<"group_allowlist">> => [100, 200]},
            #{mentioned => true, text => <<"hi">>, group_id => 200}
        )
    ).

%% allowlist 元素为 binary（jsonb 反解）也能匹配
allowlist_binary_ids_match_test() ->
    ?assert(
        agent_trigger_policy:should_trigger(
            #{<<"group_allowlist">> => [<<"200">>]},
            #{mentioned => true, text => <<"hi">>, group_id => 200}
        )
    ).
