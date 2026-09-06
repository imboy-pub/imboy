-module(channel_reaction_summary_migration_tests).

-include_lib("eunit/include/eunit.hrl").

empty_reaction_summary_uses_json_object_test() ->
    {ok, Up} = file:read_file(
        "priv/migrations/00000084_channel_reaction_summary_empty_object.up.sql"
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(Up, <<"COALESCE((">>)
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(Up, <<"), '{}'::jsonb)">>)
    ).

%% 回归背景：00000084 把空对象兜底改为 '{}'::jsonb，但聚合仍是
%% json_object_agg（返回 json），COALESCE(json, jsonb) 无公共类型，首次点赞
%% 即抛 42846 cannot_coerce 并回滚整个反应事务。channel_message.reaction_summary
%% 列类型为 jsonb，00000089 起聚合必须用 jsonb_object_agg。
reaction_summary_agg_must_be_jsonb_test() ->
    {ok, Up} = file:read_file(
        "priv/migrations/00000089_channel_reaction_summary_jsonb_agg.up.sql"
    ),
    ?assertNotEqual(nomatch, binary:match(Up, <<"jsonb_object_agg">>)),
    %% "jsonb_object_agg" 不含子串 "json_object_agg"（json 后是 b 不是 _），
    %% 该断言可精确捕获混用 json_object_agg 的回归
    ?assertEqual(nomatch, binary:match(Up, <<"json_object_agg">>)).
