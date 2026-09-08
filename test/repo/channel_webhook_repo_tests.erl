-module(channel_webhook_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% WH-02：channel_webhook repo 摘要/轮换契约（真库）。

uid() ->
    erlang:unique_integer([positive]) +
        erlang:phash2(binary:encode_hex(crypto:strong_rand_bytes(8))).

digest(Bin) -> binary:encode_hex(crypto:hash(sha256, Bin), lowercase).

%% 每 VM 唯一 token 片段（摘要唯一索引跨运行不撞）
uniq() -> integer_to_binary(uid()).

%% rotate：新摘要生效 + 旧摘要进宽限窗（future 可查）；过期宽限不命中
rotate_grace_window_test_() ->
    ?TEST_WITH_DB(fun() ->
        ChannelId = uid(),
        Id = uid(),
        OldDigest = digest(<<"old-token-", (uniq())/binary>>),
        NewDigest = digest(<<"new-token-", (uniq())/binary>>),
        {ok, _} = elib_pg:execute(
            <<
                "INSERT INTO public.channel_webhook (id, channel_id, name, token,"
                " token_digest, token_prefix, bot_uid, creator_uid)"
                " VALUES ($1,$2,'wt','',$3,'oldpre',$4,$5)"
            >>,
            [Id, ChannelId, OldDigest, uid(), uid()]
        ),
        Future = elib_dt:to_rfc3339(os:system_time(second) + 600),
        {ok, _} = channel_webhook_repo:rotate(Id, NewDigest, <<"newpre">>, OldDigest, Future),
        %% 新摘要命中
        New = channel_webhook_repo:find_by_digest(NewDigest),
        ?assert(is_map(New)),
        %% 宽限窗内旧摘要命中
        Grace = channel_webhook_repo:find_by_grace_digest(OldDigest),
        ?assert(is_map(Grace)),
        %% 窗口过期（把 grace_until 拨到过去）→ 不命中
        Past = elib_dt:to_rfc3339(os:system_time(second) - 10),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.channel_webhook SET grace_until = $2 WHERE id = $1">>,
            [Id, Past]
        ),
        Empty = channel_webhook_repo:find_by_grace_digest(OldDigest),
        ?assertEqual(#{}, Empty)
    end).

%% touch_last_used 更新最近使用
touch_last_used_test_() ->
    ?TEST_WITH_DB(fun() ->
        Id = uid(),
        {ok, _} = elib_pg:execute(
            <<
                "INSERT INTO public.channel_webhook (id, channel_id, name, token,"
                " token_digest, bot_uid, creator_uid)"
                " VALUES ($1,$2,'wt','',$3,$4,$5)"
            >>,
            [Id, uid(), digest(<<"t-", (uniq())/binary>>), uid(), uid()]
        ),
        ok = channel_webhook_repo:touch_last_used(Id),
        {ok, [RowMap]} = elib_pg:query(
            <<"SELECT last_used_at FROM public.channel_webhook WHERE id = $1">>, [Id]
        ),
        ?assertNotEqual(undefined, maps:get(<<"last_used_at">>, RowMap)),
        ok
    end).
