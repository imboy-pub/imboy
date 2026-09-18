%% moya_queue_filter_tests — 待评队列「快速检索」筛选回归测试。
%%
%% 为什么必须有它：
%%   /moya/review-queue 的筛选链路（handler 读 QS → logic 白名单 → repo 拼 SQL → PG）
%%   原先**一个测试都没有**，于是两个「静默失效」缺陷同时上线：
%%
%%   ① 计数 SQL 复用主查询片段 → 引用不存在的 $4 → PG 报错 → total 回落 0。
%%      表现为「列表有数据、total 恒 0」：分页 hasNext 算错，前端一筛选就像空了。
%%   ② 时间边界写成 `$N::timestamptz`：本仓 pg_conf 给 timestamptz 注册了
%%      epgsql_codec_rfc3339_bin，其 encode/3 走 elib_dt:rfc3339_to/2，而该解析器
%%      **要求字符串带时区**，否则返回 {error, empty_input} → codec 退化为 <<0:64>>
%%      （PG 纪元 2000-01-01）→ `submitted_at >= 2000-01-01` 恒真 →
%%      时间条件静默变成「不过滤」，返回全量且不报错。
%%      而 handler/logic 的 ?TIME_PARAM_RE 恰好放行三种无时区形态
%%      （`YYYY-MM-DD` / `...THH:MM` / `...THH:MM:SS`）——踩中即失效。
%%
%% 本文件分两层：
%%   A 纯函数层（无 DB，恒运行）：占位符编号契约 + 时间边界 SQL 形态 + 白名单一致性。
%%   B 真库层（一次性 marker 库，inttest_marker_db 配方，env 前缀 MOYA_INTTEST，
%%     全链迁移至当前 head；供给失败显式 FAIL 无 skip）：用**生产同一份 SQL 片段**
%%     跑真 PG，断言「未来时间边界必须筛空」——②的直接探针。
%%
%% 带真实数据的端到端（真 HTTP + 真库 + 期望值现场算）另见
%% scripts/smoke_moya_queue_filter.sh。
-module(moya_queue_filter_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% A 纯函数层（无 DB）
%%%===================================================================

%% A1：占位符编号必须与「本查询已占用到几」一致。
%% 契约：最大占位符下标 == StartIndex - 1 + 参数个数。写死 StartIndex 会让
%% 计数查询（只有 $1）引用不存在的 $4 —— 这正是缺陷 ①。
queue_conds_placeholder_contract_test() ->
    Full = #{
        assignment_id => 11,
        task_id => 22,
        learner_id => 33,
        submitted_from => <<"2026-09-14">>,
        submitted_to => <<"2026-09-15">>
    },
    %% 计数查询：$1 已被 GroupIds 占用 → 筛选自 $2 起
    {Sql2, P2} = moya_submission_repo:queue_conds(Full, 2),
    C2 = iolist_to_binary(Sql2),
    ?assertEqual([2, 3, 4, 5, 6], lists:sort(placeholders(C2))),
    ?assertEqual(5, length(P2)),
    %% 主查询：$1/$2/$3 = GroupIds/Size/Offset → 筛选自 $4 起
    {Sql4, P4} = moya_submission_repo:queue_conds(Full, 4),
    C4 = iolist_to_binary(Sql4),
    ?assertEqual([4, 5, 6, 7, 8], lists:sort(placeholders(C4))),
    ?assertEqual(5, length(P4)),
    %% 参数个数 + StartIndex - 1 必须等于最大下标：
    %% 少一个 → PG 报「bind message supplies N parameters」→ 被 catch 成
    %% {error,_} → total 回落 0（缺陷 ① 的症状）。
    ?assertEqual(lists:max(placeholders(C4)), 4 - 1 + length(P4)),
    ?assertEqual(lists:max(placeholders(C2)), 2 - 1 + length(P2)).

%% A2：单个筛选键的编号（最简形态，便于一眼定位回归）。
queue_conds_single_key_test() ->
    {Sql2, _} = moya_submission_repo:queue_conds(#{task_id => 1}, 2),
    ?assertEqual([2], placeholders(iolist_to_binary(Sql2))),
    {Sql4, _} = moya_submission_repo:queue_conds(#{task_id => 1}, 4),
    ?assertEqual([4], placeholders(iolist_to_binary(Sql4))).

%% A3：时间边界必须是 ($N::text)::timestamptz 两道转换 —— 缺陷 ② 的形态守卫。
%% 裸 $N::timestamptz 会让 codec 去解析入参；无时区串解析失败即静默退化为纪元。
time_cond_must_double_cast_test_() ->
    From2 = iolist_to_binary(moya_submission_repo:queue_cond_sql(submitted_from, 2)),
    To4 = iolist_to_binary(moya_submission_repo:queue_cond_sql(submitted_to, 4)),
    [
        ?_assertEqual(<<" AND hs.submitted_at >= ($2::text)::timestamptz">>, From2),
        ?_assertEqual(<<" AND hs.submitted_at < ($4::text)::timestamptz">>, To4),
        %% 显式禁止裸形态：$N 紧跟 ::timestamptz 即视为回归
        ?_assertEqual(
            nomatch,
            re:run(<<From2/binary, To4/binary>>, <<"\\$\\d+::timestamptz">>, [
                {capture, none}
            ])
        )
    ].

%% A4：白名单口径不得漂移 —— handler 与 logic 的 ?TIME_PARAM_RE 必须逐字一致。
%% 两处都放行「无时区」形态；只改一处会让安全边界分叉。
time_param_re_is_identical_in_handler_and_logic_test() ->
    {ok, H} = file:read_file("src/api/moya_review_handler.erl"),
    {ok, L} = file:read_file("src/logic/moya_review_logic.erl"),
    Pattern =
        <<
            "^\\\\d{4}-\\\\d{2}-\\\\d{2}(T\\\\d{2}:\\\\d{2}(:\\\\d{2}(\\\\.\\\\d+)?)?"
            "(Z|[+-]\\\\d{2}:\\\\d{2})?)?$"
        >>,
    ?assertMatch({_, _}, binary:match(H, Pattern)),
    ?assertMatch({_, _}, binary:match(L, Pattern)).

%%%===================================================================
%%% B 真库层（一次性 marker 库；供给失败显式 FAIL，无静默 skip）
%%%===================================================================

%% 白名单放行的全部形态（与 ?TIME_PARAM_RE 对应）。
%%
%% 关键在「无时区」那三种：elib_dt:rfc3339_to/2 解析不了它们，裸 $N::timestamptz
%% 会退化成 2000-01-01。带时区的两种本来就没问题（所以不能只测带时区的）。
accepted_formats() ->
    [
        <<"2099-01-01">>,
        <<"2099-01-01T00:00">>,
        <<"2099-01-01T00:00:00">>,
        <<"2099-01-01T00:00:00+08:00">>,
        <<"2099-01-01T00:00:00Z">>
    ].

setup_conn() ->
    %% 一次性 marker 库（inttest_marker_db 配方）：env 覆盖（<= imboy.pg_conf
    %% 回退）→ 建库 → 12 扩展 → erlang_migrate:up 全链；任一失败显式 error。
    inttest_marker_db:provision(#{
        env_prefix => <<"MOYA_INTTEST">>,
        %% 与生产 pg_conf 同款 codec —— 不带上它就测不出缺陷 ②
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

close_conn(State) ->
    inttest_marker_db:release(State).

%% B1（缺陷 ② 直接探针）：行是 2030 年，submitted_from 指向 2099 年 —— 必须筛空。
%% 用**生产片段** moya_submission_repo:queue_cond_sql/2 拼 SQL，不是抄一份；
%% 抄一份就测不到拼装错误。裸 $N::timestamptz 时它会退化成纪元 2000-01-01，
%% 于是 2030 的行被「命中」→ 本断言失败。无需任何夹具数据，故不会空转。
future_boundary_filters_everything_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            [?_test(future_boundary_filters_everything(C))]
        end}}.

future_boundary_filters_everything(C) ->
    Frag = iolist_to_binary(moya_submission_repo:queue_cond_sql(submitted_from, 1)),
    Sql =
        <<
            "SELECT count(*)::bigint FROM "
            "(SELECT '2030-01-01T00:00:00+08:00'::timestamptz AS submitted_at) hs "
            "WHERE true",
            Frag/binary
        >>,
    lists:foreach(
        fun(V) ->
            {ok, _, [{N}]} = epgsql:equery(C, Sql, [V]),
            ?assertEqual({submitted_from, V, 0}, {submitted_from, V, N})
        end,
        accepted_formats()
    ).

%% B2：反向守卫 —— 过去的时间边界不能把该留的行筛掉（防矫枉过正）。
past_boundary_keeps_row_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            [?_test(past_boundary_keeps_row(C))]
        end}}.

past_boundary_keeps_row(C) ->
    lists:foreach(
        fun({Key, Cmp}) ->
            Frag = iolist_to_binary(moya_submission_repo:queue_cond_sql(Key, 1)),
            Sql =
                <<
                    "SELECT count(*)::bigint FROM "
                    "(SELECT '2030-01-01T00:00:00+08:00'::timestamptz AS submitted_at) hs "
                    "WHERE true",
                    Frag/binary
                >>,
            {ok, _, [{N}]} = epgsql:equery(C, Sql, [Cmp]),
            %% 2030 年的行，边界在 2026 年 → 必须仍被命中
            ?assertEqual({Key, 1}, {Key, N})
        end,
        [{submitted_from, <<"2026-09-14">>}]
    ).

%% B3（缺陷 ① 探针，需要库里有数据才有意义）：
%% 带筛选时 total 必须等于「同条件下取全量时的列表长度」。
%% 计数 SQL 引用了不存在的 $N → PG 报错 → count_queue_run 回落 0 → 此处立即暴露。
count_matches_list_under_filter_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            case all_groups_with_submitted(C) of
                [] ->
                    %% 无数据则无从分辨；真实数据的端到端在 smoke 脚本里
                    [];
                Groups ->
                    [
                        ?_test(begin
                            lists:foreach(
                                fun(F) ->
                                    {ok, Rows, Total} =
                                        moya_submission_repo:queue_tx(C, Groups, F, 1, 500),
                                    ?assertEqual({F, length(Rows)}, {F, Total})
                                end,
                                [#{} | [#{task_id => T} || T <- top_task_ids(C, 3)]]
                            )
                        end)
                    ]
            end
        end}}.

all_groups_with_submitted(C) ->
    Sql =
        <<
            "SELECT DISTINCT g.id FROM homework_submission hs "
            "JOIN group_task_assignment a ON a.id = hs.assignment_id "
            "JOIN group_task gt ON gt.task_id = a.task_id "
            "JOIN \"group\" g ON g.id = gt.group_id "
            "WHERE hs.status = 'submitted' LIMIT 20"
        >>,
    {ok, _, Rows} = epgsql:equery(C, Sql, []),
    [Id || {Id} <- Rows].

top_task_ids(C, N) ->
    Sql =
        <<
            "SELECT gt.id FROM homework_submission hs "
            "JOIN group_task_assignment a ON a.id = hs.assignment_id "
            "JOIN group_task gt ON gt.task_id = a.task_id "
            "WHERE hs.status = 'submitted' GROUP BY gt.id ORDER BY count(*) DESC LIMIT $1"
        >>,
    {ok, _, Rows} = epgsql:equery(C, Sql, [N]),
    [Id || {Id} <- Rows].

%%%===================================================================
%%% helpers
%%%===================================================================

%% 取出 SQL 里全部 $N 的数字（按出现顺序）。
placeholders(Sql) ->
    case re:run(Sql, <<"\\$(\\d+)">>, [global, {capture, all_but_first, binary}]) of
        {match, Caps} -> [binary_to_integer(N) || [N] <- Caps];
        nomatch -> []
    end.
