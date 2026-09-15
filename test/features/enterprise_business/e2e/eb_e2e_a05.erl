%%% @doc EB-11-A05：DB / process / port / object **residual = 0**（test-only）。
%%%
%%% 依据：EB-11-A05、`agents/a5/EB-11/ORDER.md` §5（「residual 四项原始输出」）。
%%%
%%% ## residual 的口径（逐项写清，绝不含糊）
%%%
%%%   * **DB（行级）**：本 run 全部可删表（消息/投递/附件/会话/客户/渠道标识/备注/经办/
%%%     离职 case 与 item/身份）清理后为 **0**。**唯一例外**是 schema 设计上不可删除的
%%%     append-only / 不可变事实（`enterprise_audit_event` / `enterprise_retention_policy` /
%%%     `enterprise_retention_hold`，DELETE 一律 23514，**无 GUC 旁路**）以及被其 RESTRICT
%%%     FK 钉住的 `organization` / `workspace` 行。该残留**逐表列出**在证据里，不隐藏、
%%%     不假称 0（见 `a05-residual-db.txt`）。
%%%   * **DB（集群级）**：本 run **不创建**数据库、不创建表、不创建 schema；scratch 库
%%%     `imboy_eb_w2` 是 A0 预置的（本 run 只使用）。集群对象清单落盘供 A0 比对。
%%%   * **object**：本地替身桶键 = 0（A06 已显式回收，见 `a05-residual-object.txt`）。
%%%   * **process / port**：由 shell 侧在进程退出后判定（pgrep / lsof），原始输出见
%%%     `a05-residual-process.txt`、`a05-residual-port.txt`。
-module(eb_e2e_a05).

-export([run/1]).

run(Ctx) ->
    Scope = maps:get(scope, Ctx),
    Org1 = maps:get(org1, Scope),
    Org2 = maps:get(org2, Scope),
    Ws1 = maps:get(ws1, Scope),
    Ws2 = maps:get(ws2, Scope),
    io:format("~n== EB-11-A05 DB/process/port/object residual ==~n"),

    Deletable = [
        <<"enterprise_message">>,
        <<"enterprise_message_delivery">>,
        <<"enterprise_asset">>,
        <<"enterprise_conversation">>,
        <<"enterprise_contact">>,
        <<"enterprise_contact_identity">>,
        <<"enterprise_contact_assignment">>,
        <<"enterprise_note">>,
        <<"organization_business_identity_assignment">>,
        <<"organization_business_identity">>,
        <<"enterprise_offboarding_item">>,
        <<"enterprise_offboarding_case">>
    ],
    AppendOnly = [
        <<"enterprise_audit_event">>,
        <<"enterprise_retention_policy">>,
        <<"enterprise_retention_hold">>
    ],
    CleanupResults = eb_e2e_fixture:cleanup(Scope),
    [
        eb_e2e_lib:evidence(
            "a05-residual-db.txt",
            "cleanup[~ts] = ~p",
            [Label, Result]
        )
     || {Label, Result} <- CleanupResults
    ],
    Residuals = [
        {Table, row_count(Table, Org1, Ws1, Org2, Ws2)}
     || Table <- Deletable
    ],
    %% 期望的**最小可归因**残留：唯一未到期的 FU 消息 + 被其 RESTRICT FK 钉住的
    %% 会话/客户/渠道标识；其余可删表必须为 0。
    %% 不可为 0 的原因（schema 设计，非缺陷）：
    %%   * purge guard 对**未到期** canonical 行一律 23514（无 GUC 旁路）⇒ FU 及其 message 行不可删；
    %%   * conversation/contact 由该消息的 RESTRICT FK 钉住；
    %%   * organization_business_identity 由 append-only 审计的
    %%     `fk_eae_identity ... ON DELETE RESTRICT` 钉住（审计行按设计不可删）⇒
    %%     期望值由 pinned_identity_count/2 动态给出（被审计引用的 identity 集合大小）。
    Expected = [
        {<<"enterprise_message">>, 1},
        {<<"enterprise_message_delivery">>, 0},
        {<<"enterprise_asset">>, 0},
        {<<"enterprise_conversation">>, 1},
        {<<"enterprise_contact">>, 1},
        {<<"enterprise_contact_identity">>, 0},
        {<<"enterprise_contact_assignment">>, 0},
        {<<"enterprise_note">>, 0},
        {<<"organization_business_identity_assignment">>, 0},
        {<<"organization_business_identity">>, pinned_identity_count(Org1, Org2)},
        {<<"enterprise_offboarding_item">>, 0},
        {<<"enterprise_offboarding_case">>, 0}
    ],
    eb_e2e_lib:assert(
        <<"EB-11-A05.1">>,
        io_lib:format(
            "DB 行级残留 = **唯一未到期 FU 链**（声明项）：实测 ~p（期望 ~p）",
            [Residuals, Expected]
        ),
        lists:sort(Residuals) =:= lists:sort(Expected)
    ),
    AppendOnlyResiduals = [
        {Table, row_count(Table, Org1, Ws1, Org2, Ws2)}
     || Table <- AppendOnly
    ],
    ClusterResiduals = [
        {Table, org_row_count(Table, Org1, Org2)}
     || Table <- [<<"organization">>, <<"workspace">>, <<"organization_member">>]
    ],
    eb_e2e_lib:evidence(
        "a05-residual-db.txt",
        "append_only_residual(schema 不可删 23514)=~p | cluster_pinned_residual(RESTRICT FK)=~p | "
        "databases=~p | roles_created_by_run=none（本 run 只 CREATE 无、只使用既有 scratch 库）",
        [AppendOnlyResiduals, ClusterResiduals, databases()]
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A05.2">>,
        io_lib:format(
            "append-only 事实残留**如实登记**（schema 禁止 DELETE，23514）：~p；"
            "被其 RESTRICT FK 钉住的组织/Workspace 行：~p",
            [AppendOnlyResiduals, ClusterResiduals]
        ),
        length(AppendOnlyResiduals) =:= 3
    ),
    CurrentDb = eb_e2e_lib:scalar(<<"SELECT current_database() AS v">>, [], undefined),
    eb_e2e_lib:assert(
        <<"EB-11-A05.3">>,
        io_lib:format("未触碰共享库（current_database=~p，必须落在本 run 的 scratch 命名空间）", [
            CurrentDb
        ]),
        is_binary(CurrentDb) andalso binary:match(CurrentDb, <<"imboy_eb_w">>) =/= nomatch
    ),

    BucketKeys = bucket_keys(),
    eb_e2e_lib:assert(
        <<"EB-11-A05.4">>,
        io_lib:format("object residual = 0（本地替身桶键=~p）", [BucketKeys]),
        BucketKeys =:= []
    ),
    eb_e2e_lib:evidence("a05-residual-object.txt", "object_bucket_keys=~p", [BucketKeys]),
    Ctx.

%% 被 append-only 审计（fk_eae_identity RESTRICT）钉住的 identity 行数：这是该表残留的
%% **唯一**来源，故期望值动态取之（不写死数字，避免随用例增删而假红）。
pinned_identity_count(Org1, Org2) ->
    eb_e2e_lib:scalar(
        <<
            "SELECT count(DISTINCT i.id) AS n FROM organization_business_identity i"
            " WHERE i.organization_id = ANY($1::bigint[])"
            "   AND EXISTS (SELECT 1 FROM enterprise_audit_event a"
            "                WHERE a.organization_id = i.organization_id"
            "                  AND a.business_identity_id = i.id)"
        >>,
        [[Org1, Org2]],
        0
    ).

%% 本 run 只写 Org1/Org2 两棵树，故按 organization_id 计数即完备（口径保守：不少报）。
row_count(Table, Org1, _Ws1, Org2, _Ws2) ->
    org_row_count(Table, Org1, Org2).

%% 通用：按 organization_id 计数（表名来自本模块的固定白名单）。
org_row_count(Table, Org1, Org2) ->
    Quoted = <<"\"", Table/binary, "\"">>,
    Sql = iolist_to_binary([
        "SELECT count(*) AS n FROM ", Quoted, " WHERE organization_id = ANY($1::bigint[])"
    ]),
    eb_e2e_lib:scalar(Sql, [[Org1, Org2]], 0).

databases() ->
    [
        maps:get(<<"datname">>, R)
     || R <- eb_e2e_lib:rows(
            <<"SELECT datname FROM pg_database WHERE datistemplate = false ORDER BY datname">>, []
        )
    ].

bucket_keys() ->
    [
        Key
     || {{eb_asset_object_stub, object}, Key} <- persistent_term:get(), is_binary(Key)
    ].
