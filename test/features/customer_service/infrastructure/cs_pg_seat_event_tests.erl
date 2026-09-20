%%% @doc BE-S01a：event 写入路径的 workspace_id 冻结语句契约（纯静态，零 DB）。
%%%
%%% 迁移 00000135 给 customer_service_event 追加 workspace_id（SSE 分区列，
%%% MIG-00 §3.6）后，NOT NULL 成立的前提是**唯一**写入点（cs_pg_seat 的
%%% ?SQL_INSERT_EVENT / event_params/2）同批带上该列——本套件把该前提钉死在
%%% 冻结语句上：语句漂移（列缺失/占位符数目不符）即红。
-module(cs_pg_seat_event_tests).

-include_lib("eunit/include/eunit.hrl").

insert_event_statement_test() ->
    Statements = cs_pg_seat:sql_statements(),
    [Insert] = [
        S
     || S <- Statements, binary:match(S, <<"INSERT INTO customer_service_event">>) =/= nomatch
    ],
    %% 列清单逐字含 workspace_id（第 3 位——org 之后、session 之前）。
    ?assert(
        binary:match(Insert, <<" (id, organization_id, workspace_id, session_id,">>) =/= nomatch
    ),
    %% 占位符 9 个（原 8 + workspace_id）。
    ?assert(
        binary:match(Insert, <<"VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9::jsonb)">>) =/= nomatch
    ),
    %% 非真空：旧形状（无 workspace_id 列、8 参）必须不再命中。
    ?assert(binary:match(Insert, <<"(id, organization_id, session_id">>) =:= nomatch),
    ?assert(binary:match(Insert, <<"VALUES ($1, $2, $3, $4, $5, $6, $7, $8::jsonb)">>) =:= nomatch).

%% 迁移 135 up/down 的语句形状静态核对（文件存在 + 关键 DDL 片段在文），
%% 保证「迁移与写入点同批」不被静默拆散。
migration_carries_workspace_column_test() ->
    Up = read("priv/migrations/00000135_customer_service_seat_sse.up.sql"),
    Down = read("priv/migrations/00000135_customer_service_seat_sse.down.sql"),
    %% up：expand（nullable 追加）→ backfill（session 真源 → Org 默认 →
    %% 最小 active Workspace 三级链）→ contract（NOT NULL + 复合 FK + 索引）。
    lists:foreach(
        fun(Fragment) -> ?assert(binary:match(Up, Fragment) =/= nomatch) end,
        [
            <<"ADD COLUMN IF NOT EXISTS workspace_id bigint">>,
            <<"FROM customer_service_session s">>,
            <<"FROM organization_default_workspace d">>,
            <<"ALTER COLUMN workspace_id SET NOT NULL">>,
            <<"i_cse_org_ws_id">>,
            <<"USING btree (organization_id, workspace_id, id)">>,
            %% append-only 守卫的冻结列清单在 up 里扩展 workspace_id（回填后
            %% UPDATE 改该列同样 23514 拒绝）。
            <<"NEW.workspace_id">>
        ]
    ),
    %% down：索引/约束/列全删；守卫恢复为迁移 125 的原始形状。
    lists:foreach(
        fun(Fragment) -> ?assert(binary:match(Down, Fragment) =/= nomatch) end,
        [
            <<"DROP INDEX IF EXISTS i_cse_org_ws_id">>,
            <<"DROP COLUMN IF EXISTS workspace_id">>
        ]
    ).

read(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.
