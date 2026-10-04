%%% @doc 扩展点：**bounded purge**（用例级 Port）。
%%%
%%% 依据：用户裁决 §三（原文）——「为 ... bounded purge 提供用例级 Port，**禁止暴露
%%% 通用任意事务接口**」；EB-03R §2.3 T3 的精确语义。
%%%
%%% 放在 `infrastructure/` 的原因：本扩展点的**唯一实现**是 bounded purge worker
%%%（`eb_pg_purge` / `eb_pg_purge_port`），而 `application/**` **不得**出现
%%% `eb_pg_` 前缀模块名（A04 的机械判据）。把契约与实现放在同一层，使「application
%%% 只经 Port 触达 purge」这件事**结构上成立**，而不是靠命名约定。
%%%
%%% 这是**真扩展点**：只有 `-callback` 声明，零实现、零 meck。
%%%
%%% ## 为什么必须窄
%%%
%%% 原先 application 层直连 `eb_pg_purge`（`eb_retention_app:purge_batch/2`），
%%% 于是「谁能删物理行、删多少、什么时钟、什么并发策略」全部变成调用方可改的实现
%%% 细节。本扩展点把能力收成**一个具名用例**：参数是
%%% `(OrgId, WorkspaceId, NowMs, Limit)`，返回是 `{ok, #{deleted := N}}`。
%%%
%%% 契约里**不存在**：`exec/1`、`query/2`、`transaction/1`、任意 SQL 入口、
%%% 表名/条件参数。实现内部才允许 `SKIP LOCKED` + `LIMIT`：
%%%   * 注入时钟（`NowMs`）是**唯一**的准入闸门，实现不读系统时间；
%%%   * active hold 由 DB 约束 / 锁序列化保障（不得只靠应用层「先查后删」）；
%%%   * 整批一个事务，任一行被 DB 守卫拒绝 ⇒ 整批回滚（**失败宁可多保留**）。
%%%
%%% 铁律 6：前两个业务参数是 `organization_id` 与 `workspace_id`；跨租户错配
%%% 必须是「删 0 行」而不是「删到别人的行」。
-module(eb_purge_port).

-moduledoc "扩展点：bounded purge（用例级 Port）。".
-export_type([purge_result/0]).

-type purge_result() :: #{
    deleted := non_neg_integer(),
    %% 被 domain 裁决为不合格而跳过的行（`[{MessageId, Reason}]`），多保留的证据。
    skipped => [{integer(), term()}],
    %% 真的删了行才有审计 id；一行未删时不写审计（不得产生「空审计」）。
    audit_id => integer() | undefined
}.

%% @doc 执行一批 bounded purge。
%%
%% `NowMs` 是**注入时钟**的毫秒值（调用方从 `eb_clock_port` 取），实现不得读系统时间。
%% `Limit` 是本批上限（实现必须把它作为 SQL 的绑定参数，不得字符串拼接）。
%%
%% 返回 `{ok, #{deleted := N}}`（可能附带 `skipped` / `audit_id` 证据）或 `{error, _}`。
%% **不得**返回「部分成功」：要么整批提交，要么整批回滚。
-callback purge_batch(
    OrgId :: integer(), WorkspaceId :: integer(), NowMs :: integer(), Limit :: pos_integer()
) ->
    {ok, purge_result()} | {error, term()}.
%% EB-06-A18：过渡信封 `/3`（`Opts :: #{now, batch_limit}`）与它的唯一调用点
%% （`eb_retention_app:run_purge/5`）已在本卡内一并删除（E6-D3 / E5-D2 的递延项）：
%% 调用点改走上面的 `/4` 窄形状后，这里不再保留任何接受 map 参数的信封 —— 契约面
%% 因此不可能被用来把任意参数转发给 purge worker。本端口只声明 `/4` 一个 callback。
