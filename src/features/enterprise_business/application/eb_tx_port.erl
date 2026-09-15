%%% @doc 扩展点：**用例级事务**（原子消息接受 / 会话审计）。
%%%
%%% 依据：用户裁决 §三（原文）——
%%%   「application 不得直连持久化实现模块（PG 直连）；为原子消息、会话审计
%%%     和 bounded purge 提供用例级 Port，**禁止暴露通用任意事务接口**。」
%%% 与 plan §2.1 #18（canonical message + policy snapshot + audit 必须同一事务提交）。
%%%
%%% 这是**真扩展点**：只有 `-callback` 声明，零实现、零 meck。
%%%
%%% ## 为什么是「用例级」而不是「通用事务」
%%%
%%% 原实现（R0 分类三的 `LAYER_DEVIATION`）里，application 层拿到的是
%%% 持久化实现模块的**模块名**：它可以开事务、可以发任意 SQL。于是
%%% 「事务边界」变成应用层可以随意书写的实现细节，租户贯穿、审计同事务、
%%% 幂等语义都退化为**调用方纪律**而不是**契约**。
%%%
%%% 本扩展点把能力收敛为**具名用例**：信封是 `(OrgId, WorkspaceId, Params)`，
%%% 事务在实现内部开启与提交，**提交成功才返回**。契约层因此不可能被用来
%%% 执行任意 SQL——`exec/1`、`query/2`、`transaction/1` 这类把任意语句交给
%%% 应用层的形状**在这里不存在**（`scripts/check_eb_port_closure.sh` 以导出面
%%% 白名单机械判定，A04 的负例正是「给本模块加一个 `exec/1`」）。
%%%
%%% 铁律 6：前两个业务参数是 `organization_id` 与 `workspace_id`，且实现必须在
%%% **同一事务**的每一条语句上都带这两个键。
-module(eb_tx_port).

-export_type([accepted_message/0, audit_id/0, params/0]).

-type params() :: map().
-type audit_id() :: integer().

%% 接受一条 canonical message：`#{message := ..., policy := ..., audit := ...}` 摘要。
%% 至少含 `message_id` / `policy_id` / `policy_version` / `audit_id` / `replayed`。
-type accepted_message() :: map().

%% @doc **原子接受企业消息**：同一事务内完成
%%   canonical message（只追加，幂等键 `(Org, Conversation, client_msg_id)`）+
%%   retention policy 快照 + append-only 审计。
%%
%% 语义要求（与 plan §2.1 #18 逐字对齐）：
%%   * **提交成功才返回 `{ok, _}`**；任一步失败即整体回滚，不得留下半条事实；
%%   * 幂等重放返回既有行并标记 `replayed => true`（不增行、不改写既有密文）；
%%   * AAD / 密文由调用方（application）准备好后经 `Params` 传入，本扩展点不接触明文。
-callback accept_message(OrgId :: integer(), WorkspaceId :: integer(), Params :: params()) ->
    {ok, accepted_message()} | {error, term()}.

%% @doc **会话建立的审计与事务同提交**：把一条会话生命周期审计事实
%%（如 `conversation.open`）与当前用例的其它写入放进同一事务，避免「用例回滚、
%% 审计已落库」或「用例成功、审计丢失」两种失真。
-callback append_conversation_audit(
    OrgId :: integer(), WorkspaceId :: integer(), Params :: params()
) ->
    {ok, audit_id()} | {error, term()}.
