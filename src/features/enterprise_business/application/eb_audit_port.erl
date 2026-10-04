%%% @doc 扩展点：append-only 审计写入（EB-02 冻结契约；实现随 EB-03 落地）。
%%%
%%% 这是**真扩展点**：只有 `-callback` 声明，零实现、零 meck。
%%%
%%% 冻结语义（plan EB-D05 / §4.1 / §4.3）：
%%%   * **append-only**：本契约只有 `append/2`，不提供 update / delete /
%%%     rewrite。审计事实一旦写入即不可变更，纠正只能再追加一条。
%%%   * 首个业务参数是 `organization_id`（铁律 6）——审计事件同样 tenant-scoped。
%%%   * `detail` 不得包含 secret / 密文 / 明文 / Authorization（plan §5.4）。
%%%   * 服务端「接受」语义要求 canonical message + policy snapshot + 本审计
%%%     在同一事务内提交成功（plan §2.1 #18）；因此 `append/2` 必须能被
%%%     application 纳入同一事务上下文，而不是事后异步补写。
-module(eb_audit_port).

-moduledoc "扩展点：append-only 审计写入（EB-02 冻结契约）。".
-export_type([event/0, audit_id/0]).

-type audit_id() :: integer().
-type event() :: map().

%% @doc 追加一条审计事实。成功返回审计 id 供引用（如 hold 的 audit id）。
%% 失败必须显式返回 error，不得静默丢弃——静默丢弃会让「接受成功」失去证据。
-callback append(OrgId :: integer(), Event :: event()) ->
    {ok, audit_id()} | {error, term()}.
