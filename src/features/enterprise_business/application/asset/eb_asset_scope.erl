%%% @doc EB-07：企业附件的**逐请求授权判定**（成员状态 + 经办 ACL）。
%%%
%%% 依据：
%%%   * `eb_auth_port` 的契约口径——授权事实必须**逐请求**从权威源重读，
%%%     JWT 自报的 `member_status` / `role` / `permissions` 一律不采信。这是
%%%     EB-07-A02「suspended 后旧 JWT 的新下载请求立即被拒」的实现前提：
%%%     本模块**不缓存**任何事实，也不跨请求复用返回值。
%%%   * 硬约束 2：对象路径与 DB ACL **都绑定 Org/Workspace**；跨租户一律
%%%     `{error, not_found}`（不区分「不存在」与「不是你的」，避免枚举）。
%%%   * 硬约束 6：事实 ≠ 授权结论。成员事实走 `eb_auth_port:load_request_facts/1`
%%%     的**事实面**，判定（能不能读这个附件）在本模块，由本模块给出结论。
%%%
%%% ## 判定链（全部 fail-closed，任一环失败即拒）
%%%
%%%   1. 载入事实：无成员关系 ⇒ `{forbidden, no_member}`；
%%%   2. 成员状态必须是 `active`；`suspended` / `removed` / 未知值 ⇒
%%%      `{forbidden, {member_status, Status}}`（**事实照报，判定照拒**）；
%%%   3. 会话归属：会话必须存在于**同一** `(Org, Workspace)`，否则 `{error, not_found}`；
%%%   4. 经办 ACL：请求者在该 Org 必须有一条 **active** 经办关系，且其
%%%      `business_identity_id` 等于**会话当前**经办身份；否则 `{forbidden, not_assignee}`。
%%%      交接后（会话经办身份指向接任者）这条判定自动翻面：接任者通过、原经办被拒。
-module(eb_asset_scope).

-export([authorize/6, authorize_contact/5, member_only/5, active_assignment_for/3]).

%% @doc 会话级授权：`(Auth, Store, OrgId, WorkspaceId, ConversationId, ActorUserId)`。
%%
%% 返回 `ok` 或 `{error, Reason}`；`Reason` 里**只出现**判定结论与事实值，
%% 不含任何存储侧信息。
-spec authorize(module(), module(), integer(), integer(), term(), term()) -> ok | {error, term()}.
authorize(Auth, Store, OrgId, WorkspaceId, ConversationId, ActorUserId) ->
    case member_only(Auth, Store, OrgId, WorkspaceId, ActorUserId) of
        {ok, Facts} ->
            assignee_check(Store, OrgId, WorkspaceId, ConversationId, Facts);
        {error, _} = Err ->
            Err
    end.

%% @doc CSB-02S D6：**访客（contact 主体）**的会话级授权分支。
%%
%% 与成员分支（`authorize/6`）完全独立：访客没有 IMBoy 成员事实，授权锚是
%% 「会话存在于同一 (Org, Workspace) 且会话的 contact 与令牌作用域主体逐字
%% 相等」。访客令牌的真实性由 CS 侧 digest 校验裁决（A6/E2E 语义），这里
%% 只做会话归属判定；跨租户 / 不存在 ⇒ `{error, not_found}`（不区分，避免枚举），
%% 归属不符 ⇒ `{error, {forbidden, contact_scope_mismatch}}`（403 面）。
-spec authorize_contact(module(), integer(), integer(), term(), term()) ->
    ok | {error, term()}.
authorize_contact(Store, OrgId, WorkspaceId, ConversationId, ActorContactId) ->
    case is_integer(ActorContactId) andalso is_integer(ConversationId) of
        false ->
            {error, {forbidden, no_actor}};
        true ->
            case Store:fetch_conversation(OrgId, WorkspaceId, ConversationId) of
                {ok, Conversation} ->
                    case maps:get(contact_id, Conversation, undefined) of
                        ActorContactId -> ok;
                        _Other -> {error, {forbidden, contact_scope_mismatch}}
                    end;
                {error, not_found} ->
                    {error, not_found};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 只判成员状态（无会话语境时使用，例如未绑定会话的独立附件）。
-spec member_only(module(), module(), integer(), integer(), term()) ->
    {ok, map()} | {error, term()}.
member_only(Auth, _Store, OrgId, WorkspaceId, ActorUserId) when is_atom(Auth) ->
    case is_integer(ActorUserId) of
        false ->
            {error, {forbidden, no_actor}};
        true ->
            case
                Auth:load_request_facts(#{
                    organization_id => OrgId,
                    workspace_id => WorkspaceId,
                    user_id => ActorUserId
                })
            of
                {ok, Facts} ->
                    status_gate(Facts);
                {error, no_member} ->
                    {error, {forbidden, no_member}};
                {error, Reason} ->
                    %% 事实源不可用一律 fail-closed（绝不「默认放行」）
                    {error, {forbidden, {facts_unavailable, Reason}}}
            end
    end;
member_only(_Auth, _Store, _OrgId, _WorkspaceId, _ActorUserId) ->
    {error, {forbidden, auth_port_unavailable}}.

status_gate(Facts) ->
    Member = maps:get(member, Facts, #{}),
    case maps:get(status, Member, undefined) of
        active -> {ok, Facts};
        Status -> {error, {forbidden, {member_status, Status}}}
    end.

assignee_check(Store, OrgId, WorkspaceId, ConversationId, Facts) ->
    case is_integer(ConversationId) of
        false ->
            ok;
        true ->
            case Store:fetch_conversation(OrgId, WorkspaceId, ConversationId) of
                {ok, Conversation} ->
                    identity_gate(
                        maps:get(business_identity_id, Conversation, undefined),
                        maps:get(assignments, Facts, [])
                    );
                {error, _} ->
                    %% 跨 Org / 跨 Workspace / 不存在 ⇒ 同一答案（避免枚举）
                    {error, not_found}
            end
    end.

identity_gate(IdentityId, Assignments) when is_integer(IdentityId) ->
    case active_assignment_for(IdentityId, undefined, Assignments) of
        {ok, _Assignment} -> ok;
        {error, _} = Err -> Err
    end;
identity_gate(_IdentityId, _Assignments) ->
    {error, {forbidden, no_assignee}}.

%% @doc 该身份的 active 经办关系（`Assignments` 来自 `load_request_facts/1` 的
%% `assignments` 字段；SQL 侧已过滤 `status = 'active'`）。`FunctionKey` 给
%% `undefined` 时不比对 function_key。
-spec active_assignment_for(integer(), binary() | undefined, [map()]) ->
    {ok, map()} | {error, term()}.
active_assignment_for(IdentityId, FunctionKey, Assignments) when is_list(Assignments) ->
    Hits = [
        A
     || A <- Assignments,
        maps:get(business_identity_id, A, undefined) =:= IdentityId,
        maps:get(status, A, active) =:= active,
        FunctionKey =:= undefined orelse maps:get(function_key, A, undefined) =:= FunctionKey
    ],
    case Hits of
        [Assignment | _] -> {ok, Assignment};
        [] -> {error, {forbidden, not_assignee}}
    end;
active_assignment_for(_IdentityId, _FunctionKey, _Assignments) ->
    {error, {forbidden, not_assignee}}.
