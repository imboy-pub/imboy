-module(organization_owner_invariant).

%% Organization Owner 领域规则（纯函数，无 IO/无 SQL）。
%%
%% Core Contract C04：
%%   * SOURCE OF TRUTH = active `organization_member(role=owner)`；
%%   * 每个 Organization 恰好一个 active Human owner；
%%   * `organization.owner_id` 只是兼容投影；
%%   * Agent（account_type /= 0）不得成为 owner（C12：Agent 最多 role=member）。
%%
%% 本模块只承载 transfer command 的**判定规则**与稳定错误文案；
%% 锁序与事务编排见 organization_owner_transfer（application），
%% SQL 见 organization_owner_store（infrastructure）。

-export([
    validate_self_transfer/2,
    validate_current_owner/2,
    validate_actor_membership/1,
    validate_target_membership/1
]).

-spec validate_self_transfer(integer(), integer()) -> ok | {error, {400, binary()}}.
validate_self_transfer(ActorUid, TargetUid) when ActorUid =:= TargetUid ->
    {error, {400, <<"新主 Owner 不能是当前主 Owner"/utf8>>}};
validate_self_transfer(_, _) ->
    ok.

%% 锁定的 organization 行裁决操作人是否为当前投影 owner。
-spec validate_current_owner(map(), integer()) -> ok | {error, {403, binary()}}.
validate_current_owner(#{<<"owner_id">> := OwnerId}, ActorUid) when OwnerId =:= ActorUid ->
    ok;
validate_current_owner(_, _) ->
    {error, {403, <<"仅当前主 Owner 可转移 Owner"/utf8>>}}.

%% 锁定的 actor 成员行必须是 active owner（C04：只有 owner 发起转移）。
-spec validate_actor_membership(map()) -> ok | {error, {403, binary()}}.
validate_actor_membership(#{<<"role">> := <<"owner">>, <<"status">> := <<"active">>}) ->
    ok;
validate_actor_membership(_) ->
    {error, {403, <<"当前主 Owner 成员状态无效"/utf8>>}}.

%% 锁定的 target 成员行：
%%   * 必须是 active 成员（409）；
%%   * 已是 owner 则幂等拒绝（409，与既有响应兼容）；
%%   * 非 Human（account_type =/= 0，含 Agent 1 / 现存 2/3 过渡值）拒绝（409，C04/C12）。
-spec validate_target_membership(map()) -> ok | {error, {409, binary()}}.
validate_target_membership(#{<<"status">> := <<"active">>, <<"role">> := <<"owner">>}) ->
    {error, {409, <<"目标用户已是 Owner"/utf8>>}};
validate_target_membership(#{
    <<"status">> := <<"active">>, <<"role">> := _Role, <<"account_type">> := AccountType
}) when
    AccountType =/= 0, AccountType =/= <<"0">>
->
    {error, {409, <<"目标用户不能成为 Owner：仅 Human 成员可担任（Agent 不允许）"/utf8>>}};
validate_target_membership(#{<<"status">> := <<"active">>, <<"role">> := Role}) when
    Role =:= <<"admin">>; Role =:= <<"member">>
->
    ok;
validate_target_membership(_) ->
    {error, {409, <<"该用户不是组织成员或已被移除"/utf8>>}}.
