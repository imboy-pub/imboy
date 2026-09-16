%%% @doc Organization Department 纯域逻辑（Core Contract C10/C15）。
%%%
%%% 职责：部门名/状态机的合法性裁决、环检测的纯函数、出站投影白名单。
%%% 本模块零 SQL、零 elib_pg、零应用启动依赖——可在无 DB 的 EUnit 下独立验证。
%%%
%%% 铁律（C10/C15 冻结语义，任何调用方不得绕过）：
%%%   * Department 是**纯目录**：不承载任何 Workspace/CS/Agent/资源权限；
%%%   * status 只有 active|archived；V1 只有 active -> archived（无 restore API）；
%%%   * 禁止 self-parent 与祖先环（DB 触发器是权威，本模块提供快速失败预检）；
%%%   * department_admin 只是 department_member.is_admin 标记——本模块不提供
%%%     任何「授权」原语，也没有任何函数会写 department 表之外的资源。
-module(organization_department).

-export([
    valid_name/1,
    transition/2,
    statuses/0,
    ensure_not_self_parent/2,
    ensure_not_in_ancestors/2,
    project/2,
    archive_decision/1
]).

%% 状态全集（C10 冻结；新增状态须先改 Core Contract）
-define(STATUSES, [active, archived]).

%% 名称上限与 department.name varchar(200) 对齐
-define(MAX_NAME_BYTES, 200).

%% ===================================================================
%% 名称
%% ===================================================================

%% @doc 部门名：非空二进制、去首尾空白后 1..200 字节（varchar(200)）。
-spec valid_name(term()) -> ok | {error, {invalid_name, term()}}.
valid_name(Name) when is_binary(Name), byte_size(Name) > 0, byte_size(Name) =< ?MAX_NAME_BYTES ->
    case string:trim(Name) of
        <<>> -> {error, {invalid_name, Name}};
        _Trimmed -> ok
    end;
valid_name(Name) ->
    {error, {invalid_name, Name}}.

%% ===================================================================
%% 状态机
%% ===================================================================

-spec statuses() -> [active | archived].
statuses() ->
    ?STATUSES.

%% @doc 状态迁移裁决：V1 只允许 active -> archived。
%% 重复 archive（archived -> archived）由调用方按幂等语义处理（不进状态机）；
%% archived -> active（restore）不在 V1 API 范围，显式拒绝。
-spec transition(active | archived, active | archived) ->
    {ok, archived} | {error, {invalid_transition, active | archived, active | archived}}.
transition(active, archived) ->
    {ok, archived};
transition(From, To) ->
    {error, {invalid_transition, From, To}}.

%% @doc archive 前置裁决（幂等语义在调用方）：
%%   * 已 archived => {ok, idempotent}（重复 archive 返回当前状态，零写入）；
%%   * active      => {ok, proceed}（进入 archive 流程）。
-spec archive_decision(active | archived) -> {ok, proceed | idempotent} | {error, term()}.
archive_decision(active) ->
    {ok, proceed};
archive_decision(archived) ->
    {ok, idempotent};
archive_decision(Other) ->
    {error, {invalid_department_status, Other}}.

%% ===================================================================
%% 环检测（纯函数；DB 触发器是权威，这里是应用层快速失败）
%% ===================================================================

%% @doc self-parent 必拒（与 DB CHECK ck_organization_department_not_self_parent 同语义）。
-spec ensure_not_self_parent(term(), term()) -> ok | {error, {self_parent, term()}}.
ensure_not_self_parent(Id, Id) when Id =/= undefined ->
    {error, {self_parent, Id}};
ensure_not_self_parent(_Id, _ParentId) ->
    ok.

%% @doc 祖先环必拒：新父链（从新父向上到根的 id 列表）中不得出现自身。
%% AncestorIds 由基础设施层以递归 CTE（在行锁内）取回。
-spec ensure_not_in_ancestors(term(), [term()]) -> ok | {error, {cycle, term(), [term()]}}.
ensure_not_in_ancestors(Id, AncestorIds) when is_list(AncestorIds) ->
    case lists:member(Id, AncestorIds) of
        true -> {error, {cycle, Id, AncestorIds}};
        false -> ok
    end;
ensure_not_in_ancestors(Id, Other) ->
    {error, {cycle, Id, Other}}.

%% ===================================================================
%% 出站投影
%% ===================================================================

%% @doc 白名单投影：只放行 Keys 里声明的字段（未填值出站为 null）。
%% 目录读面不外泄 version/审计内部键以外的任何键（白名单外一个不出站）。
-spec project([atom()], map()) -> map().
project(Keys, Row) when is_list(Keys), is_map(Row) ->
    maps:from_list([
        {K, present(maps:get(K, Row, undefined))}
     || K <- Keys
    ]).

present(undefined) ->
    null;
present(Value) ->
    Value.
