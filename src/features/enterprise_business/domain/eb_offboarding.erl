%%% @doc 离职冻结、交接（offboarding）状态机与 rebind CAS 的领域纯函数。
%%%
%%% 依据：plan v4.1 EB-D07、§2.1 #5/#8、§4.1、§4.3、§5.4。
%%%
%%% 纯净性（铁律 4）：无 I/O、无进程、无隐式时间/随机源；`Now` 由调用方注入。
%%%
%%% 冻结的状态机（EB-D07）：
%%%
%%%     draft -> frozen -> transferring -> verifying -> completed
%%%                            |                |
%%%                            +--> failed <----+
%%%                                       |
%%%                                       +--> transferring   （重试）
%%%
%%% `completed` 是终态；不得跳过 `verifying`（完成证明是独立 Gate）。
%%% 同一 `(organization_id, leaver_user_id)` 同时最多一个未完成 case。
%%% rebind 必须走 CAS：`version` 不匹配返回 `stale_version`；成功时资源 id 与
%%% owner 逐字不变，只换 assignee。
-module(eb_offboarding).

-export([
    case_statuses/0,
    unfinished_statuses/0,
    valid_transition/2,
    transition/2,
    rebind/3,
    unfinished_case_unique/1,
    resume_after_failure/1
]).

-type case_status() ::
    draft | frozen | transferring | verifying | completed | failed.
-type item_status() :: pending | success | failed.
-type case_map() :: map().
-type item() :: map().
-type assignment() :: map().

-export_type([case_status/0, item_status/0, assignment/0]).

%% ===================================================================
%% 状态值域
%% ===================================================================

%% @doc offboarding case 的合法状态值域（声明顺序即 plan EB-D07 的顺序）。
-spec case_statuses() -> [case_status()].
case_statuses() ->
    [draft, frozen, transferring, verifying, completed, failed].

%% @doc 未完成状态集合 = 值域减去终态 `completed`。
-spec unfinished_statuses() -> [case_status()].
unfinished_statuses() ->
    [draft, frozen, transferring, verifying, failed].

%% ===================================================================
%% 状态迁移
%% ===================================================================

%% @doc 判定 case 状态迁移是否合法；非法迁移返回
%% `{error, {invalid_transition, From, To}}`。
-spec valid_transition(term(), term()) -> ok | {error, term()}.
valid_transition(draft, frozen) ->
    ok;
valid_transition(frozen, transferring) ->
    ok;
valid_transition(transferring, verifying) ->
    ok;
valid_transition(transferring, failed) ->
    ok;
valid_transition(verifying, completed) ->
    ok;
valid_transition(verifying, failed) ->
    ok;
valid_transition(failed, transferring) ->
    ok;
valid_transition(From, To) ->
    {error, {invalid_transition, From, To}}.

%% @doc `valid_transition/2` 的推进形式：合法时返回 `{ok, To}`。
-spec transition(term(), term()) -> {ok, term()} | {error, term()}.
transition(From, To) ->
    case valid_transition(From, To) of
        ok -> {ok, To};
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% rebind（CAS）
%% ===================================================================

%% @doc 以 CAS 把 identity 的 assignment 从 `from_user` rebind 到 `to_user`。
%%
%% `ExpectedVersion` 必须等于 assignment 当前 `version`，否则返回
%% `{error, {stale_version, ExpectedVersion, ActualVersion}}`——并发裁决由
%% application 把该结果翻译成 DB 级 CAS（铁律 7）。
%%
%% 成功时产出新的 assignment 描述：`organization_id`、`identity_id`、
%% `resource_id` 逐字不变（owner / 主键不变），只换 `user_id` 并推进 version。
%% 自交接（`to_user =:= from_user`）被拒绝。
-spec rebind(assignment(), integer(), integer()) -> {ok, assignment()} | {error, term()}.
rebind(Assignment, ExpectedVersion, Now) when is_map(Assignment) ->
    IdentityId = maps:get(identity_id, Assignment, undefined),
    FromUser = maps:get(from_user, Assignment, undefined),
    ToUser = maps:get(to_user, Assignment, undefined),
    ActualVersion = maps:get(version, Assignment, undefined),
    case
        {
            is_integer(IdentityId),
            is_integer(FromUser),
            is_integer(ToUser),
            is_integer(ActualVersion)
        }
    of
        {true, true, true, true} ->
            rebind_checked(Assignment, ExpectedVersion, Now, FromUser, ToUser, ActualVersion);
        _Invalid ->
            {error, invalid_assignment}
    end;
rebind(_NotAMap, _ExpectedVersion, _Now) ->
    {error, invalid_assignment}.

rebind_checked(Assignment, ExpectedVersion, Now, FromUser, ToUser, ActualVersion) ->
    case ToUser =:= FromUser of
        true ->
            {error, self_handover};
        false ->
            case ActualVersion =:= ExpectedVersion of
                false ->
                    {error, {stale_version, ExpectedVersion, ActualVersion}};
                true ->
                    {ok, #{
                        %% owner / 主键逐字不变
                        organization_id => maps:get(organization_id, Assignment, undefined),
                        identity_id => maps:get(identity_id, Assignment, undefined),
                        %% assignment 列名的同义镜像（CAS 输入键是 identity_id）
                        business_identity_id => maps:get(identity_id, Assignment, undefined),
                        resource_id => maps:get(
                            resource_id, Assignment, maps:get(identity_id, Assignment)
                        ),
                        %% 只换 assignee
                        user_id => ToUser,
                        status => active,
                        version => ActualVersion + 1,
                        assigned_at => Now,
                        ended_at => undefined
                    }}
            end
    end.

%% ===================================================================
%% 未完成 case 唯一性
%% ===================================================================

%% @doc 同一 `(organization_id, leaver_user_id)` 同时最多一个未完成 case。
-spec unfinished_case_unique([case_map()]) -> ok | {error, term()}.
unfinished_case_unique(Cases) when is_list(Cases) ->
    Unfinished = [C || C <- Cases, is_unfinished(C)],
    case first_duplicate_unfinished(Unfinished, #{}) of
        ok -> ok;
        {error, _} = Err -> Err
    end;
unfinished_case_unique(_NotAList) ->
    {error, invalid_cases}.

is_unfinished(Case) ->
    lists:member(maps:get(status, Case, undefined), unfinished_statuses()).

first_duplicate_unfinished([], _Seen) ->
    ok;
first_duplicate_unfinished([Case | Rest], Seen) ->
    Key = {
        maps:get(organization_id, Case, undefined),
        maps:get(leaver_user_id, Case, undefined)
    },
    case maps:is_key(Key, Seen) of
        true -> {error, {duplicate_unfinished_case, Key}};
        false -> first_duplicate_unfinished(Rest, Seen#{Key => true})
    end.

%% ===================================================================
%% 失败重试
%% ===================================================================

%% @doc 把 failed item 置回可重试状态，同时保留失败原因。
%%
%% 不变式：`idempotency_key` 逐字不变（重试不得产生第二条消息/审计），
%% `attempt` 递增，`reason` 保留并镜像到 `last_reason` 供查询。
-spec resume_after_failure(item()) -> {ok, item()} | {error, term()}.
resume_after_failure(#{status := failed} = Item) ->
    Reason = maps:get(reason, Item, undefined),
    Attempt = maps:get(attempt, Item, 0),
    {ok, Item#{
        status := pending,
        reason := Reason,
        last_reason => Reason,
        attempt := Attempt + 1
    }};
resume_after_failure(#{status := Status}) ->
    {error, {not_failed, Status}};
resume_after_failure(_NotAMap) ->
    {error, invalid_item}.
