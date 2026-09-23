-module(enterprise_group_logic).

%%%
% enterprise_group_logic 是 EPGZ-03 Workspace 企业群 use case（INT-04 创建 /
% INT-05 幂等添加成员 / INT-06 幂等移除成员）的业务逻辑层。
%
% 选型（checkpoint 冻结）：
%   * 企业群 = 现有 "group".scope='workspace' 群（migration 77 双体验设计），
%     不建新表、不加「应用创建」标记列——manifest INT-05/06 的 grant 是
%     "workspace scoped + group in workspace"，OA 可管理本 Org Workspace 内
%     全部 workspace 群，无 own-created 限制，故标记列无判定用途。
%   * owner 是 mapped Human（INT-04 owner_external_user_id，默认首个成员）：
%     group.owner_uid + role=4 群主成员行；OA 不可移除群 owner
%     （owner invariant），owner 转让走现有人类/管理端流程。
%   * 成员输入一律 external_user_id：添加时全部必须已由本 app 映射（active）
%     且是目标 Workspace 的 active workspace_member（Group Member ⊆
%     Workspace Member，trg_group_member_ws_subset DB 兜底 + 本层前置校验）；
%     移除时不要求仍是 workspace 成员（workspace 级联停用后的幂等清退）。
%   * 错误统一 {error, {ErrorCode, Detail}}（stable 13 码）：
%       - invalid_request          参数非法 / 非 workspace 成员 / owner 不可移除
%       - resource_not_found       workspace 或 group 不存在 / 跨 Org /
%                                  archived workspace / 个人群（不泄露存在性）
%       - identity_not_mapped      external 未映射（Detail 列出全部缺失项）
%%%

-export([
    create_group_tx/3,
    add_members_tx/4,
    remove_members_tx/4,
    group_detail_tx/3,
    update_group_tx/4,
    archive_group_tx/3,
    set_member_roles_tx/4,
    boundary_workspace_tx/3,
    list_groups_tx/3,
    list_members_tx/4
]).

-define(MAX_TITLE_LEN, 200).
-define(MAX_INTRODUCTION_LEN, 2000).
-define(MAX_MEMBERS, 200).

%% OA 可分配的角色集合（migration 00000001 group_member.role 注释：
%%   0 未定义 1 普通成员 2 嘉宾 3 管理员 4 群主 5 副群主）。
%% 4（群主）**不开放给 OA**——群主转让走人类/管理端流程；
%% 0（未定义）不是有效角色。显式枚举即白名单（表外值一律 invalid_request）。
-define(ASSIGNABLE_ROLES, [1, 2, 3, 5]).

%% 群详情面成员列表硬上限（与 repo ?DETAIL_MEMBER_LIMIT 同源；超过即拒）。
-define(MAX_DETAIL_MEMBERS, 200).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc INT-04：创建 Workspace 企业群（成员用 external_user_id 列表）。
%% Input（map）：
%%   workspace_id            必填 integer
%%   title                   必填 binary 1..200
%%   members                 必填 [binary] 1..200 项（自动去重，去重后非空）
%%   owner_external_user_id  可选 binary，缺省取首个成员；必须在 members 内
%%   introduction            可选 binary ≤2000（缺省空串）
%% 返回 {ok, #{group_id, workspace_id, owner_user_id, member_count}}。
%% 业务失败返回 {error, {Code, Detail}}（调用方事务回滚）。
-spec create_group_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
create_group_tx(Conn, Ctx, Input) when is_map(Input) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    case parse_create_input(Input) of
        {ok, Creator} ->
            create_in_workspace(Conn, OrgId, AppId, Creator);
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end;
create_group_tx(_Conn, _Ctx, _Other) ->
    {error, {<<"invalid_request">>, input_not_map}}.

%% @doc INT-05：向企业群幂等添加已映射成员。
%% 已在群（active）→ 计入 already_member，不重复开世代；重复调用同结果。
-spec add_members_tx(any(), map(), integer(), [binary()]) ->
    {ok, map()} | {error, {binary(), term()}}.
add_members_tx(Conn, Ctx, GroupId, ExternalIds) ->
    mutate_members(Conn, Ctx, GroupId, ExternalIds, add).

%% @doc INT-06：从企业群幂等移除成员；不可破坏 owner invariant
%% （群 owner（owner_uid）不可被 OA 移除；移除后必须仍剩 active 群主）。
%% 已不在群（status<>1）→ 计入 already_absent，重复调用同结果。
-spec remove_members_tx(any(), map(), integer(), [binary()]) ->
    {ok, map()} | {error, {binary(), term()}}.
remove_members_tx(Conn, Ctx, GroupId, ExternalIds) ->
    mutate_members(Conn, Ctx, GroupId, ExternalIds, remove).

%%%===================================================================
%%% FULL-02 生命周期（详情 / 更新 / 归档 / 成员角色）
%%%===================================================================

%% @doc 群详情：Workspace 边界定位（resource_not_found 语义同 INT-05/06）+
%% Application 归属 + 成员列表（最小字段 + 硬上限）。
%% 返回 {ok, #{group_id, workspace_id, title, introduction, owner_user_id,
%% member_count, members := [#{user_id, role}], origin := #{owner_app, mine}
%% | null}}；成员行超过上限 → {error, {invalid_request, members_over_limit}}
%% （绝不静默截断）。
-spec group_detail_tx(any(), map(), integer()) ->
    {ok, map()} | {error, {binary(), term()}}.
group_detail_tx(Conn, Ctx, GroupId) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    case locate_group(Conn, OrgId, GroupId) of
        {ok, Group} ->
            Gid = maps:get(<<"id">>, Group),
            case enterprise_group_repo:active_member_rows_tx(Conn, Gid) of
                {ok, Rows} when length(Rows) =< ?MAX_DETAIL_MEMBERS ->
                    case origin_view(Conn, Gid, OrgId, AppId) of
                        {ok, Origin} ->
                            {ok, #{
                                <<"group_id">> => Gid,
                                <<"workspace_id">> => maps:get(<<"workspace_id">>, Group),
                                <<"title">> => maps:get(<<"title">>, Group, <<>>),
                                <<"owner_user_id">> => maps:get(<<"owner_uid">>, Group),
                                <<"member_count">> => maps:get(<<"member_count">>, Group, 0),
                                <<"members">> => [member_view(R) || R <- Rows],
                                <<"origin">> => Origin
                            }};
                        {error, _} = Err ->
                            Err
                    end;
                {ok, _TooMany} ->
                    {error, {<<"invalid_request">>, members_over_limit}};
                {error, too_many_members} ->
                    {error, {<<"invalid_request">>, members_over_limit}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 群更新（标题/简介；成员输入用 external_user_id，与 INT-05/06 一致）。
%% Input（atom 键，至少一项）：title（1..200）/ introduction（<=2000）。
%% 空更新 → invalid_request（不产生无意义写入与审计噪音）。
-spec update_group_tx(any(), map(), integer(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
update_group_tx(Conn, Ctx, GroupId, Input) when is_map(Input) ->
    OrgId = maps:get(organization_id, Ctx),
    case parse_update_input(Input) of
        {ok, Title, Intro} ->
            case locate_group(Conn, OrgId, GroupId) of
                {ok, Group} ->
                    Gid = maps:get(<<"id">>, Group),
                    %% 未给出的字段保持原值（部分更新；不把缺省写成空串）。
                    NewTitle = default(Title, maps:get(<<"title">>, Group, <<>>)),
                    NewIntro = default(Intro, maps:get(<<"introduction">>, Group, <<>>)),
                    case
                        enterprise_group_repo:update_group_meta_tx(
                            Conn, Gid, NewTitle, NewIntro
                        )
                    of
                        ok ->
                            {ok, #{
                                <<"group_id">> => Gid,
                                <<"title">> => NewTitle,
                                <<"introduction">> => NewIntro,
                                <<"updated">> => true
                            }};
                        {error, not_found} ->
                            {error, {<<"resource_not_found">>, group_not_found}};
                        {error, Reason} ->
                            {error, {<<"internal_error">>, Reason}}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end;
update_group_tx(_Conn, _Ctx, _GroupId, _Input) ->
    {error, {<<"invalid_request">>, input_not_map}}.

%% @doc 群归档（生命周期终点）：group.status -> 0（禁用），归属行 -> archived。
%% 归档后该群对全部 internal 路径不可见（find_group_in_org_tx 恒过滤 status=1）
%% ——消息发送/成员增删随之 fail-closed。
%% Application membership 边界：**OA 只能归档本 Application 建立的企业群**
%% （归属行 application_id 必须等于本 app）；无归属行（人类自建群）或他人归属
%% → {error, {invalid_request, not_group_owner_application}}。
%% 幂等：已归档（status=0 且归属 archived）→ {ok, #{archived => true, already => true}}。
-spec archive_group_tx(any(), map(), integer()) ->
    {ok, map()} | {error, {binary(), term()}}.
archive_group_tx(Conn, Ctx, GroupId) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    case enterprise_group_repo:find_group_in_org_any_tx(Conn, OrgId, GroupId) of
        {ok, Group} ->
            Gid = maps:get(<<"id">>, Group),
            case own_origin(Conn, Gid, OrgId, AppId) of
                ok ->
                    do_archive(Conn, Group, Gid);
                {error, _} = Err ->
                    Err
            end;
        {error, not_found} ->
            {error, {<<"resource_not_found">>, group_not_found}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

-spec do_archive(any(), map(), pos_integer()) -> {ok, map()} | {error, {binary(), term()}}.
do_archive(Conn, Group, Gid) ->
    case maps:get(<<"status">>, Group) of
        1 ->
            case enterprise_group_repo:set_group_status_tx(Conn, Gid, 0) of
                ok ->
                    case enterprise_group_origin_repo:archive_tx(Conn, Gid) of
                        {ok, _} ->
                            {ok, #{
                                <<"group_id">> => Gid,
                                <<"archived">> => true,
                                <<"already">> => false
                            }};
                        {error, Reason} ->
                            {error, {<<"internal_error">>, Reason}}
                    end;
                {error, not_found} ->
                    {error, {<<"resource_not_found">>, group_not_found}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        _AlreadyArchived ->
            case enterprise_group_origin_repo:archive_tx(Conn, Gid) of
                {ok, _} ->
                    {ok, #{
                        <<"group_id">> => Gid, <<"archived">> => true, <<"already">> => true
                    }};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end
    end.

%% @doc 成员角色管理（幂等）：Input #{roles => [#{external_user_id, role}]}。
%%   * role 必须 ∈ {1,2,3,5}（4=群主不可经 OA 分配，0 未定义非法）；
%%   * 目标必须已由本 app 映射（active）且是**本群 active 成员**
%%     （不在群内 → invalid_request not_member；未映射 → identity_not_mapped）；
%%   * 不允许改群主（owner_uid）的角色（owner invariant 的一部分）：
%%     角色分配后 active 群主（role=4）计数不得变化——owner_uid 行的角色改动
%%     一律拒（invalid_request cannot_change_owner_role）。
%% 返回 {ok, #{group_id, updated := N, unchanged := M, members := [...]}}。
-spec set_member_roles_tx(any(), map(), integer(), term()) ->
    {ok, map()} | {error, {binary(), term()}}.
set_member_roles_tx(Conn, Ctx, GroupId, Roles) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    case parse_roles_input(Roles) of
        {ok, Wanted} ->
            case locate_group(Conn, OrgId, GroupId) of
                {ok, Group} ->
                    Gid = maps:get(<<"id">>, Group),
                    OwnerUid = maps:get(<<"owner_uid">>, Group),
                    case resolve_all_mapped(Conn, OrgId, AppId, [E || {E, _} <- Wanted]) of
                        {ok, ExtToUid} ->
                            case check_role_targets(Conn, Gid, OwnerUid, Wanted, ExtToUid) of
                                ok ->
                                    apply_roles(Conn, Gid, Wanted, ExtToUid);
                                {error, _} = Err ->
                                    Err
                            end;
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end.

%% ===================================================================
%% FULL-02 internals
%% ===================================================================

-spec member_view(map()) -> map().
member_view(R) ->
    #{
        <<"user_id">> => maps:get(<<"user_id">>, R),
        <<"role">> => maps:get(<<"role">>, R),
        <<"status">> => maps:get(<<"status">>, R)
    }.

%% @doc 归属视图：null（无归属行 = 人类自建群）或
%% #{application_id, workspace_id, status, mine}。
-spec origin_view(any(), pos_integer(), integer(), integer()) ->
    {ok, null | map()} | {error, term()}.
origin_view(Conn, Gid, _OrgId, AppId) ->
    case enterprise_group_origin_repo:find_tx(Conn, Gid) of
        {ok, Row} ->
            OwnerApp = maps:get(<<"application_id">>, Row),
            {ok, #{
                <<"application_id">> => OwnerApp,
                <<"workspace_id">> => maps:get(<<"workspace_id">>, Row),
                <<"status">> => maps:get(<<"status">>, Row),
                <<"mine">> => OwnerApp =:= AppId
            }};
        {error, not_found} ->
            {ok, null};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

-spec own_origin(any(), pos_integer(), integer(), integer()) -> ok | {error, {binary(), term()}}.
own_origin(Conn, Gid, OrgId, AppId) ->
    case enterprise_group_origin_repo:owns_tx(Conn, Gid, OrgId, AppId) of
        {ok, true} ->
            ok;
        {ok, false} ->
            {error, {<<"invalid_request">>, not_group_owner_application}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

-spec parse_update_input(map()) ->
    {ok, undefined | binary(), undefined | binary()} | {error, term()}.
parse_update_input(Input) ->
    Title = maps:get(title, Input, undefined),
    Intro = maps:get(introduction, Input, undefined),
    case {Title, Intro} of
        {undefined, undefined} ->
            {error, empty_update};
        _ ->
            case valid_title(Title) andalso valid_introduction(Intro) of
                true -> {ok, Title, Intro};
                false -> {error, invalid_group_meta}
            end
    end.

-spec valid_title(undefined | binary()) -> boolean().
valid_title(undefined) ->
    true;
valid_title(T) when is_binary(T) ->
    byte_size(T) > 0 andalso byte_size(T) =< ?MAX_TITLE_LEN;
valid_title(_) ->
    false.

-spec valid_introduction(undefined | binary()) -> boolean().
valid_introduction(undefined) ->
    true;
valid_introduction(I) when is_binary(I) ->
    byte_size(I) =< ?MAX_INTRODUCTION_LEN;
valid_introduction(_) ->
    false.

-spec default(undefined | T, T) -> T.
default(undefined, Default) ->
    Default;
default(Value, _Default) ->
    Value.

-spec parse_roles_input(term()) ->
    {ok, [{binary(), integer()}]} | {error, term()}.
parse_roles_input(Roles) when is_list(Roles), length(Roles) > 0 ->
    case length(Roles) =< ?MAX_MEMBERS of
        false ->
            {error, too_many_roles};
        true ->
            parse_roles_each(Roles, [])
    end;
parse_roles_input(Roles) when is_list(Roles) ->
    {error, empty_roles};
parse_roles_input(_) ->
    {error, roles_not_list}.

-spec parse_roles_each([term()], [{binary(), integer()}]) ->
    {ok, [{binary(), integer()}]} | {error, term()}.
parse_roles_each([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_roles_each([#{external_user_id := Ext, role := Role} | Rest], Acc) when
    is_binary(Ext), Ext =/= <<>>, is_integer(Role)
->
    case lists:member(Role, ?ASSIGNABLE_ROLES) of
        true -> parse_roles_each(Rest, [{Ext, Role} | Acc]);
        false -> {error, {role_not_assignable, Role}}
    end;
parse_roles_each(_Bad, _Acc) ->
    {error, invalid_role_entry}.

%% @doc 角色目标前置（逐项）：
%%   * 目标不能是群 owner（owner_uid）→ cannot_change_owner_role；
%%   * 目标必须是本群 active 成员 → not_member。
-spec check_role_targets(any(), pos_integer(), integer(), [{binary(), integer()}], map()) ->
    ok | {error, {binary(), term()}}.
check_role_targets(_Conn, _Gid, _OwnerUid, [], _ExtToUid) ->
    ok;
check_role_targets(Conn, Gid, OwnerUid, [{Ext, _Role} | Rest], ExtToUid) ->
    Uid = maps:get(Ext, ExtToUid),
    case Uid =:= OwnerUid of
        true ->
            {error, {<<"invalid_request">>, cannot_change_owner_role}};
        false ->
            case enterprise_group_repo:group_member_status_tx(Conn, Gid, Uid) of
                {ok, #{<<"status">> := 1}} ->
                    check_role_targets(Conn, Gid, OwnerUid, Rest, ExtToUid);
                {ok, _Inactive} ->
                    {error, {<<"invalid_request">>, {not_member, Ext}}};
                {error, not_found} ->
                    {error, {<<"invalid_request">>, {not_member, Ext}}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end
    end.

-spec apply_roles(any(), pos_integer(), [{binary(), integer()}], map()) ->
    {ok, map()} | {error, {binary(), term()}}.
apply_roles(Conn, Gid, Wanted, ExtToUid) ->
    fold_roles(Conn, Gid, Wanted, ExtToUid, {0, 0, []}).

-spec fold_roles(
    any(),
    pos_integer(),
    [{binary(), integer()}],
    map(),
    {integer(), integer(), [
        map()
    ]}
) ->
    {ok, map()} | {error, {binary(), term()}}.
fold_roles(_Conn, Gid, [], _ExtToUid, {Updated, Unchanged, Acc}) ->
    {ok, #{
        <<"group_id">> => Gid,
        <<"updated">> => Updated,
        <<"unchanged">> => Unchanged,
        <<"members">> => lists:reverse(Acc)
    }};
fold_roles(Conn, Gid, [{Ext, Role} | Rest], ExtToUid, {Updated, Unchanged, Acc}) ->
    Uid = maps:get(Ext, ExtToUid),
    case enterprise_group_repo:set_member_role_tx(Conn, Gid, Uid, Role) of
        {ok, OldRole} ->
            Item = #{
                <<"external_user_id">> => Ext,
                <<"user_id">> => Uid,
                <<"role">> => Role,
                <<"previous_role">> => OldRole
            },
            case OldRole =:= Role of
                true ->
                    fold_roles(Conn, Gid, Rest, ExtToUid, {Updated, Unchanged + 1, [Item | Acc]});
                false ->
                    fold_roles(Conn, Gid, Rest, ExtToUid, {Updated + 1, Unchanged, [Item | Acc]})
            end;
        {error, not_found} ->
            %% 前置已判成员身份；此处是并发退群的终局兜底。
            {error, {<<"invalid_request">>, {not_member, Ext}}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% @doc 边界定位（FULL-02 handler 接线用）：给出该群**所属 workspace_id**，
%% 供 handler 在同一事务内调用 enterprise_internal_boundary:enforce/4 做 Grant
%% 资源边界判定。可见性语义与 INT-05/06 完全一致（locate_group/3）：跨 Org /
%% 个人群 / 已归档群一律 {error, {resource_not_found, _}}，不泄露存在性。
-spec boundary_workspace_tx(any(), map(), integer()) ->
    {ok, pos_integer()} | {error, {binary(), term()}}.
boundary_workspace_tx(Conn, Ctx, GroupId) ->
    case locate_group(Conn, maps:get(organization_id, Ctx), GroupId) of
        {ok, Group} -> {ok, maps:get(<<"workspace_id">>, Group)};
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% INT-04 internals
%% ===================================================================

-spec create_in_workspace(any(), integer(), integer(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
create_in_workspace(Conn, OrgId, AppId, #{
    introduction := Intro,
    members := Members0,
    owner_ext := OwnerExt,
    title := Title,
    workspace_id := WsId
}) ->
    case enterprise_group_repo:find_workspace_in_org_tx(Conn, OrgId, WsId) of
        {ok, #{<<"status">> := <<"active">>}} ->
            Members = lists:usort(Members0),
            case resolve_all_mapped(Conn, OrgId, AppId, Members) of
                {ok, ExtToUid} ->
                    Uids = [maps:get(M, ExtToUid) || M <- Members],
                    case enterprise_group_repo:non_ws_member_uids_tx(Conn, WsId, Uids) of
                        {ok, []} ->
                            insert_group_with_members(
                                Conn,
                                OrgId,
                                AppId,
                                WsId,
                                Title,
                                Intro,
                                Members,
                                OwnerExt,
                                ExtToUid
                            );
                        {ok, NonMembers} ->
                            BadExt = [
                                M
                             || M <- Members,
                                lists:member(maps:get(M, ExtToUid), NonMembers)
                            ],
                            {error, {<<"invalid_request">>, {not_workspace_members, BadExt}}};
                        {error, Reason} ->
                            {error, {<<"internal_error">>, Reason}}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {ok, _} ->
            {error, {<<"resource_not_found">>, workspace_not_active}};
        {error, not_found} ->
            {error, {<<"resource_not_found">>, workspace_not_found}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

-spec insert_group_with_members(
    any(),
    integer(),
    integer(),
    integer(),
    binary(),
    binary(),
    [binary()],
    binary(),
    map()
) ->
    {ok, map()} | {error, {binary(), term()}}.
insert_group_with_members(Conn, OrgId, AppId, WsId, Title, Intro, Members, OwnerExt, ExtToUid) ->
    OwnerUid = maps:get(OwnerExt, ExtToUid),
    Gid = enterprise_group_repo:next_group_id(),
    case
        enterprise_group_repo:create_workspace_group_tx(
            Conn, Gid, OwnerUid, Title, Intro, WsId, elib_dt:now()
        )
    of
        {ok, _GroupRow} ->
            UidRoles =
                [{OwnerUid, 4}] ++
                    [{maps:get(M, ExtToUid), 1} || M <- Members, M =/= OwnerExt],
            case activate_one_by_one(Conn, Gid, UidRoles, ok) of
                ok ->
                    %% Application membership（FULL-02）：建群同事务登记归属行
                    %% （哪份 Application 在哪个 Org/Workspace 建立该企业群）。
                    %% 归属行禁止物理删除、归档单向（migration 00000140 触发器）。
                    case enterprise_group_origin_repo:insert_tx(Conn, Gid, OrgId, AppId, WsId) of
                        {ok, _} ->
                            {ok, #{
                                <<"group_id">> => Gid,
                                <<"workspace_id">> => WsId,
                                <<"owner_user_id">> => OwnerUid,
                                <<"member_count">> => length(UidRoles),
                                <<"origin_application_id">> => AppId
                            }};
                        {error, Reason} ->
                            {error, {<<"internal_error">>, {group_origin, Reason}}}
                    end;
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% @doc 顺序激活（owner 先行）；任一失败返回错误（调用方回滚整个事务）。
-spec activate_one_by_one(any(), pos_integer(), [{integer(), integer()}], ok | {error, term()}) ->
    ok | {error, term()}.
activate_one_by_one(_Conn, _Gid, _Rest, {error, _} = Err) ->
    Err;
activate_one_by_one(_Conn, _Gid, [], ok) ->
    ok;
activate_one_by_one(Conn, Gid, [{Uid, Role} | Rest], ok) ->
    case enterprise_group_repo:activate_member_tx(Conn, Gid, Uid, Role, <<"enterprise_oa">>) of
        {ok, _} ->
            activate_one_by_one(Conn, Gid, Rest, ok);
        {error, Reason} ->
            activate_one_by_one(Conn, Gid, Rest, {error, {Uid, Reason}})
    end.

%% ===================================================================
%% INT-05 / INT-06 internals
%% ===================================================================

-spec mutate_members(any(), map(), integer(), [binary()], add | remove) ->
    {ok, map()} | {error, {binary(), term()}}.
mutate_members(Conn, Ctx, GroupId, ExternalIds, Op) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    case locate_group(Conn, OrgId, GroupId) of
        {ok, Group} ->
            case parse_member_list(ExternalIds) of
                {ok, Members} ->
                    mutate_checked(Conn, OrgId, AppId, Group, lists:usort(Members), Op);
                {error, Detail} ->
                    {error, {<<"invalid_request">>, Detail}}
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 群定位 + 边界：不存在/跨 Org/personal/disabled/ws archived 统一
%% resource_not_found（不泄露存在性）。
-spec locate_group(any(), integer(), integer()) -> {ok, map()} | {error, {binary(), term()}}.
locate_group(Conn, OrgId, GroupId) when is_integer(GroupId), GroupId > 0 ->
    case enterprise_group_repo:find_group_in_org_tx(Conn, OrgId, GroupId) of
        {ok, #{<<"ws_status">> := <<"active">>} = Group} ->
            {ok, Group};
        {ok, _} ->
            {error, {<<"resource_not_found">>, workspace_not_active}};
        {error, not_found} ->
            {error, {<<"resource_not_found">>, group_not_found}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end;
locate_group(_Conn, _OrgId, _GroupId) ->
    {error, {<<"invalid_request">>, invalid_group_id}}.

-spec mutate_checked(any(), integer(), integer(), map(), [binary()], add | remove) ->
    {ok, map()} | {error, {binary(), term()}}.
mutate_checked(Conn, OrgId, AppId, Group, Members, Op) ->
    Gid = maps:get(<<"id">>, Group),
    case resolve_all_mapped(Conn, OrgId, AppId, Members) of
        {ok, ExtToUid} ->
            case precheck(Conn, Group, Members, ExtToUid, Op) of
                ok ->
                    fold_one_by_one(Conn, Gid, Members, ExtToUid, Op, {0, 0});
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 操作前置：add 要求全部为 workspace active 成员（DB 子集触发器同源）；
%% remove 只做 owner invariant（不要求仍是 workspace 成员）。
-spec precheck(any(), map(), [binary()], map(), add | remove) ->
    ok | {error, {binary(), term()}}.
precheck(Conn, Group, Members, ExtToUid, add) ->
    WsId = maps:get(<<"workspace_id">>, Group),
    Uids = [maps:get(M, ExtToUid) || M <- Members],
    case enterprise_group_repo:non_ws_member_uids_tx(Conn, WsId, Uids) of
        {ok, []} ->
            ok;
        {ok, NonMembers} ->
            BadExt = [M || M <- Members, lists:member(maps:get(M, ExtToUid), NonMembers)],
            {error, {<<"invalid_request">>, {not_workspace_members, BadExt}}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end;
precheck(Conn, Group, Members, ExtToUid, remove) ->
    RemoveUids = [maps:get(M, ExtToUid) || M <- Members],
    owner_invariant(Conn, Group, RemoveUids).

-spec fold_one_by_one(
    any(), pos_integer(), [binary()], map(), add | remove, {integer(), integer()}
) ->
    {ok, map()} | {error, {binary(), term()}}.
fold_one_by_one(Conn, Gid, [Ext], ExtToUid, Op, {Changed, Already}) ->
    Uid = maps:get(Ext, ExtToUid),
    case apply_member_op(Conn, Gid, Uid, Op) of
        {ok, true} ->
            finish_fold(Conn, Gid, Op, Changed + 1, Already);
        {ok, false} ->
            finish_fold(Conn, Gid, Op, Changed, Already + 1);
        {error, Reason} ->
            {error, {<<"internal_error">>, {Ext, Reason}}}
    end;
fold_one_by_one(Conn, Gid, [Ext | Rest], ExtToUid, Op, {Changed, Already}) ->
    Uid = maps:get(Ext, ExtToUid),
    case apply_member_op(Conn, Gid, Uid, Op) of
        {ok, true} ->
            fold_one_by_one(Conn, Gid, Rest, ExtToUid, Op, {Changed + 1, Already});
        {ok, false} ->
            fold_one_by_one(Conn, Gid, Rest, ExtToUid, Op, {Changed, Already + 1});
        {error, Reason} ->
            {error, {<<"internal_error">>, {Ext, Reason}}}
    end.

-spec apply_member_op(any(), pos_integer(), integer(), add | remove) ->
    {ok, boolean()} | {error, term()}.
apply_member_op(Conn, Gid, Uid, add) ->
    enterprise_group_repo:activate_member_tx(Conn, Gid, Uid, 1, <<"enterprise_oa">>);
apply_member_op(Conn, Gid, Uid, remove) ->
    enterprise_group_repo:deactivate_member_tx(Conn, Gid, Uid, <<"enterprise_oa_remove">>).

-spec finish_fold(any(), pos_integer(), add | remove, integer(), integer()) ->
    {ok, map()} | {error, {binary(), term()}}.
finish_fold(Conn, Gid, Op, Changed, Already) ->
    case enterprise_group_repo:refresh_member_count_tx(Conn, Gid) of
        {ok, Count} ->
            Base = #{<<"group_id">> => Gid, <<"member_count">> => Count},
            {ok,
                case Op of
                    add ->
                        Base#{<<"added">> => Changed, <<"already_member">> => Already};
                    remove ->
                        Base#{<<"removed">> => Changed, <<"already_absent">> => Already}
                end};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% @doc owner invariant：
%%   1. 群 owner（group.owner_uid）在移除集合内 → 拒（OA 不可移除 owner）；
%%   2. 移除后 active 群主（role=4, status=1）数量为 0 → 拒（最后一个群主）。
-spec owner_invariant(any(), map(), [integer()]) -> ok | {error, {binary(), term()}}.
owner_invariant(Conn, Group, RemoveUids) ->
    OwnerUid = maps:get(<<"owner_uid">>, Group),
    Gid = maps:get(<<"id">>, Group),
    case lists:member(OwnerUid, RemoveUids) of
        true ->
            {error, {<<"invalid_request">>, cannot_remove_group_owner}};
        false ->
            case enterprise_group_repo:count_active_owners_tx(Conn, Gid) of
                {ok, Owners} ->
                    RemovingOwners = count_removing_owners(Conn, Gid, RemoveUids),
                    case Owners - RemovingOwners of
                        N when N > 0 ->
                            ok;
                        _ ->
                            {error, {<<"invalid_request">>, last_owner_not_removable}}
                    end;
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end
    end.

-spec count_removing_owners(any(), integer(), [integer()]) -> non_neg_integer().
count_removing_owners(Conn, Gid, RemoveUids) ->
    lists:foldl(
        fun(Uid, Acc) ->
            case enterprise_group_repo:group_member_status_tx(Conn, Gid, Uid) of
                {ok, #{<<"role">> := 4, <<"status">> := 1}} ->
                    Acc + 1;
                _ ->
                    Acc
            end
        end,
        0,
        RemoveUids
    ).

%% ===================================================================
%% 校验与解析
%% ===================================================================

-spec parse_create_input(map()) -> {ok, map()} | {error, term()}.
parse_create_input(Input) ->
    WsId = maps:get(workspace_id, Input, undefined),
    Title = maps:get(title, Input, undefined),
    Members = maps:get(members, Input, undefined),
    OwnerExt0 = maps:get(owner_external_user_id, Input, undefined),
    Intro = maps:get(introduction, Input, <<>>),
    case is_integer(WsId) andalso WsId > 0 of
        false ->
            {error, invalid_workspace_id};
        true ->
            case
                is_binary(Title) andalso
                    byte_size(Title) > 0 andalso
                    byte_size(Title) =< ?MAX_TITLE_LEN
            of
                false ->
                    {error, invalid_title};
                true ->
                    case validate_member_batch(Members) of
                        ok ->
                            OwnerExt =
                                case OwnerExt0 of
                                    undefined -> hd(Members);
                                    _ -> OwnerExt0
                                end,
                            check_owner_and_intro(OwnerExt, Members, Intro, WsId, Title)
                    end
            end
    end.

-spec check_owner_and_intro(binary(), [binary()], binary(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
check_owner_and_intro(OwnerExt, Members, Intro, WsId, Title) ->
    case lists:member(OwnerExt, Members) of
        false ->
            {error, owner_not_in_members};
        true ->
            case is_binary(Intro) andalso byte_size(Intro) =< ?MAX_INTRODUCTION_LEN of
                true ->
                    {ok, #{
                        workspace_id => WsId,
                        title => Title,
                        members => Members,
                        owner_ext => OwnerExt,
                        introduction => Intro
                    }};
                false ->
                    {error, invalid_introduction}
            end
    end.

-spec parse_member_list(term()) -> {ok, [binary()]} | {error, term()}.
parse_member_list(Members) when is_list(Members), length(Members) > 0 ->
    case validate_member_batch(Members) of
        ok ->
            {ok, Members};
        {error, _} = Err ->
            Err
    end;
parse_member_list(Members) when is_list(Members) ->
    {error, empty_members};
parse_member_list(_) ->
    {error, members_not_list}.

-spec validate_member_batch(term()) -> ok | {error, term()}.
validate_member_batch(Members) when is_list(Members) ->
    case length(Members) of
        0 ->
            {error, empty_members};
        N when N > ?MAX_MEMBERS ->
            {error, too_many_members};
        _ ->
            validate_each_external(Members)
    end;
validate_member_batch(_) ->
    {error, members_not_list}.

-spec validate_each_external([binary()]) -> ok | {error, term()}.
validate_each_external([]) ->
    ok;
validate_each_external([E | Rest]) ->
    case is_binary(E) andalso byte_size(E) > 0 andalso byte_size(E) =< 256 of
        true ->
            validate_each_external(Rest);
        false ->
            {error, invalid_external_id}
    end.

%% @doc 批量解析 external -> uid；任一未映射即 identity_not_mapped（列出全部缺失）。
-spec resolve_all_mapped(any(), integer(), integer(), [binary()]) ->
    {ok, map()} | {error, {binary(), term()}}.
resolve_all_mapped(Conn, OrgId, AppId, Members) ->
    case enterprise_external_identity_repo:resolve_tx(Conn, OrgId, AppId, Members) of
        {ok, Rows} ->
            Mapped =
                #{
                    maps:get(<<"external_user_id">>, Row) => maps:get(<<"user_id">>, Row)
                 || Row <- Rows
                },
            Missing = [M || M <- Members, not maps:is_key(M, Mapped)],
            case Missing of
                [] ->
                    {ok, Mapped};
                _ ->
                    {error, {<<"identity_not_mapped">>, Missing}}
            end;
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% ===================================================================
%% V2.1 Internal 只读面（INT-26/27）
%% ===================================================================

%% @doc INT-26：本 Application origin 建立的企业群 keyset 列表
%% （active workspace + scope=workspace + Grant 覆盖 W 收窄在 repo SQL 内；
%% family=groups，filter 冻结为空 map——origin app 绑定已由 payload 顶层
%% application_id 承担）。信封 {items, limit, has_more, next_cursor}。
%% 投影冻结：group_id(int64), workspace_id(int64), title(string),
%% member_count(integer), created_at(RFC3339 string)。
-spec list_groups_tx(any(), map(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
list_groups_tx(Conn, Ctx, Opts) when is_map(Opts) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    Limit = maps:get(limit, Opts, 50),
    Cursor = maps:get(cursor, Opts, undefined),
    Filter = #{},
    case enterprise_internal_read_page:resolve(Cursor, Ctx, <<"groups">>, Filter) of
        {ok, Pivot} ->
            case
                enterprise_group_repo:internal_origin_page_tx(
                    Conn, OrgId, AppId, Pivot, Limit + 1
                )
            of
                {ok, Rows0} when length(Rows0) > Limit ->
                    group_page_reply(
                        Conn, Ctx, lists:sublist(Rows0, Limit), Limit, true
                    );
                {ok, Rows} ->
                    group_page_reply(Conn, Ctx, Rows, Limit, false);
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, invalid_request} ->
            {error, {<<"invalid_request">>, cursor_invalid}};
        {error, security_gate_closed} ->
            {error, {<<"security_gate_closed">>, cursor_signing_key_unavailable}}
    end;
list_groups_tx(_Conn, _Ctx, _Opts) ->
    {error, {<<"invalid_request">>, opts_not_map}}.

%% @doc INT-27：群 active 成员 keyset 列表（created_at ASC, id ASC——§10.2
%% 唯一升序 family）。群定位与 Grant 覆盖判定由 handler 先行
%% （boundary_workspace_tx + enforce INT-27）；本函数只做成员分页。
%% cursor filter 冻结为 #{group_id => G}（游标跨群使用即拒绝）。
%% 投影冻结：user_id(int64), role(integer 0..5), created_at(RFC3339 string)。
-spec list_members_tx(any(), map(), integer(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
list_members_tx(Conn, Ctx, GroupId, Opts) when
    is_integer(GroupId), GroupId > 0, is_map(Opts)
->
    Limit = maps:get(limit, Opts, 50),
    Cursor = maps:get(cursor, Opts, undefined),
    Filter = #{<<"group_id">> => GroupId},
    case enterprise_internal_read_page:resolve(Cursor, Ctx, <<"group_members">>, Filter) of
        {ok, Pivot} ->
            case enterprise_group_repo:internal_member_page_tx(Conn, GroupId, Pivot, Limit + 1) of
                {ok, Rows0} when length(Rows0) > Limit ->
                    member_page_reply(
                        Conn, Ctx, GroupId, lists:sublist(Rows0, Limit), Limit, true
                    );
                {ok, Rows} ->
                    member_page_reply(Conn, Ctx, GroupId, Rows, Limit, false);
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, invalid_request} ->
            {error, {<<"invalid_request">>, cursor_invalid}};
        {error, security_gate_closed} ->
            {error, {<<"security_gate_closed">>, cursor_signing_key_unavailable}}
    end;
list_members_tx(_Conn, _Ctx, _GroupId, _Opts) ->
    {error, {<<"invalid_request">>, invalid_list_input}}.

-spec group_view(map()) -> map().
group_view(Row) ->
    #{
        <<"group_id">> => maps:get(<<"id">>, Row),
        <<"workspace_id">> => maps:get(<<"workspace_id">>, Row),
        <<"title">> => maps:get(<<"title">>, Row),
        <<"member_count">> => maps:get(<<"member_count">>, Row),
        <<"created_at">> => maps:get(<<"created_at">>, Row)
    }.

-spec member_view_page(map()) -> map().
member_view_page(Row) ->
    #{
        <<"user_id">> => maps:get(<<"user_id">>, Row),
        <<"role">> => maps:get(<<"role">>, Row),
        <<"created_at">> => maps:get(<<"created_at">>, Row)
    }.

-spec group_page_reply(any(), map(), [map()], pos_integer(), boolean()) ->
    {ok, map()} | {error, {binary(), term()}}.
group_page_reply(_Conn, Ctx, Rows, Limit, HasMore) ->
    Next =
        case HasMore andalso Rows =/= [] of
            true ->
                Last = lists:last(Rows),
                Tuple = {maps:get(<<"created_at">>, Last), maps:get(<<"id">>, Last)},
                case enterprise_internal_read_page:encode(Ctx, <<"groups">>, #{}, Tuple) of
                    {ok, Cursor} -> Cursor;
                    {error, EncodeReason} -> erlang:error({cursor_encode_failed, EncodeReason})
                end;
            false ->
                null
        end,
    %% A-R：结构化访问日志在 handler；usage 计数面待 ck_eau_metric 扩展迁移
    %%（A0 分配号，proposal 见 A2 RESULT）。
    {ok, #{
        <<"items">> => [group_view(R) || R <- Rows],
        <<"limit">> => Limit,
        <<"has_more">> => HasMore,
        <<"next_cursor">> => Next
    }}.

-spec member_page_reply(any(), map(), pos_integer(), [map()], pos_integer(), boolean()) ->
    {ok, map()} | {error, {binary(), term()}}.
member_page_reply(_Conn, Ctx, GroupId, Rows, Limit, HasMore) ->
    Filter = #{<<"group_id">> => GroupId},
    Next =
        case HasMore andalso Rows =/= [] of
            true ->
                Last = lists:last(Rows),
                %% 升序 family：tie-breaker 是 group_member.id（投影外键，
                %% repo 行未携带时由 SQL 列补——internal_member_page_tx 已选 id）。
                Tuple = {maps:get(<<"created_at">>, Last), maps:get(<<"id">>, Last)},
                case
                    enterprise_internal_read_page:encode(
                        Ctx, <<"group_members">>, Filter, Tuple
                    )
                of
                    {ok, Cursor} -> Cursor;
                    {error, EncodeReason} -> erlang:error({cursor_encode_failed, EncodeReason})
                end;
            false ->
                null
        end,
    {ok, #{
        <<"items">> => [member_view_page(R) || R <- Rows],
        <<"limit">> => Limit,
        <<"has_more">> => HasMore,
        <<"next_cursor">> => Next
    }}.
