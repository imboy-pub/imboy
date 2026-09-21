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
    remove_members_tx/4
]).

-define(MAX_TITLE_LEN, 200).
-define(MAX_INTRODUCTION_LEN, 2000).
-define(MAX_MEMBERS, 200).

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
                                Conn, WsId, Title, Intro, Members, OwnerExt, ExtToUid
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
    any(), integer(), binary(), binary(), [binary()], binary(), map()
) ->
    {ok, map()} | {error, {binary(), term()}}.
insert_group_with_members(Conn, WsId, Title, Intro, Members, OwnerExt, ExtToUid) ->
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
                    {ok, #{
                        <<"group_id">> => Gid,
                        <<"workspace_id">> => WsId,
                        <<"owner_user_id">> => OwnerUid,
                        <<"member_count">> => length(UidRoles)
                    }};
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
