-module(organization_logic).

%% 通用 Organization 本体逻辑；不派生 Workspace、Group 或垂直业务权限。

-export([create/2, mine/3, detail/2, update/5]).
%% ORG-02 adapter：archive/restore/deletion-preflight 应用层挂接
%% （路由注册归 ORG-10；实现委托 src/lib/organization）。
-export([archive/2, restore/2, deletion_preflight/1]).

-include("log.hrl").

%% APP 侧建企不存在"工作区名"入参：默认工作区用固定名（同 org 内唯一即可）。
-define(DEFAULT_WS_NAME, <<"默认工作区"/utf8>>).

-spec create(integer(), term()) -> {ok, map()} | {error, {integer(), binary()}}.
create(Uid, Name0) when is_integer(Uid), Uid > 0 ->
    case normalize_name(Name0) of
        error ->
            {error, {400, <<"Organization 名称不能为空且不超过 200 字符"/utf8>>}};
        {ok, Name} ->
            create_validated(Uid, Name)
    end;
create(_, _) ->
    {error, {400, <<"用户身份无效"/utf8>>}}.

-spec mine(integer(), integer(), integer()) -> {ok, map()} | {error, {500, binary()}}.
mine(Uid, Page0, Size0) when is_integer(Uid), Uid > 0 ->
    Page = max(1, Page0),
    Size = max(1, min(100, Size0)),
    case organization_repo:page_by_member(Uid, Page, Size) of
        {ok, Result} ->
            Items = maps:get(list, Result, []),
            {ok, Result#{list => [public_view(Item) || Item <- Items]}};
        {error, Reason} ->
            ?ERROR_LOG([organization_page_failed, Uid, Reason]),
            internal_error(<<"查询失败，请稍后重试"/utf8>>)
    end;
mine(_, _, _) ->
    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}.

-spec detail(integer(), integer()) ->
    {ok, map()} | {error, {403 | 404 | 500, binary()}}.
detail(Uid, OrgId) when is_integer(Uid), Uid > 0, is_integer(OrgId), OrgId > 0 ->
    case organization_repo:find_by_id(OrgId) of
        {ok, Org} ->
            add_member_role(Uid, OrgId, Org);
        {error, not_found} ->
            {error, {404, <<"Organization 不存在"/utf8>>}};
        {error, Reason} ->
            ?ERROR_LOG([organization_detail_failed, OrgId, Uid, Reason]),
            internal_error(<<"查询失败，请稍后重试"/utf8>>)
    end;
detail(_, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

-spec update(integer(), integer(), term(), term(), term()) ->
    {ok, map()} | {error, {400 | 403 | 404 | 409 | 500, binary()}}.
update(Uid, OrgId, Name0, Branding0, Settings0) when
    is_integer(Uid), Uid > 0, is_integer(OrgId), OrgId > 0
->
    case normalize_patch(Name0, Branding0, Settings0) of
        {error, Msg} ->
            {error, {400, Msg}};
        {ok, {Name, BrandingJson, SettingsJson}} ->
            update_validated(Uid, OrgId, Name, BrandingJson, SettingsJson)
    end;
update(_, _, _, _, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% @doc 建企（**APP 侧** `POST /organizations`）。
%%
%% 与 Admin 建企（organization_admin_logic:admin_create/4）同构：org 行 + 唯一
%% Owner 成员 + **默认 Workspace 模板**（workspace 行 / Org 默认关系 /
%% owner workspace_member / 全员群 General / 公告频道 Announcements 含订阅），
%% 全部在同一事务内，失败零残留（计划 §106「不能留下半初始化状态」）。
%% APP 建企只收一个名称，故默认工作区用固定名（Admin 侧由调用方显式给名）。
%%
%% 此前本路径只写 org + owner 成员（裸建企）：owner 名下没有任何工作区，
%% 真机上表现为「创建企业后进入工作区壳看到『还没有工作区』」；更严重的是
%% organization_default_workspace 无行 → 后续凭邀请码/邀请加入的成员拿到的
%% workspace_id/group_id/channel_id 全是 none（GZ-J03「仅默认 Workspace、
%% 全员群和公告频道关系正确」不成立）。
create_validated(Uid, Name) ->
    Tx = fun(Conn) ->
        case organization_repo:create_tx(Conn, Uid, Name) of
            {ok, Org} ->
                OrgId = maps:get(<<"id">>, Org),
                case organization_member_repo:find_active_tx(Conn, OrgId, Uid, <<"role">>) of
                    {ok, #{<<"role">> := <<"owner">>}} ->
                        %% 默认 Workspace 模板：与建工作区/Admin 建企同一原语
                        {ok, _Template} = workspace_ds:create_default_template_tx(
                            Conn, Uid, OrgId, ?DEFAULT_WS_NAME
                        ),
                        {ok, Org#{<<"member_role">> => <<"owner">>}};
                    {ok, _} ->
                        throw({abort_tx, owner_membership_not_created});
                    {error, Reason} ->
                        throw({abort_tx, {owner_membership_not_created, Reason}})
                end;
            {error, Reason} ->
                throw({abort_tx, {organization_create_failed, Reason}})
        end
    end,
    case elib_pg:with_tx(Tx) of
        {ok, Org} ->
            ?INFO_LOG([organization_created, Uid, maps:get(<<"id">>, Org)]),
            {ok, public_view(Org)};
        {error, Reason} ->
            ?ERROR_LOG([organization_create_failed, Uid, Reason]),
            internal_error(<<"创建失败，请稍后重试"/utf8>>);
        {rollback, Reason} ->
            ?ERROR_LOG([organization_create_failed, Uid, Reason]),
            internal_error(<<"创建失败，请稍后重试"/utf8>>)
    end.

add_member_role(Uid, OrgId, Org) ->
    case organization_member_repo:find_active(OrgId, Uid, <<"role">>) of
        {ok, #{<<"role">> := Role}} ->
            {ok, public_view(Org#{<<"member_role">> => Role})};
        {error, not_found} ->
            {error, {403, <<"仅 Organization 成员可查看详情"/utf8>>}};
        {error, Reason} ->
            ?ERROR_LOG([organization_detail_acl_failed, OrgId, Uid, Reason]),
            internal_error(<<"查询失败，请稍后重试"/utf8>>)
    end.

update_validated(Uid, OrgId, Name, BrandingJson, SettingsJson) ->
    Tx = fun(Conn) ->
        Org =
            case organization_repo:find_for_update_tx(Conn, OrgId) of
                {ok, #{<<"status">> := <<"active">>} = Row} ->
                    Row;
                {ok, _Archived} ->
                    throw({abort_tx, {409, <<"Organization 已归档，不能更新"/utf8>>}});
                {error, not_found} ->
                    throw({abort_tx, {404, <<"Organization 不存在"/utf8>>}});
                {error, Reason1} ->
                    throw({abort_tx, {internal, Reason1}})
            end,
        ActorRole =
            case organization_member_repo:find_active_for_share_tx(Conn, OrgId, Uid, <<"role">>) of
                {ok, #{<<"role">> := Role}} when Role =:= <<"owner">>; Role =:= <<"admin">> ->
                    Role;
                {ok, _} ->
                    throw({abort_tx, {403, <<"仅 Organization Owner 或 Admin 可更新"/utf8>>}});
                {error, not_found} ->
                    throw({abort_tx, {403, <<"仅 Organization Owner 或 Admin 可更新"/utf8>>}});
                {error, Reason2} ->
                    throw({abort_tx, {internal, Reason2}})
            end,
        case organization_repo:update_tx(Conn, OrgId, Name, BrandingJson, SettingsJson) of
            {ok, Updated} ->
                {ok, Updated#{<<"member_role">> => ActorRole}};
            {error, Reason3} ->
                throw({abort_tx, {internal, {Org, Reason3}}})
        end
    end,
    case elib_pg:with_tx(Tx) of
        {ok, Updated} ->
            ?INFO_LOG([organization_updated, OrgId, Uid]),
            {ok, public_view(Updated)};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            ?ERROR_LOG([organization_update_failed, OrgId, Uid, Reason]),
            internal_error(<<"更新失败，请稍后重试"/utf8>>);
        {rollback, Reason} ->
            ?ERROR_LOG([organization_update_failed, OrgId, Uid, Reason]),
            internal_error(<<"更新失败，请稍后重试"/utf8>>)
    end.

%% ORG-02（C16）：归档（幂等 command，owner/admin）。
-spec archive(integer(), integer()) -> {ok, map()} | {error, {400 | 403 | 404 | 500, binary()}}.
archive(Uid, OrgId) when is_integer(Uid), Uid > 0, is_integer(OrgId), OrgId > 0 ->
    organization_lifecycle:archive(Uid, OrgId);
archive(_, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% ORG-02（C16）：恢复（幂等 command，archived 态唯一放行的写入口）。
-spec restore(integer(), integer()) -> {ok, map()} | {error, {400 | 403 | 404 | 500, binary()}}.
restore(Uid, OrgId) when is_integer(Uid), Uid > 0, is_integer(OrgId), OrgId > 0 ->
    organization_lifecycle:restore(Uid, OrgId);
restore(_, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% ORG-02（C17）：User deletion preflight 应用层入口，返回稳定 blocker 列表。
%% DEPENDENCY_FACTS_UNAVAILABLE（缺域/超时/不可用/不一致）归口 503。
-spec deletion_preflight(integer()) ->
    {ok, map()} | {error, {400 | 503, binary()}}.
deletion_preflight(Uid) when is_integer(Uid), Uid > 0 ->
    case organization_deletion_preflight:run(Uid) of
        {ok, Result} ->
            {ok, Result};
        {error, _Reason} ->
            ?ERROR_LOG([organization_deletion_preflight_unavailable, Uid, _Reason]),
            {error, {503, <<"依赖域事实不可用，删除预检被拒绝"/utf8>>}}
    end;
deletion_preflight(_) ->
    {error, {400, <<"用户身份无效"/utf8>>}}.

normalize_name(Name) when is_binary(Name) ->
    try unicode:characters_to_binary(string:trim(Name)) of
        Trimmed when byte_size(Trimmed) > 0 ->
            case string:length(Trimmed) =< 200 of
                true -> {ok, Trimmed};
                false -> error
            end;
        _ ->
            error
    catch
        _:_ -> error
    end;
normalize_name(_) ->
    error.

normalize_patch(undefined, undefined, undefined) ->
    {error, <<"至少提供 name、branding、settings 中的一项"/utf8>>};
normalize_patch(Name0, Branding0, Settings0) ->
    case
        {
            normalize_optional_name(Name0),
            normalize_optional_object(Branding0, <<"branding">>),
            normalize_optional_object(Settings0, <<"settings">>)
        }
    of
        {{ok, Name}, {ok, Branding}, {ok, Settings}} ->
            {ok, {Name, Branding, Settings}};
        {{error, Msg}, _, _} ->
            {error, Msg};
        {_, {error, Msg}, _} ->
            {error, Msg};
        {_, _, {error, Msg}} ->
            {error, Msg}
    end.

normalize_optional_name(undefined) ->
    {ok, null};
normalize_optional_name(Name) ->
    case normalize_name(Name) of
        {ok, Normalized} -> {ok, Normalized};
        error -> {error, <<"Organization 名称不能为空且不超过 200 字符"/utf8>>}
    end.

normalize_optional_object(undefined, _Field) ->
    {ok, null};
normalize_optional_object(Value, Field) when is_map(Value) ->
    Encoded = jsone:encode(Value, [native_utf8]),
    case byte_size(Encoded) =< 65536 of
        true -> {ok, Encoded};
        false -> {error, <<Field/binary, " 不能超过 64 KiB"/utf8>>}
    end;
normalize_optional_object(_, Field) ->
    {error, <<Field/binary, " 必须是对象"/utf8>>}.

public_view(Org) ->
    Org#{
        <<"branding">> => json_object(maps:get(<<"branding">>, Org, #{})),
        <<"settings">> => json_object(maps:get(<<"settings">>, Org, #{}))
    }.

json_object(Value) when is_map(Value) ->
    Value;
json_object(Value) when is_binary(Value) ->
    try jsone:decode(Value) of
        Decoded when is_map(Decoded) -> Decoded;
        _ -> #{}
    catch
        _:_ -> #{}
    end;
json_object(_) ->
    #{}.

internal_error(Msg) ->
    {error, {500, Msg}}.
