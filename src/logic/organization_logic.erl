-module(organization_logic).

%% 通用 Organization 本体逻辑；不派生 Workspace、Group 或垂直业务权限。

-export([create/2, mine/3, detail/2, update/5]).

-include("log.hrl").

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

create_validated(Uid, Name) ->
    Tx = fun(Conn) ->
        case organization_repo:create_tx(Conn, Uid, Name) of
            {ok, Org} ->
                case
                    organization_member_repo:find_active_tx(
                        Conn, maps:get(<<"id">>, Org), Uid, <<"role">>
                    )
                of
                    {ok, #{<<"role">> := <<"owner">>}} ->
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
