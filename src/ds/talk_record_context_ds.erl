-module(talk_record_context_ds).
-moduledoc "Extracted talk-record service responsibility; public entry remains talk_record_ds.".
-export([lock_transfer_context_tx/4]).
-export([capabilities/0, creation_contexts/3, validate_context_tx/3]).
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-define(CURSOR_FAMILY_CONTEXTS, <<"talk_record_creation_contexts">>).

%% ===================================================================
%% 能力面 / Capabilities
%% ===================================================================

%% @doc 部署能力面（§11.4）：V1 处理器未接入——asr/summary/voice_id 如实
%% false；整卡可被 config {imboy, talk_record_capabilities} 覆写。
-spec capabilities() -> map().
capabilities() ->
    Cap = talk_record_processor_config:capabilities(
        normalize_limits(maps:merge(default_capabilities(), override_capabilities()))
    ),
    Cap#{<<"client_processing">> => talk_record_client_policy:public()}.

%% The recording contract must never advertise more than the real render path.
normalize_limits(Cap) ->
    Limits = [
        {<<"max_duration_ms">>, 7200000},
        {<<"max_recording_bytes">>, 2147483648},
        {<<"max_part_bytes">>, 33554432}
    ],
    lists:foldl(
        fun({Key, HardMax}, Acc) ->
            case maps:get(Key, Acc, 0) of
                N when is_integer(N), N > 0 -> Acc#{Key => min(N, HardMax)};
                _ -> Acc#{Key => 0, <<"recording_enabled">> => false}
            end
        end,
        Cap,
        Limits
    ).

-spec default_capabilities() -> map().
default_capabilities() ->
    #{
        <<"scopes">> => [<<"personal">>, <<"organization">>],
        <<"languages">> => [<<"zh-CN">>, <<"en-US">>],
        <<"source_types">> => [<<"live_recording">>],
        <<"max_duration_ms">> => 7200000,
        <<"max_recording_bytes">> => 2147483648,
        <<"max_part_bytes">> => 33554432,
        <<"recording_enabled">> => true,
        <<"asr_enabled">> => false,
        <<"summary_enabled">> => false,
        <<"voice_id_enabled">> => false,
        <<"processing_available">> => false,
        <<"queue_limit">> => 2,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"processor_location">> => <<"on_premise">>,
        <<"retention_policy">> => #{
            <<"personal">> => <<"default">>,
            <<"organization">> => <<"per_workspace_policy">>
        }
    }.

-spec override_capabilities() -> map().
override_capabilities() ->
    case config_ds:env(imboy, talk_record_capabilities, #{}) of
        Override when is_map(Override) -> Override;
        _ -> #{}
    end.

%% @private 归属组合核验（§1/§8）：organization 记录要求 org active +
%% 当前有效成员 + （选中 ws 时）ws 同组织、active 且有权创建。
%% 任何组合失效 → context_changed（409），不静默改归属。
-spec validate_context_tx(any(), integer(), map()) -> ok | {error, context_changed}.
validate_context_tx(Conn, _Uid, #{scope := <<"personal">>}) ->
    _ = Conn,
    ok;
validate_context_tx(Conn, Uid, #{
    scope := <<"organization">>,
    organization_id := OrgId,
    workspace_id := WsId
}) ->
    OrgSql = <<"SELECT 1 FROM organization o WHERE o.id = $1 AND o.status = 'active' FOR SHARE">>,
    MemberSql =
        <<"SELECT 1 FROM organization_member om WHERE om.organization_id = $1",
            " AND om.user_id = $2 AND om.status = 'active' FOR SHARE">>,
    case {elib_pg:query(Conn, OrgSql, [OrgId]), elib_pg:query(Conn, MemberSql, [OrgId, Uid])} of
        {{ok, [_]}, {ok, [_]}} ->
            validate_workspace_tx(Conn, Uid, OrgId, WsId);
        _ ->
            {error, context_changed}
    end.

-spec validate_workspace_tx(any(), integer(), integer(), integer()) ->
    ok | {error, context_changed}.
validate_workspace_tx(_Conn, _Uid, _OrgId, 0) ->
    ok;
validate_workspace_tx(Conn, Uid, OrgId, WsId) ->
    case lock_workspace_access_tx(Conn, Uid, Uid, OrgId, WsId, create) of
        ok -> validate_locked_workspace_tx(Conn, Uid, OrgId, WsId);
        Error -> Error
    end.

validate_locked_workspace_tx(Conn, Uid, OrgId, WsId) ->
    %% 创建能力口径比读谓词严：workspace 必须仍为 active（archived 可读不可写）；
    %% 授权口径与 organization_resource_access_repo 一致：ws active 成员或
    %% 本组织 owner/admin。
    Sql =
        <<"SELECT 1 FROM workspace w WHERE w.id = $1 AND w.organization_id = $2",
            " AND w.status = 'active' AND (",
            " EXISTS (SELECT 1 FROM workspace_member wm WHERE wm.workspace_id = w.id",
            "  AND wm.user_id = $3 AND wm.status = 'active')",
            " OR EXISTS (SELECT 1 FROM organization_member om2 WHERE om2.organization_id = $2",
            "  AND om2.user_id = $3 AND om2.status = 'active'",
            "  AND om2.role IN ('owner','admin')))">>,
    case elib_pg:query(Conn, Sql, [WsId, OrgId, Uid]) of
        {ok, [_]} ->
            ok;
        {ok, []} ->
            {error, context_changed};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_ws_check_failed, WsId, Reason]),
            {error, context_changed}
    end.

%% Transfer must acquire parent and membership locks before the record lock.
%% Org UPDATE serializes multi-person governance and avoids actor/target inversions.
lock_transfer_context_tx(
    Conn,
    Uid,
    TargetUid,
    #{<<"organization_id">> := OrgId} = Row
) ->
    OrgSql = <<"SELECT id FROM organization WHERE id=$1 AND status='active' FOR UPDATE">>,
    case elib_pg:query(Conn, OrgSql, [OrgId]) of
        {ok, [_]} -> lock_transfer_members_tx(Conn, Uid, TargetUid, OrgId, Row);
        {ok, []} -> {error, context_changed};
        {error, _} -> {error, db_error}
    end.

lock_transfer_members_tx(Conn, Uid, TargetUid, OrgId, Row) ->
    Sql =
        <<"SELECT user_id FROM organization_member WHERE organization_id=$1",
            " AND user_id IN ($2,$3) AND status='active' ORDER BY user_id FOR SHARE">>,
    case elib_pg:query(Conn, Sql, [OrgId, Uid, TargetUid]) of
        {ok, [_, _]} ->
            lock_workspace_access_tx(
                Conn,
                Uid,
                TargetUid,
                OrgId,
                maps:get(<<"workspace_id">>, Row, null),
                transfer
            );
        {ok, _} ->
            {error, target_not_in_directory};
        {error, _} ->
            {error, db_error}
    end.

lock_workspace_access_tx(_Conn, _Uid, _Target, _Org, Ws, _Mode) when
    Ws =:= 0; Ws =:= null
->
    ok;
lock_workspace_access_tx(Conn, Uid, Target, OrgId, WsId, Mode) ->
    Status =
        case Mode of
            create -> <<"status='active'">>;
            transfer -> <<"status IN ('active','archived')">>
        end,
    Sql =
        <<"SELECT id FROM workspace WHERE id=$1 AND organization_id=$2 AND ", Status/binary,
            " FOR SHARE">>,
    case elib_pg:query(Conn, Sql, [WsId, OrgId]) of
        {ok, [_]} ->
            Members =
                <<"SELECT user_id,status FROM workspace_member WHERE workspace_id=$1",
                    " AND user_id IN ($2,$3) ORDER BY user_id FOR SHARE">>,
            case elib_pg:query(Conn, Members, [WsId, Uid, Target]) of
                {ok, Rows} -> stable_workspace_members_tx(Conn, OrgId, Uid, Target, Rows, Mode);
                {error, _} -> {error, db_error}
            end;
        {ok, []} ->
            {error, context_changed};
        {error, _} ->
            {error, db_error}
    end.

%% A missing workspace membership row cannot be gap-locked. Never widen the
%% decision based on a later, newly inserted row; only the already locked active
%% rows or already locked org owner/admin role may authorize this transaction.
stable_workspace_members_tx(Conn, OrgId, Uid, Target, Rows, Mode) ->
    Sql =
        <<"SELECT user_id,role FROM organization_member WHERE organization_id=$1",
            " AND user_id IN ($2,$3) AND status='active'">>,
    case elib_pg:query(Conn, Sql, [OrgId, Uid, Target]) of
        {ok, Roles} ->
            Allowed = fun(User) ->
                lists:any(
                    fun(#{<<"user_id">> := Id, <<"role">> := Role}) ->
                        Id =:= User andalso (Role =:= <<"owner">> orelse Role =:= <<"admin">>)
                    end,
                    Roles
                ) orelse
                    lists:any(
                        fun(#{<<"user_id">> := Id, <<"status">> := Status}) ->
                            Id =:= User andalso Status =:= <<"active">>
                        end,
                        Rows
                    )
            end,
            case lists:all(Allowed, lists:usort([Uid, Target])) of
                true -> ok;
                false when Mode =:= transfer -> {error, target_not_in_directory};
                false -> {error, context_changed}
            end;
        {error, _} ->
            {error, db_error}
    end.

%% ===================================================================
%% 创建上下文 / Creation contexts
%% ===================================================================

%% @doc 个人/有权企业+工作区集合（§8）：不做全组织树；org keyset 升序，
%% limit 20..100；personal 始终随首页返回。
-spec creation_contexts(integer(), binary() | undefined, pos_integer()) ->
    {ok, map()} | {error, term()}.
creation_contexts(Uid, CursorBin, Limit) ->
    case
        talk_record_cursor_ds:decode_page_cursor(CursorBin, ?CURSOR_FAMILY_CONTEXTS, #{
            <<"uid">> => Uid
        })
    of
        {ok, undefined} ->
            page_contexts(Uid, undefined, Limit);
        {ok, AfterId} when is_integer(AfterId) ->
            page_contexts(Uid, AfterId, Limit);
        {ok, _} ->
            {error, cursor_invalid};
        {error, key_unavailable} ->
            page_contexts(Uid, undefined, Limit, no_cursor);
        {error, _} ->
            {error, cursor_invalid}
    end.

-spec page_contexts(integer(), integer() | undefined, pos_integer()) ->
    {ok, map()} | {error, term()}.
page_contexts(Uid, AfterId, Limit) ->
    page_contexts(Uid, AfterId, Limit, normal).

-spec page_contexts(integer(), integer() | undefined, pos_integer(), atom()) ->
    {ok, map()} | {error, term()}.
page_contexts(Uid, AfterId, Limit, CursorMode) ->
    case member_orgs(Uid, AfterId, Limit) of
        {ok, Orgs} ->
            {ok, Contexts} = attach_workspaces(Uid, Orgs),
            Next =
                case {CursorMode, length(Orgs) =:= Limit} of
                    {no_cursor, _} ->
                        null;
                    {_, true} ->
                        LastId = maps:get(<<"id">>, lists:last(Orgs)),
                        case
                            talk_record_cursor_ds:encode_cursor(
                                ?CURSOR_FAMILY_CONTEXTS, #{<<"uid">> => Uid}, LastId
                            )
                        of
                            {ok, C} -> C;
                            {error, _} -> null
                        end;
                    _ ->
                        null
                end,
            {ok, #{
                <<"personal">> => personal_context(),
                <<"organizations">> => Contexts,
                <<"next_cursor">> => Next
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec member_orgs(integer(), integer() | undefined, pos_integer()) ->
    {ok, [map()]} | {error, term()}.
member_orgs(Uid, AfterId, Limit) ->
    {Frag, Params} =
        case AfterId of
            undefined -> {<<>>, [Uid, Limit]};
            Id -> {<<" AND o.id > $3">>, [Uid, Limit, Id]}
        end,
    Sql =
        <<"SELECT o.id, o.name FROM organization o",
            " JOIN organization_member om ON om.organization_id = o.id",
            " AND om.user_id = $1 AND om.status = 'active'", " WHERE o.status = 'active'",
            Frag/binary, " ORDER BY o.id ASC LIMIT $2">>,
    elib_pg:query(Sql, Params).

%% @private 有权工作区（读口径：workspace_member active 或本组织 owner/admin；
%% archived 可读），按 org 分组。
-spec attach_workspaces(integer(), [map()]) -> {ok, [map()]}.
attach_workspaces(_Uid, []) ->
    {ok, []};
attach_workspaces(Uid, Orgs) ->
    OrgIds = [maps:get(<<"id">>, O) || O <- Orgs],
    Sql =
        <<"SELECT w.organization_id, w.id, w.name FROM workspace w",
            " WHERE w.organization_id = ANY($1::bigint[]) AND w.status IN ('active','archived')",
            " AND (EXISTS (SELECT 1 FROM workspace_member wm WHERE wm.workspace_id = w.id",
            "  AND wm.user_id = $2 AND wm.status = 'active')",
            " OR EXISTS (SELECT 1 FROM organization_member om WHERE om.organization_id = w.organization_id",
            "  AND om.user_id = $2 AND om.status = 'active'",
            "  AND om.role IN ('owner','admin')))", " ORDER BY w.organization_id ASC, w.id ASC">>,
    WsRows =
        case elib_pg:query(Sql, [OrgIds, Uid]) of
            {ok, Rows} ->
                Rows;
            {error, Reason} ->
                ?ERROR_LOG([talk_record_ctx_ws_failed, Uid, Reason]),
                []
        end,
    ByOrg = group_workspaces(WsRows),
    {ok, [org_context(O, ByOrg) || O <- Orgs]}.

-spec group_workspaces([map()]) -> #{integer() => [map()]}.
group_workspaces(Rows) ->
    lists:foldl(
        fun(Row, Acc) ->
            OrgId = maps:get(<<"organization_id">>, Row),
            Item = #{<<"id">> => maps:get(<<"id">>, Row), <<"name">> => maps:get(<<"name">>, Row)},
            maps:update_with(OrgId, fun(L) -> L ++ [Item] end, [Item], Acc)
        end,
        #{},
        Rows
    ).

-spec org_context(map(), #{integer() => [map()]}) -> map().
org_context(Org, ByOrg) ->
    Base = context_base(<<"organization">>),
    Base#{
        <<"organization_id">> => maps:get(<<"id">>, Org),
        <<"name">> => maps:get(<<"name">>, Org),
        <<"client_processing">> => talk_record_client_policy:for_record(#{
            <<"scope">> => <<"organization">>,
            <<"organization_id">> => maps:get(<<"id">>, Org)
        }),
        <<"workspaces">> => maps:get(maps:get(<<"id">>, Org), ByOrg, [])
    }.

-spec personal_context() -> map().
personal_context() ->
    (context_base(<<"personal">>))#{
        <<"client_processing">> =>
            talk_record_client_policy:for_record(#{<<"scope">> => <<"personal">>})
    }.

-spec context_base(binary()) -> map().
context_base(Scope) ->
    Cap = capabilities(),
    #{
        <<"scope">> => Scope,
        <<"capabilities">> => #{
            <<"can_create">> => maps:get(<<"recording_enabled">>, Cap, true),
            <<"processing_available">> => maps:get(<<"processing_available">>, Cap, false)
        },
        <<"retention_policy">> => maps:get(<<"retention_policy">>, Cap, #{}),
        <<"processing_notice_version">> => maps:get(<<"policy_version">>, Cap, <<>>)
    }.
