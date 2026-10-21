-module(talk_record_view_ds).
-moduledoc "Extracted talk-record service responsibility; public entry remains talk_record_ds.".
-export([
    actor_role/2,
    capabilities_for_actor/2,
    detail_payload/3,
    list_item/2,
    etag/1,
    idempotency_ttl_seconds/0
]).
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% 角色与出站 DTO
%% ===================================================================

%% @private 可见行上的角色（owner / grant editor / grant viewer）。
-spec actor_role(integer(), map()) -> owner | editor | viewer.
actor_role(Uid, #{<<"owner_user_id">> := Uid}) ->
    owner;
actor_role(Uid, #{<<"id">> := RecordId}) ->
    case talk_record_repo:find_active_grant(RecordId, Uid) of
        {ok, #{<<"role">> := <<"editor">>}} -> editor;
        _ -> viewer
    end.

%% @private 当前主体的操作能力面（§9）。export 投影叠加策略检查点
%% （§8 企业导出还必须通过企业 export capability，owner 不能绕过——
%% T19 talk_record_export_policy 单一真源；策略禁用时详情 caps 与
%% POST /exports 门禁一致拒绝）。P3（Q-review）：editor 的 export 投影
%% 与 POST /exports 门禁同口径——**任何活跃 grant 行 allow_export=true
%% 即可导出**（§1 授权表：editor/viewer 均可单独携带 allow_export），
%% 不因角色恒 false。
-spec capabilities_for_actor(integer(), map()) -> map().
capabilities_for_actor(Uid, Row) ->
    PolicyOk = talk_record_export_policy:allowed(
        maps:get(<<"scope">>, Row, <<"personal">>), maps:get(<<"organization_id">>, Row, null)
    ),
    case actor_role(Uid, Row) of
        owner ->
            #{
                <<"edit">> => true,
                <<"share">> => true,
                <<"export">> => PolicyOk,
                <<"delete">> => true,
                <<"transfer_owner">> => maps:get(<<"scope">>, Row) =:= <<"organization">>
            };
        editor ->
            #{
                <<"edit">> => true,
                <<"share">> => false,
                <<"export">> => PolicyOk andalso export_allowed(Uid, Row),
                <<"delete">> => false
            };
        viewer ->
            #{
                <<"edit">> => false,
                <<"share">> => false,
                <<"delete">> => false,
                <<"export">> => PolicyOk andalso export_allowed(Uid, Row)
            }
    end.

-spec export_allowed(integer(), map()) -> boolean().
export_allowed(Uid, #{<<"id">> := RecordId}) ->
    case talk_record_repo:find_active_grant(RecordId, Uid) of
        {ok, #{<<"allow_export">> := true}} -> true;
        _ -> false
    end.

%% @private 详情 DTO（§4：元数据 + 各状态 + 可见范围 + revision）。
-spec detail_payload(integer(), map(), map()) -> map().
detail_payload(Uid, Row, Caps) ->
    #{
        <<"id">> => maps:get(<<"id">>, Row),
        <<"owner_user_id">> => maps:get(<<"owner_user_id">>, Row),
        <<"client_recording_id">> => maps:get(<<"client_recording_id">>, Row),
        <<"title">> => maps:get(<<"title">>, Row),
        <<"scope">> => maps:get(<<"scope">>, Row),
        <<"organization_id">> => maps:get(<<"organization_id">>, Row),
        <<"workspace_id">> => maps:get(<<"workspace_id">>, Row),
        <<"language">> => maps:get(<<"language">>, Row),
        <<"notes">> => maps:get(<<"notes">>, Row),
        <<"capture_status">> => maps:get(<<"capture_status">>, Row),
        <<"media_status">> => maps:get(<<"media_status">>, Row),
        <<"transcript_status">> => maps:get(<<"transcript_status">>, Row),
        <<"summary_status">> => maps:get(<<"summary_status">>, Row),
        <<"processing_mode">> => maps:get(<<"processing_mode">>, Row),
        <<"duration_ms">> => maps:get(<<"duration_ms">>, Row),
        <<"revision">> => maps:get(<<"revision">>, Row),
        <<"content_revision">> => maps:get(<<"content_revision">>, Row),
        <<"acl_revision">> => maps:get(<<"acl_revision">>, Row),
        <<"custody_status">> => maps:get(<<"custody_status">>, Row),
        <<"visibility">> => visibility_of(Uid, Row),
        <<"capabilities_for_actor">> => Caps,
        <<"etag">> => etag(maps:get(<<"revision">>, Row))
    }.

%% @private 列表条目 DTO（§4：只带列表所需状态投影）。
-spec list_item(integer(), map()) -> map().
list_item(Uid, Row) ->
    #{
        <<"id">> => maps:get(<<"id">>, Row),
        <<"title">> => maps:get(<<"title">>, Row),
        <<"scope">> => maps:get(<<"scope">>, Row),
        <<"organization_id">> => maps:get(<<"organization_id">>, Row),
        <<"workspace_id">> => maps:get(<<"workspace_id">>, Row),
        <<"capture_status">> => maps:get(<<"capture_status">>, Row),
        <<"media_status">> => maps:get(<<"media_status">>, Row),
        <<"transcript_status">> => maps:get(<<"transcript_status">>, Row),
        <<"summary_status">> => maps:get(<<"summary_status">>, Row),
        <<"processing_mode">> => maps:get(<<"processing_mode">>, Row),
        <<"duration_ms">> => maps:get(<<"duration_ms">>, Row),
        <<"revision">> => maps:get(<<"revision">>, Row),
        <<"visibility">> => visibility_of(Uid, Row)
    }.

-spec visibility_of(integer(), map()) -> binary().
visibility_of(Uid, #{<<"owner_user_id">> := Uid}) ->
    <<"owned">>;
visibility_of(_Uid, _Row) ->
    <<"shared">>.

%% @doc 强 ETag（§11.2：record.revision 域）。
-spec etag(integer()) -> binary().
etag(Revision) when is_integer(Revision) ->
    <<"\"", (integer_to_binary(Revision))/binary, "\"">>.

%% ===================================================================
%% 幂等 / 游标内部
%% ===================================================================

-spec idempotency_ttl_seconds() -> pos_integer().
idempotency_ttl_seconds() ->
    case config_ds:env(imboy, talk_record_idempotency_ttl_seconds, 604800) of
        N when is_integer(N), N > 0 -> N;
        _ -> 604800
    end.
