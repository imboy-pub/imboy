-module(talk_record_content_speakers).
-moduledoc "面聊发言人逻辑（T15）：清单/人工标记（manual）/本人认领（self_claim）。".
%%%
% speakers 域业务逻辑（§4 GET/PATCH /:id/speakers + POST claim）。
%
% GET：匿名标签/人工姓名/绑定来源/可认领状态/revision；不暴露 voice
% embedding（V1 无该数据；出站 DTO 白名单字段）。
%
% PATCH（display_name / matched_user_id；owner/editor）：
%   * matched_user_id 绑定是人工标记：服务端写 identity_source=manual +
%     confirmed_by（客户端不可指定 identity_source——请求体出现即 422）；
%     null 解绑回 anonymous；
%   * 目标须符合可见目录（§4）：org 记录要求同 org active 成员 + 绑定 ws
%     时 ws 资源授权（复用 organization_resource_access_repo 统一谓词，
%     T05 creation-contexts 同口径）；personal 记录以目标用户存在为最小
%     判据（V1 无联系人目录，报告登记差异）；目录外 → 422；
%   * If-Match speaker revision；成功推 record.content_revision（§11.2
%     署名属内容变化）+ 审计 speaker_edit。人工标记 ≠ 生物认证。
%
% claim（POST /:id/speakers/:speaker_id/claim）：
%   * 只允许本人（matched_user_id := JWT uid；服务端取，请求体忽略）；
%   * 前置：记录可见（否则 404）+ 现场参与者（talk_record_participant
%     user_id=本人；§4「有记录访问权的现场参与者才可认领」；参与者 ≠
%     授权 ≠ 认证）；
%   * 他人已绑定 → 409 + 审计（冲突审计独立提交：业务判定虽在事务内，
%     409 回滚不吞审计）；本人已认领 → 幂等返回现状；
%   * 成功写 self_claim + confirmed_by + revision+1 + content_revision+1 +
%     审计 speaker_claim。认领记录不是生物认证（§4 注释义务：identity_source
%     语义即承载，payload 不出现任何 verified/认证 字样字段）。
%%%

-export([list/2, update/5, claim/3]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(MAX_DISPLAY_NAME_CHARS, 120).

%% ===================================================================
%% GET /:id/speakers
%% ===================================================================

%% @doc 说话人清单（local_label/id 正序；量小不分页，repo 上限 200）。
-spec list(integer(), integer()) -> {ok, map()} | {error, term()}.
list(Uid, RecordId) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, _Row} ->
            case
                elib_pg:with_tx(
                    fun(Conn) -> talk_record_speaker_repo:list_by_record_tx(Conn, RecordId) end
                )
            of
                {ok, Rows} ->
                    {ok, #{<<"speakers">> => [speaker_dto(S) || S <- Rows]}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 出站 DTO 白名单（无 embedding/无认证宣称字段；
%% claimable = 未绑定任何用户时可认领）。
-spec speaker_dto(map()) -> map().
speaker_dto(S) ->
    Revision = maps:get(<<"revision">>, S, 1),
    Id = maps:get(<<"id">>, S),
    Matched = maps:get(<<"matched_user_id">>, S),
    #{
        <<"id">> => Id,
        <<"local_label">> => maps:get(<<"local_label">>, S),
        <<"display_name">> => maps:get(<<"display_name">>, S),
        <<"matched_user_id">> => Matched,
        <<"identity_source">> => maps:get(<<"identity_source">>, S),
        <<"confirmed_by">> => maps:get(<<"confirmed_by">>, S),
        <<"claimable">> => Matched =:= null,
        <<"revision">> => Revision,
        <<"etag">> => talk_record_content_common:speaker_etag(Id, Revision),
        <<"created_at">> => maps:get(<<"created_at">>, S),
        <<"updated_at">> => maps:get(<<"updated_at">>, S)
    }.

%% ===================================================================
%% PATCH /:id/speakers/:speaker_id
%% ===================================================================

%% @doc 人工标记（display_name / matched_user_id + If-Match speaker revision）。
%% 成功返回 #{speaker => DTO, content_revision => N}。
-spec update(integer(), integer(), integer(), binary() | undefined, map()) ->
    {ok, map()} | {error, term()}.
update(Uid, RecordId, SpeakerId, IfMatch, Body) when is_map(Body) ->
    case talk_record_content_common:parse_resource_if_match(<<"speaker">>, SpeakerId, IfMatch) of
        {ok, ExpectedRevision} ->
            update_parsed(Uid, RecordId, SpeakerId, ExpectedRevision, Body);
        {error, Reason} ->
            {error, Reason}
    end;
update(_Uid, _RecordId, _SpeakerId, _IfMatch, _Body) ->
    {error, invalid_body}.

-spec update_parsed(integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_parsed(Uid, RecordId, SpeakerId, ExpectedRevision, Body) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, Row} ->
            update_visible(Uid, RecordId, SpeakerId, ExpectedRevision, Body, Row);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_visible(integer(), integer(), integer(), pos_integer(), map(), map()) ->
    {ok, map()} | {error, term()}.
update_visible(Uid, RecordId, SpeakerId, ExpectedRevision, Body, Row) ->
    case talk_record_content_common:editable_role_gate(Uid, Row) of
        ok ->
            update_gated(Uid, RecordId, SpeakerId, ExpectedRevision, Body, Row);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_gated(integer(), integer(), integer(), pos_integer(), map(), map()) ->
    {ok, map()} | {error, term()}.
update_gated(Uid, RecordId, SpeakerId, ExpectedRevision, Body, Row) ->
    case parse_update_fields(Body) of
        {ok, Changes} ->
            update_fields(Uid, RecordId, SpeakerId, ExpectedRevision, Changes, Row);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 字段白名单：display_name（≤120）／matched_user_id（正整数或
%% null=解绑）；identity_source 出现即 422（服务端专属，§4 不能由客户端指定）。
-spec parse_update_fields(map()) -> {ok, map()} | {error, atom()}.
parse_update_fields(Body) ->
    case maps:is_key(<<"identity_source">>, Body) of
        true ->
            {error, invalid_identity_source};
        false ->
            Picked = maps:with([<<"display_name">>, <<"matched_user_id">>], Body),
            case map_size(Picked) of
                0 ->
                    {error, missing_editable_field};
                _ ->
                    validate_update_fields(Picked)
            end
    end.

-spec validate_update_fields(map()) -> {ok, map()} | {error, atom()}.
validate_update_fields(Picked) ->
    Checks = [
        {<<"display_name">>, fun valid_display_name/1, invalid_speaker_name},
        {<<"matched_user_id">>, fun valid_nullable_id/1, invalid_matched_user_id}
    ],
    talk_record_content_common:validate_fields(Checks, Picked, ok).

-spec valid_display_name(term()) -> boolean().
valid_display_name(Name) ->
    talk_record_content_common:valid_text(Name, ?MAX_DISPLAY_NAME_CHARS).

-spec valid_nullable_id(term()) -> boolean().
valid_nullable_id(null) ->
    true;
valid_nullable_id(N) ->
    case talk_record_content_common:parse_pos_int(N) of
        {ok, _} -> true;
        error -> false
    end.

-spec update_fields(integer(), integer(), integer(), pos_integer(), map(), map()) ->
    {ok, map()} | {error, term()}.
update_fields(Uid, RecordId, SpeakerId, ExpectedRevision, Changes, Row) ->
    case
        elib_pg:with_tx(
            fun(Conn) ->
                update_tx(Conn, Uid, RecordId, SpeakerId, ExpectedRevision, Changes, Row)
            end,
            [{reraise, false}]
        )
    of
        {ok, Speaker, ContentRevision} ->
            {ok, #{
                <<"speaker">> => speaker_dto(Speaker),
                <<"content_revision">> => ContentRevision
            }};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_speaker_update_rolled_back, SpeakerId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 事务体：行存在（复合匹配 404）→ 目录校验（matched 绑定时）→
%% 组装 identity_source/confirmed_by（服务端专属列，随 CAS 一并落库）→
%% content_revision → 审计。
-spec update_tx(any(), integer(), integer(), integer(), pos_integer(), map(), map()) ->
    {ok, map(), pos_integer()} | {error, term()}.
update_tx(Conn, Uid, RecordId, SpeakerId, ExpectedRevision, Changes, Row) ->
    case talk_record_speaker_repo:find_tx(Conn, RecordId, SpeakerId) of
        {ok, _Speaker} ->
            bind_target_tx(Conn, Uid, RecordId, Changes, Row, fun(Sets) ->
                cas_update_tx(Conn, Uid, RecordId, SpeakerId, ExpectedRevision, Sets, Changes)
            end);
        {error, not_found} ->
            {error, speaker_not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private matched_user_id 绑定的目录校验 + 服务端专属列组装：
%% 绑定 → identity_source=manual + confirmed_by=Uid；解绑（null）→
%% anonymous + confirmed_by=NULL；仅改 display_name → 不动来源列。
-spec bind_target_tx(any(), integer(), integer(), map(), map(), function()) ->
    {ok, map(), pos_integer()} | {error, term()}.
bind_target_tx(Conn, Uid, RecordId, Changes, Row, Next) ->
    case maps:find(<<"matched_user_id">>, Changes) of
        {ok, TargetUid} when is_integer(TargetUid) ->
            bind_manual_tx(Conn, Uid, RecordId, Changes, Row, TargetUid, Next);
        {ok, null} ->
            Next(Changes#{
                <<"identity_source">> => <<"anonymous">>,
                <<"confirmed_by">> => null
            });
        error ->
            Next(Changes)
    end.

-spec bind_manual_tx(any(), integer(), integer(), map(), map(), integer(), function()) ->
    {ok, map(), pos_integer()} | {error, term()}.
bind_manual_tx(Conn, Uid, _RecordId, Changes, Row, TargetUid, Next) ->
    case talk_record_speaker_repo:target_in_directory_tx(Conn, Row, Uid, TargetUid) of
        {ok, true} ->
            Next(Changes#{
                <<"identity_source">> => <<"manual">>,
                <<"confirmed_by">> => Uid
            });
        {ok, false} ->
            {error, target_not_in_directory};
        {error, Reason} ->
            {error, Reason}
    end.

-spec cas_update_tx(any(), integer(), integer(), integer(), pos_integer(), map(), map()) ->
    {ok, map(), pos_integer()} | no_return().
cas_update_tx(Conn, Uid, RecordId, SpeakerId, ExpectedRevision, Sets, Changes) ->
    case talk_record_speaker_repo:update_tx(Conn, RecordId, SpeakerId, ExpectedRevision, Sets) of
        {ok, Updated} ->
            finish_speaker_update_tx(Conn, Uid, RecordId, SpeakerId, Updated, Changes);
        {error, not_found} ->
            {error, precondition_failed};
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

-spec finish_speaker_update_tx(any(), integer(), integer(), integer(), map(), map()) ->
    {ok, map(), pos_integer()} | no_return().
finish_speaker_update_tx(Conn, Uid, RecordId, SpeakerId, Updated, Changes) ->
    case talk_record_repo:bump_content_revision_tx(Conn, RecordId) of
        {ok, ContentRevision} ->
            Metadata = #{
                <<"fields">> => lists:sort(
                    maps:keys(maps:with([<<"display_name">>, <<"matched_user_id">>], Changes))
                ),
                <<"revision">> => maps:get(<<"revision">>, Updated),
                <<"content_revision">> => ContentRevision,
                <<"identity_source">> => maps:get(<<"identity_source">>, Updated)
            },
            case
                talk_record_repo:insert_audit_tx(
                    Conn, RecordId, Uid, <<"speaker_edit">>, SpeakerId, Metadata
                )
            of
                {ok, _} -> {ok, Updated, ContentRevision};
                {error, Reason} -> throw({talk_record_rollback, {error, Reason}})
            end;
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% ===================================================================
%% POST /:id/speakers/:speaker_id/claim
%% ===================================================================

%% @doc 本人认领（无 If-Match：幂等动作，冲突以 409 仲裁——若要求 If-Match
%% 则并发认领会退化为 412，与 §4「已有他人认领返回409」冲突）。
%% 成功返回 #{speaker => DTO, content_revision => N}；幂等重认领返回现状。
-spec claim(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
claim(Uid, RecordId, SpeakerId) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, _Row} ->
            claim_visible(Uid, RecordId, SpeakerId);
        {error, Reason} ->
            {error, Reason}
    end.

-spec claim_visible(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
claim_visible(Uid, RecordId, SpeakerId) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> claim_tx(Conn, Uid, RecordId, SpeakerId) end,
            [{reraise, false}]
        )
    of
        {ok, Speaker, ContentRevision} ->
            {ok, #{
                <<"speaker">> => speaker_dto(Speaker),
                <<"content_revision">> => ContentRevision
            }};
        {already, Speaker} ->
            {ok, #{<<"speaker">> => speaker_dto(Speaker), <<"claimed">> => true}};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            claim_rollback(Uid, RecordId, SpeakerId, Reason);
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_speaker_claim_rolled_back, SpeakerId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 冲突回滚也要留审计（§4 已有他人认领 409+审计）：业务事务已
%% 回滚，审计独立提交（talk_record_repo:insert_audit 自带事务）。
-spec claim_rollback(integer(), integer(), integer(), term()) -> {error, term()}.
claim_rollback(Uid, RecordId, SpeakerId, speaker_claim_conflict) ->
    _ = talk_record_repo:insert_audit(
        RecordId,
        Uid,
        <<"speaker_claim">>,
        SpeakerId,
        #{<<"outcome">> => <<"conflict">>}
    ),
    {error, speaker_claim_conflict};
claim_rollback(_Uid, _RecordId, _SpeakerId, Reason) ->
    {error, Reason}.

%% @private 事务体：参与者前置 → 现状回读（幂等/冲突分派）→ CAS 认领 →
%% content_revision → 审计。冲突路径不在此写审计（回滚会吞掉），
%% claim_conflict_audit/3 独立提交。
-spec claim_tx(any(), integer(), integer(), integer()) ->
    {ok, map(), pos_integer()} | {already, map()} | {error, term()}.
claim_tx(Conn, Uid, RecordId, SpeakerId) ->
    case talk_record_speaker_repo:participant_exists_tx(Conn, RecordId, Uid) of
        {ok, true} ->
            claim_participant_tx(Conn, Uid, RecordId, SpeakerId);
        {ok, false} ->
            %% 有记录访问权但不在参与者名单：403（不泄漏名单内容）
            {error, claim_not_participant};
        {error, Reason} ->
            {error, Reason}
    end.

-spec claim_participant_tx(any(), integer(), integer(), integer()) ->
    {ok, map(), pos_integer()} | {already, map()} | no_return().
claim_participant_tx(Conn, Uid, RecordId, SpeakerId) ->
    case talk_record_speaker_repo:find_tx(Conn, RecordId, SpeakerId) of
        {ok, #{<<"matched_user_id">> := Uid, <<"identity_source">> := <<"self_claim">>} = Speaker} ->
            {already, Speaker};
        {ok, #{<<"matched_user_id">> := Other}} when is_integer(Other), Other =/= Uid ->
            %% 冲突：throw 回滚走 claim_rollback 统一补冲突审计
            throw({talk_record_rollback, {error, speaker_claim_conflict}});
        {ok, _Unbound} ->
            claim_apply_tx(Conn, Uid, RecordId, SpeakerId);
        {error, not_found} ->
            {error, speaker_not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec claim_apply_tx(any(), integer(), integer(), integer()) ->
    {ok, map(), pos_integer()} | no_return().
claim_apply_tx(Conn, Uid, RecordId, SpeakerId) ->
    case talk_record_speaker_repo:claim_tx(Conn, RecordId, SpeakerId, Uid) of
        {ok, Speaker} ->
            finish_claim_tx(Conn, Uid, RecordId, SpeakerId, Speaker);
        {error, not_found} ->
            %% CAS 窗口内被他人抢先绑定：回读分派冲突
            throw({talk_record_rollback, {error, speaker_claim_conflict}});
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

-spec finish_claim_tx(any(), integer(), integer(), integer(), map()) ->
    {ok, map(), pos_integer()} | no_return().
finish_claim_tx(Conn, Uid, RecordId, SpeakerId, Speaker) ->
    case talk_record_repo:bump_content_revision_tx(Conn, RecordId) of
        {ok, ContentRevision} ->
            Metadata = #{
                <<"revision">> => maps:get(<<"revision">>, Speaker),
                <<"content_revision">> => ContentRevision,
                <<"matched_user_id">> => Uid
            },
            case
                talk_record_repo:insert_audit_tx(
                    Conn, RecordId, Uid, <<"speaker_claim">>, SpeakerId, Metadata
                )
            of
                {ok, _} -> {ok, Speaker, ContentRevision};
                {error, Reason} -> throw({talk_record_rollback, {error, Reason}})
            end;
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.
