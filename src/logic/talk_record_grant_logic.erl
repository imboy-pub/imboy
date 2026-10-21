-module(talk_record_grant_logic).
-moduledoc "面聊记录显式授权逻辑（T17）：grants 读取/全量替换/撤销。".
%%%
% grants 域业务逻辑（§4 grants 四端点中的三个 + §11.3 最小暴露）。
%
% GET /:id/grants（§4/§11.3）：
%   * owner：全量活跃 ACL（含 allow_export/expires_at；revoked 行属
%     服务端历史，不进对外名单——T18 冻结“对外仅活跃名单”）；
%   * 非 owner：仅自己被授权的活跃行（完整接收者名单只有 owner 可读，
%     不披露服务器成员关系）；
%   * 无权限/不存在统一 404（visible 谓词）。
%
% PUT /:id/grants（§4）：
%   * owner + If-Match（acl_revision 域）；全量提交事务更新；
%   * 语义上全量 PUT 不含 owner 行（owner 在主表）——提交清单出现
%     owner_user_id 即 422（“不能移除最后 owner”由主表不变量保证）；
%   * 不得授权超归属范围人员：org 记录复用 T15 目录校验
%     （talk_record_speaker_repo:target_in_directory_tx：同 org active
%     成员 + ws 资源授权谓词）；personal 复用其最小判据（user 表存在
%     ——联系人能力最小实现现状，见 talk_record_grant_repo 模块头）；
%   * 与并发删除互斥：事务内 SELECT ... FOR UPDATE 拿锁后复核
%     deleted_at（并发删除 → 404）与 acl_revision（并发授权 → 412）。
%
% DELETE /:id/grants/:grant_id（§4/§11.3）：
%   * owner + If-Match；立即生效——读侧活跃谓词、票据签发/续签与
%     Range 重验（acl_revision 绑定进票据 claims）同步拒绝；
%   * 已导出副本不可收回为 UI 文案语义，后端无回收义务（§11.3）。
%
% acl ETag 惯例（与 T18 UI 冻结契约对齐）：
%   * 令牌 `acl:<acl_revision>`（T18 talkRecordAclEtagOf 同式派生）；
%   * HTTP 响应头为 RFC 引号形态 `"acl:2"`；payload etag 字段为令牌
%     本体（不带引号）；If-Match 兼容令牌与纯 acl_revision 两种形态
%     （复用 talk_record_content_common:parse_resource_if_match）。
%
% 版本域纪律（§11.2）：授权/撤销只推 acl_revision，不碰
% record.revision / content_revision。
%%%

-export([
    list/2,
    replace/4,
    revoke/4,
    parse_grants/1,
    acl_etag/1,
    parse_acl_if_match/1
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(MAX_QUERY_CHARS, 100).

%% ===================================================================
%% GET /:id/grants
%% ===================================================================

%% @doc 授权名单。owner → 全量活跃行；非 owner → 仅自己（§11.3）。
%% 返回 {ok, #{list, acl_revision, etag}} | {error, Reason}。
-spec list(integer(), integer()) -> {ok, map()} | {error, term()}.
list(Uid, RecordId) ->
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, #{<<"owner_user_id">> := Uid, <<"acl_revision">> := AclRev} = Row} ->
            respond_grants(RecordId, AclRev, undefined, Row);
        {ok, #{<<"acl_revision">> := AclRev}} ->
            %% 非 owner：完整名单仅 owner 可读，只返回自己的活跃权限行
            respond_grants(RecordId, AclRev, Uid, undefined);
        {error, Reason} ->
            {error, Reason}
    end.

-spec respond_grants(integer(), pos_integer(), integer() | undefined, term()) ->
    {ok, map()} | {error, term()}.
respond_grants(RecordId, AclRev, GranteeFilter, _Row) ->
    {ok, Rows} =
        elib_pg:with_conn(fun(Conn) ->
            talk_record_grant_repo:list_active_tx(Conn, RecordId, GranteeFilter)
        end),
    {ok, #{
        <<"list">> => [grant_dto(R) || R <- Rows],
        <<"acl_revision">> => AclRev,
        <<"etag">> => acl_etag(AclRev)
    }}.

%% @private 授权行 DTO（T18 TalkRecordGrantModel 消费形状：
%% id/record_id/grantee_user_id/grantee_display_name/role/allow_export/
%% granted_by/revoked_at/expires_at；revoked_at 恒 null——仅活跃行）。
-spec grant_dto(map()) -> map().
grant_dto(R) ->
    #{
        <<"id">> => maps:get(<<"id">>, R),
        <<"record_id">> => maps:get(<<"record_id">>, R),
        <<"grantee_user_id">> => maps:get(<<"grantee_user_id">>, R),
        <<"grantee_display_name">> => maps:get(<<"grantee_display_name">>, R, <<>>),
        <<"role">> => maps:get(<<"role">>, R),
        <<"allow_export">> => maps:get(<<"allow_export">>, R, false),
        <<"granted_by">> => maps:get(<<"granted_by">>, R),
        <<"revoked_at">> => null,
        <<"expires_at">> => maps:get(<<"expires_at">>, R, null)
    }.

%% ===================================================================
%% PUT /:id/grants（全量替换）
%% ===================================================================

%% @doc owner 全量提交。Body：{grants: [{grantee_user_id, role,
%% allow_export, expires_at?}, ...]}（T18 TalkRecordGrantInput.toJson 形状）。
%% If-Match：acl 令牌或纯 acl_revision。返回保存后的全量名单与新 acl ETag。
-spec replace(integer(), integer(), binary() | undefined, map()) ->
    {ok, map()} | {error, term()}.
replace(Uid, RecordId, IfMatch, Body) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, #{<<"owner_user_id">> := Uid} = Row} ->
            replace_as_owner(Uid, RecordId, Row, IfMatch, Body);
        {ok, _} ->
            %% editor/viewer 不可改 ACL（§1：不可改 ACL 或删除原记录）
            {error, forbidden};
        {error, Reason} ->
            {error, Reason}
    end.

-spec replace_as_owner(integer(), integer(), map(), binary() | undefined, map()) ->
    {ok, map()} | {error, term()}.
replace_as_owner(Uid, RecordId, Row, IfMatch, Body) ->
    case parse_acl_if_match(IfMatch) of
        {ok, ExpectedAcl} ->
            case parse_grants(Body) of
                {ok, Entries} ->
                    case owner_row_check(Row, Entries) of
                        ok ->
                            run_replace(Uid, RecordId, Row, ExpectedAcl, Entries);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private owner 在主表：全量 PUT 不含 owner 行——清单出现 owner 即 422
%% （§2 grant 表“不能移除最后 owner”的主表侧校验）。
-spec owner_row_check(map(), [map()]) -> ok | {error, grant_owner_row}.
owner_row_check(#{<<"owner_user_id">> := OwnerUid}, Entries) ->
    case lists:any(fun(E) -> maps:get(grantee_user_id, E) =:= OwnerUid end, Entries) of
        true -> {error, grant_owner_row};
        false -> ok
    end.

-spec run_replace(integer(), integer(), map(), pos_integer(), [map()]) ->
    {ok, map()} | {error, term()}.
run_replace(Uid, RecordId, Row, ExpectedAcl, Entries) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> replace_tx(Conn, Uid, RecordId, Row, ExpectedAcl, Entries) end,
            [{reraise, false}]
        )
    of
        {ok, NewAcl} ->
            reload(RecordId, NewAcl);
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, {error, Reason}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_grants_replace_rolled_back, Uid, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 事务体：行锁（与并发删除互斥 + CAS 复核）→ 目录校验（逐
%% 目标，超归属范围 422）→ 全量替换 → acl_revision 推进 → 审计。
%% 返回 {ok, NewAcl}（with_tx 原样透传 fun 返回值）。
-spec replace_tx(any(), integer(), integer(), map(), pos_integer(), [map()]) ->
    {ok, pos_integer()}.
replace_tx(Conn, Uid, RecordId, _Row, ExpectedAcl, Entries) ->
    case talk_record_repo:lock_for_acl_tx(Conn, RecordId, ExpectedAcl) of
        {ok, LockedRow} ->
            directory_check_all(Conn, LockedRow, Entries),
            {ok, Revoked, Upserted} = talk_record_grant_repo:replace_tx(
                Conn, RecordId, [E#{granted_by => Uid} || E <- Entries]
            ),
            {ok, NewAcl} = talk_record_repo:bump_acl_revision_tx(Conn, RecordId),
            {ok, _AuditId} = talk_record_repo:insert_audit_tx(
                Conn,
                RecordId,
                Uid,
                <<"grants_replace">>,
                null,
                #{
                    <<"acl_revision">> => NewAcl,
                    <<"revoked">> => Revoked,
                    <<"upserted">> => Upserted,
                    <<"entries">> => length(Entries)
                }
            ),
            {ok, NewAcl};
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private 目录校验（§1/§8：不得授权超归属范围；复用 T15 目录校验）。
-spec directory_check_all(any(), map(), [map()]) -> ok.
directory_check_all(Conn, Row, Entries) ->
    lists:foreach(
        fun(#{grantee_user_id := TargetUid}) ->
            case talk_record_speaker_repo:target_in_directory_tx(Conn, Row, TargetUid) of
                {ok, true} -> ok;
                {ok, false} -> throw({talk_record_rollback, {error, target_not_in_directory}});
                {error, Reason} -> throw({talk_record_rollback, {error, Reason}})
            end
        end,
        Entries
    ).

%% ===================================================================
%% DELETE /:id/grants/:grant_id（撤销）
%% ===================================================================

%% @doc owner 撤销单条授权；If-Match；立即生效（§4/§11.3）。
%% 返回撤销后的全量名单与新 acl ETag（列表即时更新）。
-spec revoke(integer(), integer(), integer(), binary() | undefined) ->
    {ok, map()} | {error, term()}.
revoke(Uid, RecordId, GrantId, IfMatch) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, #{<<"owner_user_id">> := Uid}} ->
            revoke_as_owner(Uid, RecordId, GrantId, IfMatch);
        {ok, _} ->
            {error, forbidden};
        {error, Reason} ->
            {error, Reason}
    end.

-spec revoke_as_owner(integer(), integer(), integer(), binary() | undefined) ->
    {ok, map()} | {error, term()}.
revoke_as_owner(Uid, RecordId, GrantId, IfMatch) ->
    case parse_acl_if_match(IfMatch) of
        {ok, ExpectedAcl} ->
            run_revoke(Uid, RecordId, GrantId, ExpectedAcl);
        {error, Reason} ->
            {error, Reason}
    end.

-spec run_revoke(integer(), integer(), integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
run_revoke(Uid, RecordId, GrantId, ExpectedAcl) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> revoke_tx(Conn, Uid, RecordId, GrantId, ExpectedAcl) end,
            [{reraise, false}]
        )
    of
        {ok, NewAcl} ->
            reload(RecordId, NewAcl);
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, {error, Reason}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_grant_revoke_rolled_back, Uid, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 事务体：行锁 → 撤销活跃行（0 行 = 已撤/不存在 → 404）→
%% acl_revision 推进 → 审计（target_id=grant_id）。返回 {ok, NewAcl}。
-spec revoke_tx(any(), integer(), integer(), integer(), pos_integer()) -> {ok, pos_integer()}.
revoke_tx(Conn, Uid, RecordId, GrantId, ExpectedAcl) ->
    case talk_record_repo:lock_for_acl_tx(Conn, RecordId, ExpectedAcl) of
        {ok, _LockedRow} ->
            case talk_record_grant_repo:revoke_tx(Conn, RecordId, GrantId) of
                {ok, #{<<"grantee_user_id">> := GranteeUid, <<"role">> := Role}} ->
                    {ok, NewAcl} = talk_record_repo:bump_acl_revision_tx(Conn, RecordId),
                    {ok, _AuditId} = talk_record_repo:insert_audit_tx(
                        Conn,
                        RecordId,
                        Uid,
                        <<"grant_revoke">>,
                        GrantId,
                        #{
                            <<"acl_revision">> => NewAcl,
                            <<"grantee_user_id">> => GranteeUid,
                            <<"role">> => Role
                        }
                    ),
                    {ok, NewAcl};
                {error, Reason} ->
                    throw({talk_record_rollback, {error, Reason}})
            end;
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private 提交后回读全量活跃名单（响应体与新 ETag 同源）。
-spec reload(integer(), pos_integer()) -> {ok, map()} | {error, term()}.
reload(RecordId, NewAcl) ->
    {ok, Rows} =
        elib_pg:with_conn(fun(Conn) ->
            talk_record_grant_repo:list_active_tx(Conn, RecordId, undefined)
        end),
    {ok, #{
        <<"list">> => [grant_dto(R) || R <- Rows],
        <<"acl_revision">> => NewAcl,
        <<"etag">> => acl_etag(NewAcl)
    }}.

%% ===================================================================
%% 入参解析 / acl ETag
%% ===================================================================

%% @doc PUT 请求体 → 归一化条目列表
%% #{grantee_user_id, role(<<"viewer">>|<<"editor">>), allow_export,
%% expires_at(rfc3339 binary | null)}。清单内重复被授权人 → 422。
-spec parse_grants(map()) -> {ok, [map()]} | {error, atom()}.
parse_grants(#{<<"grants">> := Grants} = Body) when is_list(Grants) ->
    OwnerGuard = maps:get(owner_user_id, Body, undefined),
    case parse_grant_entries(Grants, []) of
        {ok, Entries} ->
            duplicate_check(Entries, OwnerGuard);
        {error, Reason} ->
            {error, Reason}
    end;
parse_grants(_) ->
    {error, invalid_grants_body}.

-spec parse_grant_entries([term()], [map()]) -> {ok, [map()]} | {error, atom()}.
parse_grant_entries([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_grant_entries([E | Rest], Acc) when is_map(E) ->
    case parse_grant_entry(E) of
        {ok, Entry} -> parse_grant_entries(Rest, [Entry | Acc]);
        {error, Reason} -> {error, Reason}
    end;
parse_grant_entries(_, _Acc) ->
    {error, invalid_grants_body}.

-spec parse_grant_entry(map()) -> {ok, map()} | {error, atom()}.
parse_grant_entry(Entry) ->
    Grantee = maps:get(<<"grantee_user_id">>, Entry, 0),
    Role = maps:get(<<"role">>, Entry, <<>>),
    AllowExport = maps:get(<<"allow_export">>, Entry, false),
    ExpiresAt = maps:get(<<"expires_at">>, Entry, null),
    case
        {valid_uid(Grantee), valid_role(Role), is_boolean(AllowExport), valid_expires_at(ExpiresAt)}
    of
        {true, true, true, true} ->
            {ok, #{
                grantee_user_id => Grantee,
                role => Role,
                allow_export => AllowExport,
                expires_at => norm_expires_at(ExpiresAt)
            }};
        {false, _, _, _} ->
            {error, invalid_grantee_user_id};
        {_, false, _, _} ->
            {error, invalid_grant_role};
        {_, _, false, _} ->
            {error, invalid_grants_body};
        {_, _, _, false} ->
            {error, invalid_expires_at}
    end.

%% @private 清单内同一被授权人重复 → 422（全量 PUT 语义不允许歧义）。
-spec duplicate_check([map()], term()) -> {ok, [map()]} | {error, grant_duplicate}.
duplicate_check(Entries, _OwnerGuard) ->
    Ids = [maps:get(grantee_user_id, E) || E <- Entries],
    case length(Ids) =:= length(lists:usort(Ids)) of
        true -> {ok, Entries};
        false -> {error, grant_duplicate}
    end.

-spec valid_uid(term()) -> boolean().
valid_uid(N) when is_integer(N), N > 0 -> true;
valid_uid(B) when is_binary(B) ->
    try
        binary_to_integer(B) > 0
    catch
        _:_ -> false
    end;
valid_uid(_) ->
    false.

-spec valid_role(term()) -> boolean().
valid_role(R) when R =:= <<"viewer">>; R =:= <<"editor">> -> true;
valid_role(_) -> false.

%% @private expires_at：null/缺失不限；提供则须为可解析 RFC3339 binary
%% （解析失败 422；过期值由读侧活跃谓词自然失效，不做时间前瞻裁决）。
-spec valid_expires_at(term()) -> boolean().
valid_expires_at(null) ->
    true;
valid_expires_at(undefined) ->
    true;
valid_expires_at(B) when is_binary(B), byte_size(B) =< 64 ->
    case elib_dt:rfc3339_to(B) of
        I when is_integer(I) -> true;
        _ -> false
    end;
valid_expires_at(_) ->
    false.

-spec norm_expires_at(term()) -> binary() | null.
norm_expires_at(null) -> null;
norm_expires_at(undefined) -> null;
norm_expires_at(B) when is_binary(B) -> B.

%% @doc acl ETag 令牌（T18 talkRecordAclEtagOf 同式：`acl:<rev>`）。
-spec acl_etag(pos_integer()) -> binary().
acl_etag(AclRevision) ->
    <<"acl:", (integer_to_binary(AclRevision))/binary>>.

%% @doc If-Match 解析（复用内容域解析器：接受 `acl:<rev>` 令牌与纯
%% revision `N` 两形态，带/不带引号与 W/ 前缀；缺头 428；非法 422）。
-spec parse_acl_if_match(binary() | undefined) -> {ok, pos_integer()} | {error, atom()}.
parse_acl_if_match(IfMatch) ->
    talk_record_content_common:parse_resource_if_match(<<"acl">>, 0, IfMatch).
