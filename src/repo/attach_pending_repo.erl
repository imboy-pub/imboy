-module(attach_pending_repo).
-moduledoc "已签发 presigned PUT 但尚未 confirm 的对象登记表（迁移 00000054）。".
%%%
% 已签发 presigned PUT 但尚未 confirm 的对象登记表（迁移 00000054）
%
% 存在意义见 #20：attach_logic:presign/5 原来签发后不落任何库行，
% 而 attachment_repo:orphan_list_for_delete/1 只扫 attachment 表。
% 于是"PUT 上去但从不 confirm"的对象在库里根本不存在，清理器看不见它，
% 占用的空间永远收不回来。本表补上这条登记。
%
% 生命周期：presign 写入 → confirm 删除 → 超龄未删 = 从未 confirm，由
% attach_cleanup_logic 连同 S3 对象一并回收。
%%%

-export([claim_expired_tx/3]).
-export([add/4]).
-export([register_v5/5, register_v5/6, status_lookup/1, register_content_pin/2]).
-export([remove/1]).
-export([list_expired/1]).
-export([delete_by_keys/1]).
-export([get_by_key/1]).

-include("log.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% V5 恢复合同：会话绝对 deadline=登记+2h；单次能力默认 3600s（02 §12）
-define(V5_SESSION_DEADLINE_SECONDS, 7200).
-define(V5_PUT_EXPIRY_SECONDS, 3600).

tablename() ->
    elib_pg_sql:public_tablename(<<"attach_pending">>).

%% @doc 登记一次 presign 签发。
%%
%% ON CONFLICT DO UPDATE 而非 DO NOTHING：同一 object_key 被重新签发时
%% （客户端重试 / 换 MIME 重来），过期计时必须从最后一次签发算起，
%% 否则会按第一次的时间把正在上传的对象提前删掉。
-spec add(binary(), binary(), binary(), integer()) -> ok | {error, term()}.
add(ObjectKey, Bucket, Scope, Uid) ->
    Tb = tablename(),
    Sql = [
        <<"INSERT INTO ">>,
        Tb,
        <<
            " (object_key, bucket, scope, creator_user_id, created_at)"
            " VALUES ($1, $2, $3, $4, NOW())"
            " ON CONFLICT (object_key) DO UPDATE SET"
            " bucket = EXCLUDED.bucket,"
            " scope = EXCLUDED.scope,"
            " creator_user_id = EXCLUDED.creator_user_id,"
            " created_at = NOW()"
        >>
    ],
    normalize(elib_pg:execute(Sql, [ObjectKey, Bucket, Scope, Uid])).

%% @doc 按 object_key 查登记行（multipart 直传端点的归属/范围校验用）。
%% 返回 bucket/scope/creator_user_id：upload 端点据此把对象写入正确桶、
%% 复核教学 scope mime 白名单，并拒绝非本人 pending 记录的 key。
-spec get_by_key(binary()) -> {ok, map()} | {error, not_found | term()}.
get_by_key(ObjectKey) ->
    Tb = tablename(),
    Sql = [
        <<"SELECT object_key, bucket, scope, creator_user_id, created_at FROM ">>,
        Tb,
        <<" WHERE object_key = $1 LIMIT 1">>
    ],
    case elib_pg:query(Sql, [ObjectKey]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, R} -> {error, R}
    end.

%% @doc confirm 成功后销账。对象已登记进 attachment 表，不再是待确认状态。
-spec remove(binary()) -> ok | {error, term()}.
remove(ObjectKey) ->
    Tb = tablename(),
    Sql = [<<"DELETE FROM ">>, Tb, <<" WHERE object_key = $1">>],
    normalize(elib_pg:execute(Sql, [ObjectKey])).

%% @doc 列出超龄仍未 confirm 的登记（单批上限 500，与 orphan 清理一致）。
%%
%% 单位是**小时**不是天：presign 有效期只有分钟级，超过若干小时还没 confirm
%% 就是垃圾。attachment 的 orphan 清理用 30 天 + 7 天下限，那是保护"已确认但
%% 暂未被引用"的正常附件，两者语义不同不可混用。
-spec list_expired(integer()) -> {ok, [map()]} | {error, term()}.
list_expired(AgeHours) ->
    Tb = tablename(),
    AttachTb = elib_pg_sql:public_tablename(<<"attachment">>),
    Sql = [
        <<"SELECT p.object_key, p.bucket FROM ">>,
        Tb,
        <<" p WHERE p.created_at < NOW() - ($1 * INTERVAL '1 hour')">>,
        %% 关键守卫：已经登记进 attachment 表的对象一律不碰。
        %% confirm 成功后销账（pending_remove）如果失败，pending 行会残留，
        %% 少了这个条件就会把一个**已转正的附件**连同 S3 对象删掉 —— 那是
        %% 真正的数据丢失，比"收不回垃圾"严重得多。宁可漏收，不可误删。
        <<" AND NOT EXISTS (SELECT 1 FROM ">>,
        AttachTb,
        <<" a WHERE a.path = p.object_key)">>,
        <<" ORDER BY p.created_at ASC LIMIT 500">>
    ],
    elib_pg:query(Sql, [AgeHours]).

%% @doc 按 object_key 批量删除登记行（S3 对象删除成功后调用）
-spec delete_by_keys([binary()]) -> ok | {error, term()}.
delete_by_keys([]) ->
    ok;
delete_by_keys(Keys) ->
    Tb = tablename(),
    Placeholders = iolist_to_binary(
        lists:join(
            <<",">>,
            [[<<"$">>, integer_to_binary(I)] || I <- lists:seq(1, length(Keys))]
        )
    ),
    Sql = [
        <<"DELETE FROM ">>,
        Tb,
        <<" WHERE object_key IN (">>,
        Placeholders,
        <<")">>
    ],
    normalize(elib_pg:execute(Sql, Keys)).

%% ===================================================================
%% V5 恢复合同（02-main-plan §12）：登记先行、失败关闭
%% ===================================================================

%% @doc V5 事务登记：client_upload_id + 原声明快照 + 绝对 deadline/能力到期。
%% 由迁移 178 的受限函数 fn_v5_register_pending 在库内做 UUID 查重、
%% 快照等值校验、key 终身不复用与位置冻结；COMMIT 成功才允许签发。
%% 幂等：同 UUID 同快照重试返回原 session_state；不一致则 {error, _}，
%% 调用方绝不签发 URL（AC01）。
-spec register_v5(
    binary(),
    binary(),
    binary(),
    integer(),
    undefined | binary(),
    map() | undefined
) ->
    {ok, #{session_state => binary(), client_upload_id => binary() | undefined}}
    | {error, term()}.
register_v5(ObjectKey, Bucket, Scope, Uid, ClientUploadId, Registration) when
    is_map(Registration), map_size(Registration) > 0
->
    Now = erlang:system_time(second),
    Deadline = Now + ?V5_SESSION_DEADLINE_SECONDS,
    Expiry = Now + ?V5_PUT_EXPIRY_SECONDS,
    Subject = #{
        <<"kind">> => <<"user">>,
        <<"principal">> => integer_to_binary(Uid)
    },
    Snapshot = #{
        <<"subject">> => Subject,
        <<"declaration">> => Registration
    },
    Args = [
        ObjectKey,
        Bucket,
        Scope,
        Uid,
        ClientUploadId,
        jsone:encode(Snapshot),
        iso8601(Deadline),
        iso8601(Expiry),
        null,
        null,
        null,
        null
    ],
    Sql = [
        <<
            "SELECT out_object_key, out_session_state, out_client_upload_id"
            " FROM fn_v5_register_pending($1,$2,$3,$4,"
        >>,
        case ClientUploadId of
            undefined -> <<"NULL::uuid">>;
            _ -> <<"$5::uuid">>
        end,
        <<
            ",$6::jsonb,$7::timestamptz,$8::timestamptz,"
            "NULL,NULL,NULL,NULL)"
        >>
    ],
    %% 无 UUID 的旧客户端走 legacy add（保留旧行为）
    case ClientUploadId of
        undefined ->
            case add(ObjectKey, Bucket, Scope, Uid) of
                ok -> {ok, #{session_state => <<"registered">>, client_upload_id => undefined}};
                {error, R} -> {error, R}
            end;
        _ ->
            Params = [A || {I, A} <- lists:zip(lists:seq(1, 12), Args), I =/= 5],
            case elib_pg:query(Sql, Params) of
                {ok, [Row | _]} ->
                    {ok, #{
                        session_state => maps:get(<<"out_session_state">>, Row),
                        client_upload_id => maps:get(<<"out_client_upload_id">>, Row)
                    }};
                {ok, []} ->
                    {error, registration_not_returned};
                {error, R} ->
                    {error, R}
            end
    end.

register_v5(ObjectKey, Bucket, Scope, Uid, ClientUploadId) ->
    register_v5(ObjectKey, Bucket, Scope, Uid, ClientUploadId, undefined).

%% @doc status/renew 查找：client_upload_id 或 object_key。
-spec status_lookup(binary()) -> {ok, map()} | {error, not_found | term()}.
status_lookup(IdOrKey) ->
    Tb = tablename(),
    Sql = [
        <<"SELECT object_key, bucket, scope, creator_user_id, session_state,">>,
        <<" client_upload_id, upload_expires_at, session_deadline">>,
        <<" FROM ">>,
        Tb,
        <<" WHERE (client_upload_id::text = $1 OR object_key = $1) LIMIT 1">>
    ],
    case elib_pg:query(Sql, [IdOrKey]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, R} -> {error, R}
    end.

iso8601(UnixSeconds) ->
    Base = calendar:system_time_to_rfc3339(UnixSeconds, [{unit, second}, {offset, "Z"}]),
    unicode:characters_to_binary(Base).

%% @doc V5 内容 pin（02 §14）：upload_content 从 NULL 一次固定；
%% 同 receipt 幂等；不同值 {error, content_mismatch}（网关 409）。
%% 由迁移 179 的受限函数 fn_v5_pin_pending_content 在库内裁决。
-spec register_content_pin(binary(), map()) -> ok | {error, term()}.
register_content_pin(ObjectKey, Receipt) ->
    case status_lookup(ObjectKey) of
        {ok, _} ->
            Sql = [
                <<"SELECT fn_v5_pin_pending_content($1, $2::jsonb)">>
            ],
            case elib_pg:query(Sql, [ObjectKey, jsone:encode(Receipt)]) of
                {ok, [#{<<"fn_v5_pin_pending_content">> := true}]} -> ok;
                {error, #error{code = <<"23510">>}} -> {error, content_mismatch};
                {error, #error{code = <<"P0002">>}} -> {error, not_found};
                {ok, _} -> {error, invalid_pin_result};
                {error, R} -> {error, R}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% elib_pg:execute/2 的成功返回有 {ok, N} 与 {ok, N, Rows} 两种形态
normalize({ok, _}) -> ok;
normalize({ok, _, _}) -> ok;
normalize({error, R}) -> {error, R}.

%% Serialize with attachment confirmation before rechecking pending eligibility.
claim_expired_tx(Conn, Key, AgeHours) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT pg_advisory_xact_lock(hashtextextended($1,168))">>,
            [Key]
        )
    of
        {ok, _} ->
            elib_pg:query(
                Conn,
                [
                    <<"DELETE FROM ">>,
                    tablename(),
                    <<
                        " p WHERE p.object_key=$1"
                        " AND p.created_at<NOW()-($2*INTERVAL '1 hour')"
                        " AND NOT EXISTS(SELECT 1 FROM attachment a WHERE a.path=p.object_key)"
                        " RETURNING p.bucket"
                    >>
                ],
                [Key, AgeHours]
            );
        {error, R} ->
            {error, R}
    end.
