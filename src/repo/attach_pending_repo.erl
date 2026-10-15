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
-export([register_v5_tx/7, renew_capability/3, renew_capability_tx/4]).
-export([lock_session_tx/2, confirm_session_tx/3]).
-export([intent_pending_delete_tx/4, intent_pending_delete/3]).
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
    {ok, #{
        session_state => binary(),
        client_upload_id => binary() | undefined,
        deadline => integer(),
        expires_at => integer()
    }}
    | {error, term()}.
%% legacy：无登记快照（renew 同 key 重签、无 UUID 旧客户端）走 add，
%% 保留 V5 之前的行为；不写入 fn_v5 快照与 deadline。
register_v5(ObjectKey, Bucket, Scope, Uid, _ClientUploadId, undefined) ->
    case add(ObjectKey, Bucket, Scope, Uid) of
        ok -> {ok, #{session_state => <<"registered">>, client_upload_id => undefined}};
        {error, R} -> {error, R}
    end;
%% Old clients still use key-based confirm; new writes always get a controlled session.
register_v5(ObjectKey, Bucket, Scope, Uid, undefined, Registration) when is_map(Registration) ->
    register_v5(ObjectKey, Bucket, Scope, Uid, elib_uuid:gen_v7(), Registration);
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
    execute_registration(fun elib_pg:query/2, Args).

register_v5(ObjectKey, Bucket, Scope, Uid, ClientUploadId) ->
    register_v5(ObjectKey, Bucket, Scope, Uid, ClientUploadId, undefined).

%% @doc 事务内 V5 登记（企业内部入口的幂等事务回调使用，F19：稳定身份
%% 只在事务内保存，签发一律由 handler COMMIT 分支完成）。
%% Registration 为完整快照（必含 <<"subject">> 与 <<"declaration">>，
%% 如 kind=application）；复用 fn_v5_register_pending 协议（uuid 查重/
%% 快照等值/key 不复用/位置冻结）。
-spec register_v5_tx(
    any(), binary(), binary(), binary(), integer() | binary(), binary() | undefined, map()
) ->
    {ok, #{
        session_state => binary(),
        client_upload_id => binary(),
        deadline => integer(),
        expires_at => integer()
    }}
    | {error, term()}.
register_v5_tx(Conn, ObjectKey, Bucket, Scope, UidOrPrincipal, ClientUploadId, Registration) when
    is_map(Registration), map_size(Registration) > 0, ClientUploadId =/= undefined
->
    Now = erlang:system_time(second),
    Deadline = Now + ?V5_SESSION_DEADLINE_SECONDS,
    Expiry = Now + ?V5_PUT_EXPIRY_SECONDS,
    Args = [
        ObjectKey,
        Bucket,
        Scope,
        principal_to_int(UidOrPrincipal),
        ClientUploadId,
        jsone:encode(Registration),
        iso8601(Deadline),
        iso8601(Expiry),
        null,
        null,
        null,
        null
    ],
    execute_registration(fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, Args).

execute_registration(Query, Args) ->
    case registration_params(Query, Args) of
        {ok, Params} ->
            Sql = <<"SELECT out_object_key, out_session_state, out_client_upload_id "
                    "FROM fn_v5_register_pending($1,$2,$3,$4,$5::uuid,$6::jsonb,"
                    "$7::timestamptz,$8::timestamptz,$9::text,$10::text,$11::text,$12::text)">>,
            case Query(Sql, Params) of
                {ok, [Row]} -> persisted_registration(Query, Row);
                {ok, []} -> {error, registration_not_returned};
                {error, _} = E -> E
            end;
        {error, _} = E -> E
    end.

registration_params(Query, [Key,Bucket,Scope,Uid,Cid,Snapshot,Deadline,Expiry|_]) ->
    %% Replays retain even unknown legacy locations; never backfill from current defaults.
    case Query(<<"SELECT bucket, storage_driver, storage_namespace, storage_container, "
                 "storage_locator_sha256 FROM attach_pending WHERE client_upload_id=$1::uuid">>, [Cid]) of
        {ok, [Row]} ->
            Location = [maps:get(K,Row,null) || K <- [<<"storage_driver">>,<<"storage_namespace">>,
                <<"storage_container">>,<<"storage_locator_sha256">>]],
            {ok, [Key,maps:get(<<"bucket">>,Row),Scope,Uid,Cid,Snapshot,Deadline,Expiry] ++ Location};
        {ok, []} ->
            case attachment_storage:registration_location(Scope,Bucket,Key) of
                {ok, #{driver := Driver,namespace := Ns,container := Container,locator_digest := Digest}} ->
                    FrozenBucket = case Driver of <<"garage">> -> Container; _ -> Bucket end,
                    {ok, [Key,FrozenBucket,Scope,Uid,Cid,Snapshot,Deadline,Expiry,
                          Driver,Ns,Container,Digest]};
                {error, _} = E -> E
            end;
        {error, _} = E -> E
    end.

%% A UUID replay returns the original key and committed window, never this attempt's random key/time.
persisted_registration(Query, Row) ->
    Key = maps:get(<<"out_object_key">>, Row),
    Sql = [
        <<"SELECT object_key, bucket, scope, session_state, client_upload_id, confirmed_attachment_id, ">>,
        <<"storage_driver, storage_namespace, storage_container, storage_locator_sha256, ">>,
        <<"extract(epoch from session_deadline)::bigint AS deadline, ">>,
        <<"extract(epoch from upload_expires_at)::bigint AS expires_at FROM ">>,
        tablename(), <<" WHERE object_key=$1">>
    ],
    case Query(Sql, [Key]) of
        {ok, [Session]} ->
            {ok, #{object_key => maps:get(<<"object_key">>, Session),
                   bucket => maps:get(<<"bucket">>, Session),
                   scope => maps:get(<<"scope">>, Session),
                   session_state => maps:get(<<"session_state">>, Session),
                   client_upload_id => maps:get(<<"client_upload_id">>, Session),
                   confirmed_attachment_id => maps:get(<<"confirmed_attachment_id">>, Session),
                   storage_driver => maps:get(<<"storage_driver">>, Session, null),
                   storage_namespace => maps:get(<<"storage_namespace">>, Session, null),
                   storage_container => maps:get(<<"storage_container">>, Session, null),
                   storage_locator_sha256 => maps:get(<<"storage_locator_sha256">>, Session, null),
                   deadline => maps:get(<<"deadline">>, Session),
                   expires_at => maps:get(<<"expires_at">>, Session)}};
        {ok, []} -> {error, registration_not_returned};
        {error, Reason} -> {error, Reason}
    end.

principal_to_int(I) when is_integer(I) -> I;
principal_to_int(B) when is_binary(B) ->
    case
        try
            binary_to_integer(B)
        catch
            _:_ -> 0
        end
    of
        N when is_integer(N) -> N;
        _ -> 0
    end.

%% @doc renew：同 key 单调提交新能力到期（02 §143）。
%% 条件 UPDATE（registered 且绝对 deadline 未过）与 intent/GC 以行锁线性化：
%% intent 先提交则 0 行（终态），本函数先提交则 intent 读到更晚 expiry。
%% NewExpirySec 由调用方以 min(now+TTL, deadline) 冻结传入；历史最晚值保留。
-spec renew_capability(binary(), integer(), integer()) ->
    {ok, #{committed_expires_at => integer(), deadline => integer()}} | {error, term()}.
renew_capability(Key, Uid, NewExpirySec) ->
    renew_result(elib_pg:query(renew_sql(), [Key, Uid, iso8601(NewExpirySec)])).

%% 事务内版本（企业内部入口幂等事务回调使用）。
-spec renew_capability_tx(any(), binary(), integer() | binary(), integer()) ->
    {ok, #{committed_expires_at => integer(), deadline => integer()}} | {error, term()}.
renew_capability_tx(Conn, Key, UidOrPrincipal, NewExpirySec) ->
    renew_result(
        elib_pg:query(
            Conn, renew_sql(), [Key, principal_to_int(UidOrPrincipal), iso8601(NewExpirySec)]
        )
    ).

renew_sql() ->
    <<
        "UPDATE attach_pending SET upload_expires_at = "
        "GREATEST(COALESCE(upload_expires_at, to_timestamp($3)), to_timestamp($3)) "
        "WHERE object_key = $1 AND creator_user_id = $2 "
        "AND session_state = 'registered' "
        "AND session_deadline IS NOT NULL AND session_deadline > now() "
        "RETURNING extract(epoch from upload_expires_at)::bigint AS committed_expiry, "
        "extract(epoch from session_deadline)::bigint AS deadline"
    >>.

renew_result({ok, [Row | _]}) ->
    {ok, #{
        committed_expires_at => maps:get(<<"committed_expiry">>, Row),
        deadline => maps:get(<<"deadline">>, Row)
    }};
renew_result({ok, []}) ->
    {error, not_renewable};
renew_result({error, R}) ->
    {error, R}.

%% @doc 持锁重查（02 §4：真实对象字节校验在事务外，持锁后必须重查
%% 会话状态与 upload_content）：锁定 pending 行并回读最新 pin 快照，
%% 字节比对基准以此为准，锁前读到的快照不构成授权。
-spec lock_session_tx(any(), binary()) -> {ok, map() | not_found} | {error, term()}.
lock_session_tx(Conn, ObjectKey) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT object_key, session_state, upload_content, upload_registration, creator_user_id, scope,"
                " storage_driver, storage_namespace, storage_container,"
                " storage_locator_sha256"
                " FROM attach_pending WHERE object_key=$1 FOR UPDATE"
            >>,
            [ObjectKey]
        )
    of
        {ok, [Row]} -> {ok, decode_content(decode_registration(Row))};
        {ok, []} -> {ok, not_found};
        {error, _} = Error -> Error
    end.

decode_registration(Row) ->
    case maps:get(<<"upload_registration">>,Row,undefined) of
        B when is_binary(B) -> Row#{<<"upload_registration">> => jsone:decode(B)};
        _ -> Row
    end.

%% jsonb 列形态归一：epgsql 依连接配置可能回已解码 map 或原始 binary，
%% pin 复核层统一按 map 消费。
decode_content(Row) ->
    case maps:get(<<"upload_content">>, Row, undefined) of
        B when is_binary(B) -> Row#{<<"upload_content">> => jsone:decode(B)};
        _ -> Row
    end.

%% @doc confirm 线性化点（02 §4）：持锁后合法 registered→confirmed，
%% metadata 与会话确认同事务；同 attachment_id 重放返回原 id（幂等），
%% 已确认会话换 id / 非法来源态 → {error, confirm_conflict}（23510）。
-spec confirm_session_tx(any(), binary(), integer() | binary()) ->
    {ok, ConfirmedId :: integer()} | {error, term()}.
confirm_session_tx(Conn, ObjectKey, AttachmentId) ->
    case
        elib_pg:query(Conn, <<"SELECT fn_v5_confirm_pending($1, $2)">>, [
            ObjectKey, ec_cnv:to_integer(AttachmentId)
        ])
    of
        {ok, [#{<<"fn_v5_confirm_pending">> := Id}]} -> {ok, Id};
        {ok, []} -> {error, confirm_not_returned};
        {error, #error{code = <<"23510">>}} -> {error, confirm_conflict};
        {error, #error{code = <<"P0002">>}} -> {error, not_found};
        {error, R} -> {error, R}
    end.

%% W4-01 入口收口（02 §4/§5）：pending 域删除意图（迁移 184）。同事务
%% 状态迁移 + ready job + 审计；字节由 Worker 删。Mode='reject'（主动拒绝，
%% registered→rejected→delete_intent 两跳）|'sweep'（回收清扫，能力窗已耗尽）。
-spec intent_pending_delete_tx(pid() | any(), binary(), binary(), binary()) ->
    ok | {error, term()}.
intent_pending_delete_tx(Conn, ObjectKey, Mode, Reason) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT fn_v5_intent_pending_delete($1, $2, $3)">>,
            [ObjectKey, Reason, Mode]
        )
    of
        {ok, _} -> ok;
        {error, #error{code = <<"23510">>}} -> {error, intent_conflict};
        {error, #error{code = <<"23505">>}} -> {error, tombstone};
        {error, #error{code = <<"P0002">>}} -> {error, not_found};
        {error, R} -> {error, R}
    end.

-spec intent_pending_delete(binary(), binary(), binary()) -> ok | {error, term()}.
intent_pending_delete(ObjectKey, Mode, Reason) ->
    elib_pg:with_tx(fun(Conn) ->
        intent_pending_delete_tx(Conn, ObjectKey, Mode, Reason)
    end).

%% @doc status/renew 查找：client_upload_id 或 object_key。
-spec status_lookup(binary()) -> {ok, map()} | {error, not_found | term()}.
status_lookup(IdOrKey) ->
    Tb = tablename(),
    Sql = [
        <<"SELECT object_key, bucket, scope, creator_user_id, session_state,">>,
        <<" client_upload_id, upload_expires_at, session_deadline, upload_registration, ">>,
        <<"storage_driver, storage_namespace, storage_container, storage_locator_sha256">>,
        <<" FROM ">>,
        Tb,
        <<" WHERE (client_upload_id::text = $1 OR object_key = $1) LIMIT 1">>
    ],
    case elib_pg:query(Sql, [IdOrKey]) of
        {ok, [Row | _]} ->
            %% 登记快照同口径归一（renew 的 declared_mime 按 map 消费）
            case maps:get(<<"upload_registration">>, Row, undefined) of
                B when is_binary(B) -> {ok, Row#{<<"upload_registration">> => jsone:decode(B)}};
                _ -> {ok, Row}
            end;
        {ok, []} ->
            {error, not_found};
        {error, R} ->
            {error, R}
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
