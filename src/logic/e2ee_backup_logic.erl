-module(e2ee_backup_logic).
%%%===================================================================
%%% @doc E2EE 加密密钥备份 Logic 层（4S 模式）
%%%
%%% 零信任红线：服务端零密码学。本模块只做鉴权后的参数校验、
%%% 版本单调性检查与密文透传存取；密文包由客户端用恢复口令
%%% 派生密钥加密（PBKDF2 + AES-256-GCM），服务端不可解。
%%% 明文拦截（PEM 头拒收）是零信任的机器可查下限：
%%% 挡住客户端 bug 误传明文私钥。
%%%===================================================================

%%===================================================================
%%% API Functions Export
%%===================================================================
-export([put_backup/2]).
-export([get_backup/1]).
-export([info/1]).
-export([delete_backup/1]).

%% base64 密文包上限（1MB，身份私钥 + 房间 session keys 远小于此）
-define(MAX_PAYLOAD_BYTES, 1048576).
%% KDF 迭代下限（客户端保证 >=310000；服务端存储下限防降级攻击，与 DB CHECK 一致）
-define(MIN_KDF_ITERATIONS, 100000).
%% 元数据字段上限（security-reviewer M1：拦掉 kdf_salt 存储放大面；与 DB 约束一致）
-define(MAX_SALT_BYTES, 256).
-define(MAX_HASH_BYTES, 128).
-define(MAX_ALGO_BYTES, 40).
-define(MAX_INT4, 2147483647).

%%===================================================================
%%% API Functions
%%===================================================================

%% @doc 上传新版本加密备份；backup_version 必须 = 当前最新 + 1（防并发覆盖）
-spec put_backup(integer(), map()) -> {ok, map()} | {error, binary(), integer()}.
put_backup(Uid, Params) ->
    Version = maps:get(<<"backup_version">>, Params, 0),
    Payload = maps:get(<<"encrypted_payload">>, Params, <<>>),
    case validate_put(Version, Params, Payload) of
        {error, Msg} ->
            {error, Msg, 400};
        ok ->
            case expected_version(Uid) of
                {error, Msg, Code} ->
                    {error, Msg, Code};
                {ok, Version} ->
                    do_save(Uid, Version, Params, Payload);
                {ok, _Expected} ->
                    {error, <<"version_conflict">>, 409}
            end
    end.

%% @doc 拉取最新版本备份（含密文与 KDF 参数，恢复端用）
-spec get_backup(integer()) -> {ok, map()} | {error, binary(), integer()}.
get_backup(Uid) ->
    case e2ee_backup_ds:latest(Uid) of
        {ok, Row} ->
            {ok, maps:without([<<"id">>, <<"uid">>], Row)};
        {error, not_found} ->
            {error, <<"backup_not_found">>, 404};
        {error, _Reason} ->
            {error, <<"backup_query_failed">>, 500}
    end.

%% @doc 备份存在性探测（恢复横幅用）；响应不含密文与盐值
-spec info(integer()) -> {ok, map()} | {error, binary(), integer()}.
info(Uid) ->
    case e2ee_backup_ds:latest(Uid) of
        {ok, Row} ->
            {ok, #{
                <<"has_backup">> => true,
                <<"backup_version">> => maps:get(<<"backup_version">>, Row),
                <<"algo">> => maps:get(<<"algo">>, Row),
                <<"kdf_iterations">> => maps:get(<<"kdf_iterations">>, Row),
                <<"created_at">> => maps:get(<<"created_at">>, Row)
            }};
        {error, not_found} ->
            {ok, #{<<"has_backup">> => false}};
        {error, _Reason} ->
            {error, <<"backup_query_failed">>, 500}
    end.

%% @doc 删除用户全部备份版本
-spec delete_backup(integer()) -> {ok, map()} | {error, binary(), integer()}.
delete_backup(Uid) ->
    case e2ee_backup_ds:delete_by_uid(Uid) of
        {ok, Count} ->
            {ok, #{<<"deleted">> => Count}};
        {error, _Reason} ->
            {error, <<"backup_delete_failed">>, 500}
    end.

%%===================================================================
%%% Internal Functions
%%===================================================================

%% @doc 上传参数校验（含明文拦截）
-spec validate_put(term(), map(), term()) -> ok | {error, binary()}.
validate_put(Version, Params, Payload) ->
    KdfSalt = maps:get(<<"kdf_salt">>, Params, <<>>),
    KdfIterations = maps:get(<<"kdf_iterations">>, Params, 0),
    PayloadHash = maps:get(<<"payload_hash">>, Params, <<>>),
    Algo = maps:get(<<"algo">>, Params, <<"pbkdf2-sha256/aes-256-gcm">>),
    if
        not (is_integer(Version) andalso Version > 0 andalso Version =< ?MAX_INT4) ->
            {error, <<"invalid_backup_version">>};
        not (is_binary(KdfSalt) andalso byte_size(KdfSalt) > 0 andalso
            byte_size(KdfSalt) =< ?MAX_SALT_BYTES) ->
            {error, <<"invalid_kdf_salt">>};
        not (is_integer(KdfIterations) andalso KdfIterations >= ?MIN_KDF_ITERATIONS andalso
            KdfIterations =< ?MAX_INT4) ->
            {error, <<"invalid_kdf_iterations">>};
        not (is_binary(PayloadHash) andalso byte_size(PayloadHash) > 0 andalso
            byte_size(PayloadHash) =< ?MAX_HASH_BYTES) ->
            {error, <<"invalid_payload_hash">>};
        not (is_binary(Algo) andalso byte_size(Algo) > 0 andalso byte_size(Algo) =< ?MAX_ALGO_BYTES) ->
            {error, <<"invalid_algo">>};
        not (is_binary(Payload) andalso byte_size(Payload) > 0) ->
            {error, <<"encrypted_payload_required">>};
        byte_size(Payload) > ?MAX_PAYLOAD_BYTES ->
            {error, <<"encrypted_payload_too_large">>};
        true ->
            validate_ciphertext(Payload)
    end.

%% @doc 明文拦截：payload 必须是合法 base64，且解码后不得是 PEM 明文私钥
-spec validate_ciphertext(binary()) -> ok | {error, binary()}.
validate_ciphertext(<<"-----BEGIN", _/binary>>) ->
    {error, <<"plaintext_payload_rejected">>};
validate_ciphertext(Payload) ->
    try base64:decode(pad_base64(Payload)) of
        <<"-----BEGIN", _/binary>> ->
            {error, <<"plaintext_payload_rejected">>};
        _Decoded ->
            ok
    catch
        _:_ ->
            {error, <<"invalid_base64_payload">>}
    end.

%% @doc 补齐 unpadded base64（客户端可能发无填充 base64）
-spec pad_base64(binary()) -> binary().
pad_base64(B) ->
    case byte_size(B) rem 4 of
        2 -> <<B/binary, "==">>;
        3 -> <<B/binary, "=">>;
        _ -> B
    end.

%% @doc 期望的下一个版本号 = 当前最新 + 1，无备份时为 1
-spec expected_version(integer()) -> {ok, integer()} | {error, binary(), integer()}.
expected_version(Uid) ->
    case e2ee_backup_ds:latest(Uid) of
        {ok, #{<<"backup_version">> := Current}} ->
            {ok, Current + 1};
        {error, not_found} ->
            {ok, 1};
        {error, _Reason} ->
            {error, <<"backup_query_failed">>, 500}
    end.

-spec do_save(integer(), integer(), map(), binary()) -> {ok, map()} | {error, binary(), integer()}.
do_save(Uid, Version, Params, Payload) ->
    Record = #{
        <<"uid">> => Uid,
        <<"backup_version">> => Version,
        <<"algo">> => maps:get(<<"algo">>, Params, <<"pbkdf2-sha256/aes-256-gcm">>),
        <<"kdf_salt">> => maps:get(<<"kdf_salt">>, Params),
        <<"kdf_iterations">> => maps:get(<<"kdf_iterations">>, Params),
        <<"encrypted_payload">> => Payload,
        <<"payload_hash">> => maps:get(<<"payload_hash">>, Params)
    },
    case e2ee_backup_ds:save(Record) of
        {ok, _Id} ->
            {ok, #{<<"backup_version">> => Version}};
        {error, Reason} ->
            case is_unique_violation(Reason) of
                %% 并发 put 撞版本：UNIQUE(uid, backup_version) 兜底
                true -> {error, <<"version_conflict">>, 409};
                false -> {error, <<"backup_save_failed">>, 500}
            end
    end.

%% @doc 识别 PG 唯一约束冲突（多种 epgsql 错误形态，MIRROR payment_callback_logic）
-spec is_unique_violation(term()) -> boolean().
is_unique_violation({pgsql_error, #{code := <<"23505">>}}) -> true;
is_unique_violation({error, {pgsql_error, #{code := <<"23505">>}}}) -> true;
is_unique_violation({error, error, _, unique_violation, _, _}) -> true;
is_unique_violation({error, {error, error, <<"23505">>, unique_violation, _, _}}) -> true;
is_unique_violation(_) -> false.
