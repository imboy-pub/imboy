%%% @doc 企业托管加密实现（`eb_crypto_port` 的真实现）。
%%%
%%% 依据：plan v4.1 EB-D05、§2.1 #9/#13、§4.1、§8 EB-03；EB-02 的
%%% `application/eb_crypto_port.erl` 是冻结契约（本模块声明同一 behaviour）。
%%%
%%% 设计要点：
%%%   * **不自造密码学**：AEAD 一律走 `elib_cipher:aes_gcm_encrypt/2` /
%%%     `aes_gcm_decrypt/2`（AES-256-GCM）；子密钥派生走
%%%     `elib_cipher:derive_master_password/2`（PBKDF2-HMAC-SHA256，标准 KDF，
%%%     输入是 32 字节随机密钥而不是口令）；组织域摘要走 `crypto:mac(hmac, sha256, ...)`。
%%%   * **AAD 进入密钥**：子密钥的盐 = SHA-256(域分隔前缀 || 作用域字节)，因此
%%%     同一密文换作用域后连密钥都不同；封装里另存 `aad_hash` 供解封前 fail-fast
%%%     比对。二者叠加使「把 A 会话的密文搬到 B 会话」既过不了摘要比对，也过不了
%%%     GCM 认证（见套件 aad_is_bound_into_key_not_only_digest 用例）。
%%%   * **fail-closed**：缺主密钥 / 密钥长度不对 / 未知密钥版本 / 密钥版本不符 /
%%%     AAD 不符 / 密文被篡改，一律返回 error 元组，绝不回落明文、默认密钥或旧版本。
%%%   * **不读全局单例**：主密钥由装配层经 `KeyRef` 传入（本模块不调 config_ds），
%%%     也不产生任何日志（日志不可能泄露明文）。
%%%
%%% 关于 `elib_kdf` 的口径偏离（已登记 RESULT.deviations）：
%%% `elib_kdf` 只导出**口令存储串**（`$v2$pbkdf2_sha512$...`）形态，且被
%%% `kdf_v2_enabled` 配置门控（默认 false ⇒ `hash_v2/2` 返回 `{error, disabled}`），
%%% 不提供裸对称子密钥 API，也没有可供「资源域隔离」使用的盐入参；把它接进来
%%% 会让企业消息加密依赖「口令哈希」的配置开关。因此本模块复用**同一模块族**
%%% 里语义正确的 KDF（`elib_cipher:derive_master_password/2`，PBKDF2-HMAC-SHA256）
%%% 与 AES-GCM（`elib_cipher:aes_gcm_*`），算法全部是标准原语。
-module(eb_managed_crypto).

-behaviour(eb_crypto_port).

-export([
    seal/3,
    open/3,
    seal_scoped/3,
    open_scoped/3,
    subject_hmac/4,
    aad_bytes/1,
    aad_hash/1,
    scope_bytes/1,
    algorithm/0,
    key_ref/1,
    open_message_body/8
]).

-define(ALG, <<"aes-256-gcm">>).
-define(KDF_LABEL, <<"pbkdf2_sha256+scope-salt-v1">>).
-define(KEY_BYTES, 32).
-define(AAD_LABEL, <<"imboy.enterprise.message.aad.v1">>).
-define(SCOPE_LABEL, <<"imboy.enterprise.resource.aad.v1">>).
-define(HMAC_LABEL, <<"imboy.enterprise.subject-hmac.v1">>).
-define(SUBKEY_LABEL, <<"imboy.enterprise.managed.aes-gcm.v1">>).

%% ===================================================================
%% eb_crypto_port 实现（消息作用域：4 字段 AAD）
%% ===================================================================

%% @doc 加密：Aad 必须逐字带 OrgId/WorkspaceId/ConversationId/MessageId。
%%
%% **严格形态**：缺任一字段即 `{error, {invalid_aad, Field}}`，不会退化成
%% 「资源作用域」解释（那是 `seal_scoped/3` 的宽松入口，由调用方显式选择）。
-spec seal(map(), binary(), term()) -> {ok, map()} | {error, term()}.
seal(Aad, Plaintext, KeyRef) when is_binary(Plaintext) ->
    case {resolve_key(KeyRef), aad_bytes(Aad)} of
        {{error, _} = Err, _} ->
            Err;
        {_Ok, {error, _} = Err} ->
            Err;
        {{ok, Key, Version}, {ok, ScopeBytes}} ->
            seal_with(Key, Version, ScopeBytes, Plaintext)
    end;
seal(_Aad, _Plaintext, _KeyRef) ->
    {error, invalid_plaintext}.

%% @doc 解密：AAD / key_version / 密钥材料任一不符即失败，绝不降级。
-spec open(map(), map(), term()) -> {ok, binary()} | {error, term()}.
open(Aad, Sealed, KeyRef) ->
    case {is_map(Sealed), resolve_key(KeyRef), aad_bytes(Aad)} of
        {false, _KeyRefState, _AadState} ->
            {error, invalid_sealed};
        {true, {error, _} = Err, _AadState} ->
            Err;
        {true, _Ok, {error, _} = Err} ->
            Err;
        {true, {ok, Key, RefVersion}, {ok, ScopeBytes}} ->
            open_with(Sealed, ScopeBytes, Key, RefVersion)
    end.

%% @doc CSB-02S D5：从**消息行三列**（cipher / key_version / aad_hash）重建
%% sealed 结构并解密——读面（list_messages）在 keyring 可用时服务端解出明文体。
%% AAD 由 (OrgId, WorkspaceId, ConversationId, MessageId) 四字段重建，与
%% `eb_pg_canonical_tx:seal_body/6` 的封口逐字同口径。
-spec open_message_body(
    integer(),
    integer(),
    integer(),
    integer(),
    binary(),
    integer(),
    binary(),
    term()
) ->
    {ok, binary()} | {error, term()}.
open_message_body(
    OrgId, WorkspaceId, ConversationId, MessageId, Cipher, KeyVersion, AadHash, KeyRef
) when
    is_binary(Cipher), is_integer(KeyVersion), is_binary(AadHash)
->
    Aad = #{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        conversation_id => ConversationId,
        message_id => MessageId
    },
    Sealed = #{
        alg => ?ALG,
        kdf => ?KDF_LABEL,
        key_version => KeyVersion,
        aad_hash => AadHash,
        cipher => Cipher
    },
    open(Aad, Sealed, KeyRef);
open_message_body(
    _OrgId, _WorkspaceId, _ConversationId, _MessageId, _Cipher, _KeyVersion, _AadHash, _KeyRef
) ->
    {error, invalid_sealed}.

%% ===================================================================
%% 通用资源作用域（客户资料 / 附件等非消息资源）
%% ===================================================================

%% @doc 资源作用域加密：Aad 可以是消息 4 字段形态，也可以是
%% `#{organization_id, workspace_id, resource_type, resource_id}`（AAD 绑定
%% OrgId/WorkspaceId/资源）。
-spec seal_scoped(map(), binary(), term()) -> {ok, map()} | {error, term()}.
seal_scoped(Scope, Plaintext, KeyRef) when is_binary(Plaintext) ->
    case {resolve_key(KeyRef), scope_bytes(Scope)} of
        {{ok, Key, Version}, {ok, Bytes}} ->
            seal_with(Key, Version, Bytes, Plaintext);
        {{error, _} = Err, _} ->
            Err;
        {_Ok, {error, _} = Err} ->
            Err
    end;
seal_scoped(_Scope, _Plaintext, _KeyRef) ->
    {error, invalid_plaintext}.

%% @doc 资源作用域解密。
-spec open_scoped(map(), map(), term()) -> {ok, binary()} | {error, term()}.
open_scoped(Scope, Sealed, KeyRef) ->
    case is_map(Sealed) of
        false ->
            {error, invalid_sealed};
        true ->
            case resolve_key(KeyRef) of
                {ok, Key, RefVersion} ->
                    case scope_bytes(Scope) of
                        {ok, Bytes} -> open_with(Sealed, Bytes, Key, RefVersion);
                        {error, _} = Err -> Err
                    end;
                {error, _} = Err ->
                    Err
            end
    end.

%% ===================================================================
%% 组织域 HMAC（客户渠道标识 subject 摘要）
%% ===================================================================

%% @doc 组织域 HMAC-SHA256（64 位小写 hex，满足 EB-01 的 ck_eci_subject_hmac）。
%%
%% 同一 Org + channel + subject 稳定可复现（用于幂等去重），跨 Org / 跨 channel
%% 不同（不可跨租户字典反查）；返回值本身不含明文 subject。
-spec subject_hmac(integer(), binary(), binary(), term()) -> {ok, binary()} | {error, term()}.
subject_hmac(OrgId, Domain, Subject, KeyRef) when
    is_integer(OrgId), is_binary(Domain), is_binary(Subject)
->
    case resolve_key(KeyRef) of
        {ok, Key, Version} ->
            SaltMaterial =
                <<?HMAC_LABEL/binary, "|", (integer_to_binary(OrgId))/binary, "|", Domain/binary>>,
            case derive_subkey(Key, Version, SaltMaterial) of
                {ok, SubKey} ->
                    {ok, binary:encode_hex(crypto:mac(hmac, sha256, SubKey, Subject), lowercase)};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end;
subject_hmac(_OrgId, _Domain, _Subject, _KeyRef) ->
    {error, invalid_subject_input}.

%% ===================================================================
%% 作用域字节 / 摘要
%% ===================================================================

%% @doc 消息作用域（eb_crypto_port:aad/0 的 4 字段）编码为可比较字节。
-spec aad_bytes(term()) -> {ok, binary()} | {error, term()}.
aad_bytes(Aad) when is_map(Aad) ->
    Fields = [organization_id, workspace_id, conversation_id, message_id],
    case first_invalid_field(Fields, Aad) of
        undefined ->
            {ok,
                <<?AAD_LABEL/binary, "|", (int_bin(maps:get(organization_id, Aad)))/binary, "|",
                    (int_bin(maps:get(workspace_id, Aad)))/binary, "|",
                    (int_bin(maps:get(conversation_id, Aad)))/binary, "|",
                    (int_bin(maps:get(message_id, Aad)))/binary>>};
        Field ->
            {error, {invalid_aad, Field}}
    end;
aad_bytes(_NotAMap) ->
    {error, {invalid_aad, aad}}.

%% @doc 通用资源作用域编码；消息 4 字段形态亦可。
-spec scope_bytes(term()) -> {ok, binary()} | {error, term()}.
scope_bytes(Scope) when is_map(Scope) ->
    case aad_bytes(Scope) of
        {ok, Bytes} ->
            {ok, Bytes};
        {error, _} ->
            resource_scope_bytes(Scope)
    end;
scope_bytes(_NotAMap) ->
    {error, {invalid_scope, scope}}.

resource_scope_bytes(Scope) ->
    OrgId = maps:get(organization_id, Scope, undefined),
    WorkspaceId = maps:get(workspace_id, Scope, undefined),
    ResourceType = maps:get(resource_type, Scope, undefined),
    ResourceId = maps:get(resource_id, Scope, undefined),
    case
        {
            is_integer(OrgId),
            is_integer(WorkspaceId),
            is_binary(ResourceType),
            is_integer(ResourceId)
        }
    of
        {true, true, true, true} ->
            {ok,
                <<?SCOPE_LABEL/binary, "|", (int_bin(OrgId))/binary, "|",
                    (int_bin(WorkspaceId))/binary, "|", ResourceType/binary, "|",
                    (int_bin(ResourceId))/binary>>};
        _ ->
            {error, {invalid_scope, resource}}
    end.

%% @doc 作用域摘要（64 位小写 hex）。可用于 DB 中的 `aad_hash` 列。
-spec aad_hash(term()) -> {ok, binary()} | {error, term()}.
aad_hash(Scope) ->
    case scope_bytes(Scope) of
        {ok, Bytes} -> {ok, hash_hex(Bytes)};
        {error, _} = Err -> Err
    end.

%% @doc 算法标识（封装里逐字记录，解密时比对）。
-spec algorithm() -> binary().
algorithm() ->
    ?ALG.

%% ===================================================================
%% 密钥引用
%% ===================================================================

%% @doc 归一化/校验一个密钥引用；成功时返回定型的 ref。
%%
%% 形态：
%%   #{key := binary(32), key_version := pos_integer()}         单密钥
%%   #{keys := #{pos_integer() => binary(32)}, key_version := V} 密钥环（按版本选择）
-spec key_ref(term()) -> {ok, map()} | {error, term()}.
key_ref(Ref) ->
    case resolve_key(Ref) of
        {ok, Key, Version} -> {ok, #{key => Key, key_version => Version}};
        {error, _} = Err -> Err
    end.

resolve_key(Ref) when is_map(Ref) ->
    case maps:get(keys, Ref, undefined) of
        Keyring when is_map(Keyring) ->
            resolve_keyring(Keyring, maps:get(key_version, Ref, undefined));
        _Single ->
            resolve_single(Ref)
    end;
resolve_key(_NotAMap) ->
    {error, missing_key}.

resolve_single(Ref) ->
    case maps:get(key, Ref, undefined) of
        Key when is_binary(Key) ->
            case byte_size(Key) =:= ?KEY_BYTES of
                true -> key_version(maps:get(key_version, Ref, 1), Key);
                false -> {error, invalid_key_length}
            end;
        _MissingOrInvalid ->
            {error, missing_key}
    end.

key_version(Version, Key) when is_integer(Version), Version >= 1 ->
    {ok, Key, Version};
key_version(Version, _Key) ->
    {error, {invalid_key_version, Version}}.

resolve_keyring(Keyring, undefined) ->
    _ = Keyring,
    {error, missing_key_version};
resolve_keyring(Keyring, Version) when is_integer(Version), Version >= 1 ->
    case maps:get(Version, Keyring, undefined) of
        Key when is_binary(Key), byte_size(Key) =:= ?KEY_BYTES -> {ok, Key, Version};
        Key when is_binary(Key) -> {error, invalid_key_length};
        _Missing -> {error, {unknown_key_version, Version}}
    end;
resolve_keyring(_Keyring, Version) ->
    {error, {invalid_key_version, Version}}.

%% ===================================================================
%% 封装 / 解封
%% ===================================================================

seal_with(Key, Version, ScopeBytes, Plaintext) ->
    case derive_subkey(Key, Version, ScopeBytes) of
        {ok, SubKey} ->
            case elib_cipher:aes_gcm_encrypt(Plaintext, SubKey) of
                {ok, Cipher} ->
                    {ok, #{
                        alg => ?ALG,
                        kdf => ?KDF_LABEL,
                        key_version => Version,
                        aad_hash => hash_hex(ScopeBytes),
                        cipher => Cipher
                    }};
                {error, Reason} ->
                    {error, {seal_failed, Reason}}
            end;
        {error, _} = Err ->
            Err
    end.

open_with(Sealed, ScopeBytes, Key, RefVersion) ->
    case sealed_fields(Sealed) of
        {ok, Alg, SealedVersion, Cipher, AadHash} ->
            case Alg =:= ?ALG of
                false ->
                    {error, {unsupported_algorithm, Alg}};
                true ->
                    open_checked(ScopeBytes, SealedVersion, Cipher, AadHash, Key, RefVersion)
            end;
        {error, _} = Err ->
            Err
    end.

open_checked(ScopeBytes, SealedVersion, Cipher, AadHash, Key, RefVersion) ->
    case SealedVersion =:= RefVersion of
        false ->
            {error, {key_version_mismatch, RefVersion, SealedVersion}};
        true ->
            case constant_time_eq(hash_hex(ScopeBytes), AadHash) of
                false -> {error, aad_mismatch};
                true -> decrypt(Cipher, Key, SealedVersion, ScopeBytes)
            end
    end.

decrypt(Cipher, Key, Version, ScopeBytes) ->
    case derive_subkey(Key, Version, ScopeBytes) of
        {ok, SubKey} ->
            case elib_cipher:aes_gcm_decrypt(Cipher, SubKey) of
                {ok, Plaintext} -> {ok, Plaintext};
                {error, Reason} -> {error, {open_failed, Reason}}
            end;
        {error, _} = Err ->
            Err
    end.

sealed_fields(Sealed) ->
    Alg = maps:get(alg, Sealed, undefined),
    Version = maps:get(key_version, Sealed, undefined),
    Cipher = maps:get(cipher, Sealed, undefined),
    AadHash = maps:get(aad_hash, Sealed, undefined),
    case {is_binary(Alg), is_integer(Version), is_binary(Cipher), is_binary(AadHash)} of
        {true, true, true, true} -> {ok, Alg, Version, Cipher, AadHash};
        _ -> {error, invalid_sealed}
    end.

%% ===================================================================
%% KDF / 工具
%% ===================================================================

derive_subkey(Key, Version, ScopeBytes) ->
    Salt = crypto:hash(sha256, ScopeBytes),
    Material = <<?SUBKEY_LABEL/binary, "|", (int_bin(Version))/binary, "|", Key/binary>>,
    try elib_cipher:derive_master_password(Material, Salt) of
        {ok, SubKey} -> {ok, SubKey};
        {error, Reason} -> {error, {kdf_failed, Reason}}
    catch
        _:_ -> {error, kdf_failed}
    end.

first_invalid_field([], _Aad) ->
    undefined;
first_invalid_field([Field | Rest], Aad) ->
    case maps:get(Field, Aad, undefined) of
        Value when is_integer(Value) -> first_invalid_field(Rest, Aad);
        _MissingOrInvalid -> Field
    end.

int_bin(Value) when is_integer(Value) ->
    integer_to_binary(Value);
int_bin(Value) when is_binary(Value) ->
    Value.

hash_hex(Bytes) ->
    binary:encode_hex(crypto:hash(sha256, Bytes), lowercase).

%% 摘要比较用等长常量时间比较（避免把 aad_hash 比对做成可测量侧信道）。
constant_time_eq(A, B) when byte_size(A) =:= byte_size(B) ->
    crypto:hash_equals(A, B);
constant_time_eq(_A, _B) ->
    false.
