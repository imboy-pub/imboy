-module(imboy_plugin_signature).

%% @status FROZEN (roadmap-only, 2026-06)：v2 动态加载子系统暂停投入。
%% 当前生产走配置驱动模块化单体路线（见 product-profile-and-plugin-registry-design.md §3.1）。
%% 修改前请确认是否真要重启动态平台方向。冻结≠移除。
%% FROZEN: v2 dynamic plugin loading subsystem is suspended (roadmap-only).
%% Current production route: config-driven monolith. See §3.1 before resuming.

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_signature - 插件 Ed25519 签名工具（P6-T4）
%%% Plugin Ed25519 signature toolkit
%%%
%%% 用途 / Use cases:
%%%   - install 时 loader 验证插件签名（防止恶意/篡改）
%%%   - script/plugin_sign.escript 离线签名工具
%%%
%%% 算法 / Algorithm:
%%%   - Ed25519（OTP crypto 模块内置，零依赖）
%%%   - 签名长度固定 64 字节
%%%   - 公钥 32 字节，私钥 32 字节
%%%
%%% 私钥管理 / Private key management:
%%%   - 私钥**绝不**入库或入版本控制
%%%   - 离线生成 + 离线签名（推荐 air-gapped 工作站）
%%%   - 公钥分发：core 配置中预置可信公钥列表
%%%
%%% Source of truth: docs/plugin/contract.md §10 + roadmap P6-T4/T5
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-export([
    generate_keypair/0,
    sign_data/2,
    verify_data/3,
    sign_file/2,
    verify_file/2,
    verify_file/3
]).

%% 32 bytes
-type public_key() :: binary().
%% 32 bytes
-type private_key() :: binary().
%% 64 bytes
-type signature() :: binary().

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc 生成 Ed25519 密钥对（仅供 dev / CLI 工具使用）。
%% Generate Ed25519 keypair (dev / CLI tools only).
%% **生产环境私钥应离线生成**。Production keys MUST be generated offline.
-spec generate_keypair() -> {ok, public_key(), private_key()}.
generate_keypair() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, Pub, Priv}.

%% @doc 用 Ed25519 私钥签名任意数据。
%% Sign arbitrary data with Ed25519 private key.
-spec sign_data(iodata(), private_key()) -> {ok, signature()}.
sign_data(Data, PrivateKey) when is_binary(PrivateKey) ->
    Sig = crypto:sign(eddsa, none, Data, [PrivateKey, ed25519]),
    {ok, Sig}.

%% @doc 用 Ed25519 公钥验证签名。
%% Verify signature with Ed25519 public key.
-spec verify_data(iodata(), public_key(), signature()) ->
    ok | {error, signature_invalid}.
verify_data(Data, PublicKey, Signature) when
    is_binary(PublicKey), is_binary(Signature)
->
    case crypto:verify(eddsa, none, Data, Signature, [PublicKey, ed25519]) of
        true -> ok;
        false -> {error, signature_invalid}
    end.

%% @doc 签名文件内容（按字节流，不规范化空白）。
%% Sign file content (byte-stream, no whitespace normalization).
-spec sign_file(file:filename_all(), private_key()) ->
    {ok, signature()} | {error, term()}.
sign_file(Path, PrivateKey) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            sign_data(Bin, PrivateKey);
        {error, _} = E ->
            E
    end.

%% @doc 验证文件签名（自动读取可信公钥列表）。
%% Verify file signature using trusted public keys from app config.
%% 策略与 imboy_plugin_loader:verify_plugin_signature/2 一致：
%%   - 无可信公钥 → ok（向后兼容）
%%   - 签名文件不存在 → ok（无签名 = 不强制）
%%   - 任一公钥验证通过 → ok
%%   - 全部失败 → {error, no_matching_key}
%%
%% 严格模式 / Strict mode（P1 插件市场签名校验）:
%%   `{imboy, plugin_signature_required}` 为 true 时（默认 false）：
%%   - 无可信公钥 → {error, no_trusted_keys}
%%   - 签名文件不存在 → {error, signature_missing}
-spec verify_file(file:filename_all(), file:filename_all()) ->
    ok | {error, term()}.
verify_file(FilePath, SigPath) ->
    TrustedKeys = application:get_env(imboy, plugin_trusted_public_keys, []),
    Strict = application:get_env(imboy, plugin_signature_required, false) =:= true,
    HasKeys = is_list(TrustedKeys) andalso TrustedKeys =/= [],
    case {HasKeys, file:read_file(SigPath)} of
        {false, _} when Strict ->
            {error, no_trusted_keys};
        {false, _} ->
            ok;
        {_, {error, enoent}} when Strict ->
            {error, signature_missing};
        {_, {error, enoent}} ->
            ok;
        {true, {ok, Signature}} ->
            verify_against_keys(FilePath, TrustedKeys, Signature);
        {true, {error, _} = E} ->
            E
    end.

%% @doc 验证文件签名。
%% Verify file signature.
-spec verify_file(file:filename_all(), public_key(), signature()) ->
    ok | {error, term()}.
verify_file(Path, PublicKey, Signature) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            verify_data(Bin, PublicKey, Signature);
        {error, _} = E ->
            E
    end.

%% ===================================================================
%% Internal helpers
%% ===================================================================

%% @doc 逐一尝试可信公钥验证签名。
verify_against_keys(_FilePath, [], _Signature) ->
    {error, no_matching_key};
verify_against_keys(FilePath, [PubKey | Rest], Signature) ->
    case verify_file(FilePath, PubKey, Signature) of
        ok -> ok;
        {error, _} -> verify_against_keys(FilePath, Rest, Signature)
    end.
