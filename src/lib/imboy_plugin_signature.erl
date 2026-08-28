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
    verify_file/3,
    signature_required/0,
    trusted_public_keys/0,
    validate_config/0
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
%% 严格模式 / Strict mode（P1 插件市场签名校验 + SEC-02 商务版强制）:
%%   signature_required/0 为 true 时（plugin_signature_required 配置或
%%   商务版档位强制）：
%%   - 无可信公钥 → {error, no_trusted_keys}
%%   - 签名文件不存在 → {error, signature_missing}
-spec verify_file(file:filename_all(), file:filename_all()) ->
    ok | {error, term()}.
verify_file(FilePath, SigPath) ->
    TrustedKeys = trusted_public_keys(),
    Strict = signature_required(),
    HasKeys = TrustedKeys =/= [],
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

%% @doc 签名是否强制（SEC-02，审计 #44）。
%%
%% 档位口径与 deploy/preflight.sh 的 IMBOY_PRODUCT_PROFILE 一致
%% （imboy_env:override_product_profile/0 映射为 {imboy, product_profile}）：
%%   - community：按 {imboy, plugin_signature_required}（默认 false），
%%     维持现状（宽松 + lifecycle 默认关闭）。
%%   - enterprise（商务版）：**强制 true**，plugin_signature_required=false
%%     无法降级 —— 商务版 install 必须可信签名，缺 key / 缺 SIGNATURE /
%%     验签失败一律拒绝，不降级放行。
%%   - 其他任何值（拼错的档位）：fail-closed 按强制处理。拼错意味着策略
%%     放行面错误，必须收紧而非放松（与 imboy_env 对 IMBOY_PRODUCT_PROFILE
%%     非法值 fail-fast 拒启的语义一致）。
-spec signature_required() -> boolean().
signature_required() ->
    case config_ds:env(product_profile, community) of
        community ->
            application:get_env(imboy, plugin_signature_required, false) =:= true;
        _ ->
            true
    end.

%% @doc 可信公钥集合（内联 + 公钥文件路径，均过滤为合法 32 字节 Ed25519 公钥）。
%%
%%   - {imboy, plugin_trusted_public_keys}：内联公钥列表（binary）
%%   - {imboy, plugin_trusted_public_key_files}：公钥文件路径列表
%%     （raw 32 字节公钥文件；加载失败/长度非法的条目跳过并告警 ——
%%     结果集合变小即更严，fail-closed）
%%
%% 过滤非法长度同时消除 crypto:verify 对错误长度 key 抛 badarg 的
%% crash 面（原实现未过滤，install 路径上会击穿 gen_statem）。
-spec trusted_public_keys() -> [public_key()].
trusted_public_keys() ->
    Inline = filter_valid_keys(env_list(plugin_trusted_public_keys)),
    FromFiles = load_key_files(env_list(plugin_trusted_public_key_files)),
    Inline ++ FromFiles.

%% @doc 启动 / preflight 等价的配置完备性校验（EUnit 断言点）。
%%
%% 强制签名档位（商务版 / 显式 plugin_signature_required=true）下必须
%% 至少配置一个有效可信公钥，否则部署是不完备的（install 必然全部
%% {error, no_trusted_keys}）。启动侧挂钩见 imboy_app 的 strict 环境校验；
%% bash 侧等价口径见 deploy/preflight.sh 2c 段。
-spec validate_config() -> ok | {error, term()}.
validate_config() ->
    case signature_required() of
        true ->
            case trusted_public_keys() of
                [] -> {error, no_trusted_keys};
                _ -> ok
            end;
        false ->
            ok
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

%% @doc app env 读取为 list（非法类型当空列表，fail-closed）。
env_list(Key) ->
    case application:get_env(imboy, Key, []) of
        L when is_list(L) -> L;
        _ -> []
    end.

%% @doc 只保留合法长度（32 字节）的二进制公钥。
filter_valid_keys(Keys) ->
    [K || K <- Keys, is_binary(K), byte_size(K) =:= 32].

%% @doc 从公钥文件路径加载 32 字节 raw 公钥；坏文件跳过并告警。
load_key_files(Paths) ->
    lists:filtermap(
        fun(Path0) ->
            Path = unicode:characters_to_binary(Path0),
            case file:read_file(Path) of
                {ok, Bin} when is_binary(Bin), byte_size(Bin) =:= 32 ->
                    {true, Bin};
                {ok, Bin} ->
                    logger:warning(#{
                        event => plugin_trusted_key_file_invalid,
                        path => Path,
                        size => byte_size(Bin)
                    }),
                    false;
                {error, Reason} ->
                    logger:warning(#{
                        event => plugin_trusted_key_file_unreadable,
                        path => Path,
                        reason => Reason
                    }),
                    false
            end
        end,
        Paths
    ).
