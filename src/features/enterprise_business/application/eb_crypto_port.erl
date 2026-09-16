%%% @doc 扩展点：企业托管加密（EB-02 冻结契约；实现随 EB-03 落地）。
%%%
%%% 这是**真扩展点**：只有 `-callback` 声明，零实现、零 meck。
%%%
%%% 企业托管加密（EB-D05）与个人 E2EE 是**两件事**：这里由服务端用
%%% `elib_kdf` 域隔离派生的密钥 + AES-GCM 适配器加解密，AAD 至少绑定
%%% `organization_id` / `workspace_id` / `conversation_id` / `message_id`，
%%% 从而不可把某个会话的密文搬到另一个会话重放。
%%%
%%% fail-closed：缺主密钥、`key_version` 未知、AAD 不符，一律返回 error，
%%% 不得回退到明文或默认密钥。
-module(eb_crypto_port).

-export_type([aad/0, key_ref/0, sealed/0]).

%% AAD 必须逐字携带这四个作用域字段（OrgId / WorkspaceId / ConversationId / MessageId）。
-type aad() :: #{
    organization_id := integer(),
    workspace_id := integer(),
    conversation_id := integer(),
    message_id := integer()
}.

%% 密钥引用由装配层解析（表行 / 配置），扩展点本身不读全局单例。
-type key_ref() :: term().

%% 密文封装：实现必须带 key_version 与算法标识，不得返回明文。
-type sealed() :: map().

%% @doc 加密：把明文与 AAD 绑定后产出带版本的密文封装。
-callback seal(Aad :: aad(), Plaintext :: binary(), KeyRef :: key_ref()) ->
    {ok, sealed()} | {error, term()}.

%% @doc 解密：AAD 或 key_version 不符必须失败，不得降级。
-callback open(Aad :: aad(), Sealed :: sealed(), KeyRef :: key_ref()) ->
    {ok, binary()} | {error, term()}.

%% ===================================================================
%% EB-03R 契约面补齐（**只追加**：`seal/3`、`open/3` 与上面的 aad() 一字未动）
%% ===================================================================
%%
%% R0-3 的实测结论：application 层（`eb_contact_app`）真实依赖的是下面两个
%% callback，而冻结契约声明的却是 `seal/3`、`open/3`——**不是「少声明一个」，
%% 是「声明了错的签名」**（参数形状不同：资源作用域 vs 四字段消息 AAD）。
%% 两者语义并存：消息路径用四字段 `aad()` 的 `seal/3` / `open/3`，
%% 非消息资源（客户资料等）用 `scope()` 的 `seal_scoped/3`。

%% 资源作用域：至少绑定 `organization_id` 与 `workspace_id`（铁律 6），
%% 并按资源类型追加 `resource_type` + `resource_id`，使密文不可跨资源 / 跨租户重放。
-type scope() :: #{
    organization_id := integer(),
    workspace_id := integer(),
    resource_type := binary(),
    resource_id := integer()
}.

-export_type([scope/0]).

%% @doc 带资源作用域的加密：把 `scope()` 作为 AAD 绑定后产出带版本的密文封装。
%% fail-closed 与 `seal/3` 同规：缺主密钥 / key_version 未知 / AAD 不符一律 error。
-callback seal_scoped(Scope :: scope(), Plaintext :: binary(), KeyRef :: key_ref()) ->
    {ok, sealed()} | {error, term()}.

%% @doc 组织域 HMAC-SHA256（64 位小写 hex）：用于客户渠道标识
%% （`enterprise_contact_identity.subject_hmac`）——**不可逆、不含明文 subject**。
%% `Domain` 是渠道 / 用途域分隔符（如 `<<"wechat">>`），`Subject` 是待映射的原始标识。
-callback subject_hmac(
    OrgId :: integer(), Domain :: binary(), Subject :: binary(), KeyRef :: key_ref()
) ->
    {ok, binary()} | {error, term()}.
