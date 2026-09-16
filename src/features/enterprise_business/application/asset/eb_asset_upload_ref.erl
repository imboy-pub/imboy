%%% @doc EB-07：**不透明上传凭证**（presigned PUT 的本地等价物）。
%%%
%%% 依据：硬约束 3（企业下载端点不得复用返回 presigned GET 的个人
%%% `attach_logic:view_url/2` 合同；**不持久化 URL**）；`eb_asset_port` 的铁律级约束
%%% （调用方永远拿不到对象 key / bucket / endpoint / presigned 读取链接）。
%%%
%%% ## 形态
%%%
%%% ```
%%% Token = base64(#{v => 1, aad => Aad, sealed => Sealed})
%%% Aad   = #{organization_id, workspace_id, conversation_id, message_id}   %% 四字段整数
%%% ```
%%% `Sealed` 经 `eb_crypto_port:seal/3` 产出（企业托管 AEAD），明文里是
%%% `asset_id` / `actor_user_id` / `object_hash` / `mime` / `size_bytes` /
%%% `retain_until` / `issued_at` / `expires_at`。因此：
%%%
%%%   * **不透明**：token 不是 URL、不含对象 key；客户端只能原样回传；
%%%   * **作用域绑定**：AAD 绑 `Org/Workspace/会话/消息`，换租户重放必失败
%%%     （AAD 摘要比对 + GCM 认证双重）；
%%%   * **防篡改**：翻转任一字节都会在 `open/5` 处 fail-closed；
%%%   * **有时限**：`expires_at` 由调用方注入的时钟判定，**不做跨请求缓存**。
%%%
%%% ## 为什么 AAD 复用消息四字段形态
%%%
%%% `eb_crypto_port` 冻结契约里可用的入口只有 `seal/3`（四字段消息 AAD）与
%%% `seal_scoped/3`；而 `open_scoped/3` **未在契约中声明**（实现里有，但契约没有），
%%% 用例层不得调用未声明的 callback（`scripts/check_eb_port_closure.sh` 的 A01）。
%%% 因此本模块用 `seal/3` + `open/3` 这一对**成对声明**的能力，并把 AAD 取为附件的
%%% 归属作用域；未绑定消息时 `message_id` 记 `0`（该形态要求整数字段）。
-module(eb_asset_upload_ref).

-export([aad/4, mint/4, open/6, claims_scope/2]).

-define(VERSION, 1).

%% 必填 claims（缺一即拒签）。
-define(REQUIRED_FIELDS, [
    asset_id,
    actor_user_id,
    object_hash,
    mime,
    size_bytes,
    conversation_id,
    issued_at,
    expires_at
]).

%% 可选 claims：允许 `undefined`，但非 `undefined` 时必须是整数。
-define(OPTIONAL_INT_FIELDS, [retain_until, message_id]).

%% @doc 构造 AAD：`(OrgId, WorkspaceId, ConversationId, MessageId|undefined)`。
-spec aad(integer(), integer(), integer(), integer() | undefined) -> map().
aad(OrgId, WorkspaceId, ConversationId, undefined) ->
    #{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        conversation_id => ConversationId,
        message_id => 0
    };
aad(OrgId, WorkspaceId, ConversationId, MessageId) ->
    #{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        conversation_id => ConversationId,
        message_id => MessageId
    }.

%% @doc 签发凭证。`Claims` 必含 `?FIELDS` 的整数/二进制值。
-spec mint(map(), map(), module(), term()) -> {ok, binary()} | {error, term()}.
mint(Aad, Claims, Crypto, KeyRef) when is_map(Aad), is_map(Claims), is_atom(Crypto) ->
    case missing_claims(Claims) of
        [] ->
            Plaintext = term_to_binary(Claims#{v => ?VERSION}),
            case Crypto:seal(Aad, Plaintext, KeyRef) of
                {ok, Sealed} ->
                    Envelope = #{v => ?VERSION, aad => Aad, sealed => Sealed},
                    {ok, base64:encode(term_to_binary(Envelope))};
                {error, _} = Err ->
                    Err
            end;
        Missing ->
            {error, {invalid_upload_claims, Missing}}
    end;
mint(_Aad, _Claims, _Crypto, _KeyRef) ->
    {error, invalid_upload_ref_input}.

missing_claims(Claims) ->
    Missing = [F || F <- ?REQUIRED_FIELDS, maps:get(F, Claims, undefined) =:= undefined],
    Bad =
        [
            {F, V}
         || F <- ?OPTIONAL_INT_FIELDS,
            V <- [maps:get(F, Claims, undefined)],
            V =/= undefined,
            not is_integer(V)
        ],
    case Bad of
        [] -> Missing;
        _ -> Missing ++ [{invalid_optional, Bad}]
    end.

%% @doc 打开并校验凭证：
%%   1) 解包（`base64` + `binary_to_term/2` 的 `safe` 模式，不接受任意 term）；
%%   2) 用信封自带的 AAD + `Crypto:open/3` 解密（篡改 / 换作用域即失败）；
%%   3) 校验 AAD 与期望租户一致、claims 与 AAD 自洽、未过期。
%%
%% 返回 `{ok, #{aad, claims}}` 或 fail-closed 的 `{error, Reason}`。
-spec open(binary(), integer(), integer(), module(), term(), integer()) ->
    {ok, map()} | {error, term()}.
open(Token, ExpectedOrgId, ExpectedWorkspaceId, Crypto, KeyRef, NowSec) when is_binary(Token) ->
    case decode(Token) of
        {ok, Aad, Sealed} ->
            open_with_aad(Aad, Sealed, ExpectedOrgId, ExpectedWorkspaceId, Crypto, KeyRef, NowSec);
        {error, _} = Err ->
            Err
    end;
open(_Token, _OrgId, _WsId, _Crypto, _KeyRef, _NowSec) ->
    {error, invalid_upload_ref}.

decode(Token) ->
    try binary_to_term(base64:decode(Token), [safe]) of
        #{v := ?VERSION, aad := Aad, sealed := Sealed} when is_map(Aad), is_map(Sealed) ->
            {ok, Aad, Sealed};
        _Other ->
            {error, invalid_upload_ref}
    catch
        _:_ -> {error, invalid_upload_ref}
    end.

open_with_aad(Aad, Sealed, ExpectedOrgId, ExpectedWorkspaceId, Crypto, KeyRef, NowSec) ->
    case {maps:get(organization_id, Aad, undefined), maps:get(workspace_id, Aad, undefined)} of
        {ExpectedOrgId, ExpectedWorkspaceId} ->
            decrypt(Aad, Sealed, Crypto, KeyRef, NowSec);
        _Other ->
            %% 换租户重放：不进入解密路径（AAD 也不匹配，这里是显式的早退）
            {error, invalid_upload_ref}
    end.

decrypt(Aad, Sealed, Crypto, KeyRef, NowSec) ->
    case Crypto:open(Aad, Sealed, KeyRef) of
        {ok, Plaintext} ->
            check_claims(Plaintext, Aad, NowSec);
        {error, _} ->
            {error, invalid_upload_ref}
    end.

check_claims(Plaintext, Aad, NowSec) ->
    try binary_to_term(Plaintext, [safe]) of
        #{v := ?VERSION} = Claims ->
            verify_claims(Claims, Aad, NowSec);
        _Other ->
            {error, invalid_upload_ref}
    catch
        _:_ -> {error, invalid_upload_ref}
    end.

verify_claims(Claims, Aad, NowSec) ->
    ExpiresAt = maps:get(expires_at, Claims),
    case {claims_scope(Claims, Aad), ExpiresAt > NowSec} of
        {true, true} -> {ok, #{aad => Aad, claims => Claims}};
        {true, false} -> {error, expired_upload_ref};
        {false, _} -> {error, invalid_upload_ref}
    end.

%% @doc claims ↔ AAD 自洽性（message_id 的 `0` 与 `undefined` 是同一件事）。
-spec claims_scope(map(), map()) -> boolean().
claims_scope(Claims, Aad) ->
    Conv = maps:get(conversation_id, Claims, undefined),
    Msg = maps:get(message_id, Claims, undefined),
    ExpectedMsg =
        case maps:get(message_id, Aad, 0) of
            0 -> undefined;
            AadMsg -> AadMsg
        end,
    Conv =:= maps:get(conversation_id, Aad, undefined) andalso Msg =:= ExpectedMsg.
