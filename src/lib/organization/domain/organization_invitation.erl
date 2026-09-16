-module(organization_invitation).

%% Organization Invitation 领域规则（纯函数，无 IO/无 SQL）。
%%
%% Core Contract C11：
%%   * Invite、Membership、Restore、Accept、Reject、Expire、Revoke 是不同 command/state；
%%   * pending 不进 Membership；SOURCE OF TRUTH = `organization_invitation`；
%%   * V1 target_user 必填；token/code 只存 digest；
%%     同 target/Org 最多一个未终结邀请；accept 幂等（重复 accept 返回同一终态）。
%%
%% 本模块只承载判定规则、token/digest 生成与稳定错误文案；
%% 锁序与事务编排见 organization_invitation_app（application），
%% SQL 见 organization_invitation_pg（infrastructure）。
%%
%% digest 模式与仓内既有实现一致（cs_access_app:default_digest/1）：
%% `binary:encode_hex(crypto:hash(sha256, Secret))`，小写 hex 64 字符。

-export([
    new_token/0,
    token_digest/1,
    valid_token_digest/1,
    is_terminal/1,
    can_transition/2,
    is_expired/2,
    classify_accept/2,
    valid_create/3,
    default_expiry_seconds/0,
    expires_at_from_now/1
]).

%% 256-bit 熵 → 64 hex 字符明文
-define(TOKEN_BYTES, 32).
-define(DEFAULT_EXPIRY_SECONDS, 7 * 86400).

%% ===================================================================
%% token / digest（C11：明文只返回一次，库中只有 digest）
%% ===================================================================

%% @doc 生成一次性明文 token（64 hex 字符）。调用方只在 create 响应中返回一次。
-spec new_token() -> binary().
new_token() ->
    binary:encode_hex(crypto:strong_rand_bytes(?TOKEN_BYTES), lowercase).

%% @doc 明文 token → sha256 小写 hex digest（64 字符，落库形态）。
%%
%% 与 cs_access_app:default_digest/1 同一 sha256 hex 模式；显式 lowercase——
%% OTP 28 起 binary:encode_hex/1 默认大写，而迁移 CHECK
%% ck_organization_invitation_token_digest 冻结小写 `^[0-9a-f]{64}$`。
-spec token_digest(binary()) -> binary().
token_digest(Token) when is_binary(Token) ->
    binary:encode_hex(crypto:hash(sha256, Token), lowercase).

%% @doc 校验 digest 形态（与迁移 CHECK ck_organization_invitation_token_digest 同口径）。
-spec valid_token_digest(term()) -> boolean().
valid_token_digest(D) when is_binary(D), byte_size(D) =:= 64 ->
    lists:all(fun is_lower_hex/1, binary_to_list(D));
valid_token_digest(_) ->
    false.

is_lower_hex(C) when C >= $0, C =< $9 -> true;
is_lower_hex(C) when C >= $a, C =< $f -> true;
is_lower_hex(_) -> false.

%% ===================================================================
%% 状态机（C11：pending → 终态，一次性）
%% ===================================================================

-spec is_terminal(atom() | binary()) -> boolean().
is_terminal(accepted) -> true;
is_terminal(rejected) -> true;
is_terminal(expired) -> true;
is_terminal(revoked) -> true;
is_terminal(<<"accepted">>) -> true;
is_terminal(<<"rejected">>) -> true;
is_terminal(<<"expired">>) -> true;
is_terminal(<<"revoked">>) -> true;
is_terminal(_) -> false.

%% @doc 仅 pending 可进入终态；终态之间/终态回 pending 一律非法。
-spec can_transition(atom() | binary(), atom() | binary()) -> boolean().
can_transition(pending, To) -> is_terminal(To);
can_transition(<<"pending">>, To) -> is_terminal(To);
can_transition(_, _) -> false.

%% ===================================================================
%% expiry（lazy expire：读写路径按需把 pending 置为 expired）
%% ===================================================================

-spec default_expiry_seconds() -> pos_integer().
default_expiry_seconds() ->
    ?DEFAULT_EXPIRY_SECONDS.

-spec expires_at_from_now(non_neg_integer()) -> integer().
expires_at_from_now(NowSeconds) when is_integer(NowSeconds), NowSeconds >= 0 ->
    NowSeconds + ?DEFAULT_EXPIRY_SECONDS.

%% @doc pending 行是否已过有效期（ExpiresAt/Now 为 epoch 秒）。
-spec is_expired(integer() | undefined, integer()) -> boolean().
is_expired(undefined, _NowSeconds) ->
    false;
is_expired(ExpiresAt, NowSeconds) when is_integer(ExpiresAt), is_integer(NowSeconds) ->
    ExpiresAt =< NowSeconds.
%% @doc accept/reject/revoke 的 pending 行状态裁决。
%%
%% classify_accept(Now, #{<<"status">> := S, <<"expires_at">> := E}) →
%%   ok | expired | replay_accepted | rejected | revoked
%% （replay_accepted：重复 accept 幂等返回同一终态的分支标记。）
-spec classify_accept(integer(), map()) -> ok | expired | replay_accepted | rejected | revoked.
classify_accept(NowSeconds, #{<<"status">> := <<"pending">>, <<"expires_at">> := ExpiresAt}) ->
    case is_expired(ExpiresAt, NowSeconds) of
        true -> expired;
        false -> ok
    end;
classify_accept(_NowSeconds, #{<<"status">> := <<"accepted">>}) ->
    replay_accepted;
classify_accept(_NowSeconds, #{<<"status">> := <<"rejected">>}) ->
    rejected;
classify_accept(_NowSeconds, #{<<"status">> := <<"revoked">>}) ->
    revoked;
classify_accept(_NowSeconds, #{<<"status">> := <<"expired">>}) ->
    expired;
classify_accept(_, _) ->
    expired.

%% ===================================================================
%% create 入参判定
%% ===================================================================

%% @doc create command 的纯参量校验（组织/成员存在性等由应用层在锁内裁决）。
%% 返回 ok | {error, {400, Msg}}。
-spec valid_create(integer(), integer(), integer()) -> ok | {error, {400, binary()}}.
valid_create(OrgId, InviterUid, TargetUid) when
    is_integer(OrgId),
    OrgId > 0,
    is_integer(InviterUid),
    InviterUid > 0,
    is_integer(TargetUid),
    TargetUid > 0
->
    ok;
valid_create(_, _, _) ->
    {error, {400, <<"organization_id、invited_by、target_user_id 必须是正整数"/utf8>>}}.
