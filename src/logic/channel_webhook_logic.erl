-module(channel_webhook_logic).

%%%
% 频道 incoming webhook 业务逻辑 / Channel incoming webhook business logic
%
% 管理端（须频道管理员 role>=2）：create / disable / list（list 只回 token 前 8 位掩码）。
% 入站端（免 JWT，token 即凭证）：incoming/2 —— 限流 → 查 token → 以 bot_uid 发消息。
%
% channel locking：webhook 只能发到建它时绑定的 channel。token 不携带任何路由
% 信息，目标频道只来自 channel_webhook 表行（表结构天然保证）。
%
% 防钓鱼 Bot 标签：消息作者是 account_type=2 的 system_bot 用户（服务端事实标记），
% 且 publish payload 附 is_bot=true（channel_ds:publish_message 原样 JSON 编码透传）。
%%%

-export([create/3]).
-export([disable/3]).
-export([list/2]).
-export([incoming/2]).

%% webhook 管理要求的最低角色（2=管理员，3=创建者）
-define(MIN_MANAGE_ROLE, 2).
%% list 时 token 明文保留前缀长度
-define(TOKEN_PREFIX_LEN, 8).

%% ===================================================================
%% 管理端 API（JWT 认证，频道管理员）
%% ===================================================================

%% @doc 创建 webhook（返回完整 token，仅此一次）
-spec create(integer(), binary(), binary()) -> {ok, map()} | {error, binary()}.
create(_Uid, _ChannelIdBin, <<>>) ->
    {error, <<"name 不能为空"/utf8>>};
create(Uid, ChannelIdBin, Name) ->
    with_manage_role(Uid, ChannelIdBin, fun(ChannelId) ->
        case channel_webhook_ds:create(ChannelId, Name, Uid) of
            {ok, #{<<"token">> := Token} = Webhook} ->
                {ok, Webhook#{
                    <<"url">> => <<"/api/v1/webhook/channel/", Token/binary>>
                }};
            {error, _} = Err ->
                Err
        end
    end).

%% @doc 停用 webhook（停用后 incoming 统一 404）
-spec disable(integer(), binary(), binary()) -> ok | {error, binary()}.
disable(Uid, ChannelIdBin, WebhookIdBin) ->
    with_manage_role(Uid, ChannelIdBin, fun(ChannelId) ->
        case decode_positive_id(WebhookIdBin) of
            0 -> {error, <<"webhook 不存在"/utf8>>};
            WebhookId -> channel_webhook_ds:disable(ChannelId, WebhookId)
        end
    end).

%% @doc 频道 webhook 列表（token 掩码：前 8 位 + ***，不泄露完整凭证）
-spec list(integer(), binary()) -> {ok, list(map())} | {error, binary()}.
list(Uid, ChannelIdBin) ->
    with_manage_role(Uid, ChannelIdBin, fun(ChannelId) ->
        case channel_webhook_ds:list_by_channel(ChannelId) of
            {ok, Rows} when is_list(Rows) ->
                {ok, [mask_token(Row) || Row <- Rows, is_map(Row)]};
            {error, Reason} ->
                {error, elib_cnv:safe_to_binary(Reason)}
        end
    end).

%% ===================================================================
%% 入站端 API（免 JWT，token 即凭证）
%% ===================================================================

%% @doc incoming 消息：限流（按 token）→ 查 token（status=1）→ 以 bot_uid 发文本。
%% 无效/停用 token 统一 not_found（不泄露存在性差异）；限流 rate_limited。
-spec incoming(binary(), binary()) ->
    ok | {error, rate_limited | not_found | binary()}.
incoming(_Token, <<>>) ->
    {error, <<"text 不能为空"/utf8>>};
incoming(Token, Text) ->
    %% 限流前置于查库之前，挡住无效 token 打库；FromUid 无认证主体固定 0
    case agent_rate_limiter:allow(Token, 0) of
        {deny, _} ->
            {error, rate_limited};
        allow ->
            case channel_webhook_ds:find_by_token(Token) of
                {ok, #{
                    <<"status">> := 1,
                    <<"channel_id">> := ChannelId,
                    <<"bot_uid">> := BotUid
                }} ->
                    publish(BotUid, ChannelId, Text);
                _ ->
                    {error, not_found}
            end
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% 以 bot_uid 走标准频道发布链（channel locking：ChannelId 只来自 webhook 表行；
%% 频道存在性与 bot 的编辑权限由 publish_message 内的角色检查兜底）
-spec publish(integer(), integer(), binary()) -> ok | {error, binary()}.
publish(BotUid, ChannelId, Text) ->
    Payload = #{<<"is_bot">> => true},
    case
        channel_logic_message:publish_message(
            BotUid, ec_cnv:to_binary(ChannelId), Text, <<"text">>, Payload
        )
    of
        {ok, _Message} -> ok;
        {error, _} = Err -> Err
    end.

%% 统一管理端守卫：解析频道 + 校验操作者 role>=2
-spec with_manage_role(integer(), binary(), fun((integer()) -> Ret)) ->
    Ret | {error, binary()}
when
    Ret :: ok | {ok, term()} | {error, binary()}.
with_manage_role(Uid, ChannelIdBin, Fun) ->
    case channel_logic_common:resolve_channel_id(ChannelIdBin) of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        ChannelId ->
            Role = channel_logic_common:get_user_role(ChannelId, Uid),
            case Role >= ?MIN_MANAGE_ROLE of
                true -> Fun(ChannelId);
                false -> {error, <<"只有频道管理员可以管理 webhook"/utf8>>}
            end
    end.

-spec mask_token(map()) -> map().
mask_token(#{<<"token">> := Token} = Row) when is_binary(Token) ->
    Prefix =
        case Token of
            <<P:?TOKEN_PREFIX_LEN/binary, _/binary>> -> P;
            _ -> Token
        end,
    Row#{<<"token">> => <<Prefix/binary, "***">>};
mask_token(Row) ->
    Row.

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) when is_integer(Value), Value > 0 ->
    Value;
decode_positive_id(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Int when Int > 0 -> Int;
        _ -> 0
    catch
        _:_ -> 0
    end;
decode_positive_id(_) ->
    0.
