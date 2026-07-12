-module(channel_webhook_ds).

%%%
% 频道 incoming webhook 数据服务 / Channel incoming webhook data service
%
% 职责：webhook 编排（生成 token + 建 system_bot 用户 + bot 加频道编辑 + 落表）、
%       token 查询、停用、列表。屏蔽 repo 存储细节。
% 范式：建 bot 账号镜像 ai_agent_ds:create_agent_user（建 user 行 + 标 account_type=2）。
%%%

-export([create/3]).
-export([find_by_token/1]).
-export([disable/2]).
-export([list_by_channel/1]).

-include("log.hrl").

%% account_type 枚举（对齐迁移 00000027 注释：0=human 1=ai_agent 2=system_bot）
-define(ACCOUNT_TYPE_SYSTEM_BOT, 2).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 创建 webhook：生成 token → 建 system_bot 用户 → bot 加频道编辑(role=1)
%% → 落 channel_webhook 表。token 仅创建时明文返回一次。
%% ponytail: 四步非单事务（镜像 ai_agent_ds:create 的取舍）；中途失败可能留下
%%   孤儿 bot user / channel_admin 行，管理端重建即可。真需强一致再下沉 with_tx。
-spec create(integer(), binary(), integer()) -> {ok, map()} | {error, binary()}.
create(ChannelId, Name, CreatorUid) ->
    Token = gen_token(),
    BotUid = elib_tsid:generate(user),
    case create_bot_user(BotUid, Name) of
        ok ->
            AdminData = #{
                channel_id => ChannelId,
                user_id => BotUid,
                role => 1,
                created_at => elib_dt:now()
            },
            case channel_admin_repo:add(AdminData) of
                {ok, _} ->
                    case insert_webhook(ChannelId, Name, Token, BotUid, CreatorUid) of
                        {ok, _} = Ok ->
                            Ok;
                        {error, _} = Err ->
                            %% 尽力回滚频道编辑授权，避免留下无主且现有管理端
                            %% 不可发现的 channel_admin 权限残留（security-review M2）
                            _ = channel_admin_repo:delete(ChannelId, BotUid),
                            Err
                    end;
                {error, Reason} ->
                    ?ERROR_LOG("channel_webhook_ds:create admin error ~p~n", [Reason]),
                    {error, <<"绑定频道编辑失败"/utf8>>}
            end;
        {error, Reason} ->
            ?ERROR_LOG("channel_webhook_ds:create bot user error ~p~n", [Reason]),
            {error, <<"创建 Bot 账号失败"/utf8>>}
    end.

%% @doc 按 token 查找 webhook（含停用行，状态判断在 Logic 层）
-spec find_by_token(binary()) -> {ok, map()} | {error, not_found}.
find_by_token(Token) ->
    case channel_webhook_repo:find_by_token(Token) of
        Row when is_map(Row), map_size(Row) > 0 -> {ok, Row};
        _ -> {error, not_found}
    end.

%% @doc 停用 webhook（按 channel_id 双条件，防跨频道操作）
-spec disable(integer(), integer()) -> ok | {error, binary()}.
disable(ChannelId, WebhookId) ->
    case channel_webhook_repo:set_status(ChannelId, WebhookId, 2) of
        {ok, N} when N > 0 -> ok;
        {ok, 0} -> {error, <<"webhook 不存在"/utf8>>};
        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 频道的 webhook 列表（含完整 token，掩码在 Logic 层）
-spec list_by_channel(integer()) -> {ok, list(map())} | {error, term()}.
list_by_channel(ChannelId) ->
    channel_webhook_repo:list_by_channel(ChannelId).

%% ===================================================================
%% Internal
%% ===================================================================

%% 建 bot 用户行并标记 account_type=2（镜像 ai_agent_ds:create_agent_user）
-spec create_bot_user(integer(), binary()) -> ok | {error, term()}.
create_bot_user(BotUid, Nickname) ->
    Account = <<"chbot_", (ec_cnv:to_binary(BotUid))/binary>>,
    case user_repo:create(#{id => BotUid, nickname => Nickname, account => Account}) of
        ok ->
            case user_repo:update(BotUid, #{account_type => ?ACCOUNT_TYPE_SYSTEM_BOT}) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec insert_webhook(integer(), binary(), binary(), integer(), integer()) ->
    {ok, map()} | {error, binary()}.
insert_webhook(ChannelId, Name, Token, BotUid, CreatorUid) ->
    Data = #{
        channel_id => ChannelId,
        name => Name,
        token => Token,
        bot_uid => BotUid,
        creator_uid => CreatorUid,
        status => 1
    },
    case channel_webhook_repo:add(Data) of
        {ok, Id} ->
            {ok, #{
                <<"id">> => Id,
                <<"channel_id">> => ChannelId,
                <<"name">> => Name,
                <<"token">> => Token,
                <<"bot_uid">> => BotUid
            }};
        {error, Reason} ->
            ?ERROR_LOG("channel_webhook_ds:create insert error ~p~n", [Reason]),
            {error, <<"创建 webhook 失败"/utf8>>}
    end.

%% 不可猜测随机 token：24 字节强随机 → 48 位小写 hex（fits varchar(64)）
-spec gen_token() -> binary().
gen_token() ->
    string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(24))).
