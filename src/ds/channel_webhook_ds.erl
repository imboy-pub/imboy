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

%% @doc 创建 webhook：归档前置检查（避免建孤儿 bot user）→ 生成 token →
%% 建 system_bot 用户 → 单事务【归档写守卫（FOR UPDATE 行锁）+ bot 加频道
%% 编辑(role=1) + 落 channel_webhook 表】。token 仅创建时明文返回一次。
%% ponytail: bot user 行建在事务外（user 域非 workspace 资源，回滚无意义）；
%%   事务失败时 bot user 可能残留（镜像 ai_agent_ds:create 的取舍），管理端
%%   重建即可；归档拒绝由前置检查提前短路（事务内守卫为权威兜底）。
-spec create(integer(), binary(), integer()) ->
    {ok, map()} | {error, binary() | {integer(), binary()}}.
create(ChannelId, Name, CreatorUid) ->
    %% T7 归档写守卫（A2 收口）：前置短路，避免归档频道上白建 bot user；
    %% 权威拒绝在下方事务内（ensure_writable_tx 行锁与落表原子）。
    case workspace_guard:ensure_writable({channel, ChannelId}) of
        {error, Reason} ->
            {error, Reason};
        ok ->
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
                    case
                        workspace_guard:write_tx({channel, ChannelId}, fun(Conn) ->
                            case channel_admin_repo:add(Conn, AdminData) of
                                {ok, _} ->
                                    insert_webhook_tx(
                                        Conn, ChannelId, Name, Token, BotUid, CreatorUid
                                    );
                                {error, Reason} ->
                                    {error, Reason}
                            end
                        end)
                    of
                        {ok, _} = Ok ->
                            Ok;
                        %% 稳定错误码（980 竞态兜底等）透传 + 尽力回滚编辑授权
                        {error, {Code, Msg}} when is_integer(Code) ->
                            _ = channel_admin_repo:delete(ChannelId, BotUid),
                            {error, {Code, Msg}};
                        {error, Reason} ->
                            %% 尽力回滚频道编辑授权，避免留下无主且现有管理端
                            %% 不可发现的 channel_admin 权限残留（security-review M2）
                            _ = channel_admin_repo:delete(ChannelId, BotUid),
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end;
                {error, Reason} ->
                    ?ERROR_LOG("channel_webhook_ds:create bot user error ~p~n", [Reason]),
                    {error, <<"创建 Bot 账号失败"/utf8>>}
            end
    end.

%% @doc 按 token 查找 webhook（含停用行，状态判断在 Logic 层）
-spec find_by_token(binary()) -> {ok, map()} | {error, not_found}.
find_by_token(Token) ->
    case channel_webhook_repo:find_by_token(Token) of
        Row when is_map(Row), map_size(Row) > 0 -> {ok, Row};
        _ -> {error, not_found}
    end.

%% @doc 停用 webhook（停用后 incoming 统一 404）
-spec disable(integer(), integer()) -> ok | {error, binary() | {integer(), binary()}}.
disable(ChannelId, WebhookId) ->
    %% T7 归档写守卫（A2 收口）：停用与守卫同事务（FOR UPDATE 行锁）
    case
        workspace_guard:write_tx({channel, ChannelId}, fun(Conn) ->
            channel_webhook_repo:set_status_tx(Conn, ChannelId, WebhookId, 2)
        end)
    of
        {ok, N} when N > 0 -> ok;
        {ok, 0} -> {error, <<"webhook 不存在"/utf8>>};
        {error, {Code, Msg}} when is_integer(Code) -> {error, {Code, Msg}};
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

-spec insert_webhook_tx(any(), integer(), binary(), binary(), integer(), integer()) ->
    {ok, map()} | {error, binary()}.
insert_webhook_tx(Conn, ChannelId, Name, Token, BotUid, CreatorUid) ->
    Data = #{
        channel_id => ChannelId,
        name => Name,
        token => Token,
        bot_uid => BotUid,
        creator_uid => CreatorUid,
        status => 1
    },
    case channel_webhook_repo:add_tx(Conn, Data) of
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
