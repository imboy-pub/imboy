-module(channel_webhook_ds).

%%%
% 频道 incoming webhook 数据服务 / Channel incoming webhook data service
%
% 职责：webhook 编排（生成 token + 建 system_bot 用户 + bot 加频道编辑 + 落表）、
%       token 查询、停用、列表。屏蔽 repo 存储细节。
% 事务（TX-01）：create/3 四步写【建 system_bot user 行 + 标 account_type=2 +
%       bot 加频道编辑(role=1) + 落 channel_webhook 表】收进
%       workspace_guard:write_tx 单事务，任一步失败整体回滚，零孤儿行
%       （原 bot user 两写在事务外 auto-commit，事务失败时残留无主 bot user）。
% 范式：镜像 ai_agent_ds / bot_ds 的 with_tx 收口（user_repo:create_tx/update_tx
%       尊重传入 id，user 行 id 与 bot_uid/webhook.bot_uid 贯穿同一 id）。
%%%

-export([create/3]).
-export([find_by_token/1]).
-export([disable/2]).
-export([rotate/4]).
-export([list_by_channel/1]).

-include("log.hrl").

%% account_type 枚举（对齐迁移 00000027 注释：0=human 1=ai_agent 2=system_bot）
-define(ACCOUNT_TYPE_SYSTEM_BOT, 2).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 创建 webhook：归档前置检查（避免归档频道上白跑）→ 生成 token →
%% 单事务【建 system_bot 用户 + 标 account_type=2 + 归档写守卫（FOR UPDATE 行锁）
%% + bot 加频道编辑(role=1) + 落 channel_webhook 表】。token 仅创建时明文返回一次。
%% TX-01 事务收敛：bot user 两写并入同一事务（镜像 ai_agent_ds/bot_ds 收口），
%%   任一步失败整体回滚——杜绝"事务失败残留无主 bot user"（原 ponytail 取舍）
%%   与"webhook/admin 行已提交而 bot user 缺失"两类孤儿；因此不再需要事务后
%%   channel_admin 尽力补偿（回滚即撤销全部四写）。
-spec create(integer(), binary(), integer()) ->
    {ok, map()} | {error, binary() | {integer(), binary()}}.
create(ChannelId, Name, CreatorUid) ->
    %% T7 归档写守卫（A2 收口）：前置短路（快速失败），权威拒绝在事务内
    %% （ensure_writable_tx 行锁与全部写原子）。
    case workspace_guard:ensure_writable({channel, ChannelId}) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            Token = gen_token(),
            BotUid = elib_tsid:generate(user),
            AdminData = #{
                channel_id => ChannelId,
                user_id => BotUid,
                role => 1,
                created_at => elib_dt:now()
            },
            case
                workspace_guard:write_tx({channel, ChannelId}, fun(Conn) ->
                    ok = workspace_guard:abort_on_error(
                        create_bot_user_tx(Conn, BotUid, Name)
                    ),
                    ok = workspace_guard:abort_on_error(
                        case channel_admin_repo:add(Conn, AdminData) of
                            {ok, _} -> ok;
                            {error, AddReason} -> {error, {channel_admin, AddReason}}
                        end
                    ),
                    %% 第 4 步失败同样必须整体回滚：不能把 {error, _} 作为
                    %% WriteFun 的正常返回值——epgsql:with_transaction 视为成功
                    %% 而 COMMIT，会留下 bot user + channel_admin 孤儿（step4
                    %% 故障注入用例抓出）。失败抛 abort_tx，成功值原样透传。
                    case
                        insert_webhook_tx(
                            Conn, ChannelId, Name, Token, BotUid, CreatorUid
                        )
                    of
                        {ok, _} = Ok ->
                            Ok;
                        {error, InsertReason} ->
                            throw({abort_tx, {webhook_insert, InsertReason}})
                    end
                end)
            of
                {ok, _} = Ok ->
                    Ok;
                %% 稳定错误码（980 归档拒绝/竞态兜底等）透传
                {error, {Code, Msg}} when is_integer(Code) ->
                    {error, {Code, Msg}};
                %% 单层匹配：elib_pg:with_tx 捕获 throw({abort_tx, Payload}) 后
                %% ROLLBACK 并原样返回 {error, Payload}（不再额外包裹一层）
                {error, {channel_admin, Reason}} ->
                    ?ERROR_LOG(
                        "channel_webhook_ds:create admin error ~p~n",
                        [Reason]
                    ),
                    {error, <<"创建 webhook 失败"/utf8>>};
                {error, {webhook_insert, Reason}} ->
                    ?ERROR_LOG(
                        "channel_webhook_ds:create insert abort ~p~n",
                        [Reason]
                    ),
                    {error, elib_cnv:safe_to_binary(Reason)};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

%% @doc 认证查找（WH-02 三级链，fail-closed）：
%%   1) token_digest 精确查找（新 token / 已回填存量）；
%%   2) grace_digest 查找（rotate 宽限窗内旧 token，grace_until 过滤在 SQL）；
%%   3) 旧明文双读（迁移期：存量行 token 列匹配且未回填 digest）→ 命中惰性回填。
%% 命中即 touch last_used_at（best-effort）；含停用行，状态判断在 Logic 层。
-spec find_by_token(binary()) -> {ok, map()} | {error, not_found}.
find_by_token(Token) when is_binary(Token), Token =/= <<>> ->
    Digest = digest_hex(Token),
    Row =
        case row_by_digest(Digest) of
            {ok, R} ->
                R;
            error ->
                case row_by_grace(Digest) of
                    {ok, R2} -> R2;
                    error -> row_by_legacy_and_backfill(Token)
                end
        end,
    case is_map(Row) andalso map_size(Row) > 0 of
        true ->
            _ = channel_webhook_repo:touch_last_used(
                maps:get(<<"id">>, Row, 0)
            ),
            {ok, Row};
        false ->
            {error, not_found}
    end;
find_by_token(_) ->
    {error, not_found}.

row_by_digest(Digest) ->
    case channel_webhook_repo:find_by_digest(Digest) of
        Row when is_map(Row), map_size(Row) > 0 -> {ok, Row};
        _ -> error
    end.

row_by_grace(Digest) ->
    case channel_webhook_repo:find_by_grace_digest(Digest) of
        Row when is_map(Row), map_size(Row) > 0 -> {ok, Row};
        _ -> error
    end.

%% 迁移期双读：存量行明文列匹配 → 惰性回填 digest/prefix（回填失败不阻断认证）
row_by_legacy_and_backfill(Token) ->
    case channel_webhook_repo:find_by_token(Token) of
        Row when is_map(Row), map_size(Row) > 0 ->
            WebhookId = maps:get(<<"id">>, Row, 0),
            _ = elib_pg:execute(
                <<"UPDATE ", (channel_webhook_repo:tablename())/binary,
                    " SET token_digest = $2, token_prefix = $3"
                    " WHERE id = $1 AND token_digest = ''">>,
                [
                    WebhookId,
                    digest_hex(Token),
                    binary:part(Token, 0, 8)
                ]
            ),
            Row;
        _ ->
            error
    end.

digest_hex(Token) when is_binary(Token) ->
    binary:encode_hex(crypto:hash(sha256, Token), lowercase).

%% @doc 轮换 token：生成新 token（一次返回），旧 token 进宽限窗
%% （GraceSecs 秒内仍可用，过期后稳定 404；PDT-01 webhook 契约 §3）。
%% 频道管理员校验由调用方（Logic with_manage_role）完成。
-spec rotate(integer(), integer(), binary(), non_neg_integer()) ->
    {ok, map()} | {error, binary()}.
rotate(_ChannelId, WebhookId, _Name, GraceSecs) ->
    NewToken = gen_token(),
    NewDigest = digest_hex(NewToken),
    NewPrefix = binary:part(NewToken, 0, 8),
    GraceUntil = elib_dt:to_rfc3339(os:system_time(second) + GraceSecs),
    case channel_webhook_repo:rotate(WebhookId, NewDigest, NewPrefix, <<>>, GraceUntil) of
        {ok, _} ->
            {ok, #{<<"token">> => NewToken, <<"grace_secs">> => GraceSecs}};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
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

%% 建 bot 用户行并标记 account_type=2（TX-01：事务内，镜像 bot_ds/create_bot_user_tx；
%% create_tx 尊重传入 id——user 行 id 与 bot_uid/webhook.bot_uid 贯穿同一 id，
%% update_tx 影响 0 行即回滚，杜绝 id 脱钩静默损坏）
-spec create_bot_user_tx(any(), integer(), binary()) -> ok | {error, term()}.
create_bot_user_tx(Conn, BotUid, Nickname) when BotUid > 0 ->
    Account = <<"chbot_", (ec_cnv:to_binary(BotUid))/binary>>,
    case user_repo:create_tx(Conn, #{id => BotUid, nickname => Nickname, account => Account}) of
        {ok, BotUid} ->
            case user_repo:update_tx(Conn, BotUid, #{account_type => ?ACCOUNT_TYPE_SYSTEM_BOT}) of
                {ok, 1} ->
                    ok;
                {ok, Other} ->
                    {error, {account_type_rows, Other}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, Other} ->
            {error, {id_mismatch, Other}};
        {error, Reason} ->
            {error, Reason}
    end;
create_bot_user_tx(_, _, _) ->
    {error, invalid_bot_uid}.

-spec insert_webhook_tx(any(), integer(), binary(), binary(), integer(), integer()) ->
    {ok, map()} | {error, binary()}.
insert_webhook_tx(Conn, ChannelId, Name, Token, BotUid, CreatorUid) ->
    %% WH-02（A01）：明文 token 不落库——token 列写空，仅存 SHA-256 摘要+前缀；
    %% 明文经返回值交给调用方一次性展示。
    Data = #{
        channel_id => ChannelId,
        name => Name,
        token => <<>>,
        token_digest => digest_hex(Token),
        token_prefix => binary:part(Token, 0, 8),
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
