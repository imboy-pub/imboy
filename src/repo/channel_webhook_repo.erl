-module(channel_webhook_repo).
%%%
% channel_webhook_repo 是 channel_webhook repository 缩写
% 频道 incoming webhook 数据仓库层 / Channel incoming webhook repository
%%%

-export([tablename/0]).
-export([add/1]).
-export([add_tx/2]).
-export([find_by_token/1]).
-export([list_by_channel/1]).
-export([set_status/3]).
-export([set_status_tx/4]).
%% WH-02：token 摘要化与轮换
-export([find_by_digest/1, find_by_grace_digest/1]).
-export([rotate/5, touch_last_used/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取频道 webhook 表的表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"channel_webhook">>).

%% @doc 新增 webhook（生成 TSID 主键）
-spec add(map()) -> {ok, integer()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(channel_webhook),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 事务内新增 webhook（归档写守卫同事务，DS 层 write_tx 调用）
-spec add_tx(any(), map()) -> {ok, integer()} | {error, term()}.
add_tx(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(channel_webhook),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 按 token 查找 webhook（含停用行，状态判断在 Logic 层）
-spec find_by_token(binary()) -> map().
find_by_token(Token) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE token = $1 LIMIT 1">>,
    case elib_pg:one(Sql, [Token]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 查询频道的 webhook 列表
-spec list_by_channel(integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE channel_id = $1 ORDER BY id DESC">>,
    elib_pg:query(Sql, [ChannelId]).

%% @doc 更新 webhook 状态（按 id + channel_id 双条件，防跨频道操作）
-spec set_status(integer(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, any()}.
set_status(ChannelId, WebhookId, Status) ->
    Tb = tablename(),
    elib_pg:update(
        Tb,
        #{status => Status},
        <<"id = $1 AND channel_id = $2">>,
        [WebhookId, ChannelId]
    ).

%% @doc 事务内更新 webhook 状态（归档写守卫同事务）
-spec set_status_tx(any(), integer(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, any()}.
set_status_tx(Conn, ChannelId, WebhookId, Status) ->
    Tb = tablename(),
    {Sql, Params} =
        elib_pg_sql:update(
            Tb,
            #{status => Status},
            <<"id = $1 AND channel_id = $2">>,
            [WebhookId, ChannelId]
        ),
    elib_pg:execute(Conn, Sql, Params).

%% ===================================================================
%% WH-02：token 摘要化与轮换（PDT-01 webhook 契约 §3）
%% ===================================================================

%% @doc 按新 token 摘要精确查找（新 token 只存摘要）。
-spec find_by_digest(binary()) -> map().
find_by_digest(Digest) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE token_digest = $1 LIMIT 1">>,
    case elib_pg:one(Sql, [Digest]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 按宽限摘要查找（rotate 后旧 token 的宽限窗）。
-spec find_by_grace_digest(binary()) -> map().
find_by_grace_digest(Digest) ->
    Tb = tablename(),
    Sql =
        <<"SELECT * FROM ", Tb/binary, " WHERE grace_digest = $1 AND grace_until > NOW() LIMIT 1">>,
    case elib_pg:one(Sql, [Digest]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 轮换：新摘要生效，旧摘要进宽限窗（grace_until 后稳定 404）。
%% GraceUntil 须为 RFC3339 binary（本仓 timestamptz 编解码器 elib_dt.rfc3339 形态），
%% undefined/null 表示无宽限。
-spec rotate(integer(), binary(), binary(), binary(), binary() | undefined) ->
    {ok, non_neg_integer()} | {error, term()}.
rotate(WebhookId, NewDigest, NewPrefix, OldDigest, GraceUntil) ->
    Tb = tablename(),
    GraceParam =
        case GraceUntil of
            undefined -> null;
            B when is_binary(B) -> B
        end,
    elib_pg:execute(
        <<"UPDATE ", Tb/binary,
            " SET token_digest = $2, token_prefix = $3,"
            " grace_digest = $4, grace_until = $5, updated_at = NOW()"
            " WHERE id = $1">>,
        [WebhookId, NewDigest, NewPrefix, OldDigest, GraceParam]
    ).

%% @doc 更新最近使用时间（best-effort）。
-spec touch_last_used(integer()) -> ok.
touch_last_used(WebhookId) ->
    Tb = tablename(),
    _ = elib_pg:execute(
        <<"UPDATE ", Tb/binary, " SET last_used_at = NOW() WHERE id = $1">>,
        [WebhookId]
    ),
    ok.
