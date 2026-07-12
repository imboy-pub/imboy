-module(channel_webhook_repo).
%%%
% channel_webhook_repo 是 channel_webhook repository 缩写
% 频道 incoming webhook 数据仓库层 / Channel incoming webhook repository
%%%

-export([tablename/0]).
-export([add/1]).
-export([find_by_token/1]).
-export([list_by_channel/1]).
-export([set_status/3]).

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
