-module(bot_repo).

%%%
% Bot 元数据仓库 / Bot metadata repository
% 表 bot（见 priv/migrations/00000070_bot）：user_id 主键 = bot 的 user.id（account_type=3）
% 所有 SQL 经 elib_pg 参数化；commands/permissions/events 为 jsonb。
%%%

-export([tablename/0]).
-export([create/1]).
-export([find/1]).
-export([find_by_username/1]).
-export([find_by_token/1]).
-export([update/2]).
-export([set_status/2]).
-export([page/2]).
-export([page_by_owner/3]).
-export([search/3]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"bot">>).

%% @doc 创建 Bot 行
%% Data 键：user_id(必填), name, username, owner_uid, webhook_url, api_token, verify_token,
%%          commands, permissions, events, is_public, status
-spec create(map()) -> {ok, [map()]} | {error, term()}.
create(#{user_id := UserId, name := Name, owner_uid := OwnerUid} = Data) ->
    Tb = tablename(),
    Username = maps:get(username, Data, <<>>),
    Description = maps:get(description, Data, <<>>),
    Avatar = maps:get(avatar, Data, <<>>),
    WebhookUrl = maps:get(webhook_url, Data, <<>>),
    ApiToken = maps:get(api_token, Data, <<>>),
    VerifyToken = maps:get(verify_token, Data, <<>>),
    Commands = maps:get(commands, Data, <<"[]">>),
    Permissions = maps:get(permissions, Data, <<"[]">>),
    Events = maps:get(events, Data, <<"[]">>),
    IsPublic = maps:get(is_public, Data, false),
    Status = maps:get(status, Data, 1),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (user_id, name, username, description, avatar, owner_uid,"
            "  webhook_url, api_token, verify_token, commands, permissions, events,"
            "  is_public, status, created_at, updated_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10::jsonb,$11::jsonb,$12::jsonb,"
            "  $13,$14,NOW(),NOW())"
            " RETURNING user_id">>,
    case
        elib_pg:query(Sql, [
            UserId,
            Name,
            Username,
            Description,
            Avatar,
            OwnerUid,
            WebhookUrl,
            ApiToken,
            VerifyToken,
            Commands,
            Permissions,
            Events,
            IsPublic,
            Status
        ])
    of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            ?ERROR_LOG("bot_repo:create error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 按 user_id 查单个 Bot 行
-spec find(integer()) -> {ok, map()} | {error, notfound | term()}.
find(UserId) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT user_id, name, username, description, avatar, owner_uid,"
            " webhook_url, api_token, verify_token, commands, permissions, events,"
            " is_public, status, created_at, updated_at FROM ",
            Tb/binary,
            " WHERE user_id = $1"
        >>,
    case elib_pg:query(Sql, [UserId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 按 username 查找 Bot
-spec find_by_username(binary()) -> {ok, map()} | {error, notfound | term()}.
find_by_username(Username) ->
    Tb = tablename(),
    Sql = <<
        "SELECT user_id, name, username, description, avatar, owner_uid,"
        " webhook_url, is_public, status FROM ",
        Tb/binary,
        " WHERE username = $1"
    >>,
    case elib_pg:query(Sql, [Username]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 按 api_token 查找 Bot（API 认证时使用）
-spec find_by_token(binary()) -> {ok, map()} | {error, notfound | term()}.
find_by_token(Token) ->
    Tb = tablename(),
    Sql = <<
        "SELECT user_id, name, username, owner_uid, webhook_url, verify_token,"
        " permissions, events, status FROM ",
        Tb/binary,
        " WHERE api_token = $1"
    >>,
    case elib_pg:query(Sql, [Token]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 更新 Bot 字段（部分更新）
-spec update(integer(), map()) -> {ok, [map()]} | {error, term()}.
update(UserId, Data) when is_integer(UserId), is_map(Data) ->
    Tb = tablename(),
    Fields = update_fields(Data),
    case Fields of
        [] ->
            {ok, []};
        _ ->
            Assignments = [
                update_assignment(Column, Cast, Index)
             || {{Column, _Value, Cast}, Index} <- lists:zip(
                    Fields, lists:seq(1, length(Fields))
                )
            ],
            WhereIndex = length(Fields) + 1,
            Sql = iolist_to_binary([
                <<"UPDATE ">>,
                Tb,
                <<" SET ">>,
                join_binary(Assignments),
                <<", updated_at = NOW()">>,
                <<" WHERE user_id = $">>,
                integer_to_binary(WhereIndex),
                <<" RETURNING user_id">>
            ]),
            Params = [Value || {_Column, Value, _Cast} <- Fields] ++ [UserId],
            case elib_pg:query(Sql, Params) of
                {ok, Rows} ->
                    {ok, Rows};
                {error, Reason} ->
                    ?ERROR_LOG("bot_repo:update user_id=~p error ~p~n", [UserId, Reason]),
                    {error, Reason}
            end
    end.

update_fields(Data) ->
    [
        {Column, Value, Cast}
     || {Key, Column, Cast} <- [
            {name, <<"name">>, <<>>},
            {username, <<"username">>, <<>>},
            {description, <<"description">>, <<>>},
            {avatar, <<"avatar">>, <<>>},
            {webhook_url, <<"webhook_url">>, <<>>},
            {verify_token, <<"verify_token">>, <<>>},
            {commands, <<"commands">>, <<"::jsonb">>},
            {permissions, <<"permissions">>, <<"::jsonb">>},
            {events, <<"events">>, <<"::jsonb">>},
            {is_public, <<"is_public">>, <<>>}
        ],
        {ok, Value} <- [maps:find(Key, Data)]
    ].

update_assignment(Column, Cast, Index) ->
    iolist_to_binary([Column, <<" = $">>, integer_to_binary(Index), Cast]).

join_binary([]) ->
    <<>>;
join_binary([First | Rest]) ->
    lists:foldl(
        fun(Item, Acc) -> <<Acc/binary, ", ", Item/binary>> end,
        First,
        Rest
    ).

%% @doc 设置 Bot 状态（-1=deleted, 0=disabled, 1=active）
-spec set_status(integer(), -1 | 0 | 1) -> {ok, non_neg_integer()} | {error, term()}.
set_status(UserId, Status) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => Status, updated_at => elib_dt:now()}, <<"user_id = $1">>, [
        UserId
    ]).

%% @doc 分页列出所有 Bot（管理后台）
-spec page(pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page(Page, Size) when Page > 0, Size > 0 ->
    Tb = tablename(),
    UTb = user_repo:tablename(),
    From = <<" FROM ", Tb/binary, " b JOIN ", UTb/binary, " u ON u.id = b.user_id">>,
    case elib_pg:query(<<"SELECT count(*) AS total", From/binary>>, []) of
        {ok, [#{<<"total">> := 0} | _]} ->
            {ok, empty_page(Page, Size)};
        {ok, [#{<<"total">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            ListSql = <<
                "SELECT b.user_id, b.name, b.username, b.description, b.owner_uid,"
                " b.is_public, b.status, u.nickname, u.avatar",
                From/binary,
                " ORDER BY b.created_at DESC LIMIT ",
                (integer_to_binary(Size))/binary,
                " OFFSET ",
                (integer_to_binary(Offset))/binary
            >>,
            case elib_pg:query(ListSql, []) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, Reason} ->
                    ?ERROR_LOG("bot_repo:page list error ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, []} ->
            {ok, empty_page(Page, Size)};
        {error, Reason} ->
            ?ERROR_LOG("bot_repo:page count error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 按所有者分页列出 Bot
-spec page_by_owner(pos_integer(), pos_integer(), integer()) -> {ok, map()} | {error, term()}.
page_by_owner(Page, Size, OwnerUid) when Page > 0, Size > 0 ->
    Tb = tablename(),
    Utb = user_repo:tablename(),
    From = <<" FROM ", Tb/binary, " b JOIN ", Utb/binary, " u ON u.id = b.user_id">>,
    Where = <<" WHERE b.owner_uid = $1">>,
    case elib_pg:query(<<"SELECT count(*) AS total", From/binary, Where/binary>>, [OwnerUid]) of
        {ok, [#{<<"total">> := 0} | _]} ->
            {ok, empty_page(Page, Size)};
        {ok, [#{<<"total">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            ListSql = <<
                "SELECT b.user_id, b.name, b.username, b.description,"
                " b.is_public, b.status, b.webhook_url, b.events, b.created_at,"
                " u.nickname, u.avatar",
                From/binary,
                Where/binary,
                " ORDER BY b.created_at DESC LIMIT ",
                (integer_to_binary(Size))/binary,
                " OFFSET ",
                (integer_to_binary(Offset))/binary
            >>,
            case elib_pg:query(ListSql, [OwnerUid]) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, Reason} ->
                    ?ERROR_LOG("bot_repo:page_by_owner list error ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, []} ->
            {ok, empty_page(Page, Size)};
        {error, Reason} ->
            ?ERROR_LOG("bot_repo:page_by_owner count error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 搜索 Bot（按名称或 username 模糊匹配）
-spec search(binary(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
search(Keyword, Page, Size) when Page > 0, Size > 0 ->
    Tb = tablename(),
    UTb = user_repo:tablename(),
    Like = <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>,
    From = <<" FROM ", Tb/binary, " b JOIN ", UTb/binary, " u ON u.id = b.user_id">>,
    Where = <<" WHERE b.status = 1 AND (b.name ILIKE $1 OR b.username ILIKE $1)">>,
    case elib_pg:query(<<"SELECT count(*) AS total", From/binary, Where/binary>>, [Like]) of
        {ok, [#{<<"total">> := 0} | _]} ->
            {ok, empty_page(Page, Size)};
        {ok, [#{<<"total">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            ListSql = <<
                "SELECT b.user_id, b.name, b.username, b.description,"
                " b.is_public, b.owner_uid, u.nickname, u.avatar",
                From/binary,
                Where/binary,
                " ORDER BY b.created_at DESC LIMIT ",
                (integer_to_binary(Size))/binary,
                " OFFSET ",
                (integer_to_binary(Offset))/binary
            >>,
            case elib_pg:query(ListSql, [Like]) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, Reason} ->
                    ?ERROR_LOG("bot_repo:search list error ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, []} ->
            {ok, empty_page(Page, Size)};
        {error, Reason} ->
            ?ERROR_LOG("bot_repo:search count error ~p~n", [Reason]),
            {error, Reason}
    end.

-spec empty_page(pos_integer(), pos_integer()) -> map().
empty_page(Page, Size) ->
    #{total => 0, page => Page, size => Size, list => []}.
