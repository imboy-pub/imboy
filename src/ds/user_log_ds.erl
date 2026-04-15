-module(user_log_ds).
%%%
% user_log_ds 是用户日志数据服务层
% 封装用户日志的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([add/1]).
-export([add_password_change_log/4]).
-export([add_logout_apply_log/3]).
-export([add_internal/5]).
-export([page_group_governance_log/4]).
-export([page_logout_apply_log/4]).
-export([list_logout_apply_log_chunk/4]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 添加密码修改日志
%% @param Conn 数据库连接（可选）
%% @param Uid 用户ID
%% @param Req0 HTTP 请求对象
%% @param Type 日志类型 (110: 修改密码)
%% @return {ok, Result} | {error, Reason}
-spec add_password_change_log(pid() | undefined, integer(), map(), integer()) -> {ok, any()} | {error, any()}.
add_password_change_log(Conn, Uid, Req0, Type) ->
    AppVsn = cowboy_req:header(<<"vsn">>, Req0, undefined),
    DID = cowboy_req:header(<<"did">>, Req0, undefined),
    DType = cowboy_req:header(<<"cos">>, Req0, undefined),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, undefined),
    {ok, Body} = jsone_encode:encode(#{
        <<"app_vsn">> => AppVsn,
        <<"did">> => DID,
        <<"dtype">> => DType,
        <<"ip">> => Ip
    }, [native_utf8]),
    add_internal(Conn, Type, Uid, Body, elib_dt:now()).

%% @doc 添加注销申请日志
%% @param Conn 数据库连接（可选）
%% @param Uid 用户ID
%% @param Req0 HTTP 请求对象
%% @return {ok, Result} | {error, Reason}
-spec add_logout_apply_log(pid() | undefined, integer(), map()) -> {ok, any()} | {error, any()}.
add_logout_apply_log(Conn, Uid, Req0) ->
    AppVsn = cowboy_req:header(<<"vsn">>, Req0, undefined),
    DID = cowboy_req:header(<<"did">>, Req0, undefined),
    DType = cowboy_req:header(<<"cos">>, Req0, undefined),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, undefined),
    {ok, Body} = jsone_encode:encode(#{
        <<"app_vsn">> => AppVsn,
        <<"did">> => DID,
        <<"dtype">> => DType,
        <<"ip">> => Ip
    }, [native_utf8]),
    add_internal(Conn, 102, Uid, Body, elib_dt:now()).

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 内部函数：添加用户日志
-spec add_internal(pid() | undefined, integer(), integer(), binary(), binary()) -> {ok, any()} | {error, any()}.
add_internal(undefined, Type, Uid, Body, CreatedAt) ->
    user_log_repo:add(#{
        type => Type,
        uid => Uid,
        body => Body,
        created_at => CreatedAt
    });
add_internal(Conn, Type, Uid, Body, CreatedAt) ->
    user_log_repo:add(Conn, #{
        type => Type,
        uid => Uid,
        body => Body,
        created_at => CreatedAt
    }).

%% G3: moment_logic 不应直调 user_log_repo / thin DS wrapper
-spec add(map()) -> {ok, term()} | {error, term()}.
add(Data) ->
    user_log_repo:add(Data).

%% @doc 分页查询群治理审计日志（JOIN adm_user）
%% @param WhereSql 额外 WHERE 条件（二进制，以 AND 开头或空）
%% @param Params WHERE 绑定参数列表
%% @param Page 页码
%% @param Size 每页大小
-spec page_group_governance_log(binary(), list(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page_group_governance_log(WhereSql, Params, Page, Size) ->
    TbLog = user_log_repo:tablename(),
    TbAdmUser = adm_user_repo:tablename(),
    BaseFrom = iolist_to_binary([
        <<" FROM ">>, TbLog, <<" l LEFT JOIN ">>, TbAdmUser, <<" u ON u.id = l.uid ">>,
        <<"WHERE l.type = 902 AND l.remark = 'adm_group_governance'">>,
        WhereSql
    ]),
    CountSql = iolist_to_binary([<<"SELECT COUNT(*) AS count">>, BaseFrom]),
    case elib_pg:one(CountSql, Params) of
        {ok, CountRow} ->
            Total = case maps:get(<<"count">>, CountRow, 0) of
                V when is_integer(V) -> V;
                _ -> 0
            end,
            Offset = (Page - 1) * Size,
            LimitPos = integer_to_binary(length(Params) + 1),
            OffsetPos = integer_to_binary(length(Params) + 2),
            DataSql = iolist_to_binary([
                <<"SELECT l.uid, u.account, u.nickname, l.body, l.created_at ">>,
                BaseFrom,
                <<" ORDER BY l.created_at DESC, l.uid DESC ">>,
                <<"LIMIT $">>, LimitPos, <<" OFFSET $">>, OffsetPos
            ]),
            case elib_pg:query(DataSql, Params ++ [Size, Offset]) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 分页查询注销申请日志（JOIN user）
-spec page_logout_apply_log(binary(), list(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page_logout_apply_log(WhereSql, Params, Page, Size) ->
    TbLog = user_log_repo:tablename(),
    TbUser = user_repo:tablename(),
    BaseFrom = iolist_to_binary([
        <<" FROM ">>, TbLog, <<" l LEFT JOIN ">>, TbUser, <<" u ON u.id = l.uid ">>,
        <<"WHERE l.type = 102">>, WhereSql
    ]),
    CountSql = iolist_to_binary([<<"SELECT COUNT(*) AS count">>, BaseFrom]),
    case elib_pg:one(CountSql, Params) of
        {ok, CountRow} ->
            Total = case maps:get(<<"count">>, CountRow, 0) of
                V when is_integer(V) -> V;
                _ -> 0
            end,
            Offset = (Page - 1) * Size,
            LimitPos = integer_to_binary(length(Params) + 1),
            OffsetPos = integer_to_binary(length(Params) + 2),
            DataSql = iolist_to_binary([
                <<"SELECT l.uid, u.account, u.nickname, u.status AS user_status, l.body, l.created_at ">>,
                BaseFrom,
                <<" ORDER BY l.created_at DESC, l.uid DESC ">>,
                <<"LIMIT $">>, LimitPos, <<" OFFSET $">>, OffsetPos
            ]),
            case elib_pg:query(DataSql, Params ++ [Size, Offset]) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 流式导出注销申请日志（按 offset+limit 分块）
-spec list_logout_apply_log_chunk(binary(), list(), pos_integer(), non_neg_integer()) ->
    {ok, [map()]} | {error, term()}.
list_logout_apply_log_chunk(WhereSql, Params, Limit, Offset) ->
    TbLog = user_log_repo:tablename(),
    TbUser = user_repo:tablename(),
    BaseFrom = iolist_to_binary([
        <<" FROM ">>, TbLog, <<" l LEFT JOIN ">>, TbUser, <<" u ON u.id = l.uid ">>,
        <<"WHERE l.type = 102">>, WhereSql
    ]),
    LimitPos = integer_to_binary(length(Params) + 1),
    OffsetPos = integer_to_binary(length(Params) + 2),
    DataSql = iolist_to_binary([
        <<"SELECT l.uid, u.account, u.nickname, l.body, l.created_at ">>,
        BaseFrom,
        <<" ORDER BY l.created_at DESC, l.uid DESC ">>,
        <<"LIMIT $">>, LimitPos, <<" OFFSET $">>, OffsetPos
    ]),
    elib_pg:query(DataSql, Params ++ [Limit, Offset]).
