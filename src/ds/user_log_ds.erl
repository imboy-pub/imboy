-module(user_log_ds).
%%%
% user_log_ds 是用户日志数据服务层
% 封装用户日志的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([add_password_change_log/4]).
-export([add_logout_apply_log/3]).
-export([add_internal/5]).

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
