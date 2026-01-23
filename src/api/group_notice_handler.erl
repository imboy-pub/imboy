-module(group_notice_handler).

%%%
% group_notice 控制器模块
% group_notice controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化群公告处理器
%% 根据请求中的 action 参数和HTTP方法调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 和 current_uid 等信息
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            add ->
                add(Method, Req0, State);
            edit ->
                edit(Method, Req0, State);
            delete ->
                delete(Method, Req0, State);
            page ->
                page(Method, Req0, State);
            publish ->
                publish(Method, Req0, State);
            latest ->
                latest(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 添加群公告
%% 创建新的群公告
%%
%% @param Method HTTP方法（POST）
%% @param Req0 Cowboy请求对象，包含群公告信息
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec add(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
add(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, ""),
    Gid2 = elib_hashids:decode(Gid),
    Body = maps:get(<<"body">>, PostVals, ""),
    Status = maps:get(<<"status">>, PostVals, 0),
    ExpiredAt = maps:get(<<"expired_at">>, PostVals, <<>>),
    ExpiredAt2 = elib_dt:rfc3339_to(ExpiredAt, millisecond),
    Now = elib_dt:now(),
    % ?DEBUG_LOG([ExpiredAt, ExpiredAt2]),
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            elib_response:error(Req0, "group id 格式有误");
        _ when is_integer(ExpiredAt2) == false ->
            elib_response:error(Req0,
                                 "expired_at 格式有误，应当符合rfc3339规范，正确格式为： 2024-02-14 11:16:37.129353+08:00");
        _ ->
            Data =
                #{group_id => Gid2,
                  user_id => Uid,
                  body => Body,
                  status => Status,
                  expired_at => ExpiredAt,
                  created_at => Now},
            Tb = group_notice_repo:tablename(),
            {ok, Id, _} =
                elib_pg_sql:parse_result(
                    elib_pg:insert(Tb, Data, <<"RETURNING id">>)),

            elib_response:success(Req0, [{<<"notice_id">>, Id}], "success.")
    end.

%% @doc 编辑群公告
%% 修改已存在的群公告
%%
%% @param Method HTTP方法（POST）
%% @param Req0 Cowboy请求对象，包含公告ID和修改内容
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec edit(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
edit(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Id = maps:get(<<"notice_id">>, PostVals, 0),
    Gid = maps:get(<<"gid">>, PostVals, ""),
    Gid2 = elib_hashids:decode(Gid),

    % 状态 0 待发布  1 已发布 2 取消发布
    Status = maps:get(<<"status">>, PostVals, 0),
    Body = maps:get(<<"body">>, PostVals, ""),
    ExpiredAt = maps:get(<<"expired_at">>, PostVals, <<>>),
    Now = elib_dt:now(),

    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            elib_response:error(Req0, "group id 格式有误");
        _ ->
            Data =
                #{edit_user_id => Uid,
                  body => Body,
                  status => Status,
                  expired_at => elib_dt:rfc3339_to(ExpiredAt),
                  updated_at => Now},
            Tb = group_notice_repo:tablename(),
            % 使用安全的参数化查询，避免SQL注入
            Where = <<"id = $1 AND group_id = $2">>,
            case elib_pg:update(Tb, Data, Where, [Id, Gid2]) of
                {ok, 1} ->
                    elib_response:success(Req0, [{<<"notice_id">>, Id}], "success.");
                {ok, _} ->
                    elib_response:error(Req0, "公告不存在");
                {error, Reason} ->
                    elib_response:error(Req0, Reason)
            end
    end.

%% @doc 发布群公告
%% 将待发布状态的群公告发布
%%
%% @param Method HTTP方法（POST）
%% @param Req0 Cowboy请求对象，包含公告ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec publish(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
publish(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Id = maps:get(<<"notice_id">>, PostVals, 0),
    Gid = maps:get(<<"gid">>, PostVals, ""),
    Gid2 = elib_hashids:decode(Gid),

    Now = elib_dt:now(),
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            elib_response:error(Req0, "group id 格式有误");
        _ ->
            Data =
                #{edit_user_id => Uid,
                  status => 1,
                  updated_at => Now},
            Tb = group_notice_repo:tablename(),
            % 使用安全的参数化查询，避免SQL注入
            Where = <<"id = $1 AND group_id = $2">>,
            case elib_pg:update(Tb, Data, Where, [Id, Gid2]) of
                {ok, 1} ->
                    elib_response:success(Req0, [{<<"notice_id">>, Id}], "success.");
                {ok, _} ->
                    elib_response:error(Req0, "公告不存在");
                {error, Reason} ->
                    elib_response:error(Req0, Reason)
            end
    end.

%% @doc 删除群公告
%% 删除指定的群公告
%%
%% @param Method HTTP方法（DELETE）
%% @param Req0 Cowboy请求对象，包含公告ID
%% @param _State 状态映射
%% @return 返回成功响应
%% @end
-spec delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete(<<"DELETE">>, Req0, _State) ->
    % CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Id = maps:get(<<"notice_id">>, PostVals, 0),
    Gid = maps:get(<<"gid">>, PostVals, ""),
    Gid2 = elib_hashids:decode(Gid),

    Tb = group_notice_repo:tablename(),
    % 使用安全的参数化查询，避免SQL注入
    Where = <<"id = $1 AND group_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", Where/binary>>,
    % ?DEBUG_LOG([Sql]),
    _ = elib_pg:execute(Sql, [Id, Gid2]),
    elib_response:success(Req0).

%% @doc 群公告分页列表
%% 获取群组的公告列表（分页）
%%
%% @param Method HTTP方法（GET）
%% @param Req0 Cowboy请求对象，包含群组ID和分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含公告列表的响应
%% @end
-spec page(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
page(<<"GET">>, Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    #{gid := Gid} = cowboy_req:match_qs([{gid, [], undefined}], Req0),
    Gid2 = elib_hashids:decode(Gid),
    GM = group_member_repo:find(Gid2, CurrentUid, <<"id">>),
    GMSize = maps:size(GM),
    case Gid2 of
        0 ->
            elib_response:error(Req0, "group id 必须");
        _ when GMSize == 0 ->
            elib_response:error(Req0, "你不是群成员");
        _ ->
            {Page, Size} = elib_param:page(Req0),
            Column =
                <<"id as notice_id, user_id, edit_user_id, body, status, expired_at, "
                  "updated_at, created_at">>,
            % 使用安全的参数化查询，避免SQL注入
            Where = #{group_id => Gid2},
            Tb = group_notice_repo:tablename(),
            {ok, Payload} =
                elib_pg:page_with_total(Tb, Column, Where, <<"expired_at desc">>, Page, Size),
            % 处理用户ID哈希
            List = maps:get(list, Payload, []),
            List2 =
                [elib_hashids:replace_id(
                     elib_hashids:replace_id(Item, <<"user_id">>), <<"edit_user_id">>)
                 || Item <- List],
            Payload2 = Payload#{list => List2},
            elib_response:success(Req0, Payload2)
    end.

%% @doc 获取最新群公告
%% 获取群组最新已发布的公告
%%
%% @param Method HTTP方法（GET）
%% @param Req0 Cowboy请求对象，包含群组ID
%% @param State 状态映射，包含 current_uid
%% @return 返回包含公告信息的响应
%% @end
-spec latest(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
latest(<<"GET">>, Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    #{gid := Gid} = cowboy_req:match_qs([{gid, [], undefined}], Req0),
    Gid2 = elib_hashids:decode(Gid),
    GM = group_member_repo:find(Gid2, CurrentUid, <<"id">>),
    GMSize = maps:size(GM),
    case Gid2 of
        0 ->
            elib_response:error(Req0, "group id 必须");
        _ when GMSize == 0 ->
            elib_response:error(Req0, "你不是群成员");
        _ ->
            Column =
                <<"id as notice_id, user_id, edit_user_id, body, status, expired_at, "
                  "updated_at, created_at">>,
            % 使用安全的参数化查询，避免SQL注入
            Where = <<"status = 1 AND group_id = $1">>,
            Tb = group_notice_repo:tablename(),
            Sql = <<"SELECT ",
                    Column/binary,
                    " FROM ",
                    Tb/binary,
                    " WHERE ",
                    Where/binary,
                    " ORDER BY id desc">>,
            {ok, Payload} = elib_pg:query(Sql, [Gid2]),
            % 处理用户ID哈希
            Payload2 =
                case Payload of
                    [Item] ->
                        [elib_hashids:replace_id(
                             elib_hashids:replace_id(Item, <<"user_id">>), <<"edit_user_id">>)];
                    _ ->
                        Payload
                end,
            elib_response:success(Req0, Payload2)
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

