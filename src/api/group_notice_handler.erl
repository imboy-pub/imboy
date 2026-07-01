-module(group_notice_handler).
-dialyzer({nowarn_function, [detail/3]}).

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
-include("error_code.hrl").

%% 导入错误处理函数
-import(imboy_error, [error_msg/1]).

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
            list ->
                list(Method, Req0, State);
            detail ->
                detail(Method, Req0, State);
            pin ->
                pin(Method, Req0, State);
            unpin ->
                unpin(Method, Req0, State);
            mark_read ->
                mark_read(Method, Req0, State);
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
    Gid2 = elib_cnv:safe_to_integer(Gid),
    Title = maps:get(<<"title">>, PostVals, ""),
    Body = maps:get(<<"body">>, PostVals, ""),
    Status = elib_cnv:safe_to_integer(maps:get(<<"status">>, PostVals, 0)),
    ExpiredAt = maps:get(<<"expired_at">>, PostVals, <<>>),
    ExpiredAt2 = elib_dt:rfc3339_to(ExpiredAt, millisecond),
    Now = elib_dt:now(),
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, error_msg(?ERR_TOO_MANY_REQUESTS), ?ERR_TOO_MANY_REQUESTS);
        _ when Gid2 == 0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ when Status < 0; Status > 2 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ when is_integer(ExpiredAt2) == false ->
            elib_response:error(
                Req0,
                error_msg(?ERR_INVALID_FORMAT),
                ?ERR_INVALID_FORMAT
            );
        _ ->
            Data =
                #{
                    group_id => Gid2,
                    user_id => Uid,
                    title => Title,
                    body => Body,
                    status => Status,
                    expired_at => ExpiredAt,
                    created_at => Now
                },
            case group_notice_logic:insert(Data) of
                {ok, NoticeId} ->
                    elib_response:success(Req0, #{<<"notice_id">> => NoticeId});
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end
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
    Gid2 = elib_cnv:safe_to_integer(Gid),
    Id2 = elib_cnv:safe_to_integer(Id),

    % 状态 0 待发布  1 已发布 2 取消发布
    Status = elib_cnv:safe_to_integer(maps:get(<<"status">>, PostVals, 0)),
    Title = maps:get(<<"title">>, PostVals, ""),
    Body = maps:get(<<"body">>, PostVals, ""),
    ExpiredAt = maps:get(<<"expired_at">>, PostVals, <<>>),
    Now = elib_dt:now(),

    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, error_msg(?ERR_TOO_MANY_REQUESTS), ?ERR_TOO_MANY_REQUESTS);
        _ when Gid2 == 0; Id2 == 0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ when Status < 0; Status > 2 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            Data =
                #{
                    edit_user_id => Uid,
                    title => Title,
                    body => Body,
                    status => Status,
                    expired_at => elib_dt:rfc3339_to(ExpiredAt),
                    updated_at => Now
                },
            case group_notice_logic:update(Id2, Data) of
                {ok, _} ->
                    elib_response:success(Req0, #{<<"notice_id">> => Id});
                {error, not_found} ->
                    elib_response:error(Req0, error_msg(?ERR_NOT_FOUND), ?ERR_NOT_FOUND);
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
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
    Gid2 = elib_cnv:safe_to_integer(Gid),
    Id2 = elib_cnv:safe_to_integer(Id),

    Now = elib_dt:now(),
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, error_msg(?ERR_TOO_MANY_REQUESTS), ?ERR_TOO_MANY_REQUESTS);
        _ when Gid2 == 0; Id2 == 0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            Data =
                #{
                    edit_user_id => Uid,
                    status => 1,
                    updated_at => Now
                },
            case group_notice_logic:update(Id2, Data) of
                {ok, _} ->
                    %% W1.1：持久化成功后向群内所有成员广播 S2C
                    %% `group_notice_published`，客户端据此刷新公告列表或展示
                    %% toast。广播失败不影响 HTTP 成功响应（`_ = ...`）。
                    _ = group_notice_logic:publish_notice(Uid, Gid2, Id2),
                    elib_response:success(Req0, #{<<"notice_id">> => Id});
                {error, not_found} ->
                    elib_response:error(Req0, error_msg(?ERR_NOT_FOUND), ?ERR_NOT_FOUND);
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end
    end.

%% @doc 删除群公告（软删除）
%% 删除指定的群公告
%%
%% @param Method HTTP方法（POST）
%% @param Req0 Cowboy请求对象，包含公告ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Id = maps:get(<<"notice_id">>, PostVals, 0),
    Id2 = elib_cnv:safe_to_integer(Id),

    case Id2 of
        0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_notice_logic:delete(Uid, Id2) of
                ok ->
                    elib_response:success(Req0, #{<<"notice_id">> => Id});
                {error, ?ERR_NOT_FOUND} ->
                    elib_response:error(Req0, error_msg(?ERR_NOT_FOUND), ?ERR_NOT_FOUND);
                {error, ?ERR_GROUP_PERMISSION_DENIED} ->
                    elib_response:error(Req0, <<"你没有权限删除公告"/utf8>>, ?ERR_GROUP_PERMISSION_DENIED);
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end
    end.

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
    Qs2 = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs2, undefined),
    Gid2 = elib_cnv:safe_to_integer(Gid),
    case Gid2 of
        0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            {Page, Size} = elib_param:page(Req0),
            case group_notice_logic:page(CurrentUid, Gid2, Page, Size) of
                {ok, Payload} ->
                    List = maps:get(list, Payload, []),
                    List2 =
                        List,
                    Payload2 = Payload#{list => List2},
                    elib_response:success(Req0, Payload2);
                {error, ?ERR_NOT_GROUP_MEMBER} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_NOT_GROUP_MEMBER), ?ERR_NOT_GROUP_MEMBER
                    );
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end
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
    Qs6 = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs6, undefined),
    Gid2 = elib_cnv:safe_to_integer(Gid),
    case Gid2 of
        0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_notice_logic:latest_published(CurrentUid, Gid2) of
                {ok, Payload} ->
                    Payload2 =
                        case Payload of
                            [Item] ->
                                [Item];
                            _ ->
                                Payload
                        end,
                    elib_response:success(Req0, Payload2);
                {error, ?ERR_NOT_GROUP_MEMBER} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_NOT_GROUP_MEMBER), ?ERR_NOT_GROUP_MEMBER
                    );
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end
    end.

%% @doc 查询群公告列表（新接口，支持置顶排序）
%% 获取群组的公告列表（分页）
%%
%% @param Method HTTP方法（GET）
%% @param Req0 Cowboy请求对象，包含群组ID和分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含公告列表的响应
%% @end
-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs2 = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs2, ""),
    Gid2 = elib_cnv:safe_to_integer(Gid),
    {Page, Size} = elib_param:page(Req0),

    case Gid2 of
        0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_notice_logic:list(CurrentUid, Gid2, Page, Size) of
                {ok, {Total, List}} ->
                    Payload = #{
                        <<"total">> => Total,
                        <<"page">> => Page,
                        <<"size">> => Size,
                        <<"items">> => List
                    },
                    elib_response:success(Req0, Payload);
                {error, ?ERR_NOT_GROUP_MEMBER} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_NOT_GROUP_MEMBER), ?ERR_NOT_GROUP_MEMBER
                    );
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end
    end.

%% @doc 查询公告详情
%% 获取单个公告的详细信息
%%
%% @param Method HTTP方法（GET）
%% @param Req0 Cowboy请求对象，包含公告ID
%% @param State 状态映射，包含 current_uid
%% @return 返回包含公告信息的响应
%% @end
-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs2 = cowboy_req:parse_qs(Req0),
    Id = proplists:get_value(<<"notice_id">>, Qs2, ""),
    Id2 = elib_cnv:safe_to_integer(Id),

    case Id2 of
        0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_notice_logic:detail(CurrentUid, Id2) of
                {ok, Notice} ->
                    elib_response:success(Req0, Notice);
                {error, ?ERR_NOT_GROUP_MEMBER} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_NOT_GROUP_MEMBER), ?ERR_NOT_GROUP_MEMBER
                    );
                {error, ?ERR_NOT_FOUND} ->
                    elib_response:error(Req0, error_msg(?ERR_NOT_FOUND), ?ERR_NOT_FOUND);
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end
    end.

%% @doc 置顶公告
%% 将公告置顶（仅群主和管理员）
%%
%% @param Method HTTP方法（POST）
%% @param Req0 Cowboy请求对象，包含公告ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec pin(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
pin(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Id = maps:get(<<"notice_id">>, PostVals, 0),
    Id2 = elib_cnv:safe_to_integer(Id),

    case Id2 of
        0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_notice_logic:pin(Uid, Id2) of
                ok ->
                    elib_response:success(Req0, #{<<"notice_id">> => Id});
                {error, ?ERR_NOT_FOUND} ->
                    elib_response:error(Req0, error_msg(?ERR_NOT_FOUND), ?ERR_NOT_FOUND);
                {error, ?ERR_GROUP_PERMISSION_DENIED} ->
                    elib_response:error(Req0, <<"你没有权限置顶公告"/utf8>>, ?ERR_GROUP_PERMISSION_DENIED);
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end
    end.

%% @doc 取消置顶公告
%% 取消公告置顶（仅群主和管理员）
%%
%% @param Method HTTP方法（POST）
%% @param Req0 Cowboy请求对象，包含公告ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec unpin(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
unpin(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Id = maps:get(<<"notice_id">>, PostVals, 0),
    Id2 = elib_cnv:safe_to_integer(Id),

    case Id2 of
        0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_notice_logic:unpin(Uid, Id2) of
                ok ->
                    elib_response:success(Req0, #{<<"notice_id">> => Id});
                {error, ?ERR_NOT_FOUND} ->
                    elib_response:error(Req0, error_msg(?ERR_NOT_FOUND), ?ERR_NOT_FOUND);
                {error, ?ERR_GROUP_PERMISSION_DENIED} ->
                    elib_response:error(Req0, <<"你没有权限取消置顶"/utf8>>, ?ERR_GROUP_PERMISSION_DENIED);
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end
    end.

%% @doc 标记公告为已读
%% 标记公告为已读并增加已读数
%%
%% @param Method HTTP方法（POST）
%% @param Req0 Cowboy请求对象，包含公告ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec mark_read(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
mark_read(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Id = maps:get(<<"notice_id">>, PostVals, 0),
    Id2 = elib_cnv:safe_to_integer(Id),

    case Id2 of
        0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_notice_logic:mark_as_read(Uid, Id2) of
                {ok, Notice} ->
                    Notice2 = Notice,
                    elib_response:success(Req0, Notice2);
                {error, ?ERR_NOT_GROUP_MEMBER} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_NOT_GROUP_MEMBER), ?ERR_NOT_GROUP_MEMBER
                    );
                {error, _} ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
