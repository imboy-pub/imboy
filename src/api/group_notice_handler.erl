-module(group_notice_handler).

%%%
% group_notice 控制器模块
% group_notice controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

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

add(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, ""),
    Gid2 = imboy_hashids:decode(Gid),
    Body = maps:get(<<"body">>, PostVals, ""),
    Status = maps:get(<<"status">>, PostVals, 0),
    ExpiredAt = maps:get(<<"expired_at">>, PostVals, <<>>),
    ExpiredAt2 = imboy_dt:rfc3339_to(ExpiredAt, millisecond),
    Now = imboy_dt:now(),
    % ?DEBUG_LOG([ExpiredAt, ExpiredAt2]),
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ when is_integer(ExpiredAt2) == false ->
            imboy_response:error(Req0,
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
                imboy_pg_sql:parse_result(
                    imboy_pg:insert(Tb, Data, <<"RETURNING id">>)),

            imboy_response:success(Req0, [{<<"notice_id">>, Id}], "success.")
    end.

edit(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Id = maps:get(<<"notice_id">>, PostVals, 0),
    Gid = maps:get(<<"gid">>, PostVals, ""),
    Gid2 = imboy_hashids:decode(Gid),

    % 状态 0 待发布  1 已发布 2 取消发布
    Status = maps:get(<<"status">>, PostVals, 0),
    Body = maps:get(<<"body">>, PostVals, ""),
    ExpiredAt = maps:get(<<"expired_at">>, PostVals, <<>>),
    Now = imboy_dt:now(),

    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ ->
            Data =
                #{edit_user_id => Uid,
                  body => Body,
                  status => Status,
                  expired_at => imboy_dt:rfc3339_to(ExpiredAt),
                  updated_at => Now},
            Tb = group_notice_repo:tablename(),
            % 使用安全的参数化查询，避免SQL注入
            Where = <<"id = $1 AND group_id = $2">>,
            case imboy_pg:update(Tb, Data, Where, [Id, Gid2]) of
                {ok, 1} ->
                    imboy_response:success(Req0, [{<<"notice_id">>, Id}], "success.");
                {ok, _} ->
                    imboy_response:error(Req0, "公告不存在");
                {error, Reason} ->
                    imboy_response:error(Req0, Reason)
            end
    end.

publish(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Id = maps:get(<<"notice_id">>, PostVals, 0),
    Gid = maps:get(<<"gid">>, PostVals, ""),
    Gid2 = imboy_hashids:decode(Gid),

    Now = imboy_dt:now(),
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ ->
            Data =
                #{edit_user_id => Uid,
                  status => 1,
                  updated_at => Now},
            Tb = group_notice_repo:tablename(),
            % 使用安全的参数化查询，避免SQL注入
            Where = <<"id = $1 AND group_id = $2">>,
            case imboy_pg:update(Tb, Data, Where, [Id, Gid2]) of
                {ok, 1} ->
                    imboy_response:success(Req0, [{<<"notice_id">>, Id}], "success.");
                {ok, _} ->
                    imboy_response:error(Req0, "公告不存在");
                {error, Reason} ->
                    imboy_response:error(Req0, Reason)
            end
    end.

delete(<<"DELETE">>, Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Id = maps:get(<<"notice_id">>, PostVals, 0),
    Gid = maps:get(<<"gid">>, PostVals, ""),
    Gid2 = imboy_hashids:decode(Gid),

    Tb = group_notice_repo:tablename(),
    % 使用安全的参数化查询，避免SQL注入
    Where = <<"id = $1 AND group_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", Where/binary>>,
    % ?DEBUG_LOG([Sql]),
    _ = imboy_pg:execute(Sql, [Id, Gid2]),
    imboy_response:success(Req0).

page(<<"GET">>, Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    #{gid := Gid} = cowboy_req:match_qs([{gid, [], undefined}], Req0),
    Gid2 = imboy_hashids:decode(Gid),
    GM = group_member_repo:find(Gid2, CurrentUid, <<"id">>),
    GMSize = maps:size(GM),
    case Gid2 of
        0 ->
            imboy_response:error(Req0, "group id 必须");
        _ when GMSize == 0 ->
            imboy_response:error(Req0, "你不是群成员");
        _ ->
            {Page, Size} = imboy_param:page(Req0),
            Column =
                <<"id as notice_id, user_id, edit_user_id, body, status, expired_at, "
                  "updated_at, created_at">>,
            % 使用安全的参数化查询，避免SQL注入
            Where = #{group_id => Gid2},
            Tb = group_notice_repo:tablename(),
            {ok, Payload} =
                imboy_pg:page_with_total(Tb, Column, Where, <<"expired_at desc">>, Page, Size),
            % 处理用户ID哈希
            List = maps:get(list, Payload, []),
            List2 =
                [imboy_hashids:replace_id(
                     imboy_hashids:replace_id(Item, <<"user_id">>), <<"edit_user_id">>)
                 || Item <- List],
            Payload2 = Payload#{list => List2},
            imboy_response:success(Req0, Payload2)
    end.

latest(<<"GET">>, Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    #{gid := Gid} = cowboy_req:match_qs([{gid, [], undefined}], Req0),
    Gid2 = imboy_hashids:decode(Gid),
    GM = group_member_repo:find(Gid2, CurrentUid, <<"id">>),
    GMSize = maps:size(GM),
    case Gid2 of
        0 ->
            imboy_response:error(Req0, "group id 必须");
        _ when GMSize == 0 ->
            imboy_response:error(Req0, "你不是群成员");
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
            {ok, Payload} = imboy_pg:query(Sql, [Gid2]),
            % 处理用户ID哈希
            Payload2 =
                case Payload of
                    [Item] ->
                        [imboy_hashids:replace_id(
                             imboy_hashids:replace_id(Item, <<"user_id">>), <<"edit_user_id">>)];
                    _ ->
                        Payload
                end,
            imboy_response:success(Req0, Payload2)
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).

%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
