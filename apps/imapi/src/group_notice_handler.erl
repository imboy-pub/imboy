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
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
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
    PostVals = imboy_req:post_params(Req0),
    Gid = proplists:get_value(<<"gid">>, PostVals, ""),
    Gid2 = imboy_hashids:decode(Gid),
    Body = proplists:get_value(<<"body">>, PostVals, ""),
    Status = proplists:get_value(<<"status">>, PostVals, 0),
    ExpiredAt = proplists:get_value(<<"expired_at">>, PostVals, <<>>),
    ExpiredAt2 = imboy_dt:rfc3339_to(ExpiredAt, millisecond),
    Now = imboy_dt:now(),
    % ?DEBUG_LOG([ExpiredAt, ExpiredAt2]),
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ when is_integer(ExpiredAt2) == false ->
            imboy_response:error(Req0, "expired_at 格式有误，应当符合rfc3339规范，正确格式为： 2024-02-14 11:16:37.129353+08:00");
        _ ->
            Data = #{
                group_id => Gid2,
                user_id => Uid,
                body => Body,
                status => Status,
                expired_at => ExpiredAt,
                created_at => Now
            },
            Tb = group_notice_repo:tablename(),
            {ok, _,[{Id}]} = imboy_db:insert_into(Tb, Data),

            imboy_response:success(Req0, [
                {<<"notice_id">>, Id}
            ], "success.")
    end.


edit(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Id = proplists:get_value(<<"notice_id">>, PostVals, 0),
    Gid = proplists:get_value(<<"gid">>, PostVals, ""),
    Gid2 = imboy_hashids:decode(Gid),

    % 状态 0 待发布  1 已发布 2 取消发布
    Status = proplists:get_value(<<"status">>, PostVals, 0),
    Body = proplists:get_value(<<"body">>, PostVals, ""),
    ExpiredAt = proplists:get_value(<<"expired_at">>, PostVals, <<>>),
    ExpiredAt2 = imboy_dt:rfc3339_to(ExpiredAt, millisecond),
    Now = imboy_dt:now(),

    % ?DEBUG_LOG([ExpiredAt, ExpiredAt2]),
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ when is_integer(ExpiredAt2) == false ->
            imboy_response:error(Req0, "expired_at 格式有误，应当符合rfc3339规范，正确格式为： 2024-02-14 11:16:37.129353+08:00");
        _ ->
            Data = #{
                edit_user_id => Uid,
                body => Body,
                status => Status,
                expired_at => ExpiredAt2,
                updated_at => Now
            },
            Tb = group_notice_repo:tablename(),
            Where = <<"id=", (ec_cnv:to_binary(Id))/binary, " AND group_id=", (ec_cnv:to_binary(Gid2))/binary>>,
            % {ok, _,[{Id}]} = imboy_db:update(Tb, Where, Data),
            {ok,1} = imboy_db:update(Tb, Where, Data),
            imboy_response:success(Req0, [
                {<<"notice_id">>, Id}
            ], "success.")
    end.

publish(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Id = proplists:get_value(<<"notice_id">>, PostVals, 0),
    Gid = proplists:get_value(<<"gid">>, PostVals, ""),
    Gid2 = imboy_hashids:decode(Gid),

    Now = imboy_dt:now(),

    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ ->
            Data = #{
                edit_user_id => Uid,
                status => 1,
                updated_at => Now
            },
            Tb = group_notice_repo:tablename(),
            Where = <<"id=", (ec_cnv:to_binary(Id))/binary, " AND group_id=", (ec_cnv:to_binary(Gid2))/binary>>,
            {ok,1} = imboy_db:update(Tb, Where, Data),
            imboy_response:success(Req0, [
                {<<"notice_id">>, Id}
            ], "success.")
    end.

delete(<<"DELETE">>, Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Id = proplists:get_value(<<"notice_id">>, PostVals, 0),
    Gid = proplists:get_value(<<"gid">>, PostVals, ""),
    Gid2 = imboy_hashids:decode(Gid),

    Tb = group_notice_repo:tablename(),
    Where = <<"id=", (ec_cnv:to_binary(Id))/binary, " AND group_id=", (ec_cnv:to_binary(Gid2))/binary>>,
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", Where/binary>>,
    % ?DEBUG_LOG([Sql]),
    imboy_db:execute(Sql, []),
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
            {Page, Size} = imboy_req:page_size(Req0),

            Column = <<"id notice_id, user_id, edit_user_id, body, status, expired_at, updated_at, created_at">>,
            Where = <<"group_id =", (ec_cnv:to_binary(Gid2))/binary>>,
            OrderBy = <<"expired_at desc">>,
            Tb = group_notice_repo:tablename(),
            Payload = imboy_db:page(Page, Size, Tb, Where, OrderBy, Column),
            imboy_response:success(Req0, Payload)
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
            Column = <<"id notice_id, user_id, edit_user_id, body, status, expired_at, updated_at, created_at">>,
            Where = <<"status = 1 AND group_id = ", (ec_cnv:to_binary(Gid2))/binary>>,
            OrderBy = <<"id desc">>,
            Tb = group_notice_repo:tablename(),
            Payload = imboy_db:find(Tb, Where, OrderBy, Column),
            imboy_response:success(Req0, Payload)
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
