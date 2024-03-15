-module(group_logic).
%%%
% group 业务逻辑模块
%%%
-export([face2face/4]).
-export([add/3]).
-export([dissolve/4]).

-include_lib("imlib/include/log.hrl").


face2face(_, <<>>, _, _) ->
    {error, <<"Code 必须">>};
face2face(_, _, undefined, _) ->
    {error, <<"longitude 必须">>};
face2face(_, _, _, undefined) ->
    {error, <<"latitude 必须">>};
face2face(Uid, Code, Lng, Lat) ->
    Now = imboy_dt:utc(millisecond),
    case nearby_gid(Lng, Lat, <<"50">>, <<"m">>, <<"1">>, Code) of
        {ok, _, []} ->
            imboy_db:with_transaction(fun(Conn) ->
                Gid = group_ds:gid(),
                % EPSG:4326 就是 WGS84 的代码。GPS 是基于 WGS84 的，所以通常我们得到的坐标数据都是 WGS84 的
                Location = <<"ST_GeomFromText('POINT(", Lng/binary, " ", Lat/binary, ")', 4326)">>,
                group_random_code_repo:add(Conn, #{
                    group_id => Gid,
                    user_id => Uid,
                    code => Code,
                    location => {raw, Location},
                    validity_at => Now + 7200_000,
                    created_at => Now
                }),
                group_ds:join(Uid, Gid),
                {ok, Gid}
            end);
        % {ok, _, [{Id, Gid, Location, Distance}]}
        {ok, _, [{_Id, Gid, _, _}]} ->
            group_ds:join(Uid, Gid),
            {ok, Gid};
        _ ->
            {error, "error"}
    end.

add(Count, _, _) when Count > 100 ->
    {error, "每人最多创建100个群"};
add(_, Uid, Type) ->
    Now = imboy_dt:utc(millisecond),
    imboy_db:with_transaction(fun(Conn) ->
        {ok, _,[{Gid}]} = group_repo:add(Conn, #{
            type => Type, % 类型: 1 公开群组  2 私有群组
            join_limit => 1, % 加入限制: 1 不需审核  2 需要审核  3 只允许邀请加入
            owner_uid => Uid,
            creater_uid => Uid,
            created_at => Now
        }),
        group_member_repo:add(Conn, #{
            group_id => Gid,
            user_id => Uid,
            role => 4, % 角色: 1 成员  2 嘉宾  3  管理员 4 群主
            created_at => Now
        }),
        group_ds:join(Uid, Gid),
        {ok, Gid}
    end).

dissolve(Uid, _, OwnerUid, _) when Uid =/= OwnerUid ->
    {error, "只有拥有者才能够解散该群，或者群已解散"};
dissolve(Uid, Gid, _, G) ->
    % 解散群聊后，群成员和群主都将被移除群聊。
    Now = imboy_dt:utc(millisecond),
    {ok, Body} = jsone_encode:encode(G, [native_utf8]),


    ToUidLi = group_ds:member_uids(Gid),

    imboy_db:with_transaction(fun(Conn) ->
        group_log_repo:add(Conn, #{
            % 日志类型: 100 群转让 101 群解散  200 主动退出群   201 群解散退出群  202 被踢出群
            type => 101,
            option_uid => Uid,
            group_id => Gid,
            body => Body,
            created_at => Now
        }),
        Tb = group_repo:tablename(),
        Sql = <<"DELETE FROM ", Tb/binary, " WHERE id= ", (ec_cnv:to_binary(Gid))/binary>>,
        imboy_db:execute(Conn, Sql, []),

        % TODO 群成员数量巨大的情况下的异步解散功能 2024-02-11
        case group_member_repo:list_by_gid(Gid, <<"*">>, 1_000_000) of
            {ok, _, []} ->
                [];
            {ok, ColumnLi, Li} ->
                Li2 = [ jsone_encode:encode(
                    lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnLi, tuple_to_list(V))
                    , [native_utf8]) || V <- Li],
                [group_log_repo:add(Conn, #{
                    type => 201,
                    option_uid => Uid,
                    group_id => Gid,
                    body => B2,
                    created_at => Now
                    })  || {ok, B2} <- Li2]
        end,
        Tb2 = group_member_repo:tablename(),
        Sql2 = <<"DELETE FROM ", Tb2/binary, " WHERE group_id= ", (ec_cnv:to_binary(Gid))/binary>>,
        imboy_db:execute(Conn, Sql2, []),
        group_ds:dissolve(Gid),
        % 群聊解散成功之后发送消息通知各成员客户端做相关逻辑处理
        MsgType = <<"group_dissolve">>,
        msg_s2c_ds:send(Uid, MsgType, ToUidLi, save),
        ok
    end),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


-spec nearby_gid(binary(), binary(), binary(), binary(), binary(), binary()) ->
          list().

nearby_gid(Lng, Lat, Radius, _Unit, Limit, Code) ->
    Now2 = ec_cnv:to_binary(imboy_dt:utc(millisecond)),
    Sql = <<"select
    id, group_id
    , ST_AsText(location) as location
    , ST_Distance(ST_GeographyFromText('SRID=4326;POINT(", Lng/binary, " ", Lat/binary, ")'), location) as distance
    from public.group_random_code where code = '", Code/binary, "' AND validity_at > ", Now2/binary," AND ST_DWithin(location::geography, ST_GeographyFromText('POINT(",
            Lng/binary, " ", Lat/binary, ")'), ", Radius/binary, ") order by distance asc limit ", Limit/binary, ";">>,
    ?LOG(Sql),
    imboy_db:query(Sql).


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
