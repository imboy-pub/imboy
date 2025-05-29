-module(group_logic).
%%%
% group 业务逻辑模块
%%%
-export([group_transfer/1]).
-export([face2face/4]).
-export([face2face_save/3]).
-export([add/4]).
-export([dissolve/4]).
-export([nearby_gid/6]).

-include_lib("imlib/include/log.hrl").

% group_logic:group_transfer(G)
group_transfer(G) ->
    imboy_hashids:replace_id(
        imboy_hashids:replace_id(
            imboy_hashids:replace_id(
                    imboy_hashids:replace_id(G, <<"id">>)
                , <<"creator_uid">>)
        , <<"owner_uid">>)
    , <<"gid">>).

face2face(_, <<>>, _, _) ->
    {error, <<"Code 必须">>};
face2face(_, _, undefined, _) ->
    {error, <<"longitude 必须">>};
face2face(_, _, _, undefined) ->
    {error, <<"latitude 必须">>};
face2face(Uid, Code, Lng, Lat) ->
    Now = imboy_dt:now(),
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
                    validity_at => imboy_dt:add(Now, {60, minute}),
                    created_at => Now
                }),
                group_ds:join(Uid, Gid),
                {ok, Gid}
            end);
        % {ok, _, [{Id, Gid, Location, Distance}]}
        {ok, _, [{_Id, Gid, _, _}]} ->
            GM = group_member_repo:find(Gid, Uid, <<"id">>),
            GMSize = maps:size(GM),
            if
                GMSize > 0 ->
                    ok;
                true ->
                    JoinMode = <<"face2face_join">>,
                    group_member_logic:join(JoinMode, Uid, Gid, 1, 0)
            end,
            {ok, Gid};
        _ ->
            {error, "error"}
    end.

face2face_save(Code, Gid, Uid) ->
    Row = group_random_code_repo:find_by_gid(Gid, <<"code,user_id">>),
    RowCode = maps:get(<<"code">>, Row, <<>>),
    % CreateUserId = maps:get(<<"user_id">>, Row, 0),
    G = group_repo:find_by_id(Gid, <<"id">>),
    GSize = maps:size(G),
    if
        GSize == 0 ->
            Now = imboy_dt:now(),
            imboy_db:with_transaction(fun(Conn) ->
                create_group(Conn, Gid, Uid, Now, 2, 1)
            end);
        true ->
            ok
    end,
    GM = group_member_repo:find(Gid, Uid, <<"id">>),
    GMSize = maps:size(GM),
    ?DEBUG_LOG(["group_logic/face2face_save", Code, Gid, Uid, G, GM]),
    case {GMSize, RowCode} of
        {_, <<>>} ->
            {error, <<"群ID不存在"/utf8>>};
        {0, Code}->
            group_member_logic:join(<<"face2face_join">>, Uid, Gid, 1, 0),
            {ok, <<"success">>};
        {_, Code}-> % 重复提交的时候
            {ok, <<"success">>};
        _ ->
            {error, <<"验证码有误"/utf8>>}
    end.

add(Count, _, _, _) when Count > 100 ->
    {error, "每人最多创建100个群"};
add(_, Uid, Type, MemberUids) ->
    Now = imboy_dt:now(),
    MemberUids2 = [imboy_hashids:decode(Id) || Id <- MemberUids],
    Sum = lists:sum(lists:usort([Uid|MemberUids2])),
    GidOld = imboy_db:pluck(group_repo:tablename(),
       <<"creator_uid = ", (ec_cnv:to_binary(Uid))/binary," and user_id_sum = ", (ec_cnv:to_binary(Sum))/binary>>,
       <<"id">>,
       0),
    case GidOld of
        0 ->
            % invite_[uid]_[nickname]
            UserTitle = user_ds:title(Uid),
            JoinMode = <<"invite_",  (ec_cnv:to_binary(Uid))/binary, "_", UserTitle/binary>>,
            imboy_db:with_transaction(fun(Conn) ->
                Gid = create_group(Conn, 0, Uid, Now, Type, 1),
                group_ds:join(Uid, Gid),
                [group_member_logic:join(Conn, JoinMode, Uid2, Gid) || Uid2 <- MemberUids2, Uid2 /= Uid],
                {ok, Gid}
            end);
        GidOld when GidOld > 0 ->
            {ok, GidOld}
    end.

dissolve(Uid, _, OwnerUid, _) when Uid =/= OwnerUid ->
    {error, "只有拥有者才能够解散该群，或者群已解散"};
dissolve(Uid, Gid, _, G) ->
    % 解散群聊后，群成员和群主都将被移除群聊。
    Now = imboy_dt:now(),
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
        Payload = #{
            <<"gid">> => imboy_hashids:encode(Gid),
            <<"msg_type">> => <<"group_dissolve">>
        },
        msg_s2c_ds:send(Uid, Payload, ToUidLi, save),
        ok
    end),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

create_group(Conn, Gid, Uid, Now, Type, JoinLimit) ->
    GMap =  #{
        type => Type, % 类型: 1 公开群组  2 私有群组
        join_limit => JoinLimit, % 加入限制: 1 不需审核  2 需要审核  3 只允许邀请加入
        user_id_sum => Uid, % 这里用Uid，其他的UID在 group_member_logic:join 里面累计
        owner_uid => Uid,
        creator_uid => Uid,
        created_at => Now
    },
    GMap2 = if
        Gid > 0 ->
            GMap#{id => Gid};
        true ->
            GMap
    end,
    ?DEBUG_LOG(["group_logic/create_group", Gid, GMap2]),
    {ok, _,[{Gid2}]} = group_repo:add(Conn, GMap2),
    GM = group_member_repo:find(Gid2, Uid, <<"id">>),
    GMSize = maps:size(GM),
    if
        GMSize == 0 ->
            group_member_repo:add(Conn, #{
                group_id => Gid2,
                user_id => Uid,
                role => 4, % 角色: 1 成员  2 嘉宾  3  管理员 4 群主
                created_at => Now
            });
        true ->
            ok
    end,
    Gid2.

-spec nearby_gid(binary(), binary(), binary(), binary(), binary(), binary()) ->
          list().
% group_logic:nearby_gid(<<"1234">>, <<"1234">>, <<"3333333">>, "m", <<"1234">>, <<"1234">>).
nearby_gid(Lng, Lat, Radius, _Unit, Limit, Code) ->
    Now = imboy_dt:now(),
    Sql = <<"select
    id, group_id
    , ST_AsText(location) as location
    , ST_Distance(ST_GeographyFromText('SRID=4326;POINT(", Lng/binary, " ", Lat/binary, ")'), location) as distance
    from public.group_random_code where code = '", Code/binary, "' AND validity_at > '", Now/binary,"' AND ST_DWithin(location::geography, ST_GeographyFromText('POINT(",
            Lng/binary, " ", Lat/binary, ")'), ", Radius/binary, ") order by distance asc limit ", Limit/binary, ";">>,
    ?DEBUG_LOG(Sql),
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
