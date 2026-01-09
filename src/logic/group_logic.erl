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

-include("log.hrl").

% group_logic:group_transfer(G)
group_transfer(G) ->
    imboy_hashids:replace_id(
        imboy_hashids:replace_id(
            imboy_hashids:replace_id(
            imboy_hashids:replace_id(G, <<"id">>), <<"creator_uid">>),
            <<"owner_uid">>),
        <<"gid">>).

face2face(_, <<>>, _, _) ->
    {error, <<"Code 必须"/utf8>>};
face2face(_, _, undefined, _) ->
    {error, <<"longitude 必须"/utf8>>};
face2face(_, _, _, undefined) ->
    {error, <<"latitude 必须"/utf8>>};
% group_logic:face2face(1, <<"1234">>, <<"113.88267100000002">>, <<"22.565967">>).
face2face(Uid, Code, Lng, Lat) ->
    Now = imboy_dt:now(),
    %% Postgres + epgsql 强类型：binary → float
    LngFloat = ec_cnv:to_float(Lng),
    LatFloat = ec_cnv:to_float(Lat),
    case nearby_gid(LngFloat, LatFloat, <<"50">>, <<"m">>, <<"1">>, Code) of
        {ok, []} ->
            _ = imboy_pg:with_tx(fun(Conn) ->
                                    Gid = group_ds:gid(),
                                    % EPSG:4326 就是 WGS84 的代码。GPS 是基于 WGS84 的，所以通常我们得到的坐标数据都是 WGS84 的
                                    % Location = <<"ST_GeomFromText('POINT(", Lng/binary, " ", Lat/binary, ")', 4326)">>,
                                    % 事务中需要使用Conn参数
                                    %% INSERT 参数化
                                    {Sql, Params} =
                                        imboy_pg_sql:insert_with_params(
                                            group_random_code_repo:tablename(),
                                            #{group_id => Gid,
                                              user_id => Uid,
                                              code => Code,
                                              location =>
                                                  {raw,
                                                   <<"ST_SetSRID(ST_MakePoint($1::float8,$2::float8),4326)">>},
                                              validity_at => imboy_dt:add(Now, {60, minute}),
                                              created_at => Now},
                                            <<>>,
                                            [LngFloat, LatFloat]),
                                    % ?DEBUG_LOG(Sql),
                                    {ok, _} = imboy_pg:execute(Conn, Sql, Params),
                                    group_ds:join(Uid, Gid),
                                    {ok, Gid}
                                 end);
        {ok, [#{<<"group_id">> := Gid}]} ->
            JoinMode = <<"face2face_join">>,
            _ = group_member_logic:join_group(JoinMode, Uid, Gid, #{}),
            {ok, Gid};
        _ ->
            {error, "error"}
    end.

% group_logic:face2face_save(<<"1234">>, 64, 1).
-spec face2face_save(binary(), integer(), integer()) -> {ok, binary()}.
face2face_save(Code, Gid, Uid) ->
    _ = imboy_pg:with_tx(fun(Conn) ->
                            %% 1. 读取随机码记录
                            RowCode =
                                case group_random_code_repo:find_by_gid(Gid, <<"code,user_id">>) of
                                    #{<<"code">> := RC} -> RC;
                                    _ -> throw({abort_tx, <<"gid not exist">>})
                                end,

                            %% 2. 校验 code
                            case RowCode =:= Code of
                                true -> ok;
                                false -> throw({abort_tx, <<"code error">>})
                            end,

                            %% 3. 群不存在就创建
                            _ = case group_repo:find_by_id(Gid, <<"id">>) of
                                    #{<<"id">> := _} -> ok;
                                    {error, _} ->
                                        Now = imboy_dt:now(),
                                        create_group(Conn, Gid, Uid, Now, 2, 1)
                                end,

                            %% 4. 不是群成员则加入
                            _ = case group_member_repo:find(Gid, Uid, <<"id">>) of
                                    #{<<"id">> := _} -> ok;
                                    _ ->
                                        group_member_logic:join_group(Conn,
                                                                      <<"face2face_join">>,
                                                                      Uid,
                                                                      Gid,
                                                                      #{})
                                end,
                            {ok, <<"success">>}
                         end),
    {ok, <<"success">>}.

-spec add(non_neg_integer(), integer(), integer(), [binary()]) ->
             {ok, integer() | binary()} | {error, binary()}.
add(Count, _, _, _) when Count > 100 ->
    {error, "每人最多创建100个群"};
add(_, Uid, Type, MemberUids) ->
    % 确保 MemberUids 是一个列表
    MemberUids2 = case MemberUids of
        List when is_list(List) -> List;
        _ -> []
    end,
    Now = imboy_dt:now(),
    MemberUids3 = [imboy_hashids:decode(Id) || Id <- MemberUids2, is_binary(Id)],
    Sum = lists:sum(
              lists:usort([Uid | MemberUids3])),
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"SELECT id FROM ",
            (group_repo:tablename())/binary,
            " WHERE creator_uid = $1 AND user_id_sum = $2">>,
    GidOld =
        case imboy_pg:query(Sql, [Uid, Sum]) of
            {ok, [#{<<"id">> := Gid0}]} ->
                Gid0;
            _ ->
                0
        end,
    case GidOld of
        0 ->
            % invite_[uid]_[nickname]
            UserTitle = user_ds:title(Uid),
            JoinMode = <<"invite_", (ec_cnv:to_binary(Uid))/binary, "_", UserTitle/binary>>,
            imboy_pg:with_tx(fun(Conn) ->
                                Gid = create_group(Conn, 0, Uid, Now, Type, 1),
                                group_ds:join(Uid, Gid),
                                _ = [group_member_logic:join_group(Conn, JoinMode, Uid2, Gid, #{})
                                     || Uid2 <- MemberUids3, Uid2 /= Uid],
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
    _ = imboy_pg:with_tx(fun(Conn) ->
                            _ = group_log_repo:add(Conn,
                                                   #{% 日志类型: 100 群转让 101 群解散  200 主动退出群   201 群解散退出群  202 被踢出群
                                                     type => 101,
                                                     option_uid => Uid,
                                                     group_id => Gid,
                                                     body => Body,
                                                     created_at => Now}),
                            % 使用安全的参数化查询，避免SQL注入，事务中使用Conn
                            Tb = group_repo:tablename(),
                            Sql = <<"DELETE FROM ", Tb/binary, " WHERE id = $1">>,
                            {ok, _} = imboy_pg:execute(Conn, Sql, [Gid]),

                            % TODO 群成员数量巨大的情况下的异步解散功能 2024-02-11
                            _ = case group_member_repo:list_by_gid(Gid, <<"*">>, 1_000_000) of
                                    {ok, []} -> [];
                                    {ok, Li} ->
                                        Li2 = [jsone_encode:encode(V, [native_utf8]) || V <- Li],
                                        [group_log_repo:add(Conn,
                                                            #{type => 201,
                                                              option_uid => Uid,
                                                              group_id => Gid,
                                                              body => B2,
                                                              created_at => Now})
                                         || {ok, B2} <- Li2]
                                end,
                            % 使用安全的参数化查询，避免SQL注入，事务中使用Conn
                            Tb2 = group_member_repo:tablename(),
                            Sql2 = <<"DELETE FROM ", Tb2/binary, " WHERE group_id = $1">>,
                            {ok, _} = imboy_pg:execute(Conn, Sql2, [Gid]),
                            group_ds:dissolve(Gid),
                            % 群聊解散成功之后发送消息通知各成员客户端做相关逻辑处理
                            Payload =
                                #{<<"gid">> => imboy_hashids:encode(Gid),
                                  <<"msg_type">> => <<"group_dissolve">>},
                            msg_s2c_ds:send(Uid, Payload, ToUidLi, save),
                            ok
                         end),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

% 事务版本
-spec create_group(epgsql:connection() | pid(),
                   integer() | binary(),
                   integer(),
                   binary() | integer(),
                   integer(),
                   integer()) ->
                      integer().
create_group(Conn, Gid, Uid, Now, Type, JoinLimit) ->
    GMap =
        #{type => Type, % 类型: 1 公开群组  2 私有群组
          join_limit => JoinLimit, % 加入限制: 1 不需审核  2 需要审核  3 只允许邀请加入
          user_id_sum => Uid, % 这里用Uid，其他的UID在 group_member_logic:join_group 里面累计
          owner_uid => Uid,
          creator_uid => Uid,
          created_at => Now},
    GMap2 =
        if Gid > 0 ->
               GMap#{id => Gid};
           true ->
               GMap
        end,
    % ?DEBUG_LOG(["group_logic/create_group", Gid, GMap2]),
    % 使用 imboy_pg:insert 的 RETURNING 功能获取插入的Gid
    Gid2 =
        case imboy_pg_sql:parse_result(
                 imboy_pg:insert(Conn, group_repo:tablename(), GMap2, <<"RETURNING id">>))
        of
            {ok, Id, _} ->
                Id;
            _ ->
                Gid
        end,
    % 检查群成员是否已存在，不存在则插入
    _ = case group_member_repo:find(Gid2, Uid, <<"id">>) of
            GM when map_size(GM) == 0 ->
                % 事务中插入群成员
                imboy_pg:insert(Conn,
                                group_member_repo:tablename(),
                                #{group_id => Gid2,
                                  user_id => Uid,
                                  role => 4, % 角色: 1 成员  2 嘉宾  3  管理员 4 群主
                                  created_at => Now},
                                <<>>);
            _ ->
                ok
        end,
    Gid2.

-spec nearby_gid(binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number()) ->
                    {ok, list()}.
% group_logic:nearby_gid(<<"113.88267100000002">>, <<"22.565967">>, <<"3333333">>, "m", <<"1234">>, <<"1234">>).
nearby_gid(Lng, Lat, Radius, _Unit, Limit, Code) ->
    Now = imboy_dt:now(),
    % Sql = <<"select
    % id, group_id
    % , ST_AsText(location) as location
    % , ST_Distance(ST_GeographyFromText('SRID=4326;POINT(", Lng/binary, " ", Lat/binary, ")'), location) as distance
    % from public.group_random_code where code = '", Code/binary, "' AND validity_at > '", Now/binary,"' AND ST_DWithin(location::geography, ST_GeographyFromText('POINT(",
    %         Lng/binary, " ", Lat/binary, ")'), ", Radius/binary, ") order by distance asc limit ", Limit/binary, ";">>,
    % ?DEBUG_LOG(Sql),
    % imboy_pg:query(Sql, []).
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"select\n        id,\n        group_id,\n        ST_AsText(location) "
            "as location,\n        ST_Distance(\n            ST_SetSRID(ST_MakePo"
            "int($1::float8, $2::float8), 4326)::geography,\n           "
            " location\n        ) as distance\n    from public.group_random_code\n "
            "   where code = $3\n      and validity_at > $4::timestamptz\n "
            "     and ST_DWithin(\n            location::geography,\n   "
            "         ST_SetSRID(ST_MakePoint($1::float8, $2::float8), 4326)::geo"
            "graphy,\n            $5::int\n      )\n    order by distance "
            "asc\n    limit $6::int;">>,
    LngFloat = ec_cnv:to_float(Lng),
    LatFloat = ec_cnv:to_float(Lat),
    RadiusInt = ec_cnv:to_integer(Radius),
    LimitInt = ec_cnv:to_integer(Limit),
    imboy_pg:query(Sql, [LngFloat, LatFloat, Code, Now, RadiusInt, LimitInt]).

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
