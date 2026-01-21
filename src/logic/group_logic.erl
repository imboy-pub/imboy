-module(group_logic).

%%%
% group 业务逻辑模块
%%%
%% @doc 群组业务逻辑模块
% 提供群组创建、解散、附近群组等功能

-export([group_transfer/1]).
-export([face2face/4]).
-export([face2face_save/3]).
-export([add/4]).
-export([dissolve/4]).
-export([nearby_gid/6]).

-include("log.hrl").

%% @doc 转换群组数据中的 ID 字段为 HashID 格式
%% @param G 群组数据映射
%% @return map() 转换后的群组数据
-spec group_transfer(map()) -> map().
group_transfer(G) ->
    elib_hashids:replace_fields(G, [<<"id">>, <<"creator_uid">>, <<"owner_uid">>, <<"gid">>]).

%% @doc 面对面建群
%% 通过随机码创建或加入附近的群组
%% @param Uid 用户ID
%% @param Code 随机码
%% @param Lng 经度
%% @param Lat 纬度
%% @return {ok, Gid} | {error, Reason}
-spec face2face(integer(), binary(), binary(), binary()) -> {ok, integer()} | {error, binary() | string()}.
face2face(_, <<>>, _, _) ->
    {error, <<"Code 必须"/utf8>>};
face2face(_, _, undefined, _) ->
    {error, <<"longitude 必须"/utf8>>};
face2face(_, _, _, undefined) ->
    {error, <<"latitude 必须"/utf8>>};
face2face(Uid, Code, Lng, Lat) ->
    LngFloat = ec_cnv:to_float(Lng),
    LatFloat = ec_cnv:to_float(Lat),
    case nearby_gid(LngFloat, LatFloat, <<"50">>, <<"m">>, <<"1">>, Code) of
        {ok, []} ->
            % 创建新的面对面建群
            elib_pg:with_tx(fun(Conn) ->
                group_ds:face2face_create(Conn, Uid, Code, LngFloat, LatFloat)
            end);
        {ok, [#{<<"group_id">> := Gid}]} ->
            JoinMode = <<"face2face_join">>,
            case group_member_logic:join_group(JoinMode, Uid, Gid, #{}) of
                ok -> {ok, Gid};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, "error"}
    end.

%% @doc 面对面建群保存
%% @param Code 随机码
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @return {ok, binary()}
-spec face2face_save(binary(), integer(), integer()) -> {ok, binary()}.
face2face_save(Code, Gid, Uid) ->
    group_ds:face2face_save(Code, Gid, Uid).

%% @doc 创建群组
%% 创建新群组并邀请成员加入
%% @param Count 已创建的群组数量
%% @param Uid 创建者用户ID
%% @param Type 群组类型（1 公开 2 私有）
%% @param MemberUids 初始成员ID列表（HashID格式）
%% @return {ok, Gid} | {error, Reason}
-spec add(non_neg_integer(), integer(), integer(), [binary()]) ->
             {ok, integer()} | {error, binary()}.
add(Count, _, _, _) when Count > 100 ->
    {error, <<"每人最多创建100个群"/utf8>>};
add(_, Uid, Type, MemberUids) ->
    MemberUids2 = case MemberUids of
        List when is_list(List) -> List;
        _ -> []
    end,
    Now = elib_dt:now(),
    MemberUids3 = [elib_hashids:decode(Id) || Id <- MemberUids2, is_binary(Id)],
    Sum = lists:sum(lists:usort([Uid | MemberUids3])),
    % 使用 DS 层接口检查是否已存在相同群组
    GidOld = group_ds:find_by_creator_and_sum(Uid, Sum),
    case GidOld of
        0 ->
            % invite_[uid]_[nickname]
            UserTitle = user_ds:title(Uid),
            JoinMode = <<"invite_", (ec_cnv:to_binary(Uid))/binary, "_", UserTitle/binary>>,
            elib_pg:with_tx(fun(Conn) ->
                Gid = group_ds:create_group(Conn, 0, Uid, Now, Type, 1),
                %% 【原子性修复】批量添加成员并检查结果
                Results = [group_member_logic:join_group(Conn, JoinMode, Uid2, Gid, #{})
                     || Uid2 <- MemberUids3, Uid2 /= Uid],
                %% 【原子性修复】检查是否所有成员都添加成功
                case lists:all(fun(R) -> R =:= ok end, Results) of
                    true -> {ok, Gid};
                    false -> throw({error, member_add_failed})
                end
            end);
        GidOld when GidOld > 0 ->
            {ok, GidOld}
    end.

%% @doc 解散群组
%% 解散指定群组，通知所有成员并清理相关数据
%% @param Uid 操作者用户ID
%% @param Gid 群组ID
%% @param OwnerUid 群主用户ID
%% @param G 群组信息
%% @return ok | {error, Reason}
-spec dissolve(integer(), integer(), integer(), map()) -> ok | {error, binary()}.
dissolve(Uid, Gid, OwnerUid, G) ->
    group_ds:dissolve_group(Uid, Gid, OwnerUid, G).

%% @doc 查询附近的群组
%% 基于地理位置查询附近的群组
%% @param Lng 经度
%% @param Lat 纬度
%% @param Radius 半径
%% @param Unit 单位（固定为米）
%% @param Limit 返回数量限制
%% @param Code 随机码
%% @return {ok, [map()]} | {error, Reason} 附近群组列表
-spec nearby_gid(binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number()) ->
                    {ok, [map()]} | {error, term()}.
nearby_gid(Lng, Lat, Radius, Unit, Limit, Code) ->
    % 使用 DS 层接口
    group_ds:nearby_gid(Lng, Lat, Radius, Unit, Limit, Code).

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
