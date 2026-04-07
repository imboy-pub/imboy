-module(group_ds).
%%%
% group_ds 是 group domain service 缩写
%%%
% -export ([find_by_id/2]).
-export([check_avatar/1]).
-export([gid/0]).
-export([create_group/6]).
-export([nearby_gid/6]).
-export([face2face_create/5]).
-export([face2face_save/3]).
-export([dissolve_group/4]).
-export([find_by_creator_and_sum/2]).


-export([member_uids/1]).
-export([is_member/2]).
-export([join/2]).
-export([leave/2]).
-export([dissolve/1]).

-include("cache.hrl").
-include("log.hrl").

-define(GROUP_CACHE_KEY(Gid), {group, Gid}).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 检查用户是否为群组成员
%%
%% 检查指定用户是否为指定群组的成员
%%
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @returns boolean() true表示是成员，false表示不是成员
% group_ds:is_member(62913, 11)
-spec is_member(integer(), integer()) -> boolean().
is_member(Uid, Gid) ->
    Res = group_member_repo:find(Gid, Uid, <<"id">>),
    % ?DEBUG_LOG(io:format("is_member/2  Uid ~p, Gid ~p, Res ~p, Size ~p\n", [Uid, Gid, Res, map_size(Res)])),
    case map_size(Res) of
        0 ->
            false;
        _ ->
            true
    end.

%% @doc 获取群组成员用户ID列表
%%
%% 获取指定群组所有成员的用户ID列表，使用缓存提高性能
%%
%% @param Gid 群组ID
%% @returns list() 成员用户ID列表
% group_ds:member_uids(1).
-spec member_uids(integer()) -> [integer()].
member_uids(Gid) ->
    CacheKey = ?GROUP_CACHE_KEY(Gid),
    case imboy_cache:get(CacheKey) of
        undefined ->
            case group_member_repo:list_by_gid(Gid, <<"user_id">>) of
                {ok, []} ->
                    [];
                {ok, Items} ->
                    Li = [Uid || #{<<"user_id">> := Uid} <- Items],
                    imboy_cache:set(CacheKey, Li, ?HOUR),
                    Li;
                _ ->
                    []
            end;
        {ok, Li} ->
            Li
    end.

%% @doc 用户加入群组
%%
%% 将用户添加到群组成员缓存中，如果用户已存在则不做任何操作
%%
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @returns ok 表示操作成功
% group_ds:join(1,1), group_ds:join(2,1), group_ds:join(3,1), group_ds:join(4,1).
-spec join(integer(), integer()) -> ok.
join(Uid, Gid) ->
    CacheKey = ?GROUP_CACHE_KEY(Gid),
    case member_uids(Gid) of
        [] ->
            imboy_cache:set(CacheKey, [Uid], ?HOUR);
        Li ->
            case lists:member(Uid, Li) of
                true ->
                    ok;
                false ->
                    imboy_cache:set(CacheKey, [Uid | Li], ?HOUR)
            end
    end.

%% @doc 用户离开群组
%%
%% 从群组成员缓存中移除指定用户
%%
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @returns ok 表示操作成功
% group_ds:leave(1,1).
-spec leave(integer(), integer()) -> ok.
leave(Uid, Gid) ->
    CacheKey = ?GROUP_CACHE_KEY(Gid),
    case member_uids(Gid) of
        [] ->
            ok;
        Li ->
            imboy_cache:set(CacheKey, lists:delete(Uid, Li))
    end.

%% @doc 解散群组
%%
%% 清除群组相关的缓存数据
%%
%% @param Gid 群组ID
%% @returns ok 表示操作成功
% group_ds:dissolve(Gid).
-spec dissolve(integer()) -> ok.
dissolve(Gid) ->
    CacheKey = ?GROUP_CACHE_KEY(Gid),
    imboy_cache:flush(CacheKey).

% group_ds:member_uids(1).
% -spec member_uids(integer()) -> list().
% member_uids(Gid) ->
%     Key = "/:gorup_member/" ++ integer_to_list(Gid),
%     case khepri:get(Key) of
%         {error,{khepri,node_not_found, _}} ->
%             [];
%         {ok, Val} ->
%             Val
%     end.

% group_ds:join(1,1), group_ds:join(2,1), group_ds:join(3,1), group_ds:join(4,1).
% join(Uid, Gid) ->
%     Key = "/:gorup_member/" ++ integer_to_list(Gid),
%     case khepri:exists(Key) of
%         false ->
%             khepri:put(Key, [Uid]);
%         true ->
%             leave(Uid, Gid),
%             {ok, Li} = khepri:get(Key),
%             khepri:put(Key, [Uid | Li])
%     end.

% group_ds:leave(1,1).
% leave(Uid, Gid) ->
%     Key = "/:gorup_member/" ++ integer_to_list(Gid),
%     case khepri:exists(Key) of
%         false ->
%             ok;
%         true ->
%             {ok, Li} = khepri:get(Key),
%             khepri:put(Key, lists:delete(Uid, Li))
%     end.

% group_ds:dissolve(Gid).
% dissolve(Gid) ->
%     Key = "/:gorup_member/" ++ integer_to_list(Gid),
%     khepri:delete(Key).

%% @doc 生成新的群组ID
%%
%% 获取一个新的群组ID，显式使用 public schema 的序列，避免受 search_path 影响
%%
%% @returns integer() 新的群组ID
% group_ds:gid().
-spec gid() -> integer().
gid() ->
    {ok, [#{<<"gid">> := Gid}]} = elib_pg:query("select nextval('public.group_id_seq') as gid", []),
    Gid.

%% @doc 检查和设置群组头像
%%
%% 检查群组头像是否为空，如果为空则设置默认头像
%%
%% @param Group 群组信息列表
%% @returns list() 处理后的群组信息列表
-spec check_avatar(map()) -> map().
check_avatar(Group) when is_map(Group) ->
    Default = <<"/static/image/group_default_avatar.jpeg">>,
    Avatar = maps:get(<<"avatar">>, Group, <<>>),
    case Avatar of
        <<>> -> Group#{<<"avatar">> => Default};
        _ -> Group
    end;
check_avatar(_) ->
    #{}.

%% @doc 创建群组（事务版本）
%% @param Conn 数据库连接
%% @param Gid 群组ID（0表示自动生成）
%% @param Uid 创建者用户ID
%% @param Now 创建时间
%% @param Type 群组类型（1 公开 2 私有）
%% @param JoinLimit 加入限制
%% @return integer() 创建的群组ID
-spec create_group(pid(), integer() | binary(), integer(), binary(), integer(), integer()) -> integer().
create_group(Conn, Gid, Uid, Now, Type, JoinLimit) ->
    GMap =
        #{type => Type,
          join_limit => JoinLimit,
          user_id_sum => Uid,
          owner_uid => Uid,
          creator_uid => Uid,
          created_at => Now},
    GMap2 =
        if Gid > 0 ->
               GMap#{id => Gid};
           true ->
               GMap
        end,
    % 使用 elib_pg:insert 的 RETURNING 功能获取插入的Gid
    Gid2 =
        case elib_pg_sql:parse_result(
                 elib_pg:insert(Conn, group_repo:tablename(), GMap2, <<"RETURNING id">>))
        of
            {ok, Id, _} ->
                Id;
            _ ->
                Gid
        end,
    % 检查群成员是否已存在，不存在则插入
    case group_member_repo:find(Gid2, Uid, <<"id">>) of
        GM when map_size(GM) == 0 ->
            case elib_pg:insert(Conn,
                            group_member_repo:tablename(),
                            #{group_id => Gid2,
                              user_id => Uid,
                              role => 4, % 群主
                              created_at => Now},
                            <<>>) of
                {ok, _} -> ok;
                {error, Reason} -> ?ERROR_LOG([group_member_insert_failed, Gid2, Uid, Reason])
            end;
        _ ->
            ok
    end,
    Gid2.

%% @doc 查询附近的群组（基于地理位置）
%% @param Lng 经度
%% @param Lat 纬度
%% @param Radius 半径
%% @param _Unit 单位（固定为米）
%% @param Limit 返回数量限制
%% @param Code 随机码
%% @return {ok, list(map())} 附近群组列表
-spec nearby_gid(binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number(),
                 binary() | string() | number()) ->
                    {ok, list(map())} | {error, term()}.
nearby_gid(Lng, Lat, Radius, _Unit, Limit, Code) ->
    Now = elib_dt:now(),
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
    elib_pg:query(Sql, [LngFloat, LatFloat, Code, Now, RadiusInt, LimitInt]).

%% @doc 面对面建群 - 创建随机码（事务版本）
%% @param Conn 数据库连接
%% @param Uid 用户ID
%% @param Code 随机码
%% @param Lng 经度
%% @param Lat 纬度
%% @return {ok, Gid} | {error, Reason}
-spec face2face_create(pid(), integer(), binary(), float(), float()) -> {ok, integer()} | {error, binary() | atom()}.
face2face_create(Conn, Uid, Code, Lng, Lat) ->
    Now = elib_dt:now(),
    Gid = gid(),
    {Sql, Params} =
        elib_pg_sql:insert_with_params(
            group_random_code_repo:tablename(),
            #{group_id => Gid,
              user_id => Uid,
              code => Code,
              location =>
                  {raw,
                   <<"ST_SetSRID(ST_MakePoint($1::float8,$2::float8),4326)">>},
              validity_at => elib_dt:add(Now, {60, minute}),
              created_at => Now},
            <<>>,
            [Lng, Lat]),
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _} ->
            join(Uid, Gid),
            {ok, Gid};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 面对面建群 - 保存（事务版本）
%% @param Code 随机码
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @return {ok, binary()} | {error, Reason}
-spec face2face_save(binary(), integer(), integer()) -> {ok, binary()} | {error, binary() | atom()}.
face2face_save(Code, Gid, Uid) ->
    elib_pg:with_tx(fun(Conn) ->
        % 读取随机码记录
        RowCode =
            case group_random_code_repo:find_by_gid(Gid, <<"code,user_id">>) of
                #{<<"code">> := RC} -> RC;
                _ -> throw({abort_tx, <<"gid not exist">>})
            end,

        % 校验 code
        case RowCode =:= Code of
            true -> ok;
            false -> throw({abort_tx, <<"code error">>})
        end,

        % 群不存在就创建
        case group_repo:find_by_id(Gid, <<"id">>) of
            #{<<"id">> := _} -> ok;
            {error, Reason1} ->
                ?ERROR_LOG([group_find_by_id_failed, Gid, Reason1]),
                Now = elib_dt:now(),
                create_group(Conn, Gid, Uid, Now, 2, 1);
            _ ->
                Now = elib_dt:now(),
                create_group(Conn, Gid, Uid, Now, 2, 1)
        end,

        % 不是群成员则加入
        case group_member_repo:find(Gid, Uid, <<"id">>) of
            #{<<"id">> := _} -> ok;
            _ ->
                group_member_ds:join_group(Conn,
                      <<"face2face_join">>,
                      Uid,
                      Gid,
                      #{})
        end,
        {ok, <<"success">>}
    end).

%% @doc 解散群组（完整事务版本）
%% @param Uid 操作者用户ID
%% @param Gid 群组ID
%% @param OwnerUid 群主用户ID
%% @param G 群组信息
%% @return ok | {error, Reason}
-spec dissolve_group(integer(), integer(), integer(), map()) -> ok | {error, binary()}.
dissolve_group(Uid, _Gid, OwnerUid, _G) when Uid =/= OwnerUid ->
    {error, <<"只有拥有者才能够解散该群，或者群已解散"/utf8>>};
dissolve_group(Uid, Gid, _, G) ->
    Now = elib_dt:now(),
    {ok, Body} = jsone_encode:encode(G, [native_utf8]),
    ToUidLi = member_uids(Gid),

    elib_pg:with_tx(fun(Conn) ->
        % 添加群日志
        case group_log_repo:add(Conn,
                #{type => 101,
                  option_uid => Uid,
                  group_id => Gid,
                  body => Body,
                  created_at => Now}) of
            {ok, _} -> ok;
            {error, LogReason} -> ?ERROR_LOG([group_log_add_failed, Gid, Uid, LogReason])
        end,

        % 删除群组
        Tb = group_repo:tablename(),
        Sql = <<"DELETE FROM ", Tb/binary, " WHERE id = $1">>,
        {ok, _} = elib_pg:execute(Conn, Sql, [Gid]),

        % 批量添加成员日志
        case group_member_repo:list_by_gid(Gid, <<"*">>, 1_000_000) of
            {ok, []} -> ok;
            {ok, Li} ->
                MemberLogs = [
                    #{type => 201,
                      option_uid => Uid,
                      group_id => Gid,
                      body => case jsone_encode:encode(V, [native_utf8]) of
                                  {ok, Encoded} -> Encoded;
                                  _ -> <<>>
                              end,
                      created_at => Now}
                 || V <- Li],
                case MemberLogs of
                    [] -> ok;
                    _ ->
                        case group_log_repo:batch_add(Conn, MemberLogs) of
                            {ok, _} -> ok;
                            {error, BatchReason} -> ?ERROR_LOG([group_log_batch_add_failed, Gid, BatchReason])
                        end,
                        ok
                end;
            {error, _Reason} ->
                %% 忽略错误，继续执行
                ok
        end,

        % 删除群成员
        Tb2 = group_member_repo:tablename(),
        Sql2 = <<"DELETE FROM ", Tb2/binary, " WHERE group_id = $1">>,
        {ok, _} = elib_pg:execute(Conn, Sql2, [Gid]),

        ok
    end),

    % 清除缓存
    dissolve(Gid),

    % 发送通知
    Action = <<"group_dissolve">>,
    Payload = #{<<"gid">> => Gid},
    msg_s2c_ds:send(Uid, ToUidLi, Action, <<>>, null, Payload, save),
    ok.

%% @doc 根据创建者和用户ID总和查找群组
%% @param CreatorUid 创建者用户ID
%% @param UserIdSum 用户ID总和
%% @return integer() 群组ID，不存在返回0
-spec find_by_creator_and_sum(integer(), integer()) -> integer().
find_by_creator_and_sum(CreatorUid, UserIdSum) ->
    Tb = group_repo:tablename(),
    Sql = <<"SELECT id FROM ", Tb/binary,
            " WHERE creator_uid = $1 AND user_id_sum = $2">>,
    case elib_pg:query(Sql, [CreatorUid, UserIdSum]) of
        {ok, [#{<<"id">> := Gid}]} -> Gid;
        _ -> 0
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
