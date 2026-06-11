-module(group_ds).
%%%
% group_ds 是 group domain service 缩写
%%%
-export([find_by_id/2]).
-export([update_owner_tx/3]).
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
-export([list_by_ids/2]).
-export([page/4]).
-export([update/1]).
-export([get_user_id_sum/1]).
-export([count_by_owner/1]).
-export([exists/1]).
-export([update_by_id/2]).
-export([insert/1]).
-export([page_by_owner/3]).
-export([page_joined/3]).
-export([page_managed/3]).
-export([load_aggregate/1, save_aggregate/1]).

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
-spec create_group(pid(), integer() | binary(), integer(), binary(), integer(), integer()) ->
    integer().
create_group(Conn, Gid, Uid, Now, Type, JoinLimit) ->
    %% 【TSID 修复】group.id 是 BIGINT NOT NULL 且无 default。
    %% BIGSERIAL→TSID 迁移后必须预生成 id；缺失 id 会触发 NOT NULL 违反，
    %% 而原先静默 fallback(Gid2=0) 让后续 group_member INSERT 与同事务
    %% 后续 SQL 全部撞 25P02 in_failed_sql_transaction。
    %% TSID generator 注册名为 group_info（见 imboy_app:tsid_generator_names/0）。
    Gid_for_insert =
        case Gid of
            N when is_integer(N), N > 0 -> N;
            _ -> elib_tsid:generate(group_info)
        end,
    GMap = #{
        id => Gid_for_insert,
        type => Type,
        join_limit => JoinLimit,
        user_id_sum => Uid,
        owner_uid => Uid,
        creator_uid => Uid,
        created_at => Now
    },
    %% 【一致性修复】INSERT 失败时 throw，让外层 with_tx 回滚事务，
    %% 避免事务进入 aborted 状态导致后续 SQL 全部 25P02。
    %% parse_result/1 返回 {ok, Id, ExtraMap} 三元组（见 elib_pg_sql.erl:602）。
    %% 原先匹配 {ok, Id} 二元组从未命中，永远 fallback 到 Gid=0，
    %% 这才是 group_member.group_id=0 与 25P02 链路的真正起点。
    Gid2 =
        case
            elib_pg_sql:parse_result(
                elib_pg:insert(Conn, group_repo:tablename(), GMap, <<"RETURNING id">>)
            )
        of
            {ok, Id, _Extra} when is_integer(Id), Id > 0 ->
                Id;
            {error, Reason} ->
                ?ERROR_LOG([group_create_insert_failed, Gid_for_insert, Uid, Reason]),
                throw({error, group_create_failed, Reason});
            Other ->
                ?ERROR_LOG([group_create_insert_unexpected, Gid_for_insert, Uid, Other]),
                throw({error, group_create_failed, Other})
        end,
    %% 检查群成员是否已存在，不存在则插入
    %% 【TSID 修复】group_member.id 同样 NOT NULL 无 default，需预生成。
    case group_member_repo:find(Gid2, Uid, <<"id">>) of
        GM when map_size(GM) == 0 ->
            GmId = elib_tsid:generate(group_member),
            case
                elib_pg:insert(
                    Conn,
                    group_member_repo:tablename(),
                    #{
                        id => GmId,
                        group_id => Gid2,
                        user_id => Uid,
                        % 群主
                        role => 4,
                        created_at => Now
                    },
                    <<>>
                )
            of
                {ok, _} ->
                    ok;
                {error, Reason2} ->
                    ?ERROR_LOG([group_member_insert_failed, Gid2, Uid, Reason2]),
                    throw({error, group_member_create_failed, Reason2})
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
-spec nearby_gid(
    binary() | string() | number(),
    binary() | string() | number(),
    binary() | string() | number(),
    binary() | string() | number(),
    binary() | string() | number(),
    binary() | string() | number()
) ->
    {ok, list(map())} | {error, term()}.
nearby_gid(Lng, Lat, Radius, _Unit, Limit, Code) ->
    Now = elib_dt:now(),
    Sql = <<
        "select\n        id,\n        group_id,\n        ST_AsText(location) "
        "as location,\n        ST_Distance(\n            ST_SetSRID(ST_MakePo"
        "int($1::float8, $2::float8), 4326)::geography,\n           "
        " location\n        ) as distance\n    from public.group_random_code\n "
        "   where code = $3\n      and validity_at > $4::timestamptz\n "
        "     and ST_DWithin(\n            location::geography,\n   "
        "         ST_SetSRID(ST_MakePoint($1::float8, $2::float8), 4326)::geo"
        "graphy,\n            $5::int\n      )\n    order by distance "
        "asc\n    limit $6::int;"
    >>,
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
-spec face2face_create(pid(), integer(), binary(), float(), float()) ->
    {ok, integer()} | {error, binary() | atom()}.
face2face_create(Conn, Uid, Code, Lng, Lat) ->
    Now = elib_dt:now(),
    Gid = gid(),
    {Sql, Params} =
        elib_pg_sql:insert_with_params(
            group_random_code_repo:tablename(),
            #{
                group_id => Gid,
                user_id => Uid,
                code => Code,
                location =>
                    {raw, <<"ST_SetSRID(ST_MakePoint($1::float8,$2::float8),4326)">>},
                validity_at => elib_dt:add(Now, {60, minute}),
                created_at => Now
            },
            <<>>,
            [Lng, Lat]
        ),
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
            #{<<"id">> := _} ->
                ok;
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
            #{<<"id">> := _} ->
                ok;
            _ ->
                group_member_ds:join_group(
                    Conn,
                    <<"face2face_join">>,
                    Uid,
                    Gid,
                    #{}
                )
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
        case
            group_log_repo:add(
                Conn,
                #{
                    type => 101,
                    option_uid => Uid,
                    group_id => Gid,
                    body => Body,
                    created_at => Now
                }
            )
        of
            {ok, _} -> ok;
            {error, LogReason} -> ?ERROR_LOG([group_log_add_failed, Gid, Uid, LogReason])
        end,

        % 删除群组
        Tb = group_repo:tablename(),
        Sql = <<"DELETE FROM ", Tb/binary, " WHERE id = $1">>,
        {ok, _} = elib_pg:execute(Conn, Sql, [Gid]),

        % 批量添加成员日志
        case group_member_repo:list_by_gid(Gid, <<"*">>, 1_000_000) of
            {ok, []} ->
                ok;
            {ok, Li} ->
                MemberLogs = [
                    #{
                        type => 201,
                        option_uid => Uid,
                        group_id => Gid,
                        body =>
                            case jsone_encode:encode(V, [native_utf8]) of
                                {ok, Encoded} -> Encoded;
                                _ -> <<>>
                            end,
                        created_at => Now
                    }
                 || V <- Li
                ],
                case MemberLogs of
                    [] ->
                        ok;
                    _ ->
                        case group_log_repo:batch_add(Conn, MemberLogs) of
                            {ok, _} ->
                                ok;
                            {error, BatchReason} ->
                                ?ERROR_LOG([group_log_batch_add_failed, Gid, BatchReason])
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
    Sql = <<"SELECT id FROM ", Tb/binary, " WHERE creator_uid = $1 AND user_id_sum = $2">>,
    case elib_pg:query(Sql, [CreatorUid, UserIdSum]) of
        {ok, [#{<<"id">> := Gid}]} -> Gid;
        _ -> 0
    end.

%% G3: group_logic 不应直调 group_repo / thin DS wrappers
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, any()}.
find_by_id(Gid, Column) ->
    group_repo:find_by_id(Gid, Column).

-spec update_owner_tx(any(), integer(), integer()) -> ok | {error, any()}.
update_owner_tx(Conn, Gid, NewOwnerUid) ->
    group_repo:update_owner_tx(Conn, Gid, NewOwnerUid).

%% G3: group_member_handler 不应直调 group_repo
-spec list_by_ids(list(integer()), binary()) -> {ok, list(map())} | {error, any()}.
list_by_ids(Ids, Column) -> group_repo:list_by_ids(Ids, Column).

%% G3 thin wrappers for adm_group_handler
-spec page(integer(), integer(), map(), binary()) -> {ok, map()} | {error, any()}.
page(Page, Size, Where, OrderBy) -> group_repo:page(Page, Size, Where, OrderBy).

-spec update(map()) -> {ok, non_neg_integer()} | {error, any()}.
update(Data) -> group_repo:update(Data).

%% @doc 获取群组 user_id_sum（群成员 uid 之和，用于判断唯一性）
%% @param Gid 群组ID
%% @return user_id_sum 值，不存在时返回0
-spec get_user_id_sum(integer()) -> integer().
get_user_id_sum(Gid) ->
    Tb = group_repo:tablename(),
    elib_pg:pluck_value(Tb, <<"user_id_sum">>, #{id => Gid}, #{}, 0).

%% @doc 统计用户拥有的有效群组数
%% @param OwnerUid 群主UID
%% @return 群组数量
-spec count_by_owner(integer()) -> integer().
count_by_owner(OwnerUid) ->
    Tb = group_repo:tablename(),
    elib_pg:pluck_value(Tb, <<"count(*)">>, #{status => 1, owner_uid => OwnerUid}, 0).

%% @doc 判断群组是否存在
%% @param Gid 群组ID
%% @return true | false
-spec exists(integer()) -> boolean().
exists(Gid) ->
    Tb = group_repo:tablename(),
    elib_pg:pluck_value(Tb, <<"count(*)">>, #{id => Gid}, 0) > 0.

%% @doc 按 ID 更新群组
%% @param Gid 群组ID
%% @param Data 更新数据
%% @return {ok, Count} | {error, Reason}
-spec update_by_id(integer(), map()) -> {ok, integer()} | {error, any()}.
update_by_id(Gid, Data) ->
    Tb = group_repo:tablename(),
    elib_pg:update(Tb, Data, <<"id = $1">>, [Gid]).

%% @doc 插入群组记录
%% @param Data 群组数据
%% @return {ok, _} | {error, Reason}
-spec insert(map()) -> {ok, any()} | {error, any()}.
insert(Data) ->
    Tb = group_repo:tablename(),
    elib_pg:insert(Tb, Data).

%% @doc 分页查询我拥有的群（attr=owner）
-spec page_by_owner(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, any()}.
page_by_owner(OwnerUid, Page, Size) ->
    Tb = group_repo:tablename(),
    Where = #{status => 1, owner_uid => OwnerUid},
    elib_pg:page_with_total(Tb, Where, Page, Size).

%% @doc 分页查询我加入的群（attr=join，排除自己是群主的）
-spec page_joined(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, any()}.
page_joined(Uid, Page, Size) ->
    GTb = group_repo:tablename(),
    MTb = group_member_repo:tablename(),
    Tb = <<GTb/binary, " g LEFT JOIN ", MTb/binary, " m ON g.id = m.group_id">>,
    Where = #{
        <<"g.status">> => 1,
        <<"m.status">> => 1,
        <<"m.user_id">> => Uid,
        <<"g.owner_uid">> => {op, <<"!=">>, Uid}
    },
    elib_pg:page_with_total(Tb, <<"g.*">>, Where, <<"g.id desc">>, Page, Size).

%% @doc 分页查询我管理的群（attr=manager，群主/副群主/管理员）
-spec page_managed(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, any()}.
page_managed(Uid, Page, Size) ->
    GTb = group_repo:tablename(),
    MTb = group_member_repo:tablename(),
    Tb = <<GTb/binary, " g LEFT JOIN ", MTb/binary, " m ON g.id = m.group_id">>,
    Where = #{
        <<"g.status">> => 1,
        <<"__or">> =>
            [
                #{<<"g.owner_uid">> => Uid},
                #{
                    <<"m.status">> => 1,
                    <<"m.user_id">> => Uid,
                    <<"m.role">> => {op, <<">=">>, 3}
                }
            ]
    },
    elib_pg:page_with_total(Tb, <<"g.*">>, Where, <<"g.id desc">>, Page, Size).

%% ===================================================================
%% 聚合加载/保存（T2.2：group_agg 聚合根 DS 接线）
%% SERVICE_PATTERN 的 I/O 外壳：load=读、save=写，经 Repo 层落库。
%% ===================================================================

%% @doc 加载群组聚合根：读 group 行 → 字段映射 → group_agg:rehydrate。
%% 群组不存在（空 map）返回 {error, group_not_found}。
-spec load_aggregate(integer()) -> {ok, group_agg:t()} | {error, group_not_found}.
load_aggregate(Gid) ->
    case group_repo:find_by_id(Gid, <<"id,owner_uid,member_count,type,status">>) of
        Row when is_map(Row), map_size(Row) > 0 ->
            {ok, group_agg:rehydrate(row_to_agg_map(Row))};
        _ ->
            {error, group_not_found}
    end.

%% @doc 保存群组聚合根：聚合状态 → group 更新行 → group_repo:update。
%% DS 层经 Repo 落库，不直调 elib_pg。
-spec save_aggregate(group_agg:t()) -> {ok, non_neg_integer()} | {error, any()}.
save_aggregate(Agg) ->
    group_repo:update(agg_to_row(Agg)).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 纯映射：group 行 → group_agg:rehydrate 入参（列名归一 + status 码→atom）。
-spec row_to_agg_map(map()) -> map().
row_to_agg_map(Row) ->
    #{
        <<"group_id">> => maps:get(<<"id">>, Row),
        <<"owner">> => maps:get(<<"owner_uid">>, Row, 0),
        <<"member_count">> => maps:get(<<"member_count">>, Row, 0),
        <<"type">> => maps:get(<<"type">>, Row, undefined),
        <<"status">> => status_code_to_atom(maps:get(<<"status">>, Row, 1))
    }.

%% @doc 纯映射：group_agg 聚合 → group 更新行（id 主键 + status atom→码）。
-spec agg_to_row(group_agg:t()) -> map().
agg_to_row(Agg) ->
    #{
        <<"id">> => group_agg:id(Agg),
        <<"owner_uid">> => group_agg:owner(Agg),
        <<"member_count">> => group_agg:member_count(Agg),
        <<"status">> => status_atom_to_code(group_agg:status(Agg))
    }.

%% group 表 status 语义：1=active（有效）/ 0=dissolved（软删除）。
-spec status_code_to_atom(integer() | binary()) -> active | dissolved.
status_code_to_atom(0) -> dissolved;
status_code_to_atom(<<"0">>) -> dissolved;
status_code_to_atom(_) -> active.

-spec status_atom_to_code(active | dissolved) -> 0 | 1.
status_atom_to_code(dissolved) -> 0;
status_atom_to_code(active) -> 1.
