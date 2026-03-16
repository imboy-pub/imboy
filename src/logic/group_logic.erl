-module(group_logic).
%% Stable group_collab domain facade for core group lifecycle flows.
%% API adapters should depend on this module instead of reaching repo internals directly.

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
-export([transfer/3]).
-export([transfer/4]).           % 增强的转让函数，支持保留管理员身份
-export([do_transfer/5]).
-export([do_transfer/6]).        % 增强的转让函数内部实现
-export([nearby_gid/6]).

-include("log.hrl").
-include("group_role.hrl").

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
    % 【防御性编程】确保创建者不会被重复添加
    MemberUids4 = lists:usort([U || U <- MemberUids3, U =/= Uid]),
    Sum = lists:sum(lists:usort([Uid | MemberUids4])),
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
                     || Uid2 <- MemberUids4],
                %% 【原子性修复】检查是否所有成员都添加成功
                % join_group 返回 {ok, UidSum} | {error, Reason}
                ErrorResults = [R || R <- Results, element(1, R) =:= error],
                case ErrorResults of
                    [] -> {ok, Gid};
                    _ -> throw({error, member_add_failed, ErrorResults})
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

%% @doc 群转让
%% 将群组转让给指定的新群主
%%
%% @param CurrentUid 当前用户ID
%% @param Gid 群组ID
%% @param NewOwnerUid 新群主ID
%% @return ok | {error, Reason}
-spec transfer(integer(), integer(), integer()) -> ok | {error, binary()}.
transfer(CurrentUid, Gid, NewOwnerUid) ->
    case group_repo:find_by_id(Gid, <<"*">>) of
        {error, _Reason} ->
            {error, "群组不存在"};
        G ->
            OwnerUid = maps:get(<<"owner_uid">>, G, 0),
            case do_transfer(CurrentUid, Gid, NewOwnerUid, OwnerUid, G) of
                ok -> ok;
                {error, Msg} -> {error, Msg}
            end
    end.

%% @doc 执行群转让
%% 验证权限并更新群组所有权
%%
%% @param CurrentUid 当前用户ID
%% @param Gid 群组ID
%% @param NewOwnerUid 新群主ID
%% @param OwnerUid 原群主ID
%% @param G 群组信息
%% @return ok | {error, Reason}
-spec do_transfer(integer(), integer(), integer(), integer(), map()) -> ok | {error, binary()}.
do_transfer(CurrentUid, Gid, NewOwnerUid, OwnerUid, _G) ->
    % 验证当前用户是群主
    case CurrentUid of
        OwnerUid ->
            % 验证新群主是群成员
            case group_member_repo:find(Gid, NewOwnerUid, <<"id">>) of
                #{id := _} ->
                    % 使用事务更新群主和双方角色
                    elib_pg:with_tx(fun(Conn) ->
                        case group_repo:update_owner_tx(Conn, Gid, NewOwnerUid) of
                            ok ->
                                case group_member_repo:update_role(Conn, Gid, OwnerUid, <<"normal">>) of
                                    ok ->
                                        case group_member_repo:update_role(Conn, Gid, NewOwnerUid, <<"owner">>) of
                                            ok ->
                                                % 记录群转让日志
                                                Now = elib_dt:now(),
                                                LogData = #{
                                                    <<"type">> => 9,  % 群转让操作类型
                                                    <<"option_uid">> => CurrentUid,
                                                    <<"group_id">> => Gid,
                                                    <<"body">> => jsone:encode(#{<<"from_owner_uid">> => OwnerUid,
                                                                                <<"to_owner_uid">> => NewOwnerUid,
                                                                                <<"changed_by">> => CurrentUid,
                                                                                <<"remark">> => <<"群转让"/utf8>>}),
                                                    <<"created_at">> => Now
                                                },
                                                _ = group_log_repo:add(Conn, LogData),
                                                ok;
                                            {error, Reason} -> {error, Reason}
                                        end;
                                    {error, Reason} -> {error, Reason}
                                end;
                            {error, Reason} -> {error, Reason}
                        end
                    end);
                _ ->
                    {error, "新群主必须是群成员"}
            end;
        _ ->
            {error, "只有群主可以转让群组"}
    end.

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

%% @doc 群转让（增强版，支持保留管理员身份）
%% 将群组转让给指定的新群主，可选择是否保留原群主的管理员身份
%%
%% @param CurrentUid 当前用户ID
%% @param Gid 群组ID
%% @param NewOwnerUid 新群主ID
%% @param KeepAsAdmin 是否保留原群主为管理员（true=保留为副群主，false=变为普通成员）
%% @return ok | {error, Reason}
-spec transfer(integer(), integer(), integer(), boolean()) -> ok | {error, binary()}.
transfer(CurrentUid, Gid, NewOwnerUid, KeepAsAdmin) when is_boolean(KeepAsAdmin) ->
    case group_repo:find_by_id(Gid, <<"*">>) of
        {error, _Reason} ->
            {error, "群组不存在"};
        G ->
            OwnerUid = maps:get(<<"owner_uid">>, G, 0),
            case do_transfer(CurrentUid, Gid, NewOwnerUid, OwnerUid, KeepAsAdmin, G) of
                ok -> ok;
                {error, Msg} -> {error, Msg}
            end
    end.

%% @doc 执行群转让（增强版）
%% 验证权限并更新群组所有权，支持保留原群主身份
%%
%% @param CurrentUid 当前用户ID
%% @param Gid 群组ID
%% @param NewOwnerUid 新群主ID
%% @param OwnerUid 原群主ID
%% @param KeepAsAdmin 是否保留原群主为管理员
%% @param G 群组信息
%% @return ok | {error, Reason}
-spec do_transfer(integer(), integer(), integer(), integer(), boolean(), map()) -> ok | {error, binary()}.
do_transfer(CurrentUid, Gid, NewOwnerUid, OwnerUid, KeepAsAdmin, _G) ->
    % 验证当前用户是群主
    case CurrentUid of
        OwnerUid ->
            % 验证新群主是群成员
            case group_member_repo:find(Gid, NewOwnerUid, <<"id, role">>) of
                #{<<"id">> := _} ->
                    % 使用事务更新群主和双方角色
                    elib_pg:with_tx(fun(Conn) ->
                        case group_repo:update_owner_tx(Conn, Gid, NewOwnerUid) of
                            ok ->
                                % 更新原群主角色
                                OldOwnerRole = case KeepAsAdmin of
                                    true  -> ?ROLE_VICE_OWNER;  % 保留为副群主
                                    false -> ?ROLE_MEMBER       % 变为普通成员
                                end,
                                case group_member_repo:update_role(Conn, Gid, OwnerUid, OldOwnerRole) of
                                    ok ->
                                        % 更新新群主角色
                                        case group_member_repo:update_role(Conn, Gid, NewOwnerUid, ?ROLE_OWNER) of
                                            ok ->
                                                % 记录群转让日志
                                                Now = elib_dt:now(),
                                                LogData = #{
                                                    <<"type">> => 9,  % 群转让操作类型
                                                    <<"option_uid">> => CurrentUid,
                                                    <<"group_id">> => Gid,
                                                    <<"body">> => jsone:encode(#{<<"from_owner_uid">> => OwnerUid,
                                                                                        <<"to_owner_uid">> => NewOwnerUid,
                                                                                        <<"changed_by">> => CurrentUid,
                                                                                        <<"keep_as_admin">> => KeepAsAdmin,
                                                                                        <<"remark">> => <<"群转让"/utf8>>}),
                                                    <<"created_at">> => Now
                                                },
                                                _ = group_log_repo:add(Conn, LogData),
                                                ok;
                                            {error, Reason} -> {error, Reason}
                                        end;
                                    {error, Reason} -> {error, Reason}
                                end;
                            {error, Reason} -> {error, Reason}
                        end
                    end);
                _ ->
                    {error, "新群主必须是群成员"}
            end;
        _ ->
            {error, "只有群主可以转让群组"}
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
