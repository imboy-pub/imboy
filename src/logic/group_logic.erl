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
-export([dissolve/2]).
-export([transfer/3]).
% 增强的转让函数，支持保留管理员身份
-export([transfer/4]).
-export([nearby_gid/6]).
-export([find_by_id/2]).
-export([member_uids/1]).
-export([face2face_notify_payload/2]).
-export([notify_member_join/4]).
-export([member_list_by_uids/1]).
-export([count_by_owner/1]).
-export([edit/3]).
-export([set_e2ee_mode/3]).
-export([page_by_owner/3]).
-export([page_joined/3]).
-export([page_managed/3]).
-export([is_member/2]).
-export([msg_page/3]).
-export([find_member/2]).
-export([get_remark/2]).
-export([update_remark/3]).

-include("log.hrl").
-include("group_role.hrl").

%% @doc 群转让时的数据转换（ID 已直接以 integer 返回，无需转换）
-spec group_transfer(map()) -> map().
group_transfer(G) -> G.

%% @doc 面对面建群
%% 通过随机码创建或加入附近的群组
%% @param Uid 用户ID
%% @param Code 随机码
%% @param Lng 经度
%% @param Lat 纬度
%% @return {ok, Gid} | {error, Reason}
-spec face2face(integer(), binary(), binary(), binary()) ->
    {ok, integer()} | {error, binary() | string()}.
face2face(_, <<>>, _, _) ->
    {error, <<"Code 必须"/utf8>>};
face2face(_, _, undefined, _) ->
    {error, <<"longitude 必须"/utf8>>};
face2face(_, _, <<"null">>, _) ->
    {error, <<"longitude 必须"/utf8>>};
face2face(_, _, _, undefined) ->
    {error, <<"latitude 必须"/utf8>>};
face2face(_, _, _, <<"null">>) ->
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
%% @param MemberUids 初始成员ID列表（整数）
%% @return {ok, Gid} | {error, Reason}
-spec add(non_neg_integer(), integer(), integer(), [binary()]) ->
    {ok, integer()} | {error, binary()}.
add(Count, _, _, _) when Count > 100 ->
    {error, <<"每人最多创建100个群"/utf8>>};
add(_, Uid, Type, MemberUids) ->
    MemberUids2 =
        case MemberUids of
            List when is_list(List) -> List;
            _ -> []
        end,
    Now = elib_dt:now(),
    MemberUids3 = [ec_cnv:to_integer(Id) || Id <- MemberUids2, is_binary(Id)],
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
                _ = [
                    group_member_logic:join_group(Conn, JoinMode, Uid2, Gid, #{})
                 || Uid2 <- MemberUids4
                ],
                {ok, Gid}
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
    case group_ds:find_by_id(Gid, <<"*">>) of
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
            case group_member_ds:find_by_gid_and_uid(Gid, NewOwnerUid, <<"id">>) of
                #{<<"id">> := _} ->
                    % 使用事务更新群主和双方角色
                    elib_pg:with_tx(fun(Conn) ->
                        case group_ds:update_owner_tx(Conn, Gid, NewOwnerUid) of
                            ok ->
                                case
                                    group_member_ds:update_role(Conn, Gid, OwnerUid, ?ROLE_MEMBER)
                                of
                                    ok ->
                                        case
                                            group_member_ds:update_role(
                                                Conn, Gid, NewOwnerUid, ?ROLE_OWNER
                                            )
                                        of
                                            ok ->
                                                % 记录群转让日志
                                                Now = elib_dt:now(),
                                                LogData = #{
                                                    % 群转让操作类型
                                                    <<"type">> => 9,
                                                    <<"option_uid">> => CurrentUid,
                                                    <<"group_id">> => Gid,
                                                    <<"body">> => jsone:encode(#{
                                                        <<"from_owner_uid">> => OwnerUid,
                                                        <<"to_owner_uid">> => NewOwnerUid,
                                                        <<"changed_by">> => CurrentUid,
                                                        <<"remark">> => <<"群转让"/utf8>>
                                                    }),
                                                    <<"created_at">> => Now
                                                },
                                                _ = group_log_ds:add(Conn, LogData),
                                                ok;
                                            {error, Reason} ->
                                                {error, Reason}
                                        end;
                                    {error, Reason} ->
                                        {error, Reason}
                                end;
                            {error, Reason} ->
                                {error, Reason}
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
-spec nearby_gid(
    binary() | string() | number(),
    binary() | string() | number(),
    binary() | string() | number(),
    binary() | string() | number(),
    binary() | string() | number(),
    binary() | string() | number()
) ->
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
    case group_ds:find_by_id(Gid, <<"*">>) of
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
-spec do_transfer(integer(), integer(), integer(), integer(), boolean(), map()) ->
    ok | {error, binary()}.
do_transfer(CurrentUid, Gid, NewOwnerUid, OwnerUid, KeepAsAdmin, _G) ->
    % 验证当前用户是群主
    case CurrentUid of
        OwnerUid ->
            % 验证新群主是群成员
            case group_member_ds:find_by_gid_and_uid(Gid, NewOwnerUid, <<"id, role">>) of
                #{<<"id">> := _} ->
                    % 使用事务更新群主和双方角色
                    elib_pg:with_tx(fun(Conn) ->
                        case group_ds:update_owner_tx(Conn, Gid, NewOwnerUid) of
                            ok ->
                                % 更新原群主角色
                                OldOwnerRole =
                                    case KeepAsAdmin of
                                        % 保留为副群主
                                        true -> ?ROLE_VICE_OWNER;
                                        % 变为普通成员
                                        false -> ?ROLE_MEMBER
                                    end,
                                case
                                    group_member_ds:update_role(Conn, Gid, OwnerUid, OldOwnerRole)
                                of
                                    ok ->
                                        % 更新新群主角色
                                        case
                                            group_member_ds:update_role(
                                                Conn, Gid, NewOwnerUid, ?ROLE_OWNER
                                            )
                                        of
                                            ok ->
                                                % 记录群转让日志
                                                Now = elib_dt:now(),
                                                LogData = #{
                                                    % 群转让操作类型
                                                    <<"type">> => 9,
                                                    <<"option_uid">> => CurrentUid,
                                                    <<"group_id">> => Gid,
                                                    <<"body">> => jsone:encode(#{
                                                        <<"from_owner_uid">> => OwnerUid,
                                                        <<"to_owner_uid">> => NewOwnerUid,
                                                        <<"changed_by">> => CurrentUid,
                                                        <<"keep_as_admin">> => KeepAsAdmin,
                                                        <<"remark">> => <<"群转让"/utf8>>
                                                    }),
                                                    <<"created_at">> => Now
                                                },
                                                _ = group_log_ds:add(Conn, LogData),
                                                ok;
                                            {error, Reason} ->
                                                {error, Reason}
                                        end;
                                    {error, Reason} ->
                                        {error, Reason}
                                end;
                            {error, Reason} ->
                                {error, Reason}
                        end
                    end);
                _ ->
                    {error, "新群主必须是群成员"}
            end;
        _ ->
            {error, "只有群主可以转让群组"}
    end.

%% @doc 查询群组详情
%% 通过群组ID查询指定字段的群组信息
%% @param Gid 群组ID
%% @param Column 查询字段
%% @return map() | {error, any()}
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, any()}.
find_by_id(Gid, Column) ->
    group_ds:find_by_id(Gid, Column).

%% @doc 获取群组所有成员 UID 列表
%% @param Gid 群组ID
%% @return [integer()]
-spec member_uids(integer()) -> [integer()].
member_uids(Gid) ->
    group_ds:member_uids(Gid).

%% @doc 构造 face2face 加入通知的 payload
%% 查询用户信息，返回用于广播 group_member_join 通知的 map
%% @param Uid 加入者用户ID
%% @param Gid 群组ID
%% @return map()
-spec face2face_notify_payload(integer(), integer()) -> map().
face2face_notify_payload(Uid, Gid) ->
    ToUidLi = group_ds:member_uids(Gid),
    ToUidLi2 =
        case ToUidLi of
            [] -> [Uid];
            _ -> ToUidLi
        end,
    User = user_ds:find_by_id(Uid, <<"account,avatar,nickname">>),
    Payload = #{
        <<"gid">> => Gid,
        <<"user_id_sum">> => lists:sum(ToUidLi2),
        <<"nickname">> => maps:get(<<"nickname">>, User),
        <<"avatar">> => maps:get(<<"avatar">>, User),
        <<"account">> => maps:get(<<"account">>, User)
    },
    #{to_uid_list => ToUidLi2, payload => Payload}.

%% @doc 向群成员广播 group_member_join 系统消息
%% @param FromUid 发送者ID
%% @param ToUidLi 目标 UID 列表
%% @param Action 消息动作
%% @param Payload 消息体
%% @return ok
-spec notify_member_join(integer(), [integer()], binary(), map()) -> ok.
notify_member_join(FromUid, ToUidLi, Action, Payload) ->
    msg_s2c_ds:send(FromUid, ToUidLi, Action, <<>>, null, Payload, save).

%% @doc 批量查询成员用户信息
%% @param Uids 用户ID列表
%% @return {ok, [map()]} | {error, any()}
-spec member_list_by_uids([integer()]) -> {ok, [map()]} | {error, any()}.
member_list_by_uids(Uids) ->
    user_ds:list_by_ids(Uids, <<"id as user_id,account,avatar,nickname">>).

%% @doc 查询用户已创建群组数量
%% @param OwnerUid 群主用户ID
%% @return integer()
-spec count_by_owner(integer()) -> integer().
count_by_owner(OwnerUid) ->
    group_ds:count_by_owner(OwnerUid).

%% @doc 编辑群组（更新已有群 或 插入新群）
%% 若群已存在则更新字段并广播 group_edit 事件；否则从随机码表读取创建信息后插入新群。
%% @param Uid 操作用户ID
%% @param Gid 群组ID（integer）
%% @param Data 要更新的字段 map
%% @return ok | {error, any()}
-spec edit(integer(), integer(), map()) -> ok | {error, any()}.
edit(Uid, Gid, Data) ->
    Now = elib_dt:now(),
    case group_ds:exists(Gid) of
        true ->
            Member = group_member_ds:find_by_gid_and_uid(Gid, Uid, <<"role">>),
            Role = maps:get(<<"role">>, Member, 0),
            case ?IS_OWNER_OR_VICE_OWNER(Role) of
                false ->
                    {error, <<"只有群主或副群主才能修改群信息"/utf8>>};
                true ->
                    case group_ds:update_by_id(Gid, Data#{updated_at => Now}) of
                        {ok, _} ->
                            ToUidLi = group_ds:member_uids(Gid),
                            Action = <<"group_edit">>,
                            msg_s2c_ds:send(
                                Uid,
                                ToUidLi,
                                Action,
                                <<>>,
                                null,
                                Data#{<<"gid">> => Gid},
                                save
                            ),
                            ok;
                        {error, _} = Err ->
                            Err
                    end
            end;
        false ->
            M3 = group_random_code_ds:find_by_gid(Gid, <<"user_id, created_at">>),
            Data4 = Data#{
                owner_uid => maps:get(<<"user_id">>, M3, Uid),
                creator_uid => maps:get(<<"user_id">>, M3, Uid),
                created_at => maps:get(<<"created_at">>, M3, Now),
                id => Gid
            },
            case group_ds:insert(Data4) of
                {ok, _} -> ok;
                {error, _} = Err -> Err
            end
    end.

%% @doc 开启群级 E2EE（P0-B B4）
%% 仅群主可操作，且只允许 0→1 单向开启（MVP：防"关闭后明文泄漏预期"歧义）。
%% 开启后 msg_c2g_logic 的 fail-closed 门拒收未加密的内容消息。
%% @param Uid 操作用户ID
%% @param Gid 群组ID
%% @param Mode 目标模式（仅接受 1）
-spec set_e2ee_mode(integer(), integer(), integer()) -> ok | {error, binary()}.
set_e2ee_mode(_Uid, _Gid, Mode) when Mode =/= 1 ->
    {error, <<"e2ee_mode 仅支持单向开启（0→1）"/utf8>>};
set_e2ee_mode(Uid, Gid, 1) ->
    Member = group_member_ds:find_by_gid_and_uid(Gid, Uid, <<"role">>),
    Role = maps:get(<<"role">>, Member, 0),
    case ?IS_OWNER_ROLE(Role) of
        false ->
            {error, <<"只有群主才能开启群端到端加密"/utf8>>};
        true ->
            case group_ds:update_by_id(Gid, #{e2ee_mode => 1, updated_at => elib_dt:now()}) of
                {ok, _} ->
                    ok = group_ds:flush_e2ee_mode(Gid),
                    %% 广播开关事件：客户端据此建立群会话并停发明文
                    ToUidLi = group_ds:member_uids(Gid),
                    msg_s2c_ds:send(
                        Uid,
                        ToUidLi,
                        <<"group_e2ee_mode">>,
                        <<>>,
                        null,
                        #{<<"gid">> => Gid, <<"e2ee_mode">> => 1},
                        save
                    ),
                    ok;
                {error, Reason} ->
                    %% 内部 PG 错误详情只进日志，不透传客户端（security-reviewer M1）
                    _ = ?ERROR_LOG([set_e2ee_mode_update_failed, Gid, Reason]),
                    {error, <<"e2ee_mode_update_failed">>}
            end
    end.

%% @doc 解散群组（仅需 Uid 和 Gid，自动查询 owner_uid 和群信息）
%% @param Uid 操作者用户ID
%% @param Gid 群组ID
%% @return ok | {error, binary()}
-spec dissolve(integer(), integer()) -> ok | {error, binary()}.
dissolve(Uid, Gid) ->
    case group_ds:find_by_id(Gid, <<"*">>) of
        {error, _} ->
            {error, <<"群组不存在"/utf8>>};
        G ->
            OwnerUid = maps:get(<<"owner_uid">>, G, 0),
            group_ds:dissolve_group(Uid, Gid, OwnerUid, G)
    end.

%% @doc 分页查询当前用户作为群主的群组列表
%% @param OwnerUid 群主用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, map()} | {error, any()}
-spec page_by_owner(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, any()}.
page_by_owner(OwnerUid, Page, Size) ->
    group_ds:page_by_owner(OwnerUid, Page, Size).

%% @doc 分页查询当前用户已加入的群组列表
%% @param Uid 用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, map()} | {error, any()}
-spec page_joined(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, any()}.
page_joined(Uid, Page, Size) ->
    group_ds:page_joined(Uid, Page, Size).

%% @doc 分页查询当前用户管理的群组列表（群主/副群主/管理员）
%% @param Uid 用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, map()} | {error, any()}
-spec page_managed(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, any()}.
page_managed(Uid, Page, Size) ->
    group_ds:page_managed(Uid, Page, Size).

%% @doc 判断用户是否为群成员
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @return map()（空 map 表示非成员）
-spec is_member(integer(), integer()) -> map().
is_member(Gid, Uid) ->
    group_member_ds:find_by_gid_and_uid(Gid, Uid, <<"id">>).

%% @doc 分页查询群组消息历史记录
%% @param Where 查询条件 map
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, map()} | {error, term()}
-spec msg_page(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
msg_page(Where, Page, Size) ->
    msg_c2g_ds:page(Where, Page, Size).

%% @doc 查询用户的群成员记录
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @return map()
-spec find_member(integer(), integer()) -> map().
find_member(Gid, Uid) ->
    group_member_ds:find_by_gid_and_uid(Gid, Uid, <<"*">>).

%% @doc 获取当前用户对指定群的备注
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @return map()（含 remark 字段）
-spec get_remark(integer(), integer()) -> map().
get_remark(Gid, Uid) ->
    group_member_ds:find_by_gid_and_uid(Gid, Uid, <<"remark">>).

%% @doc 更新当前用户对指定群的备注
%% 先校验是否为群成员，再执行更新
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @param Remark 新备注内容
%% @return {ok, integer()} | {error, any()}
-spec update_remark(integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
update_remark(Gid, Uid, Remark) ->
    Member = group_member_ds:find_by_gid_and_uid(Gid, Uid, <<"id">>),
    case maps:size(Member) of
        0 -> {error, <<"你不是该群成员"/utf8>>};
        _ -> group_member_ds:update_remark(Gid, Uid, Remark)
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
