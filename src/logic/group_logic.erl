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
-export([page_by_owner/3, page_by_owner/4]).
-export([page_joined/3, page_joined/4]).
-export([page_managed/3, page_managed/4]).
-export([page_by_owner_cursor/4, page_joined_cursor/4, page_managed_cursor/4]).
-export([is_member/2]).
-export([msg_page/3]).
-export([find_member/2]).
-export([get_remark/2]).
-export([update_remark/3]).

%% ==================== T5 scope 感知（双体验 v2.5.2 WP3/T5）====================
%% 新增函数，不改上方任何既有函数签名；personal 路径行为零变化。
-export([add/5]).
-export([edit_checked/3]).
-export([
    list_workspace_groups/2, list_workspace_groups/3,
    list_member_workspace_groups/4, list_member_workspace_groups/5, list_member_workspace_groups/6
]).
-export([mark_workspace_read/3]).

%% ============ W04.b（channel-group-ux v2.2 §6.4 C1）：带受理回执的群创建 ============
%% 纯增量入口：不带 request_id 的旧请求继续走 add/5，行为零变化（回归红线）。
-export([add_with_receipt/8]).

-include("log.hrl").
-include("group_role.hrl").
-include("error_code.hrl").

%% @doc 群行出站 DTO 边界（ID 已直接以 integer 返回，无需转换）。
%% 统一剥除 chat_aes_key（群消息密钥列）：detail 等端点走 SELECT *，
%% 该列会随行带出——当前无代码读写（恒空串默认值），但任何持 JWT 的
%% 用户（含非成员）都能经 group/detail 拿到；未来一旦启用该列即为
%% 群密钥明文泄漏。剥在 transfer 层，三处出站点一次收口。
-spec group_transfer(map()) -> map().
group_transfer(G) ->
    maps:remove(<<"chat_aes_key">>, G).

%% @doc 面对面建群
%% 通过随机码创建或加入附近的群组
%% @param Uid 用户ID
%% @param Code 随机码
%% @param Lng 经度
%% @param Lat 纬度
%% @return {ok, Gid} | {error, Reason}
-spec face2face(integer(), binary(), binary() | undefined, binary() | undefined) ->
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
            TxRes = elib_pg:with_tx(fun(Conn) ->
                group_ds:face2face_create(Conn, Uid, Code, LngFloat, LatFloat)
            end),
            %% with_tx 可能返回 {rollback,_}；ds 层错误可能为 atom。
            %% 统一归一到本函数的 {error, binary()} 契约，防止向调用方泄漏。
            case TxRes of
                {ok, Gid} ->
                    {ok, Gid};
                {error, Reason} when is_binary(Reason) ->
                    {error, Reason};
                {error, Reason} ->
                    {error, atom_to_binary(Reason, utf8)};
                {rollback, RollbackReason} ->
                    ?ERROR_LOG([face2face_create_rollback, Uid, Code, RollbackReason]),
                    {error, <<"面对面建群事务回滚"/utf8>>}
            end;
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
-spec face2face_save(binary(), integer(), integer()) ->
    {ok, binary()} | {error, binary() | atom()} | {rollback, term()}.
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
    % P0 终局决策（真源 priv/migrations/00000079_user_id_sum_removal.up.sql）：
    % 同一成员集允许创建多个群（微信/Telegram 同款），不做创建幂等去重；
    % user_id_sum 签名链已随迁移 00000079 整体退役。
    % invite_[uid]_[nickname]
    UserTitle = user_ds:title(Uid),
    JoinMode = <<"invite_", (ec_cnv:to_binary(Uid))/binary, "_", UserTitle/binary>>,
    elib_pg:with_tx(fun(Conn) ->
        Gid = group_ds:create_group(Conn, 0, Uid, Now, Type, 1),
        %% 【原子性修复】批量添加成员并检查结果
        lists:foreach(
            fun(Uid2) -> join_initial_member(Conn, JoinMode, Uid2, Gid) end,
            MemberUids4
        ),
        {ok, Gid}
    end).

join_initial_member(Conn, JoinMode, Uid, Gid) ->
    case group_member_logic:join_group(Conn, JoinMode, Uid, Gid, #{}) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG([group_initial_member_create_failed, Gid, Uid, Reason]),
            throw({abort_tx, <<"群创建失败，请稍后重试"/utf8>>})
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
            {error, <<"群组不存在"/utf8>>};
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
                    TxRes = elib_pg:with_tx(fun(Conn) ->
                        %% T7 归档写守卫（P0 收口）：转让与守卫同事务
                        %% （{group, Gid} 行锁；personal 群直通）
                        ok = workspace_guard:abort_on_error(
                            workspace_guard:ensure_writable_tx(Conn, {group, Gid})
                        ),
                        case group_ds:update_owner_tx(Conn, Gid, NewOwnerUid, OwnerUid) of
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
                                                save_transfer_log(Conn, LogData);
                                            {error, Reason} ->
                                                {error, Reason}
                                        end;
                                    {error, Reason} ->
                                        {error, Reason}
                                end;
                            {error, Reason} ->
                                {error, Reason}
                        end
                    end),
                    %% with_tx 可能返回 {rollback,_}；ds 层错误可能为 atom。
                    %% 统一归一到 {error, binary()} 契约，防止向调用方泄漏。
                    case TxRes of
                        ok ->
                            %% Q05/F07：转让事务提交成功后发布领域事件，由
                            %% group_event_handler 投递 group_transfer 通知
                            %% （save，离线可补）——补齐四场景唯一"零通知"缺口。
                            imboy_domain_event:publish([
                                {owner_transferred, Gid, OwnerUid, NewOwnerUid}
                            ]),
                            ok;
                        {error, Reason} when is_binary(Reason) ->
                            {error, Reason};
                        {error, Reason} ->
                            {error, atom_to_binary(Reason, utf8)};
                        {rollback, RollbackReason} ->
                            ?ERROR_LOG([group_transfer_rollback, Gid, RollbackReason]),
                            {error, <<"群转让事务回滚"/utf8>>}
                    end;
                _ ->
                    {error, <<"新群主必须是群成员"/utf8>>}
            end;
        _ ->
            {error, <<"只有群主可以转让群组"/utf8>>}
    end.

%% Audit is part of the ownership transaction; failure must roll back roles too.
-spec save_transfer_log(epgsql:connection() | pid(), map()) -> ok | no_return().
save_transfer_log(Conn, Data) ->
    case group_log_ds:add(Conn, Data) of
        {ok, _} -> ok;
        {error, _Reason} -> throw({abort_tx, <<"群转让审计写入失败"/utf8>>})
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
%% **@deprecated 无生产调用方**（Q05 盘点确认，仅集成测试引用）。生产路径
%% 是 transfer/3 → do_transfer/5。保留供 KeepAsAdmin 语义将来启用时参考；
%% 启用前须与 do_transfer/5 对齐 Q05 通知契约（事务成功后 publish
%% owner_transferred）。
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
            {error, <<"群组不存在"/utf8>>};
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
                    TxRes = elib_pg:with_tx(fun(Conn) ->
                        %% T7 归档写守卫（P0 收口）：转让与守卫同事务
                        ok = workspace_guard:abort_on_error(
                            workspace_guard:ensure_writable_tx(Conn, {group, Gid})
                        ),
                        case group_ds:update_owner_tx(Conn, Gid, NewOwnerUid, OwnerUid) of
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
                                                save_transfer_log(Conn, LogData);
                                            {error, Reason} ->
                                                {error, Reason}
                                        end;
                                    {error, Reason} ->
                                        {error, Reason}
                                end;
                            {error, Reason} ->
                                {error, Reason}
                        end
                    end),
                    %% with_tx 可能返回 {rollback,_}；ds 层错误可能为 atom。
                    %% 统一归一到 {error, binary()} 契约，防止向调用方泄漏。
                    case TxRes of
                        ok ->
                            ok;
                        {error, Reason} when is_binary(Reason) ->
                            {error, Reason};
                        {error, Reason} ->
                            {error, atom_to_binary(Reason, utf8)};
                        {rollback, RollbackReason} ->
                            ?ERROR_LOG([group_transfer_rollback, Gid, RollbackReason]),
                            {error, <<"群转让事务回滚"/utf8>>}
                    end;
                _ ->
                    {error, <<"新群主必须是群成员"/utf8>>}
            end;
        _ ->
            {error, <<"只有群主可以转让群组"/utf8>>}
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
        <<"nickname">> => maps:get(<<"nickname">>, User, <<>>),
        <<"avatar">> => maps:get(<<"avatar">>, User, <<>>),
        <<"account">> => maps:get(<<"account">>, User, <<>>)
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
-spec count_by_owner(integer()) -> non_neg_integer().
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
                            case
                                msg_s2c_ds:send(
                                    Uid,
                                    ToUidLi,
                                    Action,
                                    <<>>,
                                    null,
                                    Data#{<<"gid">> => Gid},
                                    save
                                )
                            of
                                ok ->
                                    ok;
                                {error, Reason} ->
                                    ?WARN_LOG({group_edit_notification_failed, Reason}),
                                    ok
                            end;
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
-spec set_e2ee_mode(integer(), integer(), integer()) ->
    ok | {error, any()}.
%% T7 结构化错误码 {Code, Msg} 经 set_e2ee_mode 原样透传（见实现内注释）。
set_e2ee_mode(_Uid, _Gid, Mode) when Mode =/= 1 ->
    {error, <<"e2ee_mode 仅支持单向开启（0→1）"/utf8>>};
set_e2ee_mode(Uid, Gid, 1) ->
    %% 全局硬闸下拒绝开启：storage_mode=disabled 表示部署整体不使用 E2EE，
    %% 此时放行会造出"全局关着、这个群开着"的裂缝——该群的成员设备会按群标志
    %% 继续加密，而密钥端点已关闭，群消息必然发不出去（且无法从后台回退）。
    case imboy_policy:e2ee_disabled() of
        true ->
            {error, <<"当前部署已关闭端到端加密，无法开启群加密"/utf8>>};
        false ->
            do_set_e2ee_mode(Uid, Gid)
    end.

%% @private 群级 E2EE 开启主体（全局硬闸已在 set_e2ee_mode/3 前置拦截）。
-spec do_set_e2ee_mode(integer(), integer()) -> ok | {error, any()}.
do_set_e2ee_mode(Uid, Gid) ->
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
                    case
                        msg_s2c_ds:send(
                            Uid,
                            ToUidLi,
                            <<"group_e2ee_mode">>,
                            <<>>,
                            null,
                            #{<<"gid">> => Gid, <<"e2ee_mode">> => 1},
                            save
                        )
                    of
                        ok ->
                            ok;
                        {error, Reason} ->
                            ?WARN_LOG({group_e2ee_notification_failed, Reason}),
                            ok
                    end;
                {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                    %% T7 收口：归档守卫（980）等稳定错误码原样透传
                    {error, {Code, Msg}};
                {error, Reason} ->
                    %% 内部 PG 错误详情只进日志，不透传客户端（security-reviewer M1）
                    _ = ?ERROR_LOG([set_e2ee_mode_update_failed, Gid, Reason]),
                    {error, <<"操作失败，请稍后重试"/utf8>>}
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
            case Uid =:= OwnerUid of
                true ->
                    group_ds:dissolve_group(Uid, Gid, OwnerUid, G);
                false ->
                    dissolve_by_org_manager(Uid, Gid, G)
            end
    end.

%% GZAPP-03/D04：群主之外的第二授权源——本群所属 Workspace 的 Organization
%% owner/admin（企业治理者可解散本 org 任意 ws 域群）。
%% R3-5：授权判定**已下沉到 group_ds:dissolve_by_org_manager/3 的事务内**
%% （判定与删除同事务、同 Conn），本层只做转发；拒绝文案与 fail-closed 503
%% 语义不变（在 group_ds 的事务结果映射里）。审计仍走 group_log type=101
%% （option_uid = 实际操作者 uid），消息行不删除。
-spec dissolve_by_org_manager(integer(), integer(), map()) -> ok | {error, binary()}.
dissolve_by_org_manager(Uid, Gid, G) ->
    group_ds:dissolve_by_org_manager(Uid, Gid, G).

%% @doc 分页查询当前用户作为群主的群组列表
%% @param OwnerUid 群主用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, map()} | {error, any()}
-spec page_by_owner(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, any()}.
page_by_owner(OwnerUid, Page, Size) ->
    page_by_owner(OwnerUid, Page, Size, <<>>).

%% W07.a（C2 个人群）：服务端 q 在授权交集内过滤（尾页私有对象可达）。
-spec page_by_owner(integer(), pos_integer(), pos_integer(), binary()) ->
    {ok, map()} | {error, any()}.
page_by_owner(OwnerUid, Page, Size, Q) ->
    group_ds:page_by_owner(OwnerUid, Page, Size, Q).

%% @doc 分页查询当前用户已加入的群组列表
%% @param Uid 用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, map()} | {error, any()}
-spec page_joined(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, any()}.
page_joined(Uid, Page, Size) ->
    page_joined(Uid, Page, Size, <<>>).

%% W07.a（C2 个人群）：服务端 q（attr+scope 交集内 title ILIKE）。
-spec page_joined(integer(), pos_integer(), pos_integer(), binary()) ->
    {ok, map()} | {error, any()}.
page_joined(Uid, Page, Size, Q) ->
    group_ds:page_joined(Uid, Page, Size, Q).

%% @doc 分页查询当前用户管理的群组列表（群主/副群主/管理员）
%% @param Uid 用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, map()} | {error, any()}
-spec page_managed(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, any()}.
page_managed(Uid, Page, Size) ->
    page_managed(Uid, Page, Size, <<>>).

%% W07.a（C2 个人群）：服务端 q（群主/管理角色交集内 title ILIKE）。
-spec page_managed(integer(), pos_integer(), pos_integer(), binary()) ->
    {ok, map()} | {error, any()}.
page_managed(Uid, Page, Size, Q) ->
    group_ds:page_managed(Uid, Page, Size, Q).

%% W07.b（C2 个人群 cursor 协议）：逻辑层游标页。取 Limit+1 判定
%% has_more；rows 走 group_transfer 出站投影（剥 chat_aes_key）；
%% upper=本页末行 id（has_more=false 时置 0）。
-spec page_by_owner_cursor(integer(), integer(), pos_integer(), binary()) ->
    {ok, #{list := [map()], has_more := boolean(), upper := integer()}} | {error, any()}.
page_by_owner_cursor(Uid, BeforeId, Limit, Q) ->
    case group_ds:page_by_owner_keyset(Uid, BeforeId, Limit + 1, Q) of
        {ok, Rows} -> cursor_page_rows(Rows, Limit);
        {error, Reason} -> {error, Reason}
    end.

-spec page_joined_cursor(integer(), integer(), pos_integer(), binary()) ->
    {ok, #{list := [map()], has_more := boolean(), upper := integer()}} | {error, any()}.
page_joined_cursor(Uid, BeforeId, Limit, Q) ->
    case group_ds:page_joined_keyset(Uid, BeforeId, Limit + 1, Q) of
        {ok, Rows} -> cursor_page_rows(Rows, Limit);
        {error, Reason} -> {error, Reason}
    end.

-spec page_managed_cursor(integer(), integer(), pos_integer(), binary()) ->
    {ok, #{list := [map()], has_more := boolean(), upper := integer()}} | {error, any()}.
page_managed_cursor(Uid, BeforeId, Limit, Q) ->
    case group_ds:page_managed_keyset(Uid, BeforeId, Limit + 1, Q) of
        {ok, Rows} -> cursor_page_rows(Rows, Limit);
        {error, Reason} -> {error, Reason}
    end.

-spec cursor_page_rows([map()], pos_integer()) ->
    {ok, #{list := [map()], has_more := boolean(), upper := integer()}}.
cursor_page_rows(Rows, Limit) ->
    Page = [group_transfer(R) || R <- lists:sublist(Rows, Limit)],
    HasMore = length(Rows) > Limit,
    Upper =
        case {HasMore, lists:reverse(Page)} of
            {true, [Last | _]} -> maps:get(<<"id">>, Last);
            _ -> 0
        end,
    {ok, #{list => Page, has_more => HasMore, upper => Upper}}.

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
    % P1-7b: 显式安全列（group_member 全列，无敏感字段）
    group_member_ds:find_by_gid_and_uid(
        Gid,
        Uid,
        <<
            "id,group_id,user_id,invite_code,alias,description,role,is_join,join_mode,status,updated_at,created_at,"
            "remark,mute_until,category_id"
        >>
    ).

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

%% ===================================================================
%% T5 scope 感知函数（双体验 v2.5.2 WP3/T5）
%% ===================================================================

%% @doc scope 感知的群创建（§1.4.2 授权规则：创建 Workspace Group 须为该
%% Workspace 的 Owner/Member；Guest 403；非成员 403）。
%% ScopeCtx：
%%   {personal, 0}     → 完全走既有 add/4 路径（零行为变化）
%%   {workspace, WsId} → 校验角色后带 scope 单事务创建（个人建群亦已无
%%                        语义键幂等——同成员集允许多群，P0 终局见决策文档）
%%   {OtherScope, _}   → 400
%% 初始成员入群时由 group_member_ds:join_group/5 在同事务校验
%% active workspace_member（Group Member ⊆ Workspace Member 应用层双保险，
%% DB 触发器 trg_group_member_ws_subset 是第二道兜底）。
%% scope/workspace_id 创建后不可变（§1.4.2 规则 9，edit_checked 拒绝改）。
-spec add(
    non_neg_integer(), integer(), integer(), [binary()], {binary(), integer()}
) ->
    {ok, integer()} | {error, binary()} | {error, {integer(), binary()}}.
add(_Count, _Uid, _Type, _MemberUids, {Scope, _WorkspaceId}) when
    Scope =/= <<"personal">>, Scope =/= <<"workspace">>
->
    {error, {400, <<"scope 仅支持 personal|workspace"/utf8>>}};
add(_Count, _Uid, _Type, _MemberUids, {<<"workspace">>, WorkspaceId}) when
    is_integer(WorkspaceId) =:= false orelse WorkspaceId =< 0
->
    {error, {400, <<"scope=workspace 时 workspace_id 必须"/utf8>>}};
add(Count, Uid, Type, MemberUids, {<<"workspace">>, WorkspaceId}) ->
    case workspace_logic:ensure_can_create_resource(WorkspaceId, Uid) of
        {error, {Code, Msg}} ->
            {error, {Code, Msg}};
        ok when Count > 100 ->
            {error, <<"每人最多创建100个群"/utf8>>};
        ok ->
            MemberUids2 =
                case MemberUids of
                    List when is_list(List) -> List;
                    _ -> []
                end,
            MemberUids3 = [ec_cnv:to_integer(Id) || Id <- MemberUids2, is_binary(Id)],
            %% 【防御性编程】确保创建者不会被重复添加
            MemberUids4 = lists:usort([U || U <- MemberUids3, U =/= Uid]),
            workspace_add_tx(Uid, Type, MemberUids4, WorkspaceId)
    end;
add(Count, Uid, Type, MemberUids, {<<"personal">>, _WorkspaceId}) ->
    add(Count, Uid, Type, MemberUids).

-spec workspace_add_tx(integer(), integer(), [integer()], integer()) ->
    {ok, integer()} | {error, binary() | pos_integer() | {pos_integer(), binary()}}.
workspace_add_tx(Uid, Type, MemberUids4, WorkspaceId) ->
    Now = elib_dt:now(),
    case
        elib_pg:with_tx(fun(Conn) ->
            %% T7 归档写守卫（P0 收口）：workspace 域建群 = 对 workspace 的写入，
            %% archived 时整事务拒绝（{workspace, WsId} 行锁与建群同事务；
            %% 镜像 project_ds:create 模板）。
            ok = workspace_guard:abort_on_error(
                workspace_guard:ensure_writable_tx(Conn, {workspace, WorkspaceId})
            ),
            Gid = elib_tsid:generate(group_info),
            Gid2 = group_ds:create_scoped_group(
                Conn, Gid, Uid, Now, Type, <<"workspace">>, WorkspaceId
            ),
            %% 初始成员入群：非 active workspace_member 的成员在 DS 层
            %% throw({abort_tx, workspace_membership_required}) 整体回滚
            lists:foreach(
                fun(Uid2) ->
                    join_initial_member(Conn, <<"workspace_group_create">>, Uid2, Gid2)
                end,
                MemberUids4
            ),
            {ok, Gid2}
        end)
    of
        {ok, Gid2} ->
            {ok, Gid2};
        {error, {?ERR_WORKSPACE_ARCHIVED, _Msg}} ->
            {error, ?ERR_WORKSPACE_ARCHIVED};
        {error, workspace_membership_required} ->
            {error, {409, <<"workspace_membership_required：初始成员必须先是该工作区的 active 工作区成员"/utf8>>}};
        {error, Reason} ->
            ?ERROR_LOG([workspace_group_create_failed, Uid, WorkspaceId, Reason]),
            {error, <<"群创建失败，请稍后重试"/utf8>>}
    end.

%% ===================================================================
%% W04.b（channel-group-ux v2.2 §6.4 C1）：带受理回执的群创建与重放
%% ===================================================================

%% @doc 带受理回执的群创建（新合同入口，仅当请求携带非空 request_id）。
%%
%% 身份唯一键 (Uid, create_group, RequestId)。事务序列（§6.4）：advisory
%% 串行化 → receipt 占位 → 资源（含初始名 title）→ 去重成员/世代 →
%% receipt 回填（resource_id/server_accepted_at）→ 必要持久通知入队
%% （msg_s2c_ds:send_tx，提交后投递）。任何一步失败全部回滚（J23.a：
%% 同键可再次首次受理）；重放（占位命中既有受理）不再执行资源创建与
%% 通知入队（J23.b/c）。服务端 7d 年龄独立决定完整结果（J24.b）。
%%
%% 结果形状（handler 映射稳定错误符号）：
%%   {ok, Gid, accepted | replayed}                        完整结果（<7d 且资格有效）
%%   {error, request_id_conflict}                          同键不同规范载荷
%%   {error, create_not_accepted}                          recover_only 只查未见受理
%%   {error, {create_recovery_expired, Gid}}               >=7d：仅核对元信息
%%   {error, {create_resource_unavailable, Gid|undefined}} 删除/撤权：不泄露内容
%%   {error, {Code, Msg}} / {error, Msg}                   既有校验/系统错误形状
-spec add_with_receipt(
    non_neg_integer(),
    integer(),
    integer(),
    [binary()],
    {binary(), integer()},
    binary(),
    binary(),
    boolean()
) ->
    {ok, integer(), accepted | replayed}
    | {error, request_id_conflict | create_not_accepted | binary()}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}
    | {error, {integer(), binary()}}.
add_with_receipt(
    _Count, _Uid, _Type, _MemberUids, {Scope, _}, _RequestId, _Title, _RecoverOnly
) when
    Scope =/= <<"personal">>, Scope =/= <<"workspace">>
->
    {error, {400, <<"scope 仅支持 personal|workspace"/utf8>>}};
add_with_receipt(
    _Count, _Uid, _Type, _MemberUids, {<<"workspace">>, WorkspaceId}, _RequestId, _Title, _RO
) when
    is_integer(WorkspaceId) =:= false orelse WorkspaceId =< 0
->
    {error, {400, <<"scope=workspace 时 workspace_id 必须"/utf8>>}};
add_with_receipt(Count, Uid, Type, MemberUids, {Scope, WorkspaceId}, RequestId, Title, RecoverOnly) ->
    case normalize_receipt_payload(Uid, MemberUids, Title) of
        {error, Reason} ->
            {error, Reason};
        {ok, Norm} ->
            Digest = resource_create_receipt_repo:canonical_group_digest(
                Norm#{scope => Scope, workspace_id => WorkspaceId}
            ),
            case RecoverOnly of
                true ->
                    recover_only_lookup(Uid, RequestId, Digest);
                false ->
                    case resource_create_receipt_repo:find(Uid, <<"create_group">>, RequestId) of
                        {ok, Row} ->
                            replay_receipt(Digest, Row, Uid);
                        {error, not_found} ->
                            first_accept_with_receipt(
                                Count, Uid, Type, Norm, Scope, WorkspaceId, RequestId, Digest
                            );
                        {error, Reason} ->
                            ?ERROR_LOG([group_receipt_find_failed, Uid, RequestId, Reason]),
                            {error, <<"创建受理查询失败，请稍后重试"/utf8>>}
                    end
            end
    end.

%% 规范化（§6.4 规范形式）：标题 Unicode 原值仅 trim（不大小写/NFKC）、
%% 长度 <=200 字符；成员沿用既有 add/4 的二进制十进制转换，随后去重、
%% 移除创建者、升序数字排序——摘要与实际创建共用同一份规范化结果。
-spec normalize_receipt_payload(integer(), [binary()], binary()) ->
    {ok, #{title := binary(), members := [integer()]}} | {error, {400, binary()}}.
normalize_receipt_payload(Uid, MemberUids, Title) ->
    MemberUids2 =
        case MemberUids of
            List when is_list(List) -> List;
            _ -> []
        end,
    Members = lists:usort([
        U
     || U <- [ec_cnv:to_integer(Id) || Id <- MemberUids2, is_binary(Id)],
        U =/= Uid
    ]),
    Title2 = string:trim(Title),
    case string:length(Title2) =< 200 of
        false ->
            {error, {400, <<"群名称长度不能超过200个字符"/utf8>>}};
        true ->
            {ok, #{title => Title2, members => Members}}
    end.

%% 首次受理前置校验（与 add/5 同序：scope 合法性 → workspace 资格 → 数量）。
-spec first_accept_with_receipt(
    non_neg_integer(),
    integer(),
    integer(),
    map(),
    binary(),
    integer(),
    binary(),
    binary()
) ->
    {ok, integer(), accepted | replayed}
    | {error, binary()}
    | {error, {integer(), binary()}}
    | {error, request_id_conflict | create_not_accepted | binary()}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}.
first_accept_with_receipt(
    Count, _Uid, _Type, _Norm, <<"personal">>, _WsId, _RequestId, _Digest
) when
    Count > 100
->
    {error, <<"每人最多创建100个群"/utf8>>};
first_accept_with_receipt(Count, Uid, Type, Norm, <<"workspace">>, WorkspaceId, RequestId, Digest) ->
    case workspace_logic:ensure_can_create_resource(WorkspaceId, Uid) of
        {error, {Code, Msg}} ->
            {error, {Code, Msg}};
        ok when Count > 100 ->
            {error, <<"每人最多创建100个群"/utf8>>};
        ok ->
            run_first_accept_tx(Uid, Type, Norm, <<"workspace">>, WorkspaceId, RequestId, Digest)
    end;
first_accept_with_receipt(_Count, Uid, Type, Norm, <<"personal">>, _WsId, RequestId, Digest) ->
    run_first_accept_tx(Uid, Type, Norm, <<"personal">>, 0, RequestId, Digest).

%% 首次受理事务：占位 → 资源+初始名 → 成员 → 回填 → 通知入队（同一 Conn）。
-spec run_first_accept_tx(integer(), integer(), map(), binary(), integer(), binary(), binary()) ->
    {ok, integer(), accepted | replayed}
    | {error, binary()}
    | {error, {integer(), binary()}}
    | {error, request_id_conflict | create_not_accepted | binary()}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}.
run_first_accept_tx(Uid, Type, Norm, Scope, WorkspaceId, RequestId, Digest) ->
    Now = elib_dt:now(),
    UserTitle = user_ds:title(Uid),
    case
        elib_pg:with_tx(fun(Conn) ->
            ok = abort_on_receipt_repo_error(
                resource_create_receipt_repo:serialize_tx(Conn, Uid, <<"create_group">>, RequestId)
            ),
            case
                resource_create_receipt_repo:placeholder_tx(
                    Conn, Uid, <<"create_group">>, RequestId, Digest
                )
            of
                {ok, inserted, _PlaceholderRow} ->
                    Gid = create_group_in_receipt_tx(
                        Conn, Uid, Type, Norm, Scope, WorkspaceId, Now
                    ),
                    Members = maps:get(members, Norm),
                    JoinMode = receipt_join_mode(Scope, Uid, UserTitle),
                    lists:foreach(
                        fun(Uid2) -> join_initial_member(Conn, JoinMode, Uid2, Gid) end,
                        Members
                    ),
                    {ok, _Row} = accept_receipt_or_abort(
                        resource_create_receipt_repo:accept_tx(
                            Conn, Uid, <<"create_group">>, RequestId, Gid
                        )
                    ),
                    ok = enqueue_receipt_notifications_tx(
                        Conn, Uid, Gid, <<"create_group">>, RequestId, Members
                    ),
                    {ok, Gid};
                {ok, existing, Row} ->
                    %% 同键并发前事务已提交：事务内重放，不再创建/入队。
                    replay_receipt(Digest, Row, Uid);
                {error, Reason} ->
                    throw({abort_tx, {receipt_placeholder_failed, Reason}})
            end
        end)
    of
        {ok, Gid} ->
            {ok, Gid, accepted};
        %% 事务内占位命中既有受理（同键并发前事务已提交）：重放结果原样透传。
        {ok, _Gid, replayed} = Replayed ->
            Replayed;
        {error, request_id_conflict} = Conflict ->
            Conflict;
        {error, {create_recovery_expired, _}} = Expired ->
            Expired;
        {error, {create_resource_unavailable, _}} = Unavailable ->
            Unavailable;
        {error, {?ERR_WORKSPACE_ARCHIVED, _Msg}} ->
            {error, ?ERR_WORKSPACE_ARCHIVED};
        {error, workspace_membership_required} ->
            {error, {409, <<"workspace_membership_required：初始成员必须先是该工作区的 active 工作区成员"/utf8>>}};
        {error, Reason} ->
            ?ERROR_LOG([group_receipt_create_failed, Uid, RequestId, Reason]),
            {error, <<"群创建失败，请稍后重试"/utf8>>};
        {error, throw, ThrowReason} ->
            ?ERROR_LOG([group_receipt_create_throw, Uid, RequestId, ThrowReason]),
            {error, <<"群创建失败，请稍后重试"/utf8>>}
    end.

%% 受理事务内的群创建（镜像 add/4 与 workspace_add_tx 的既有分工：
%% personal 预置 join_limit=1，workspace 走归档写守卫 + scoped 创建），
%% 外加初始名 title 与群行同事务（§6.4：群初始名称目标字段固定为 title）。
-spec create_group_in_receipt_tx(pid(), integer(), integer(), map(), binary(), integer(), binary()) ->
    integer().
create_group_in_receipt_tx(Conn, Uid, Type, Norm, <<"personal">>, _WorkspaceId, Now) ->
    Gid = group_ds:create_group(Conn, 0, Uid, Now, Type, 1),
    ok = set_initial_title_tx(Conn, Gid, maps:get(title, Norm)),
    Gid;
create_group_in_receipt_tx(Conn, Uid, Type, Norm, <<"workspace">>, WorkspaceId, Now) ->
    ok = workspace_guard:abort_on_error(
        workspace_guard:ensure_writable_tx(Conn, {workspace, WorkspaceId})
    ),
    Gid0 = elib_tsid:generate(group_info),
    Gid = group_ds:create_scoped_group(Conn, Gid0, Uid, Now, Type, <<"workspace">>, WorkspaceId),
    ok = set_initial_title_tx(Conn, Gid, maps:get(title, Norm)),
    Gid.

%% 初始群名：空标题保持既有默认 ''（与旧路径零差异），不产生额外 UPDATE。
-spec set_initial_title_tx(pid(), integer(), binary()) -> ok.
set_initial_title_tx(_Conn, _Gid, <<>>) ->
    ok;
set_initial_title_tx(Conn, Gid, Title) ->
    case group_repo:update_by_id_tx(Conn, Gid, #{<<"title">> => Title}) of
        {ok, 1} ->
            ok;
        Other ->
            throw({abort_tx, {initial_title_failed, Other}})
    end.

%% 受理通知（J23.c）：初始成员收 group_member_join（载荷与既有
%% notify_member_join 形状一致）；MsgId 由 (op,request_id,effect_kind,
%% target) 确定派生（msg_s2c_ds:send_tx），入队失败整体回滚。
-spec enqueue_receipt_notifications_tx(pid(), integer(), integer(), binary(), binary(), [integer()]) ->
    ok.
enqueue_receipt_notifications_tx(_Conn, _Uid, _Gid, _Operation, _RequestId, []) ->
    ok;
enqueue_receipt_notifications_tx(Conn, Uid, Gid, Operation, RequestId, Members) ->
    User = user_ds:find_by_id(Uid, <<"account,avatar,nickname">>),
    Payload = #{
        <<"gid">> => Gid,
        <<"nickname">> => maps:get(<<"nickname">>, User, <<>>),
        <<"avatar">> => maps:get(<<"avatar">>, User, <<>>),
        <<"account">> => maps:get(<<"account">>, User, <<>>)
    },
    EffectKey = <<Operation/binary, "|", RequestId/binary, "|group_member_join">>,
    case msg_s2c_ds:send_tx(Conn, Uid, Members, <<"group_member_join">>, Payload, EffectKey) of
        ok ->
            ok;
        {error, Reason} ->
            throw({abort_tx, {receipt_notify_stage_failed, Reason}})
    end.

-spec receipt_join_mode(binary(), integer(), binary()) -> binary().
receipt_join_mode(<<"personal">>, Uid, UserTitle) ->
    <<"invite_", (ec_cnv:to_binary(Uid))/binary, "_", UserTitle/binary>>;
receipt_join_mode(<<"workspace">>, _Uid, _UserTitle) ->
    <<"workspace_group_create">>.

%% recover_only 只查：不新增受理占位/资源/通知（§6.4 恢复状态表）。
-spec recover_only_lookup(integer(), binary(), binary()) ->
    {ok, integer(), replayed}
    | {error, request_id_conflict | create_not_accepted | binary()}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}.
recover_only_lookup(Uid, RequestId, Digest) ->
    case resource_create_receipt_repo:find(Uid, <<"create_group">>, RequestId) of
        {ok, Row} ->
            replay_receipt(Digest, Row, Uid);
        {error, not_found} ->
            %% 仅表示查询时未见已提交受理，不证明在途请求不会迟到（§6.4）；
            %% 未知意图不提供换键恢复。
            {error, create_not_accepted};
        {error, Reason} ->
            ?ERROR_LOG([group_receipt_recover_lookup_failed, Uid, RequestId, Reason]),
            {error, <<"创建受理查询失败，请稍后重试"/utf8>>}
    end.

%% 重放判定：先比对规范摘要（同键不同载荷 → 冲突，不剥键不换键），
%% 再按删除/7d/资格分派结果（J24.b/c）。
-spec replay_receipt(binary(), map(), integer()) ->
    {ok, integer(), replayed}
    | {error, request_id_conflict}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}.
replay_receipt(Digest, Row, _Uid) ->
    case maps:get(<<"request_digest">>, Row) =:= Digest of
        false ->
            {error, request_id_conflict};
        true ->
            replay_outcome(Row)
    end.

-spec replay_outcome(map()) ->
    {ok, integer(), replayed}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}.
replay_outcome(Row) ->
    case maps:get(<<"resource_id">>, Row) of
        Gid when is_integer(Gid), Gid > 0 ->
            Expired = maps:get(<<"expired">>, Row) =:= true,
            Deleted = maps:get(<<"resource_deleted">>, Row) =:= true,
            replay_decision(Gid, Deleted, Expired, maps:get(<<"uid">>, Row));
        _ ->
            %% 防御：committed 行恒有 resource_id（占位与回填同事务）；异常按不可用。
            {error, {create_resource_unavailable, undefined}}
    end.

%% 删除例外优先：不泄露名称/成员、不复活对象；receipt 保留已受理事实、
%% 不把错误改为首次创建（J24.b）。7d 过窗：只给可用于核对的 ID/状态。
%% 完整结果还需群存活 + 调用者仍是 active 成员（撤权统一不可用）。
-spec replay_decision(integer(), boolean(), boolean(), integer()) ->
    {ok, integer(), replayed}
    | {error, {create_recovery_expired | create_resource_unavailable, integer()}}.
replay_decision(Gid, true = _Deleted, _Expired, _Uid) ->
    {error, {create_resource_unavailable, Gid}};
replay_decision(Gid, false, true = _Expired, _Uid) ->
    {error, {create_recovery_expired, Gid}};
replay_decision(Gid, false, false, Uid) ->
    case group_receipt_resource_alive(Uid, Gid) of
        true ->
            {ok, Gid, replayed};
        false ->
            {error, {create_resource_unavailable, Gid}}
    end.

-spec group_receipt_resource_alive(integer(), integer()) -> boolean().
group_receipt_resource_alive(Uid, Gid) ->
    case find_by_id(Gid, <<"status">>) of
        #{<<"status">> := 1} ->
            group_member_ds:is_member(Gid, Uid);
        _ ->
            false
    end.

-spec abort_on_receipt_repo_error(ok | {error, term()}) -> ok.
abort_on_receipt_repo_error(ok) ->
    ok;
abort_on_receipt_repo_error({error, Reason}) ->
    throw({abort_tx, {receipt_repo_error, Reason}}).

-spec accept_receipt_or_abort({ok, map()} | {error, term()}) -> {ok, map()}.
accept_receipt_or_abort({ok, Row}) ->
    {ok, Row};
accept_receipt_or_abort({error, Reason}) ->
    throw({abort_tx, {receipt_accept_failed, Reason}}).

%% @doc 更新入口守卫：scope 与 workspace_id 创建后不可变（§1.4.2 规则 9）。
%% 提交了这两个字段之一即拒绝（不静默忽略，防止客户端误以为已改归属）。
-spec edit_checked(integer(), integer(), map()) ->
    ok | {error, binary()} | {error, {400, binary()}}.
edit_checked(Uid, Gid, Data) ->
    case maps:is_key(<<"scope">>, Data) orelse maps:is_key(<<"workspace_id">>, Data) of
        true ->
            {error, {400, <<"scope 与 workspace_id 创建后不可修改"/utf8>>}};
        false ->
            edit(Uid, Gid, Data)
    end.

%% @doc 工作区群列表（scope 严格分区：仅 scope='workspace' 且 status=1；
%% personal 群列表接口零行为变化）。命中部分索引 i_group_scope_ws。
-spec list_workspace_groups(integer(), integer()) -> {ok, [map()]} | {error, binary()}.
list_workspace_groups(WorkspaceId, Limit) ->
    list_workspace_groups(WorkspaceId, Limit, <<>>).

%% W07.a（C2 工作区目录）：可选服务端 q（目录授权内 title ILIKE）。
-spec list_workspace_groups(integer(), integer(), binary()) ->
    {ok, [map()]} | {error, binary()}.
list_workspace_groups(WorkspaceId, Limit, Q) ->
    list_workspace_groups(WorkspaceId, Limit, Q, 0).

%% W07.b（C2 工作区目录 cursor 协议）：BeforeId>0 为 keyset 上界。
-spec list_workspace_groups_before(integer(), integer(), integer(), binary()) ->
    {ok, [map()]} | {error, binary()}.
list_workspace_groups_before(WorkspaceId, BeforeId, Limit, Q) ->
    list_workspace_groups(WorkspaceId, Limit, Q, BeforeId).

-spec list_workspace_groups(integer(), integer(), binary(), integer()) ->
    {ok, [map()]} | {error, binary()}.
list_workspace_groups(WorkspaceId, Limit, Q, BeforeId) ->
    case workspace_ds:groups(WorkspaceId, Limit, Q, BeforeId) of
        {ok, Rows} ->
            {ok, [group_transfer(R) || R <- Rows]};
        {error, Reason} ->
            ?ERROR_LOG([list_workspace_groups_failed, WorkspaceId, Reason]),
            {error, <<"查询失败，请稍后重试"/utf8>>}
    end.

%% 查询资格在 SQL 中再次校验，不以工作区目录代替群成员关系。
list_member_workspace_groups(WorkspaceId, Uid, AfterId, Limit) ->
    list_member_workspace_groups(WorkspaceId, Uid, AfterId, Limit, false).

list_member_workspace_groups(WorkspaceId, Uid, AfterId, Limit, Preview) ->
    list_member_workspace_groups(WorkspaceId, Uid, AfterId, Limit, Preview, <<>>).

%% W07.a（C2 成员群子集）：可选服务端 q（资格 SQL 内 title ILIKE）。
list_member_workspace_groups(WorkspaceId, Uid, AfterId, Limit, Preview, Q) when
    is_boolean(Preview),
    is_integer(AfterId),
    AfterId >= 0,
    AfterId =< 9223372036854775807,
    is_integer(Limit),
    Limit > 0,
    Limit =< 200
->
    case workspace_ds:member_groups(WorkspaceId, Uid, AfterId, Limit + 1, Preview, Q) of
        {ok, Rows} ->
            Page = lists:sublist(Rows, Limit),
            HasMore = length(Rows) > Limit,
            %% 模式匹配取末行：HasMore=true 时 Page 必非空，
            %% 但不依赖该推论（Limit=0 等退化入参下 lists:last 会崩）。
            %% 游标契约与 channel 列表一致：末页（has_more=false）必须回 0，
            %% 客户端（WorkspaceApi._memberGroupRows）以 0 为终止标记。
            Next =
                case lists:reverse(Page) of
                    [LastRow | _] when HasMore -> maps:get(<<"id">>, LastRow);
                    _ -> 0
                end,
            {ok, #{
                list => [
                    group_transfer(elib_response:json_decode_field(R, <<"latest_message">>))
                 || R <- Page
                ],
                has_more => HasMore,
                next_cursor => Next
            }};
        {error, Reason} ->
            ?ERROR_LOG([member_workspace_groups_failed, WorkspaceId, Reason]),
            {error, <<"查询失败，请稍后重试"/utf8>>}
    end;
list_member_workspace_groups(_, _, _, _, _, _) ->
    {error, <<"分页参数无效"/utf8>>}.

mark_workspace_read(Uid, Gid, MsgIds) when
    is_integer(Uid),
    Uid > 0,
    is_integer(Gid),
    Gid > 0,
    Gid =< 9223372036854775807,
    is_list(MsgIds),
    length(MsgIds) > 0,
    length(MsgIds) =< 200
->
    case
        lists:all(
            fun(Id) -> is_binary(Id) andalso byte_size(Id) > 0 andalso byte_size(Id) =< 128 end,
            MsgIds
        )
    of
        false ->
            {error, {400, <<"invalid_message_ids">>}};
        true ->
            case workspace_ds:mark_group_read(Uid, Gid, lists:usort(MsgIds)) of
                {ok, [Row]} -> {ok, Row};
                {ok, []} -> {error, {404, <<"resource_not_found">>}};
                {error, _} -> {error, {500, <<"internal_error">>}}
            end
    end;
mark_workspace_read(_, _, _) ->
    {error, {400, <<"invalid_message_ids">>}}.
