-module(user_ds).

%%%
% user 领域服务模块
% user domain service 缩写
%%%

-export([webrtc_credential/1]).
-export([title/1]).
-export([title/2]).
-export([auth_webrtc_credential/2]).

%% 用户数据操作导出
-export([find_by_id/2]).
-export([find_by_mobile/2]).
-export([find_by_email/2]).
-export([find_by_account/2]).
-export([list_by_ids/2]).
-export([count/0]).
-export([insert/2]).
-export([update/2]).
-export([update_field/3]).
-export([update_status/2]).
-export([update_friends_last_seen_at/2]).
-export([delete_all_related_data/1]).
-export([insert_and_get_id/1]).
-export([bind_email/2]).
-export([get_status/1]).
-export([find_id_by_email/1]).
-export([find_id_by_mobile/1]).
-export([update_password/2]).
-export([update_status_in_tx/2]).
-export([update_password_in_tx/2]).
-export([update_allow_search/2]).
-export([may_exist/1]).
-export([reject_logout_apply/1]).
-export([approve_logout_apply/1]).
-export([batch_online_state/1]).
-export([page/4]).
-export([export_data/1]).
-export([find_expired_logout_users/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取用户显示名称
%% 根据用户ID获取用户的显示名称。如果用户设置了昵称则返回昵称，
%% 否则返回用户账号。
%% @param Uid 用户ID（整数）
%% @returns 用户显示名称（昵称优先，否则返回账号）
-spec title(pos_integer() | binary()) -> binary().
title(Uid) when is_binary(Uid) ->
    title(ec_cnv:to_integer(Uid));
title(Uid) when is_integer(Uid) ->
    U = user_repo:find_by_id(Uid, <<"account,nickname">>),
    #{<<"account">> := Account, <<"nickname">> := Nickname} = U,
    case {Account, Nickname} of
        {_, <<>>} ->
            Account;
        _ ->
            Nickname
    end.

%% @doc 获取用户显示名称和昵称（模式2）
%% 返回用户的显示名称和昵称的组合。显示名称的规则与title/1相同，
%% 但额外返回昵称信息供调用方使用。
%% @param Uid 用户ID（整数）
%% @param Mode 模式参数，当前只支持2
%% @returns {显示名称, 昵称}的元组
-spec title(pos_integer() | binary(), 2) -> {binary(), binary()}.
title(Uid, 2) when is_binary(Uid) ->
    title(ec_cnv:to_integer(Uid), 2);
title(Uid, 2) when is_integer(Uid) ->
    U = user_repo:find_by_id(Uid, <<"account,nickname">>),
    #{<<"account">> := Account, <<"nickname">> := Nickname} = U,
    Title =
        case {Account, Nickname} of
            {_, <<>>} ->
                Account;
            _ ->
                Nickname
        end,
    {Title, Nickname}.

%% @doc 生成WebRTC认证凭据
%% 为指定用户生成WebRTC连接所需的认证信息，包括TURN/STUN服务器配置。
%% 生成的凭据有效期为24小时，使用HMAC-SHA算法进行签名。
%% @param Uid 用户ID
%% @returns 包含WebRTC连接信息的map，包含ttl、服务器地址、用户名和凭据
-spec webrtc_credential(pos_integer()) -> map().
webrtc_credential(Uid) ->
    TurnUrls = config_ds:env(eturnal_turn_urls, []),
    StunUrls = config_ds:env(eturnal_stun_urls, []),
    case {TurnUrls, config_ds:env(eturnal_secret, <<>>)} of
        {[_ | _], <<>>} ->
            %% TURN 地址已配置但 secret 为空 — 拒绝生成可被伪造的凭据
            #{
                <<"error">> => <<"eturnal_secret_not_configured">>,
                <<"stun_urls">> => StunUrls
            };
        {_, Secret} ->
            UidBin = integer_to_binary(Uid),
            TmBin = integer_to_binary(elib_dt:utc(second) + 86400),
            Username = <<TmBin/binary, ":", UidBin/binary>>,
            Credential =
                base64:encode(
                    crypto:mac(hmac, sha, Secret, Username)
                ),
            #{
                <<"ttl">> => 86400,
                <<"turn_urls">> => TurnUrls,
                <<"stun_urls">> => StunUrls,
                <<"username">> => Username,
                <<"credential">> => Credential
            }
    end.

%% @doc 验证WebRTC凭据
%% 验证用户提供的WebRTC凭据是否有效。
%% 通过重新计算HMAC并与提供的凭据比较来验证身份。
%%
%% 使用示例：
%% user_ds:auth_webrtc_credential(<<"1728601800:p25vd5">>, <<"B9pddqnbi55R4Mn4JC85Qk1l7T0=">>).
%% @param Username WebRTC用户名（格式：时间戳:编码的用户ID）
%% @param Credential Base64编码的HMAC凭据
%% @returns 验证结果：true表示凭据有效，false表示无效
-spec auth_webrtc_credential(binary(), binary()) -> boolean().
auth_webrtc_credential(Username, Credential) ->
    case config_ds:env(eturnal_secret, <<>>) of
        <<>> ->
            %% secret 未配置时拒绝所有凭据（空 key 的 HMAC 结果可被任意伪造）
            false;
        Secret ->
            Credential ==
                base64:encode(
                    crypto:mac(hmac, sha, Secret, Username)
                )
    end.

%% @doc 根据ID查找用户
-spec find_by_id(integer(), binary()) -> map().
find_by_id(Uid, Column) ->
    user_repo:find_by_id(Uid, Column).

%% @doc 根据手机号查找用户
-spec find_by_mobile(binary() | string(), binary()) -> map().
find_by_mobile(Mobile, Column) ->
    user_repo:find_by_mobile(Mobile, Column).

%% @doc 根据邮箱查找用户
-spec find_by_email(binary(), binary()) -> map().
find_by_email(Email, Column) ->
    user_repo:find_by_email(Email, Column).

%% @doc 根据账号查找用户
-spec find_by_account(binary() | string(), binary()) -> map().
find_by_account(Account, Column) ->
    user_repo:find_by_account(Account, Column).

%% @doc 批量查找用户
-spec list_by_ids([integer()], binary()) -> {ok, list(map())} | {error, any()}.
list_by_ids(Ids, Column) ->
    user_repo:list_by_ids(Ids, Column).

%% @doc 插入新用户
-spec insert(map(), binary()) -> {ok, any()} | {error, any()}.
insert(Data, Returning) ->
    Tb = user_repo:tablename(),
    elib_pg:insert(Tb, Data, Returning).

%% @doc 更新用户信息
%% @param Uid 用户ID
%% @param Data 包含要更新字段的map
%% @return {ok, Count} 更新成功 | {error, Reason} 更新失败
%% @example user_ds:update(1, #{nickname => <<"新昵称"/utf8>>}).
-spec update(integer(), map()) -> {ok, any()} | {error, any()}.
update(Uid, Data) ->
    user_repo:update(Uid, Data).

%% @doc 更新用户单个字段
%% @param Uid 用户ID
%% @param Field 字段名
%% @param Val 字段值
%% @return {ok, integer()} | {error, any()}
-spec update_field(integer(), binary(), binary() | integer()) -> {ok, integer()} | {error, any()}.
update_field(Uid, Field, Val) when is_binary(Val) ->
    Tb = user_repo:tablename(),
    elib_pg:update(Tb, #{Field => Val}, <<"id = $1">>, [Uid]);
update_field(Uid, Field, Val) when is_integer(Val) ->
    Tb = user_repo:tablename(),
    elib_pg:update(Tb, #{Field => Val}, <<"id = $1">>, [Uid]).

%% @doc 更新用户状态
%% @param Uid 用户ID
%% @param Status 状态值 (-1: 删除, 0: 禁用, 1: 启用, 2: 申请注销中)
%% @return {ok, integer()} | {error, any()}
-spec update_status(integer(), integer()) -> {ok, integer()} | {error, any()}.
update_status(Uid, Status) when Status >= -1, Status =< 2 ->
    Tb = user_repo:tablename(),
    elib_pg:update(Tb, #{<<"status">> => Status}, <<"id = $1">>, [Uid]).

%% @doc 更新指定用户的所有好友关系中的最后在线时间
%% @param Uid 用户ID
%% @param Timestamp 要更新的时间戳（timestamptz格式）
%% @return ok
%% @details 该函数会更新用户作为from_user_id和to_user_id的所有好友关系记录
-spec update_friends_last_seen_at(integer(), binary()) -> ok.
update_friends_last_seen_at(Uid, Timestamp) ->
    user_repo:update_friends_last_seen_at(Uid, Timestamp).

%% @doc 删除用户相关的所有数据
%% 该函数会删除用户的所有关联数据，包括好友关系、设备信息、群组成员等。
%% 注意：此操作是破坏性的，通常只在用户注销账户时使用。
%% @param Uid 用户ID
%% @return ok
-spec delete_all_related_data(integer()) -> ok | {rollback, term()}.
delete_all_related_data(Uid) ->
    % 使用事务删除所有相关数据
    elib_pg:with_tx(fun(Conn) ->
        delete_all_related_data(Conn, Uid)
    end).

%% @doc 删除用户相关的所有数据（事务版本）
%% @private
-spec delete_all_related_data(pid(), integer()) -> ok.
delete_all_related_data(Conn, Uid) ->
    % 旧测试夹具会反复复用手机号。这里优先删除依赖表，再删除 user 主表，
    % 同时兼容当前 schema 中已经移除的历史表，避免清理阶段因缺表中断。
    ok = delete_from_table_if_exists(Conn, user_collect_repo:tablename(), <<"user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, user_denylist_repo:tablename(), <<"user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, user_device_repo:tablename(), <<"user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, user_setting_repo:tablename(), <<"user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, user_tag_repo:tablename(), <<"user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(
        Conn, user_tag_relation_repo:tablename(), <<"user_id = $1">>, [Uid]
    ),
    ok = delete_from_table_if_exists(Conn, <<"public.user_token">>, <<"uid = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, <<"public.user_group">>, <<"user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, group_category_repo:tablename(), <<"user_id = $1">>, [
        Uid
    ]),
    ok = delete_from_table_if_exists(Conn, <<"public.fts_user">>, <<"user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(
        Conn, geo_people_nearby_repo:tablename(), <<"user_id = $1">>, [Uid]
    ),
    ok = delete_from_table_if_exists(Conn, friend_repo:tablename(), <<"from_user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, friend_repo:tablename(), <<"to_user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(
        Conn, <<"public.user_friend_category">>, <<"owner_user_id = $1">>, [Uid]
    ),
    ok = delete_from_table_if_exists(Conn, group_repo:tablename(), <<"owner_uid = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, group_member_repo:tablename(), <<"user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(
        Conn, group_random_code_repo:tablename(), <<"user_id = $1">>, [Uid]
    ),
    % E2EE 级联清理（被遗忘权）：云端加密备份按 uid 清理。
    % 自研社交恢复分片/可信联系人/设备传输会话已下线，相关表随迁移下线。
    ok = delete_from_table_if_exists(Conn, <<"public.e2ee_local_backups">>, <<"uid = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, <<"public.e2ee_key_backups">>, <<"uid = $1">>, [Uid]),
    % Olm 材料必须在这里显式删：上面删 user_device 走的是直接 SQL，不经
    % user_device_ds:delete/2，因此设备吊销级联在注销路径**不会触发**。
    % 不删的后果与设备吊销同类且更严重——账号都注销了，别人仍能 claim 到它的
    % one-time key，并与一个不存在的账号建立 Olm 会话。
    ok = delete_from_table_if_exists(Conn, <<"public.olm_identity">>, <<"user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, <<"public.olm_one_time_key">>, <<"user_id = $1">>, [Uid]),
    ok = delete_from_table_if_exists(Conn, <<"public.olm_fallback_key">>, <<"user_id = $1">>, [Uid]),
    % ⚠️ 未清 public.trust_audit（设备信任决策审计流水，含 actor_uid/target_uid）。
    % 审计留存与被遗忘权的取舍是政策判断而非代码判断，需显式拍板后再动，
    % 不在此处默默删除。见 standard/gap-matrix.md E1。
    ok = delete_from_table_if_exists(Conn, user_repo:tablename(), <<"id = $1">>, [Uid]),
    ok.

%% @doc 从表中删除数据的辅助函数
%% @private
-spec delete_from_table(pid(), binary(), binary(), list()) -> ok.
delete_from_table(Conn, Table, WhereSql, Params) ->
    Sql = <<"DELETE FROM ", Table/binary, " WHERE ", WhereSql/binary>>,
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok;
        {error, Reason} ->
            erlang:error({delete_from_table_failed, Table, Reason})
    end.

-spec delete_from_table_if_exists(pid(), binary(), binary(), list()) -> ok.
delete_from_table_if_exists(Conn, Table0, WhereSql, Params) ->
    Table = qualify_table(Table0),
    case table_exists(Conn, Table) of
        true ->
            delete_from_table(Conn, Table, WhereSql, Params);
        false ->
            ok
    end.

-spec table_exists(pid(), binary()) -> boolean().
table_exists(Conn, Table) ->
    Sql = <<"SELECT to_regclass($1) IS NOT NULL AS present">>,
    case elib_pg:execute(Conn, Sql, [Table]) of
        {ok, _Cols, [{true}]} -> true;
        _ -> false
    end.

-spec qualify_table(binary()) -> binary().
qualify_table(Table) ->
    case binary:match(Table, <<".">>) of
        nomatch ->
            <<"public.", Table/binary>>;
        _ ->
            Table
    end.

%% @doc 插入用户并返回ID
%% @param Data 用户数据map
%% @return {ok, Id} | {error, Reason}
-spec insert_and_get_id(map()) -> {ok, integer()} | {error, any()}.
insert_and_get_id(Data) ->
    Tb = user_repo:tablename(),
    Id = elib_tsid:generate(user),
    Data2 = Data#{<<"id">> => Id},
    case elib_pg:insert(Tb, Data2, <<"RETURNING id">>) of
        {ok, _, [{Id}]} when is_integer(Id) ->
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 绑定邮箱
%% @param Uid 用户ID
%% @param Email 邮箱地址
%% @return {ok, Count} | {error, Reason}
-spec bind_email(integer(), binary()) -> {ok, integer()} | {error, any()}.
bind_email(Uid, Email) ->
    Tb = user_repo:tablename(),
    elib_pg:update(Tb, #{<<"email">> => Email}, <<"id = $1">>, [Uid]).

%% @doc 获取用户状态
%% @param Uid 用户ID
%% @return 状态值（-2 表示不存在）
-spec get_status(integer()) -> integer().
get_status(Uid) ->
    Tb = user_repo:tablename(),
    elib_pg:pluck_value(Tb, <<"status">>, #{id => Uid}, #{}, -2).

%% @doc 通过邮箱查找用户ID
%% @param Email 邮箱地址
%% @return 用户ID，不存在时返回0
-spec find_id_by_email(binary()) -> integer().
find_id_by_email(Email) ->
    Tb = user_repo:tablename(),
    elib_pg:pluck_value(Tb, <<"id">>, #{email => Email}, #{}, 0).

%% @doc 通过手机号查找用户ID
%% @param Mobile 手机号
%% @return 用户ID，不存在时返回0
-spec find_id_by_mobile(binary()) -> integer().
find_id_by_mobile(Mobile) ->
    Tb = user_repo:tablename(),
    elib_pg:pluck_value(Tb, <<"id">>, #{mobile => Mobile}, #{}, 0).

%% @doc 用户总数（License 规模 gate 用）。
-spec count() -> integer().
count() ->
    user_repo:count().

%% @doc 更新用户密码
%% @param Uid 用户ID
%% @param PasswordHash 密码哈希
%% user_ds:update_password(1000000051, elib_hasher:md5(<<"admin888">>)).
%% @return {ok, Count} | {error, Reason}
-spec update_password(integer(), binary()) -> {ok, integer()} | {error, any()}.
update_password(Uid, PasswordHash) ->
    Tb = user_repo:tablename(),
    elib_pg:update(Tb, #{<<"password">> => PasswordHash}, <<"id = $1">>, [Uid]).

%% @doc 更新用户状态（事务版本）
%% @param Conn 数据库连接
%% @param Uid 用户ID
%% @param Status 状态值
%% @return {ok, Count} | {error, Reason}
-spec update_status_in_tx(pid(), {integer(), integer()}) -> {ok, integer()} | {error, any()}.
update_status_in_tx(Conn, {Uid, Status}) when Status >= -1, Status =< 2 ->
    Tb = user_repo:tablename(),
    elib_pg:update(Conn, Tb, #{<<"status">> => Status}, <<"id = $1">>, [Uid]).

%% @doc 更新用户密码（事务版本）
%% @param Conn 数据库连接
%% @param Params {Uid, PasswordHash}
%% @return {ok, Count} | {error, Reason}
-spec update_password_in_tx(pid(), {integer(), binary()}) -> {ok, integer()} | {error, any()}.
update_password_in_tx(Conn, {Uid, PasswordHash}) ->
    Tb = user_repo:tablename(),
    elib_pg:update(Conn, Tb, #{<<"password">> => PasswordHash}, <<"id = $1">>, [Uid]).

%% @doc 更新用户允许搜索设置
%% @param Uid 用户ID
%% @param AllowSearch 是否允许搜索 (1=允许, 2=不允许)
%% @return {ok, Count} | {error, Reason}
-spec update_allow_search(integer(), integer()) -> {ok, integer()} | {error, any()}.
update_allow_search(Uid, AllowSearch) when AllowSearch >= 1, AllowSearch =< 2 ->
    Tb = <<"fts_user">>,
    elib_pg:update(Tb, #{<<"allow_search">> => AllowSearch}, <<"user_id = $1">>, [Uid]).

%% @doc G3: 领域 logic 不应直调 user_repo
-spec may_exist(integer()) -> boolean().
may_exist(Uid) -> user_repo:may_exist(Uid).

%% G3: adm_user_handler 不应直调 user_repo
-spec page(integer(), integer(), map(), binary()) -> {ok, map()} | {error, any()}.
page(Page, Size, Where, OrderBy) -> user_repo:page(Page, Size, Where, OrderBy).

%% G3: user_deletion_logic 不应直调 *_repo:tablename()
-spec export_data(integer()) -> {ok, map()} | {error, term()}.
export_data(Uid) ->
    try
        UserTb = user_repo:tablename(),
        UserSql = <<
            "SELECT id, account, nickname, avatar, sign, region, gender, created_at "
            "FROM ",
            UserTb/binary,
            " WHERE id = $1"
        >>,
        UserInfo =
            case elib_pg:query(UserSql, [Uid]) of
                {ok, [Row]} -> Row;
                _ -> #{}
            end,
        FriendTb = friend_repo:tablename(),
        FriendSql =
            <<"SELECT to_user_id, remark, created_at FROM ", FriendTb/binary,
                " WHERE from_user_id = $1 AND status = 1">>,
        {ok, Friends} = elib_pg:query(FriendSql, [Uid]),
        MemberTb = group_member_repo:tablename(),
        GroupTb = group_repo:tablename(),
        GroupSql =
            <<"SELECT g.id, g.title, gm.created_at FROM ", MemberTb/binary,
                " gm "
                "JOIN ", GroupTb/binary,
                " g ON g.id = gm.group_id "
                "WHERE gm.user_id = $1">>,
        {ok, Groups} = elib_pg:query(GroupSql, [Uid]),
        SettingTb = user_setting_repo:tablename(),
        SettingSql = <<"SELECT * FROM ", SettingTb/binary, " WHERE user_id = $1">>,
        Settings =
            case elib_pg:query(SettingSql, [Uid]) of
                {ok, [S]} -> S;
                _ -> #{}
            end,
        {ok, #{
            <<"user_info">> => UserInfo,
            <<"friends">> => Friends,
            <<"groups">> => Groups,
            <<"settings">> => Settings,
            <<"exported_at">> => elib_dt:now()
        }}
    catch
        _:Error -> {error, Error}
    end.

%% G3: user_deletion_logic 不应直调 user_repo:tablename()
-spec find_expired_logout_users(pos_integer(), pos_integer()) ->
    {ok, list(map())} | {error, term()}.
find_expired_logout_users(RetentionDays, BatchSize) ->
    UserTb = user_repo:tablename(),
    Sql =
        <<"SELECT id FROM ", UserTb/binary,
            " WHERE status = 2"
            " AND updated_at <= NOW() - ($1 || ' days')::INTERVAL"
            " ORDER BY updated_at ASC LIMIT $2">>,
    elib_pg:query(Sql, [RetentionDays, BatchSize]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%

%% @doc 驳回注销申请：仅当 status=2 时将用户状态恢复为 1
-spec reject_logout_apply(integer()) -> {ok, [map()]} | {ok, []} | {error, any()}.
reject_logout_apply(Uid) when is_integer(Uid), Uid > 0 ->
    Tb = user_repo:tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            %% user 表无 updated_at 列（只有 created_at），不能写该字段
            " SET status = 1"
            " WHERE id = $1 AND status = 2"
            " RETURNING id">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 审批通过注销申请：仅当 status=2 时将用户状态设为 -1（已注销）
-spec approve_logout_apply(integer()) -> {ok, [map()]} | {ok, []} | {error, any()}.
approve_logout_apply(Uid) when is_integer(Uid), Uid > 0 ->
    Tb = user_repo:tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = -1"
            " WHERE id = $1 AND status = 2"
            " RETURNING id">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 批量获取用户在线状态，避免 N+1 查询
%%
%% 防御：user_repo:find_by_id 经 elib_pg_sql:value_or_empty，在用户不存在或
%% SQL 报错时返回空 map #{}。旧实现对 #{ } 做 maps:get(<<"id">>, User) 会抛
%% badkey，把 confirm_friend_resp（单元素 [#{}}]）炸成 HTTP 500。这里跳过
%% 无 <<"id">> 键的行——空 map 原样返回，不计算在线状态。friend/list 走
%% 多行查询，空列表 [] 不进 foldl 行为不变。
-spec batch_online_state([map()]) -> [map()].
batch_online_state(Users) when is_list(Users) ->
    UidStatusMap = lists:foldl(
        fun(User, Acc) ->
            case is_map_key(<<"id">>, User) of
                false ->
                    Acc;
                true ->
                    Uid = maps:get(<<"id">>, User),
                    Status = check_online_status(Uid),
                    maps:put(Uid, Status, Acc)
            end
        end,
        #{},
        Users
    ),
    lists:map(
        fun(User) ->
            case is_map_key(<<"id">>, User) of
                false ->
                    User;
                true ->
                    Uid = maps:get(<<"id">>, User),
                    LastSeenAt = maps:get(<<"last_seen_at">>, User, <<>>),
                    Status = maps:get(Uid, UidStatusMap, offline),
                    User#{<<"status">> => Status, <<"last_seen_at">> => LastSeenAt}
            end
        end,
        Users
    ).

-spec check_online_status(integer()) -> online | offline.
check_online_status(Uid) ->
    case imboy_syn:count_user(Uid) of
        0 ->
            offline;
        _Count ->
            case user_setting_ds:chat_state_hide(Uid) of
                true -> offline;
                false -> online
            end
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(TEST).

%% find_by_id 查不到用户时经 value_or_empty 返回空 map #{}。
%% 旧实现对 #{} 做 maps:get(<<"id">>) 抛 badkey，把 confirm_friend_resp
%% 炸成 HTTP 500。空 map 必须原样返回、不崩、不计算在线状态。
batch_online_state_empty_map_degrades_test() ->
    ?assertEqual([#{}], batch_online_state([#{}])),
    ?assertEqual([#{}, #{}], batch_online_state([#{}, #{}])).

-endif.
