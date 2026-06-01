-module(user_logic).
%% Stable identity user domain boundary.
%% API adapters should call this module instead of reaching user internals directly.

%%%
% user 业务逻辑模块
%%%

-include("log.hrl").
-include("chat.hrl").
-include("common.hrl").

%% 注意：优先使用 Erlang 原生类型，不新增自定义类型

-export([online/4]).
-export([offline/3]).
-export([is_online/1, is_online/2]).
-export([online_state/1]).
-export([batch_online_state/1]).
-export([mine_state/1]).
-export([find_by_id/1, find_by_id/2]).
-export([find_by_ids/1, find_by_ids/2]).
-export([update/3]).
% -export([send_bind_email/2]).
-export([change_password/2]).
-export([set_password/2]).
-export([apply_logout/2]).
-export([cancel_logout/2]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 设置用户密码
%% 为未设置密码的用户设置登录密码。用户密码为空时才能设置。
%% 操作会记录用户日志，包含应用版本、设备ID和IP信息。
%% @param Uid 用户ID
%% @param Req0 HTTP请求对象
%% @returns 操作结果：成功返回{ok, <<"success">>}，失败返回错误信息
-spec set_password(integer(), map()) -> {ok, binary()} | {error, binary() | string()}.
set_password(Uid, Req0) ->
    PostVals = elib_param:post(Req0),
    NewPwd = maps:get(<<"new_pwd">>, PostVals, undefined),
    User = user_ds:find_by_id(Uid, ?LOGIN_COLUMN),
    OldPwd = maps:get(<<"password">>, User, not_find),
    case OldPwd of
        not_find ->
            {error, <<"用户不存在"/utf8>>};
        <<>> ->
            PwdPlaintext = elib_cipher:rsa_decrypt(NewPwd),
            PwdHash = elib_password:generate(PwdPlaintext),
            update_password_with_log(Uid, PwdHash, Req0, 110);
        _ ->
            {error, "have_set"}
    end.

%% @doc 修改用户密码
%% 验证用户当前密码后，更新为新密码。需要提供当前密码和新密码。
%% 操作会记录用户日志，包含应用版本、设备ID和IP信息。
%% @param Uid 用户ID
%% @param Req0 HTTP请求对象，包含当前密码和新密码
%% @returns 操作结果：成功返回{ok, <<"success">>}，失败返回错误信息
-spec change_password(pos_integer(), map()) -> {ok, binary()} | {error, binary()}.
change_password(Uid, Req0) ->
    PostVals = elib_param:post(Req0),
    ExistingPwd = maps:get(<<"existing_pwd">>, PostVals, undefined),
    NewPwd = maps:get(<<"new_pwd">>, PostVals, undefined),
    User = user_ds:find_by_id(Uid, ?LOGIN_COLUMN),
    ExistingPwd2 = elib_cipher:rsa_decrypt(ExistingPwd),
    case passport_logic:verify_user(ExistingPwd2, User) of
        {ok, _} ->
            PwdPlaintext = elib_cipher:rsa_decrypt(NewPwd),
            PwdHash = elib_password:generate(PwdPlaintext),
            update_password_with_log(Uid, PwdHash, Req0, 110);
        {error, Msg} ->
            {error, Msg}
    end.

%% @doc 申请注销账号
%% 将用户状态设置为申请注销中（状态=2），记录注销申请日志。
%% 注销申请后，用户将处于待注销状态，需要进一步处理。
%% @param Uid 用户ID
%% @param Req0 HTTP请求对象
%% @returns 操作结果：成功返回{ok, <<"success">>}
-spec apply_logout(integer(), map()) -> {ok, binary() | string()}.
apply_logout(Uid, Req0) ->
    _ = elib_pg:with_tx(fun(Conn) ->
        %% 更新用户状态为申请注销中
        {ok, _} = user_ds:update_status_in_tx(Conn, {Uid, 2}),
        %% 记录注销申请日志
        _ = user_log_ds:add_logout_apply_log(Conn, Uid, Req0),
        ok
    end),
    {ok, "success"}.

%% @doc 撤销注销申请
%% 将用户状态从申请注销中恢复为正常启用状态（状态=1）。
%% 用户可以撤销注销申请，恢复正常使用。
%% @param Uid 用户ID
%% @param _Req0 HTTP请求对象（当前未使用）
%% @returns 操作结果：成功返回{ok, <<"success">>}
-spec cancel_logout(integer(), any()) -> {ok, binary()} | {error, binary()}.
cancel_logout(Uid, _Req0) ->
    case user_ds:update_status(Uid, 1) of
        {ok, _} ->
            {ok, <<"success">>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 用户上线
%% 将用户标记为在线状态，并加入到在线用户管理中。
%% 会触发用户上线相关的处理，如检查离线消息等。
%%
%% @param Uid 用户ID
%% @param DType 设备类型（web、ios、android、macos、windows等）
%% @param Pid 进程PID
%% @param DID 设备ID
-spec online(pos_integer(), binary(), pid(), binary()) -> ok.
online(Uid, DType, Pid, DID) ->
    _ = imboy_syn:join(Uid, DType, Pid, DID),
    % 用异步队列实现 检查离线消息 等
    user_server:cast_online(Uid, Pid, DID, DType),
    ok.

%% @doc 用户下线
%% 将用户从在线状态中移除，处理用户下线相关的清理工作。
%% 会触发用户下线相关的处理，如检查离线消息等。
%% @param Uid 用户ID
%% @param Pid 进程PID
%% @param DID 设备ID
-spec offline(pos_integer(), pid(), binary()) -> ok.
offline(Uid, Pid, DID) ->
    _ = imboy_syn:leave(Uid, Pid),

    % 检查离线消息 用异步队列实现
    user_server:cast_offline(Uid, Pid, DID).

-spec is_online(integer()) -> boolean().
%% 检查用户是否在线
is_online(Uid) when is_integer(Uid) ->
    % 用户在线设备统计
    case imboy_syn:count_user(Uid) of
        0 ->
            false;
        _ ->
            true
    end.

% user_logic:is_online(1, <<"ios">>).
-spec is_online(integer(), binary()) -> boolean().
%% 检查用户是否在线
%% 使用 user_device_logic 检查指定设备类型的在线状态
is_online(Uid, DType) when is_integer(Uid) ->
    user_device_logic:is_online(Uid, {dtype, DType}).

-spec mine_state(integer()) -> {binary(), hide | online}.
mine_state(Uid) ->
    case user_setting_ds:chat_state_hide(Uid) of
        true ->
            {<<"status">>, hide};
        false ->
            {<<"status">>, online}
    end.

% 获取用户在线状态
-spec online_state(map()) -> map().
online_state(User) when is_map(User) ->
    Uid = maps:get(<<"id">>, User),
    LastSeenAt = maps:get(<<"last_seen_at">>, User, <<>>),
    Status =
        case imboy_syn:count_user(Uid) of
            0 ->
                offline;
            _Count ->
                case user_setting_ds:chat_state_hide(Uid) of
                    true ->
                        offline;
                    false ->
                        online
                end
        end,
    User#{<<"status">> => Status, <<"last_seen_at">> => LastSeenAt}.

%% @doc 批量获取用户在线状态
%%
%% 对多个用户批量查询在线状态，避免在列表推导式中逐个调用。
%% 注意：由于 imboy_syn 不提供批量查询接口，内部仍是逐个查询，
%% 但此函数封装了批量处理逻辑，便于后续优化。
%%
%% @param Users 用户列表
%% @returns 添加了 status 字段的用户列表
%%
%% @example
%% > Users = [#{<<"id">> => 1, <<"name">> => <<"Alice">>}, ...].
%% > user_logic:batch_online_state(Users).
%% [#{<<"id">> => 1, <<"name">> => <<"Alice">>, <<"status">> => online}, ...]
-spec batch_online_state([map()]) -> [map()].
batch_online_state(Users) when is_list(Users) ->
    % 提取所有 Uid 并查询在线状态
    UidStatusMap = lists:foldl(
        fun(User, Acc) ->
            Uid = maps:get(<<"id">>, User),
            Status = check_online_status(Uid),
            maps:put(Uid, Status, Acc)
        end,
        #{},
        Users
    ),

    % 合并在线状态到用户信息
    lists:map(
        fun(User) ->
            Uid = maps:get(<<"id">>, User),
            LastSeenAt = maps:get(<<"last_seen_at">>, User, <<>>),
            Status = maps:get(Uid, UidStatusMap, offline),
            User#{<<"status">> => Status, <<"last_seen_at">> => LastSeenAt}
        end,
        Users
    ).

%% @private
%% @doc 检查单个用户的在线状态
%% @param Uid 用户ID
%% @returns online | offline
-spec check_online_status(integer()) -> online | offline.
check_online_status(Uid) ->
    case imboy_syn:count_user(Uid) of
        0 ->
            offline;
        _Count ->
            case user_setting_ds:chat_state_hide(Uid) of
                true ->
                    offline;
                false ->
                    online
            end
    end.

%% @doc 根据用户ID查找用户信息
%% 返回默认列的用户信息。
%% @param Id 用户ID
%% @returns 用户信息列表，包含默认列的数据
-spec find_by_id(binary() | pos_integer()) -> map().
find_by_id(Id) ->
    find_by_id(Id, ?DEF_USER_COLUMN).

%% @doc 根据用户ID和指定列查找用户信息
%% 返回指定列的用户信息。
%% 自动处理头像为空的情况，设置默认头像。
%% @param Id 用户ID
%% @param Column 需要查询的列名
%% @returns 用户信息，包含指定的列数据
-spec find_by_id(binary() | pos_integer(), binary()) -> map().
find_by_id(Id, Column) when is_binary(Id) ->
    find_by_id(ec_cnv:to_integer(Id), Column);
find_by_id(Id, Column) ->
    check_avatar(user_ds:find_by_id(Id, Column)).

-spec find_by_ids([pos_integer()]) -> [map()].
find_by_ids(Ids) ->
    find_by_ids(Ids, ?DEF_USER_COLUMN).

-spec find_by_ids([pos_integer()], binary()) -> [map()].
find_by_ids([], _) ->
    [];
find_by_ids(Ids, Column) ->
    case user_ds:list_by_ids(Ids, Column) of
        {ok, []} ->
            [];
        {ok, Rows} ->
            [check_avatar(Row) || Row <- Rows];
        _ ->
            []
    end.

-spec update(integer(), binary(), list() | binary()) ->
    ok | {ok, integer() | binary()} | {error, any()}.
%% T3.2 退化外壳：资料字段校验委托 user_agg:validate_update/2（纯决策），
%% 本函数仅执行 I/O（落库 / email 占用查询 / 绑定邮件）与 reject→线格式映射。
%% 行为逐字保持 T3.1 前实现（含 email 非 binary 落 unsupported、gender/
%% allow_search 非法各自专属错误、性别/allow_search 转 integer 落库）。
update(Uid, Field, Val) ->
    case user_agg:validate_update(Field, Val) of
        {ok, {check_email, Email}} ->
            bind_email_if_available(Uid, Email);
        {ok, {set_gender, N}} ->
            user_ds:update_field(Uid, <<"gender">>, N);
        {ok, {set_allow_search, N}} ->
            %% allow_search 字段在 fts_user 表中，需要特殊处理
            user_ds:update_allow_search(Uid, N);
        {ok, {set_field, F, V}} ->
            user_ds:update_field(Uid, F, V);
        {error, bad_email_format} ->
            {error, {1, <<"">>, <<"Email 格式有误"/utf8>>}};
        {error, bad_gender} ->
            {error, {1, <<"">>, <<"性别值必须是 1、2 或 3"/utf8>>}};
        {error, bad_allow_search} ->
            {error, {1, <<"">>, <<"允许搜索值必须是 1 或 2"/utf8>>}};
        {error, unsupported_field} ->
            {error, {1, <<"">>, <<"Unsupported field">>}}
    end.

%% @doc email 占用查询 + 绑定（I/O 外壳）。格式已由 user_agg 裁定为合法，
%% 此处仅查占用：无占用则发绑定邮件，已占用返回错误。
%% @private
bind_email_if_available(Uid, Email) ->
    case maps:size(user_ds:find_by_email(Email, <<"id">>)) of
        0 ->
            send_bind_email(Uid, Email);
        _ ->
            {error, {1, <<"">>, <<"Email 被占用"/utf8>>}}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 更新密码并记录日志的辅助函数
%% @private
update_password_with_log(Uid, PwdHash, Req0, LogType) ->
    _ = elib_pg:with_tx(fun(Conn) ->
        %% 更新用户密码
        {ok, _} = user_ds:update_password_in_tx(Conn, {Uid, PwdHash}),
        %% 记录密码修改日志
        _ = user_log_ds:add_password_change_log(Conn, Uid, Req0, LogType),
        ok
    end),
    {ok, "success"}.

%% @doc 检查并设置默认头像
%% 检查用户头像是否为空，如果为空则设置默认头像。
%% 支持map和list两种数据结构格式。
%% @param User 用户信息，可以是map或list格式
%% @returns 处理后的用户信息，确保头像不为空
-spec check_avatar(map()) -> map().
check_avatar(User) when map_size(User) == 0 ->
    User;
check_avatar(User) when is_map(User) ->
    Def = <<"assets/images/def_avatar.png">>,
    K = <<"avatar">>,
    case maps:get(K, User, <<>>) of
        <<>> ->
            maps:put(K, Def, User);
        _ ->
            User
    end.

%% @doc 发送邮箱绑定确认邮件
%% 为用户发送邮箱绑定确认邮件，包含确认链接。
%% 链接有效期24小时，包含时间戳和HMAC签名验证。
%%
%% 使用示例：
%% user_logic:send_bind_email(108, <<"leeyisoft@icloud.com">>).
%% @param Uid 用户ID
%% @param Email 待绑定的邮箱地址
%% @returns 成功返回{ok, "success"}，失败返回错误信息
-spec send_bind_email(integer(), binary()) -> {ok, binary()}.
send_bind_email(Uid, Email) ->
    ExpireAtS = elib_dt:second() + 86400,
    ExpireAt = elib_dt:to_rfc3339(ExpireAtS, second),
    {Title, Nickname} = user_ds:title(Uid, 2),

    SolKey = config_ds:env(solidified_key),
    Args =
        #{
            ts => ExpireAtS,
            uin => Uid,
            mail => Email
        },

    Tk = elib_hasher:hmac_sha512(
        elib_cnv:map_to_query(Args), SolKey
    ),
    Url = elib_uri:build_query(
        config_ds:env(base_url), <<"/passport/bind_mail">>, Args#{tk => Tk}
    ),
    Body =
        <<"Hi, ", Title/binary, "：<br/><br/>IMBoy正在尝试绑定邮件地址 "/utf8,
            (elib_cnv:safe_to_binary(Email))/binary, " 到你的账号（昵称："/utf8, Nickname/binary,
            " )。<br/><br/>如果这是你的操作，请 <a href=\""/utf8, Url/binary,
            "\" target=\"_blank\">点击确认</a> 完成邮箱绑定，截止之"/utf8, ExpireAt/binary,
            "前链接有效。<br/>如果你没有操作绑定此邮箱，请忽略此邮件。<br/><br/> 如果需要了解更多信息，请访问IMBoy官方网站：ht"/utf8,
            "tps://www.imboy.pub/"/utf8>>,
    _ = elib_email:send(Email, <<"IMBoy绑定邮箱确认"/utf8>>, Body),
    {ok, <<"success">>}.
