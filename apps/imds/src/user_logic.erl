-module(user_logic).
%%%
% user 业务逻辑模块
%%%

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/def_column.hrl").

%% Types
-type user_id() :: integer().
-type user_id_hash() :: binary().
-type device_type() :: binary().
-type device_id() :: binary().
-type email() :: binary().
-type user_info() :: map() | list().
-type result() :: {ok, binary()} | {error, binary()} | {error, {integer(), binary(), binary()}}.

-export([online/4]).
-export([offline/3]).

-export([is_online/1, is_online/2]).
-export([online_state/1]).
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
%% @returns 操作结果：成功返回{ok, "success"}，失败返回错误信息
-spec set_password(user_id(), any()) -> result().
set_password(Uid, Req0) ->
    AppVsn = cowboy_req:header(<<"vsn">>, Req0, undefined),
    DID = cowboy_req:header(<<"did">>, Req0, undefined),
    DType = cowboy_req:header(<<"cos">>, Req0, undefined),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0),

    PostVals = imboy_param:post(Req0),
    NewPwd = proplists:get_value(<<"new_pwd">>, PostVals),

    % ?DEBUG_LOG(['Uid ', Uid]),
    User = user_repo:find_by_id(Uid, ?LOGIN_COLUMN),
    OldPwd = maps:get(<<"password">>, User, not_find),
    case OldPwd of
        not_find ->
            {error, "用户不存在"};
        <<>> ->
            PwdPlaintext = imboy_cipher:rsa_decrypt(NewPwd),
            Pwd2 = imboy_password:generate(PwdPlaintext),
            imboy_db:with_transaction(fun(Conn) ->
                Now = imboy_dt:now(),
                Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
                imboy_db:update(Conn,
                    user_repo:tablename(),
                    Where,
                    #{
                        <<"password">> => Pwd2
                    }),
                {ok, Body} = jsone_encode:encode(#{
                        <<"app_vsn">> => AppVsn,
                        <<"did">> => DID,
                        <<"dtype">> => DType,
                        <<"ip">> => Ip
                }, [native_utf8]),
                user_log_repo:add(Conn, #{
                    % 日志类型: 100 用户注销备份  102 用户注销申请记录 110 修改密码
                    type => 110,
                    uid => Uid,
                    body => Body,
                    created_at => Now
                }),
                ok
            end),
            {ok, "success"};
        _ ->
            {error, "have_set"}
    end.

%% @doc 修改用户密码
%% 验证用户当前密码后，更新为新密码。需要提供当前密码和新密码。
%% 操作会记录用户日志，包含应用版本、设备ID和IP信息。
%% @param Uid 用户ID
%% @param Req0 HTTP请求对象，包含当前密码和新密码
%% @returns 操作结果：成功返回{ok, "success"}，失败返回错误信息
-spec change_password(user_id(), any()) -> result().
change_password(Uid, Req0) ->
    AppVsn = cowboy_req:header(<<"vsn">>, Req0, undefined),
    DID = cowboy_req:header(<<"did">>, Req0, undefined),
    DType = cowboy_req:header(<<"cos">>, Req0, undefined),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0),


    PostVals = imboy_param:post(Req0),
    ExistingPwd = proplists:get_value(<<"existing_pwd">>, PostVals),
    NewPwd = proplists:get_value(<<"new_pwd">>, PostVals),


    User = user_repo:find_by_id(Uid, ?LOGIN_COLUMN),
        ExistingPwd2 = imboy_cipher:rsa_decrypt(ExistingPwd),

    VerifyUser = passport_logic:verify_user(ExistingPwd2, User),
    case VerifyUser of
        {ok, _} ->
            PwdPlaintext = imboy_cipher:rsa_decrypt(NewPwd),
            Pwd2 = imboy_password:generate(PwdPlaintext),
            imboy_db:with_transaction(fun(Conn) ->
                Now = imboy_dt:now(),
                Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
                imboy_db:update(Conn,
                    user_repo:tablename(),
                    Where,
                    #{
                        <<"password">> => Pwd2
                    }),
                {ok, Body} = jsone_encode:encode(#{
                        <<"app_vsn">> => AppVsn,
                        <<"did">> => DID,
                        <<"dtype">> => DType,
                        <<"ip">> => Ip
                }, [native_utf8]),
                user_log_repo:add(Conn, #{
                    % 日志类型: 100 用户注销备份  102 用户注销申请记录 110 修改密码
                    type => 110,
                    uid => Uid,
                    body => Body,
                    created_at => Now
                }),
                ok
            end),
            {ok, "success"};
        {error, Msg} ->
            {error, Msg}
    end.

%% @doc 申请注销账号
%% 将用户状态设置为申请注销中（状态=2），记录注销申请日志。
%% 注销申请后，用户将处于待注销状态，需要进一步处理。
%% @param Uid 用户ID
%% @param Req0 HTTP请求对象
%% @returns 操作结果：成功返回{ok, "success"}
-spec apply_logout(user_id(), any()) -> result().
apply_logout(Uid, Req0) ->
    AppVsn = cowboy_req:header(<<"vsn">>, Req0, undefined),
    DID = cowboy_req:header(<<"did">>, Req0, undefined),
    DType = cowboy_req:header(<<"cos">>, Req0, undefined),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0),


    % 通知用户所有朋友，该用户已经注销
    % 清理注销用户相关数据
    % 用户注销以后,用户的所有好友和群组关系需要解除
    % https://blog.51cto.com/u_15069441/4323079
    imboy_db:with_transaction(fun(Conn) ->
        Now = imboy_dt:now(),
        Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
        imboy_db:update(Conn,
            user_repo:tablename(),
            Where,
            #{
                % 状态: -1 删除  0 禁用  1 启用  2 申请注销中
                <<"status">> => 2
            }),
        {ok, Body} = jsone_encode:encode(#{
                <<"app_vsn">> => AppVsn,
                <<"did">> => DID,
                <<"dtype">> => DType,
                <<"ip">> => Ip
        }, [native_utf8]),
        user_log_repo:add(Conn, #{
            % 日志类型: 100 用户注销备份  102 用户注销申请记录 110 修改密码
            type => 102,
            uid => Uid,
            body => Body,
            created_at => Now
        }),
        ok
    end),

    % user_server:cast_apply_logout(Uid, imboy_dt:now(), #{
    %     <<"app_vsn">> => AppVsn,
    %     <<"did">> => DID,
    %     <<"dtype">> => DType,
    %     <<"ip">> => Ip
    % }),
    {ok, "success"}.

%% @doc 撤销注销申请
%% 将用户状态从申请注销中恢复为正常启用状态（状态=1）。
%% 用户可以撤销注销申请，恢复正常使用。
%% @param Uid 用户ID
%% @param _Req0 HTTP请求对象（当前未使用）
%% @returns 操作结果：成功返回{ok, "success"}
-spec cancel_logout(user_id(), any()) -> result().
cancel_logout(Uid, _Req0) ->
    Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
    imboy_db:update(
        user_repo:tablename(),
        Where,
        #{
            % 状态: -1 删除  0 禁用  1 启用  2 申请注销中
            <<"status">> => 1
        }),
    {ok, "success"}.

%% @doc 用户上线
%% 将用户标记为在线状态，并加入到在线用户管理中。
%% 会触发用户上线相关的处理，如检查离线消息等。
%%
%% @param Uid 用户ID
%% @param DType 设备类型（web、ios、android、macos、windows等）
%% @param Pid 进程PID
%% @param DID 设备ID
-spec online(user_id(), device_type(), pid(), device_id()) -> ok.
online(Uid, DType, Pid, DID) ->
    % ?DEBUG_LOG(["user_logic/online/4", Uid, Pid, DType, DID]),
    imboy_syn:join(Uid, DType, Pid, DID),
    % 用异步队列实现 检查离线消息 等
    user_server:cast_online(Uid, Pid, DID, DType),
    ok.


%% @doc 用户下线
%% 将用户从在线状态中移除，处理用户下线相关的清理工作。
%% 会触发用户下线相关的处理，如检查离线消息等。
%% @param Uid 用户ID
%% @param Pid 进程PID
%% @param DID 设备ID
-spec offline(user_id(), pid(), device_id()) -> ok.
offline(Uid, Pid, DID) ->
    imboy_syn:leave(Uid, Pid),

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
is_online(Uid, DType) when is_integer(Uid) ->
    imboy_syn:is_online(Uid, {dtype, DType}).


mine_state(Uid) ->
    case user_setting_ds:chat_state_hide(Uid) of
        true ->
            {<<"status">>, hide};
        false ->
            {<<"status">>, online}
    end.


% 获取用户在线状态
online_state(User) ->
    {<<"id">>, Uid} = lists:keyfind(<<"id">>, 1, User),
    LastSeenAt = case lists:keyfind(<<"last_seen_at">>, 1, User) of
        {<<"last_seen_at">>, Val} ->
            Val;
        false ->
            <<>>
    end,
    Status = case imboy_syn:count_user(Uid) of
        0 ->
            offline;
        _Count ->
            case user_setting_ds:chat_state_hide(Uid) of
                true ->
                    % 既然是 hide 就不能够返回hide 状态给API
                    offline;
                false ->
                    online
            end
    end,
    [{<<"status">>, Status}, {<<"last_seen_at">>, LastSeenAt} | User].


%% @doc 根据用户ID查找用户信息
%% 支持原始ID或hashid格式，返回默认列的用户信息。
%% @param Id 用户ID（可以是原始数字ID或hashid）
%% @returns 用户信息列表，包含默认列的数据
-spec find_by_id(user_id_hash() | user_id()) -> user_info().
find_by_id(Id) ->
    find_by_id(Id, ?DEF_USER_COLUMN).


%% @doc 根据用户ID和指定列查找用户信息
%% 支持原始ID或hashid格式，返回指定列的用户信息。
%% 自动处理头像为空的情况，设置默认头像。
%% @param Id 用户ID（可以是原始数字ID或hashid）
%% @param Column 需要查询的列名
%% @returns 用户信息，包含指定的列数据
-spec find_by_id(user_id_hash() | user_id(), binary()) -> user_info().
find_by_id(Id, Column) when is_binary(Id) ->
    find_by_id(imboy_hashids:decode(Id), Column);
find_by_id(Id, Column) ->
    check_avatar(user_repo:find_by_id(Id, Column)).


find_by_ids(Ids) ->
    find_by_ids(Ids, ?DEF_USER_COLUMN).


find_by_ids([], _) ->
    [];
find_by_ids(Ids, Column) ->
    case user_repo:list_by_ids(Ids, Column) of
        {ok, _, []} ->
            [];
        {ok, ColumnList, Rows} ->
            [ check_avatar(lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnList, tuple_to_list(Row))) || Row <- Rows ];
        _ ->
            []
    end.


-spec update(integer(), binary(), list() | binary()) ->
          ok | {error, {integer(), binary(), binary()}}.
update(Uid, <<"email">>, Val) ->
    IsEmail = imboy_func:is_email(Val),
    User = if
        IsEmail ->
            user_repo:find_by_email(Val, <<"id">>);
        true ->
            #{}
    end,
    case {IsEmail, maps:size(User)} of
        {true, 0} ->
            send_bind_email(Uid, Val);
        {true, _} ->
            {error, {1, <<"">>, <<"Email 被占用"/utf8>>}};
        {_, _} ->
            {error, {1, <<"">>, <<"Email 格式有误"/utf8>>}}
    end;
update(Uid, <<"sign">>, Val) ->
    Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
    imboy_db:update(user_repo:tablename(), Where, #{
        <<"sign">> => Val
    });
update(Uid, <<"nickname">>, Val) ->
    Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
    imboy_db:update(user_repo:tablename(), Where, #{
        <<"nickname">> => Val
    });
update(Uid, <<"avatar">>, Val) ->
    Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
    imboy_db:update(user_repo:tablename(), Where, #{
        <<"avatar">> => Val
    });

update(Uid, <<"region">>, Val) ->
    Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
    imboy_db:update(user_repo:tablename(), Where, #{
        <<"region">> => Val
    });

% 性别 1 男  2 女  3 保密
update(Uid, <<"gender">>, <<"1">>) ->
    Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
    imboy_db:update(user_repo:tablename(), Where, #{
        <<"gender">> => <<"1">>
    });
update(Uid, <<"gender">>, <<"2">>) ->
    Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
    imboy_db:update(user_repo:tablename(), Where, #{
        <<"gender">> => <<"2">>
    });
update(Uid, <<"gender">>, <<"3">>) ->
    Where = <<"id=", (ec_cnv:to_binary(Uid))/binary>>,
    imboy_db:update(user_repo:tablename(), Where, #{
        <<"gender">> => <<"3">>
    });
update(Uid, <<"allow_search">>, <<"1">>) ->
    Where = <<"user_id=", (ec_cnv:to_binary(Uid))/binary>>,
    imboy_db:update(fts_user_repo:tablename(), Where, #{
        <<"allow_search">> => <<"1">>
    });
update(Uid, <<"allow_search">>, <<"2">>) ->
    Where = <<"user_id=", (ec_cnv:to_binary(Uid))/binary>>,
    imboy_db:update(fts_user_repo:tablename(), Where, #{
        <<"allow_search">> => <<"2">>
    });
update(_Uid, _Field, _Val) ->
    {error, {1, <<"">>, <<"Unsupported field">>}}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


%% @doc 检查并设置默认头像
%% 检查用户头像是否为空，如果为空则设置默认头像。
%% 支持map和list两种数据结构格式。
%% @param User 用户信息，可以是map或list格式
%% @returns 处理后的用户信息，确保头像不为空
-spec check_avatar(user_info()) -> user_info().
check_avatar([]) ->
    [];
check_avatar(User) when is_map(User) ->
    Def = <<"assets/images/def_avatar.png">>,
    K = <<"avatar">>,
    case maps:get(K, User, <<>>) of
        <<>> ->
            maps:put(K, Def, User);
        _ ->
            User
    end;
check_avatar(User) ->
    Def = <<"assets/images/def_avatar.png">>,
    case lists:keyfind(<<"avatar">>, 1, User) of
        {<<"avatar">>, <<>>} ->
            % <<>> == <<"">> is true
            lists:keyreplace(<<"avatar">>, 1, User, {<<"avatar">>, Def});
        {<<"avatar">>, _Aaatar} ->
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
-spec send_bind_email(user_id(), email()) -> result().
send_bind_email(Uid, Email) ->
    ExpireAtS = imboy_dt:second() + 86400,
    ExpireAt = imboy_dt:to_rfc3339(ExpireAtS, second),
    {Title, Nickname} = user_ds:title(Uid, 2),

    SolKey = config_ds:get(solidified_key),
    Args = #{
        ts => ExpireAtS,
        uin => imboy_hashids:encode(Uid),
        mail => Email
    },
    Tk = imboy_hasher:hmac_sha512(imboy_cnv:map_to_query(Args), SolKey),
    Url = imboy_uri:build_query(
        config_ds:env(base_url),
        <<"/passport/bind_mail">>,
        Args#{tk => Tk}),
    Body = <<"Hi, ", Title/binary,
        "：<br/><br/>IMBoy正在尝试绑定邮件地址 "/utf8,
        Email/binary,
        " 到你的账号（昵称："/utf8, Nickname/binary,
        " )。<br/><br/>如果这是你的操作，请 <a href=\""/utf8, Url/binary,
        "\" target=\"_blank\">点击确认</a> 完成邮箱绑定，截止之"/utf8,
        (ec_cnv:to_binary(ExpireAt))/binary, "前链接有效。<br/>如果你没有操作绑定此邮箱，请忽略此邮件。<br/><br/> 如果需要了解更多信息，请访问IMBoy官方网站：https://www.imboy.pub/"/utf8>>,
    % ?DEBUG_LOG(Body),
    imboy_func:send_email(Email, <<"IMBoy绑定邮箱确认"/utf8>>, Body),
    {ok, "success"}.
