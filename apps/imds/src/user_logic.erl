-module(user_logic).
%%%
% user 业务逻辑模块
%%%

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/def_column.hrl").

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
-export([apply_logout/2]).
-export([cancel_logout/2]).

%% ===================================================================
%% API
%% ===================================================================

change_password(Uid, Req0) ->
    AppVsn = cowboy_req:header(<<"vsn">>, Req0, undefined),
    DID = cowboy_req:header(<<"did">>, Req0, undefined),
    DType = cowboy_req:header(<<"cos">>, Req0, undefined),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0),


    PostVals = imboy_req:post_params(Req0),
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
                Now = imboy_dt:utc(millisecond),
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

%%注销申请
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
        Now = imboy_dt:utc(millisecond),
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

    % user_server:cast_apply_logout(Uid, imboy_dt:utc(millisecond), #{
    %     <<"app_vsn">> => AppVsn,
    %     <<"did">> => DID,
    %     <<"dtype">> => DType,
    %     <<"ip">> => Ip
    % }),
    {ok, "success"}.

%%撤销注销申请
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

%dtype 设备类型 web ios android macos windows等
-spec online(integer(), binary(), pid(), binary()) -> ok.
online(Uid, DType, Pid, DID) ->
    % ?LOG(["user_logic/online/4", Uid, Pid, DType, DID]),
    imboy_syn:join(Uid, DType, Pid, DID),

    gen_server:cast(user_server, {ws_online, Uid, DType, DID}),

    % 用异步队列实现 检查离线消息 等
    user_server:cast_online(Uid, Pid, DID),
    ok.


-spec offline(Uid :: integer(), Pid :: pid(), DID :: binary()) -> ok.
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
    case imboy_syn:count_user(Uid) of
        0 ->
            [{<<"status">>, offline} | User];
        _Count ->
            case user_setting_ds:chat_state_hide(Uid) of
                true ->
                    % 既然是 hide 就不能够返回hide 状态给API
                    [{<<"status">>, offline} | User];
                false ->
                    [{<<"status">>, online} | User]
            end
    end.


-spec find_by_id(binary()) -> list().
find_by_id(Id) ->
    find_by_id(Id, ?DEF_USER_COLUMN).


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

update(_Uid, _Field, _Val) ->
    {error, {1, <<"">>, <<"Unsupported field">>}}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


%% 检查 user avatar 是否为空，如果为空设置默认
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

% user_logic:send_bind_email(108, <<"leeyisoft@icloud.com">>).
-spec send_bind_email(integer(), binary()) ->
          ok | {error, {integer(), binary(), binary()}}.
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
    % ?LOG(Body),
    imboy_func:send_email(Email, <<"IMBoy绑定邮箱确认"/utf8>>, Body),
    {ok, "success"}.
