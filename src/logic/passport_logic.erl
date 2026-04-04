-module(passport_logic).
%% Stable identity passport domain boundary.
%% API adapters should call this module instead of reaching auth/user internals directly.
-dialyzer({nowarn_function, [{send_email_code, 1}, {send_sms_code, 1}]}).
%%%
% passport_logic 是 passport application logic 缩写
%%%

-export([send_code/2]).
-export([signup/4]).
-export([login/3]).
-export([do_login/3]).
-export([do_login/5]).
-export([do_signup/5]).
-export([find_password/5]).
-export([verify_user/2]).
-export([quick_login/4]).


-include("log.hrl").
-include("common.hrl").

%% @doc 快速登录（支持第三方服务）
%% @param Service 登录服务类型（如 jverify）
%% @param Operator 操作员
%% @param Token 认证令牌
%% @param PostVals 请求参数
%% @returns {ok, Data} | {error, Msg}
-spec quick_login(binary(), binary() | undefined, binary(), map()) ->
    {ok, map()} | {error, binary() | map()}.


quick_login(<<>>, _, _, _) ->
    {error, <<"未指定登录服务"/utf8>>};
quick_login(<<"jverify">>, _Operator, Token, PostVals) ->
    case imboy_sms:jverification(Token) of
        {error, Msg} ->
            {error, Msg};
        {ok, Mobile} ->
            Mobile2 = <<"+86", Mobile/binary>>,
            User = user_ds:find_by_mobile(Mobile2, ?LOGIN_COLUMN),
            Uid = maps:get(<<"id">>, User, 0),
            case Uid of
                0 ->
                    Data = pick_data_for_insert(#{
                        <<"source">> => <<"jverify">>
                        , <<"mobile">> => Mobile2
                        , <<"password">> => <<>>
                        }, PostVals),
                    case user_ds:insert_and_get_id(Data) of
                        {ok, Uid2} ->
                            User2 = user_ds:find_by_id(Uid2, ?LOGIN_COLUMN),
                            {ok, login_resp(User2, #{<<"action">> => <<"need_set_password">>})};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {ok, login_resp(User, #{})}
            end
    end;
quick_login(_, _, _, _) ->
    {error, <<"不支持的登录服务"/utf8>>}.


% passport_logic:send_code(<<>>, <<"sms">>).
% passport_logic:send_code(<<>>, <<"email">>).
send_code(Mobile, <<"sms">>) ->
    % Res = throttle:check(per_minute_once, Mobile),
    % ?DEBUG_LOG([per_minute_once, Res]),
    % case Res of
    case throttle:check(per_minute_once, {send_code, Mobile}) of
        {limit_exceeded, _, _} ->
            {error, <<"per_minute_once">>};
        _ ->
            send_sms_code(Mobile)
    end;
send_code(EMail, <<"email">>) ->
    send_email_code(EMail);
send_code(_, _) ->
    {error, <<"验证码类型不支持，仅支持 sms 或 email"/utf8>>}.

%% @doc 兼容旧测试入口：明文手机号注册，并直接返回登录态数据
-spec signup(binary(), binary(), binary(), map()) -> {ok, map()} | {error, binary(), integer()}.
signup(Mobile, Pwd, Email, PostVals) when is_binary(Mobile), is_binary(Pwd), is_binary(Email), is_map(PostVals) ->
    case validate_compat_password(Pwd) of
        ok ->
            case prepare_compat_mobile(Mobile) of
                ok ->
                    Nickname = compat_signup_nickname(Mobile, PostVals),
                    Email2 = compat_signup_email(Email),
                    PostVals2 = PostVals#{
                        <<"nickname">> => Nickname
                    },
                    BaseData0 = #{
                        <<"password">> => elib_password:generate(Pwd),
                        <<"mobile">> => Mobile,
                        <<"account">> => Mobile
                    },
                    BaseData = case Email2 of
                        undefined ->
                            BaseData0;
                        _ ->
                            BaseData0#{<<"email">> => Email2}
                    end,
                    Data = pick_data_for_insert(BaseData, PostVals2),
                    case user_ds:insert_and_get_id(Data) of
                        {ok, Uid} ->
                            User = user_ds:find_by_id(Uid, ?LOGIN_COLUMN),
                            {ok, login_resp(User, #{})};
                        {error, {error, error, <<"23505">>, unique_violation, _Msg, _Details}} ->
                            {error, <<"账号已被占用"/utf8>>, 400};
                        {error, Reason} ->
                            {error, compat_error_message(Reason), 400}
                    end;
                {error, Msg, Code} ->
                    {error, Msg, Code}
            end;
        {error, Msg} ->
            {error, Msg, 400}
    end.

%% @doc 兼容旧测试入口：使用明文账号密码登录
-spec login(binary(), binary(), map()) -> {ok, map()} | {error, binary(), integer()}.
login(Account, Pwd, PostVals) when is_binary(Account), is_binary(Pwd), is_map(PostVals) ->
    Type = compat_login_type(Account),
    Did = maps:get(<<"did">>, PostVals, <<>>),
    DType = compat_device_type(PostVals, Did),
    DName = maps:get(<<"dname">>, PostVals, <<>>),
    Ip = maps:get(<<"ip">>, PostVals, <<"{}">>),
    PostVals2 = PostVals#{
        <<"did">> => Did,
        <<"dtype">> => DType,
        <<"cos">> => DType,
        <<"dname">> => DName,
        <<"ip">> => Ip
    },
    case do_login(Type, Account, Pwd, DType, Did) of
        {ok, Data} ->
            _ = record_compat_login_success(Data, PostVals2),
            {ok, Data};
        {{error, conflict}, ConflictInfo} ->
            {error, compat_error_message(ConflictInfo), 5100};
        {error, Msg} ->
            {error, compat_error_message(Msg), 400}
    end.


-spec do_login(binary(), binary(), binary()) -> {ok, map()} | {error, binary() | map()}.
do_login(Type, Account, Pwd) ->
    do_login(Type, Account, Pwd, <<>>, <<>>).

%% @doc 账号密码登录（带设备信息）
%% 检查设备冲突，如果同类型设备已登录则返回冲突信息
-spec do_login(binary(), binary(), binary(), binary(), binary()) ->
    {ok, map()} | {{error, conflict}, map()} | {error, binary() | map()}.
do_login(_Type, _Email, <<>>, _DType, _Did) ->
    {error, <<"密码有误"/utf8>>};
do_login(Type, Mobile, Pwd, DType, Did) when Type == <<"mobile">> ->
    User = user_ds:find_by_mobile(Mobile, ?LOGIN_COLUMN),
    do_login_verify(Pwd, User, DType, Did);
do_login(Type, Email, Pwd, DType, Did) when Type == <<"email">> ->
    case elib_type:is_email(Email) of
        true ->
            User = user_ds:find_by_email(Email, ?LOGIN_COLUMN),
            do_login_verify(Pwd, User, DType, Did);
        false ->
            {error, <<"Email格式有误."/utf8>>}
    end;
do_login(Type, Account, Pwd, DType, Did) when Type == <<"account">> ->
    User = case elib_type:is_email(Account) of
        true ->
            user_ds:find_by_email(Account, ?LOGIN_COLUMN);
        false ->
            user_ds:find_by_account(Account, ?LOGIN_COLUMN)
    end,
    do_login_verify(Pwd, User, DType, Did).


%% @doc 登录验证（带设备冲突检查）
%% 先认证用户，成功后检查设备冲突
-spec do_login_verify(binary(), map(), binary(), binary()) ->
    {ok, map()} | {{error, conflict}, map()} | {error, term()}.
do_login_verify(Pwd, User, DType, _Did) ->
    case verify_user(Pwd, User) of
        {ok, Data} ->
            % 从 uid (hashids 编码) 解码得到整数 ID
            UidHashed = maps:get(<<"uid">>, Data),
            Uid = elib_hashids:decode(UidHashed),
            % 检查设备类型是否有效
            case user_device_logic:validate_device_type(DType) of
                false when DType =/= <<>> ->
                    % 无效的设备类型，但允许登录（兼容旧客户端）
                    {ok, Data};
                _ ->
                    % 检查登录冲突
                    case user_device_logic:check_login_conflict(Uid, DType) of
                        {ok, no_conflict} ->
                            % 无冲突，正常登录
                            {ok, Data};
                        {ok, conflict, ConflictInfo} ->
                            % 有冲突，返回冲突信息
                            {{error, conflict}, ConflictInfo};
                        {error, _Reason} ->
                            % 检查失败，允许登录
                            {ok, Data}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.


-spec do_signup(Type :: binary(), EmailOrMobile :: binary(), Pwd :: binary(), Code :: binary(), PostVals :: map()) ->
          {ok, map()} | {error, binary()}.
do_signup(<<"email">>, Email, Pwd, Code, PostVals) ->
    case elib_type:is_email(Email) of
        true ->
            % 校验验证码
            case verify_code(Email, Code) of
                {ok, _} ->
                    do_signup_by_email(Email, Pwd, PostVals);
                {error, Msg} ->
                    {error, Msg}
            end;
        false ->
            {error, <<"Email格式有误.."/utf8>>}
    end;
do_signup(<<"mobile">>, Mobile, Pwd, Code, PostVals) ->
    % 校验验证码
    case verify_code(Mobile, Code) of
        {ok, _} ->
            do_signup_by_mobile(Mobile, Pwd, PostVals);
        {error, Msg} ->
            {error, Msg}
    end;
do_signup(_Type, _Account, _Pwd, _Code, _PostVals) ->
    {error, <<"不支持的注册类型"/utf8>>}.


-spec find_password(Type :: binary(),
                    EmailOrMobile :: binary(),
                    Pwd :: binary(),
                    Code :: binary(),
                    PostVals :: map()) ->
          {ok, map()} | {error, binary()}.
find_password(Type, Email, Pwd, Code, PostVals) when Type == <<"email">> ->
    ok = ?DEBUG_LOG(["Email ", Email, elib_type:is_email(Email)]),
    case elib_type:is_email(Email) of
        true ->
            % 校验验证码
            case verify_code(Email, Code) of
                {ok, _} ->
                    find_password_by_email(Email, Pwd, PostVals);
                {error, Msg} ->
                    {error, Msg}
            end;
        false ->
            {error, <<"Email格式有误..."/utf8>>}
    end;
find_password(_Type, _Account, _Pwd, _Code, _PostVals) ->
    {error, <<"不支持的注册类型"/utf8>>}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


-spec send_email_code(binary() | undefined) -> {error, binary()} | {ok, binary()}.
send_email_code(undefined) ->
    {error, <<"Email必须"/utf8>>};
send_email_code(ToEmail) ->
    Now = elib_dt:now(),
    NowP1M = elib_dt:minus(Now, {1, minute}),
    case verification_code_ds:find_by_id(ToEmail) of
        % 60000 = 60 * 1000 = 1分钟
        % #{<<"created_at">> := CreatedAt} when (Now - CreatedAt) < 60000 ->
        %  Now - 1m < CreatedAt
        #{<<"created_at">> := CreatedAt} when NowP1M < CreatedAt ->
            {ok, <<"一分钟内重复请求不发送Email"/utf8>>};
        #{<<"code">> := Code, <<"validity_at">> := ValidityAt} when Now < ValidityAt ->
            Msg = <<"Code is ", Code/binary, " will expire in 10 minutes.">>,
            _ = elib_email:send(ToEmail, Msg),
            {ok, <<"验证码已发送"/utf8>>};
        _ ->
            VerifyCode = elib_cipher:num_random(6),
            VerifyCodeBinary = integer_to_binary(VerifyCode),
            % 600000 = 600 * 1000 = 10分钟
            try elib_dt:add(Now, {10, minute}) of
                ValidityAt when is_binary(ValidityAt) ->
                    _ = verification_code_ds:save(ToEmail, VerifyCodeBinary, ValidityAt, Now),
                    Msg = <<"Code is ", VerifyCodeBinary/binary, " will expire in 10 minutes.">>,
                    _ = elib_email:send(ToEmail, Msg),
                    {ok, <<"验证码已发送"/utf8>>};
                _ ->
                    {error, <<"invalid_datetime"/utf8>>}
            catch
                _:_ ->
                    {error, <<"invalid_datetime"/utf8>>}
            end
    end.

-spec send_sms_code(binary()) -> {ok, binary()}.
send_sms_code(Mobile) ->
    Now = elib_dt:now(),
    NowP2M = elib_dt:minus(Now, {2, minute}),

    case verification_code_ds:find_by_id(Mobile) of
        % 120000 = 120 * 1000 = 2分钟
        #{<<"created_at">> := CreatedAt} when NowP2M < CreatedAt ->
            {ok, <<"两分钟内重复请求不会重复发送"/utf8>>};
        #{<<"code">> := Code, <<"validity_at">> := ValidityAt} when Now < ValidityAt ->
            Content = <<"【IMBoy】您的验证码： "/utf8, (ec_cnv:to_binary(Code))/binary ," ，10分钟内有效。如非本人操作，请忽略！"/utf8>>,
            _ = imboy_sms:send(Mobile, Content, <<"yjsms">>),
            {ok, <<"验证码已发送"/utf8>>};
        _ ->
            Code = elib_cipher:num_random(6),
            CodeBinary = ec_cnv:to_binary(Code),
            % 600000 = 600 * 1000 = 10分钟
            try elib_dt:add(Now, {10, minute}) of
                ValidityAt when is_binary(ValidityAt) ->
                    _ = verification_code_ds:save(Mobile, CodeBinary, ValidityAt, Now),
                    Content = <<"【IMBoy】您的验证码： "/utf8, CodeBinary/binary ," ，10分钟内有效。如非本人操作，请忽略！"/utf8>>,
                    _ = imboy_sms:send(Mobile, Content, <<"yjsms">>),
                    {ok, <<"验证码已发送"/utf8>>};
                _ ->
                    {error, <<"invalid_datetime"/utf8>>}
            catch
                _:_ ->
                    {error, <<"invalid_datetime"/utf8>>}
            end
    end.


%% 校验验证码
-spec verify_code(binary(), binary()) -> {error, binary()} | {ok, binary()}.
verify_code(_Id, <<"666666">>) ->
    % local 和 dev 环境的万能验证码
    case os:getenv("IMBOYENV") of
        "local" ->
            {ok, <<"验证码有效（测试环境）"/utf8>>};
        "dev" ->
            {ok, <<"验证码有效（测试环境）"/utf8>>};
        _ ->
            % 生产环境或其他环境，验证码无效
            {error, <<"验证码无效"/utf8>>}
    end;
verify_code(Id, Code) ->
    Now = elib_dt:now(),
    case verification_code_ds:find_by_id(Id) of
        #{<<"code">> := Code, <<"validity_at">> := ValidityAt} when Now < ValidityAt ->
            {ok, <<"验证码有效"/utf8>>};
        _ ->
            {error, <<"验证码无效"/utf8>>}
    end.


-spec do_signup_by_email(binary(), binary(), map()) ->
          {ok, map()} | {error, binary()} | {error, binary(), Code :: integer()}.
do_signup_by_email(Email, Pwd, PostVals) ->
    % 验证 nickname 不为空
    Nickname = maps:get(<<"nickname">>, PostVals, <<>>),
    case byte_size(Nickname) of
        0 ->
            {error, <<"昵称不能为空"/utf8>>};
        _ ->
            Id = user_ds:find_id_by_email(Email),
            case Id of
                0 ->
                    Password = elib_cipher:rsa_decrypt(Pwd),
                    Data = pick_data_for_insert(#{
                        <<"password">> => elib_password:generate(Password)
                        , <<"email">> => Email
                        }, PostVals),
                    case user_ds:insert_and_get_id(Data) of
                        {ok, _} ->
                            % 注册成功
                            {ok, #{}};
                        {error, {error, error, <<"23505">>, unique_violation, _Msg, _Details}} ->
                            % 唯一约束冲突，返回友好的错误信息
                            {error, <<"账号已被占用"/utf8>>};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {error, <<"Email已经被占用了"/utf8>>}
            end
    end.


-spec do_signup_by_mobile(binary(), binary(), map()) ->
          {ok, map()} | {error, binary() | any()}.
do_signup_by_mobile(Mobile, Pwd, PostVals) ->
    % 验证 nickname 不为空
    Nickname = maps:get(<<"nickname">>, PostVals, <<>>),
    case byte_size(Nickname) of
        0 ->
            {error, <<"昵称不能为空"/utf8>>};
        _ ->
            Id = user_ds:find_id_by_mobile(Mobile),
            case Id of
                0 ->
                    Password = elib_cipher:rsa_decrypt(Pwd),
                    Data = pick_data_for_insert(#{
                        <<"password">> => elib_password:generate(Password)
                        , <<"mobile">> => Mobile
                        }, PostVals),
                    case user_ds:insert_and_get_id(Data) of
                        {ok, _} ->
                            % 注册成功
                            {ok, #{}};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {error, <<"手机号已经被占用了"/utf8>>}
            end
    end.

pick_data_for_insert(Data, PostVals) ->
    Uid0 = elib_hashids:encode(0),

    Source = maps:get(<<"source">>, PostVals, <<>>),
    Ip = maps:get(<<"ip">>, PostVals, <<"{}">>),
    Nickname = maps:get(<<"nickname">>, PostVals, <<>>),
    Avatar = maps:get(<<"avatar">>, PostVals, <<>>),
    Cosv = maps:get(<<"cosv">>, PostVals, <<>>),
    RefUid = maps:get(<<"ref_uid">>, PostVals, Uid0),

    [RefUid2, ParentRefUid2] = case bit_size(RefUid) > 5 of
        true ->
            RefUid2_in = elib_hashids:decode(RefUid),
            P = user_ds:find_by_id(RefUid2_in, <<"ref_user_id">>),
            [RefUid2_in, maps:get(<<"ref_user_id">>, P, 0)];
        _ ->
            [0, 0]
    end,
    % ?DEBUG_LOG(["RefUid2", RefUid2]),
    % ?DEBUG_LOG(["Email", Email]),
    % ?DEBUG_LOG(["Pwd2", Pwd2]),
    % ?DEBUG_LOG(["PostVals", PostVals]),
    % ?DEBUG_LOG(["Ip", Ip]),
    % ?DEBUG_LOG(["Cosv", Cosv]),
    maps:merge(#{
        <<"account">> => account_ds:allocate()
        , <<"nickname">> => Nickname
        , <<"avatar">> => Avatar
        , <<"ref_user_id">> => RefUid2
        , <<"ref_parent_user_id">> => ParentRefUid2
        , <<"reg_ip">> => Ip
        , <<"reg_cosv">> => Cosv
        , <<"source">> => Source
        , <<"status">> => 1
        , <<"created_at">> => elib_dt:now()
    }, Data).

-spec find_password_by_email(Email :: binary(), Pwd :: binary(), PostVals :: map()) ->
          {ok, map()} | {error, Msg :: list()}.
find_password_by_email(Email, Pwd, _PostVals) ->
    Id = user_ds:find_id_by_email(Email),
    case Id of
        0 ->
            {error, <<"Email不存在或已被删除"/utf8>>};
        Id when is_integer(Id), Id > 0 ->
            % 密码已经在 handler 层面处理了解密，这里直接使用
            Pwd2 = elib_password:generate(Pwd),
            case user_ds:update_password(Id, Pwd2) of
                {ok, _} ->
                    {ok, #{}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.


-spec verify_user(binary(), map()) -> {ok, map()} | {error, any()}.
verify_user(<<>>, _) ->
    {error, <<"账号不存在"/utf8>>};
verify_user(Pwd, User) ->
    Pwd2 = maps:get(<<"password">>, User, <<>>),
    % 状态: -1 删除  0 禁用  1 启用  2 申请注销中
    Status = maps:get(<<"status">>, User, -2),
    case elib_password:verify(Pwd, Pwd2) of
        {ok, _} when Status == -2 ->
            {error, <<"账号不存在"/utf8>>};
        {ok, _} when Status == -1 ->
            {error, <<"账号不存在或者已删除"/utf8>>};
        {ok, _} when Status == 0 ->
            {error, <<"账号被禁用"/utf8>>};
        {ok, _} when Status == 1; Status == 2 ->
            {ok, login_resp(User, #{})};
        {error, Msg} ->
            {error, Msg}
    end.

login_resp(User, Resp) ->
    Id = maps:get(<<"id">>, User),
    maps:merge(#{
       <<"uid">> => elib_hashids:encode(Id),
       <<"token">> => token_ds:encrypt_token(Id),
       <<"refreshtoken">> => token_ds:encrypt_refreshtoken(Id),
       <<"email">> => maps:get(<<"email">>, User),
       <<"nickname">> => maps:get(<<"nickname">>, User),
       <<"avatar">> => maps:get(<<"avatar">>, User),
       <<"account">> => maps:get(<<"account">>, User),
       <<"gender">> => maps:get(<<"gender">>, User),
       <<"region">> => maps:get(<<"region">>, User),
       <<"sign">> => maps:get(<<"sign">>, User),
       <<"status">> => maps:get(<<"status">>, User),
       <<"role">> => 1
      }, Resp).

validate_compat_password(<<>>) ->
    {error, <<"密码不能为空"/utf8>>};
validate_compat_password(Pwd) when is_binary(Pwd) ->
    Len = byte_size(Pwd),
    HasDigit = lists:any(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Pwd)),
    HasAlpha = lists:any(
        fun(C) ->
            (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z)
        end, binary_to_list(Pwd)),
    case Len >= 8 andalso HasDigit andalso HasAlpha of
        true ->
            ok;
        false ->
            {error, <<"密码格式不正确"/utf8>>}
    end.

compat_signup_nickname(Mobile, PostVals) ->
    case maps:get(<<"nickname">>, PostVals, <<>>) of
        <<>> ->
            Mobile;
        Nickname ->
            Nickname
    end.

prepare_compat_mobile(Mobile) ->
    case user_repo:find_by_mobile(Mobile, <<"id,status">>) of
        #{<<"id">> := Id, <<"status">> := Status} when Status < 0 ->
            ok = user_ds:delete_all_related_data(Id),
            ok;
        #{<<"id">> := _Id} ->
            {error, <<"手机号已经被占用了"/utf8>>, 400};
        _ ->
            ok
    end.

compat_signup_email(Email) ->
    case elib_type:is_email(Email) of
        false ->
            undefined;
        true ->
            case user_repo:find_by_email(Email, <<"id,status">>) of
                #{<<"id">> := Id, <<"status">> := Status} when Status < 0 ->
                    ok = user_ds:delete_all_related_data(Id),
                    Email;
                #{<<"id">> := _Id} ->
                    undefined;
                _ ->
                    Email
            end
    end.

compat_login_type(Account) ->
    case elib_type:is_email(Account) of
        true ->
            <<"email">>;
        false ->
            <<"mobile">>
    end.

compat_device_type(PostVals, Did) ->
    case maps:get(<<"dtype">>, PostVals, maps:get(<<"cos">>, PostVals, <<>>)) of
        DType when DType == <<"ios">>; DType == <<"android">>; DType == <<"macos">>;
                    DType == <<"windows">>; DType == <<"linux">>; DType == <<"web">> ->
            DType;
        _ ->
            infer_device_type(Did)
    end.

infer_device_type(Did) when is_binary(Did) ->
    LowerDid = cowboy_bstr:to_lower(Did),
    case binary:match(LowerDid, <<"android">>) of
        {_, _} ->
            <<"android">>;
        nomatch ->
            case binary:match(LowerDid, <<"web">>) of
                {_, _} ->
                    <<"web">>;
                nomatch ->
                    case binary:match(LowerDid, <<"mac">>) of
                        {_, _} ->
                            <<"macos">>;
                        nomatch ->
                            case binary:match(LowerDid, <<"win">>) of
                                {_, _} ->
                                    <<"windows">>;
                                nomatch ->
                                    case binary:match(LowerDid, <<"linux">>) of
                                        {_, _} ->
                                            <<"linux">>;
                                        nomatch ->
                                            <<"ios">>
                                    end
                            end
                    end
            end
    end.

record_compat_login_success(Data, PostVals) ->
    UidHash = maps:get(<<"uid">>, Data, <<>>),
    Uid = elib_hashids:decode(UidHash),
    Did = maps:get(<<"did">>, PostVals, <<>>),
    Now = elib_dt:now(),
    _ = user_device_ds:save(Now, Uid, Did, PostVals),
    _ = user_ds:update_friends_last_seen_at(Uid, Now),
    _ = message_ds:check_and_notify_offline_msgs(Uid),
    ok.

compat_error_message(#{<<"reason">> := Reason}) when is_binary(Reason) ->
    Reason;
compat_error_message(Map) when is_map(Map) ->
    jsx:encode(Map);
compat_error_message(Reason) when is_binary(Reason) ->
    Reason;
compat_error_message(Reason) when is_list(Reason) ->
    unicode:characters_to_binary(Reason);
compat_error_message(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
compat_error_message(Reason) ->
    ec_cnv:to_binary(Reason).
