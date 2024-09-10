-module(passport_logic).
%%%
% passport_logic 是 passport application logic 缩写
%%%

-export([send_code/2]).
-export([do_login/3]).
-export([do_signup/5]).
-export([find_password/5]).
-export([verify_user/2]).
-export([quick_login/4]).

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/def_column.hrl").


quick_login(<<"jverify">>, _Operator, Token, PostVals) ->
    case imboy_sms:jverification(Token) of
        {error, Msg} ->
            {error, Msg};
        {ok, Mobile} ->
            Mobile2 = <<"+86", Mobile/binary>>,
            User = user_repo:find_by_mobile(Mobile2, ?LOGIN_COLUMN),
            Uid = maps:get(<<"id">>, User, 0),
            case Uid of
                0 ->
                    Tb = user_repo:tablename(),
                    Data = pick_data_for_insert(#{
                        <<"source">> => <<"jverify">>
                        , <<"mobile">> => Mobile2
                        , <<"password">> => <<>>
                        }, PostVals),
                    {ok, _, [{Uid2}]} = imboy_db:insert_into(Tb, Data, <<"RETURNING id;">>),
                    User2 = user_repo:find_by_id(Uid2, ?LOGIN_COLUMN),
                    {ok, login_resp(User2, #{<<"action">> => <<"need_set_password">>})};
                _ ->
                    {ok, login_resp(User, #{})}
            end
    end;
quick_login(_, _, _, _) ->
    {error, <<"不支持的已经登录服务"/utf8>>}.


% passport_logic:send_code(<<"">>, <<"sms">>).
% passport_logic:send_code(<<"">>, <<"email">>).
send_code(Mobile, <<"sms">>) ->
    % Res = throttle:check(per_minute_once, Mobile),
    % ?LOG([per_minute_once, Res]),
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
    {error, <<"暂未实现功能.">>}.


-spec do_login(binary(), binary(), binary()) -> {ok, any()} | {error, any()}.
do_login(_Type, _Email, <<>>) ->
    {error, "密码有误"};
do_login(Type, Mobile, Pwd) when Type == <<"mobile">> ->
    User = user_repo:find_by_mobile(Mobile, ?LOGIN_COLUMN),
    % ?LOG([do_login, Mobile, Pwd, User]),
    verify_user(Pwd, User);
do_login(Type, Email, Pwd) when Type == <<"email">> ->
    case imboy_func:is_email(Email) of
        true ->
            User = user_repo:find_by_email(Email, ?LOGIN_COLUMN),
            verify_user(Pwd, User);
        false ->
            {error, "Email格式有误"}
    end;
do_login(Type, Account, Pwd) when Type == <<"account">> ->
    User = case imboy_func:is_email(Account) of
        true ->
            user_repo:find_by_email(Account, ?LOGIN_COLUMN);
        false ->
            user_repo:find_by_account(Account, ?LOGIN_COLUMN)
    end,
    verify_user(Pwd, User).


-spec do_signup(Type :: binary(), EmailOrMobile :: binary(), Pwd :: binary(), Code :: binary(), PostVals :: list()) ->
          {ok, Msg :: list()} | {error, Msg :: list()} | {error, Msg :: list(), Code :: integer()}.
do_signup(<<"email">>, Email, Pwd, Code, PostVals) ->
    case imboy_func:is_email(Email) of
        true ->
            % 校验验证码
            case verify_code(Email, Code) of
                {ok, _} ->
                    do_signup_by_email(Email, Pwd, PostVals);
                {error, Msg} ->
                    {error, Msg}
            end;
        false ->
            {error, "Email格式有误"}
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
    {error, "不支持的注册类型"}.


-spec find_password(Type :: binary(),
                    EmailOrMobile :: binary(),
                    Pwd :: binary(),
                    Code :: binary(),
                    PostVals :: list()) ->
          {ok, Msg :: list()} | {error, Msg :: list()} | {error, Msg :: list(), Code :: integer()}.
find_password(Type, Email, Pwd, Code, PostVals) when Type == <<"email">> ->
    case imboy_func:is_email(Email) of
        true ->
            % 校验验证码
            case verify_code(Email, Code) of
                {ok, _} ->
                    find_password_by_email(Email, Pwd, PostVals);
                {error, Msg} ->
                    {error, Msg}
            end;
        false ->
            {error, "Email格式有误"}
    end;
find_password(_Type, _Account, _Pwd, _Code, _PostVals) ->
    {error, "不支持的注册类型"}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


-spec send_email_code(binary()) -> {error, list()} | {ok, any()}.
send_email_code(undefined) ->
    {error, "Email必须"};
send_email_code(ToEmail) ->
    Now = imboy_dt:utc(millisecond),
    case verification_code_repo:find_by_id(ToEmail) of
        % 60000 = 60 * 1000 = 1分钟
        {ok, _Col, [{_, _, _, CreatedAt}]} when (Now - CreatedAt) < 60000 ->
            {ok, "一分钟内重复请求不发送Email"};
        {ok, _Col, [{ToEmail, Code, ValidityAt, _}]} when Now < ValidityAt ->
            Msg = <<"Code is ", Code/binary, " will expire in 10 minutes.">>,
            % ?LOG(Msg),
            % {ok, Msg};
            imboy_func:send_email(ToEmail, Msg);
        % {ok, _Col, []} ->
        _ ->
            VerifyCode = imboy_func:num_random(6),
            % 600000 = 600 * 1000 = 10分钟
            verification_code_repo:save(ToEmail, VerifyCode, Now + 600000, Now),
            Code2 = integer_to_binary(VerifyCode),
            Msg = <<"Code is ", Code2/binary, " will expire in 10 minutes.">>,
            % ?LOG(Msg),
            % {ok, Msg}
            imboy_func:send_email(ToEmail, Msg)
    end.

send_sms_code(Mobile) ->
    Now = imboy_dt:utc(millisecond),
    case verification_code_repo:find_by_id(Mobile) of
        % 120000 = 120 * 1000 = 2分钟
        {ok, _Col, [{_, _, _, CreatedAt}]} when (Now - CreatedAt) < 120000 ->
            {ok, "两分钟内重复请求不会重复发送"};
        {ok, _Col, [{Mobile, Code, ValidityAt, _}]} when Now < ValidityAt ->
            Content = <<"【IMBoy】您的验证码： "/utf8, (ec_cnv:to_binary(Code))/binary ," ，10分钟内有效。如非本人操作，请忽略！"/utf8>>,
            imboy_sms:send(Mobile, Content, <<"yjsms">>);
        % {ok, _Col, []} ->
        _ ->
            Code = imboy_func:num_random(6),
            % 600000 = 600 * 1000 = 10分钟
            verification_code_repo:save(Mobile, Code, Now + 600000, Now),
            Content = <<"【IMBoy】您的验证码： "/utf8, (ec_cnv:to_binary(Code))/binary ," ，10分钟内有效。如非本人操作，请忽略！"/utf8>>,
            imboy_sms:send(Mobile, Content, <<"yjsms">>)
    end.


%% 校验验证码
-spec verify_code(binary(), binary()) -> {error, list()} | {ok, list()}.
verify_code(Id, Code) ->
    Now = imboy_dt:utc(millisecond),
    case verification_code_repo:find_by_id(Id) of
        {ok, _Col, [{_, Code, ValidityAt, _}]} when Now < ValidityAt ->
            {ok, "验证码有效"};
        _ ->
            {error, "验证码无效"}
    end.


-spec do_signup_by_email(binary(), binary(), list()) ->
          {ok, Msg :: list()} | {error, Msg :: list()} | {error, Msg :: list(), Code :: integer()}.
do_signup_by_email(Email, Pwd, PostVals) ->
    Tb = user_repo:tablename(),
    Id = imboy_db:pluck(Tb
        , <<"email='", Email/binary, "'">>
        , <<"id">>
        , 0),
    case Id of
        0 ->
            Password = imboy_cipher:rsa_decrypt(Pwd),
            Data = pick_data_for_insert(#{
                <<"password">> => imboy_password:generate(Password)
                , <<"email">> => Email
                }, PostVals),
            % {ok, _, [{Uid}]} = imboy_db:insert_into(Tb, Data),
            {ok, _, _} = imboy_db:insert_into(Tb, Data),
            % 注册成功
            {ok, #{}};
        _ ->
            {error, "Email已经被占用了"}
    end.


-spec do_signup_by_mobile(binary(), binary(), list()) ->
          {ok, list()} | {error, list()}.
do_signup_by_mobile(Mobile, Pwd, PostVals) ->
    Tb = user_repo:tablename(),
    Id = imboy_db:pluck(Tb
        , <<"mobile='", Mobile/binary, "'">>
        , <<"id">>
        , 0),
    case Id of
        0 ->
            Password = imboy_cipher:rsa_decrypt(Pwd),
            Data = pick_data_for_insert(#{
                <<"password">> => imboy_password:generate(Password)
                , <<"mobile">> => Mobile
                }, PostVals),
            {ok, _, _} = imboy_db:insert_into(Tb, Data),
            % {ok, _, [{Uid}]} = imboy_db:insert_into(Tb, Data),
            % ["do_signup_by_mobile",{ok,1,[{43}]}]
            % ?LOG(["do_signup_by_mobile", Uid]),
            % 注册成功
            {ok, #{}};
        _ ->
            {error, "手机号已经被占用了"}
    end.

pick_data_for_insert(Data, PostVals) ->
    Uid0 = imboy_hashids:encode(0),

    Source = proplists:get_value(<<"source">>, PostVals, <<>>),
    Ip = proplists:get_value(<<"ip">>, PostVals, <<"{}">>),
    Nickname = proplists:get_value(<<"nickname">>, PostVals, <<>>),
    Avatar = proplists:get_value(<<"avatar">>, PostVals, <<>>),
    Cosv = proplists:get_value(<<"cosv">>, PostVals, <<>>),
    RefUid = proplists:get_value(<<"ref_uid">>, PostVals, Uid0),

    [RefUid2, ParentRefUid2] = case bit_size(RefUid) > 5 of
        true ->
            RefUid2_in = imboy_hashids:decode(RefUid),
            P = user_repo:find_by_id(RefUid2_in, <<"ref_user_id">>),
            [RefUid2_in, maps:get(<<"ref_user_id">>, P, 0)];
        _ ->
            [0, 0]
    end,
    % ?LOG(["RefUid2", RefUid2]),
    Account = integer_to_binary(account_server:allocate()),
    % ?LOG(["Email", Email]),
    % ?LOG(["Pwd2", Pwd2]),
    % ?LOG(["PostVals", PostVals]),
    % ?LOG(["Ip", Ip]),
    % ?LOG(["Cosv", Cosv]),
    maps:merge(#{
        <<"account">> => Account
        , <<"nickname">> => Nickname
        , <<"avatar">> => Avatar
        , <<"ref_user_id">> => RefUid2
        , <<"ref_parent_user_id">> => ParentRefUid2
        , <<"reg_ip">> => Ip
        , <<"reg_cosv">> => Cosv
        , <<"source">> => Source
        , <<"status">> => 1
        , <<"created_at">> => imboy_dt:utc(millisecond)
    }, Data).

-spec find_password_by_email(Email :: binary(), Pwd :: binary(), PostVals :: list()) ->
          {ok, Msg :: list()} | {error, Msg :: list()} | {error, Msg :: list(), Code :: integer()}.
find_password_by_email(Email, Pwd, _PostVals) ->
    Id = imboy_db:pluck(user_repo:tablename()
        , <<"email='", Email/binary, "'">>
        , <<"id">>
        , 0),
    case Id of
        0 ->
            {error, "Email不存在或已被删除"};
        Id ->
            PwdPlaintext = imboy_cipher:rsa_decrypt(Pwd),
            % Now = imboy_dt:utc(millisecond),
            Pwd2 = imboy_password:generate(PwdPlaintext),
            Where = <<"id=", (ec_cnv:to_binary(Id))/binary>>,
            Res = imboy_db:update(user_repo:tablename()
                , Where
                , #{<<"password">> => Pwd2}
            ),
            case Res of
                {ok, _} ->
                    {ok, #{}};
                Res ->
                    Res
            end
    end.


-spec verify_user(binary(), map()) -> {ok, map()} | {error, any()}.
verify_user(<<>>, _) ->
    {error, "账号不存在"};
verify_user(Pwd, User) ->
    Pwd2 = maps:get(<<"password">>, User, <<>>),
    % 状态: -1 删除  0 禁用  1 启用  2 申请注销中
    Status = maps:get(<<"status">>, User, -2),
    case imboy_password:verify(Pwd, Pwd2) of
        {ok, _} when Status == -2 ->
            {error, "账号不存在"};
        {ok, _} when Status == -1 ->
            {error, "账号不存在或者已删除"};
        {ok, _} when Status == 0 ->
            {error, "账号被禁用"};
        {ok, _} when Status == 1; Status == 2 ->
            {ok, login_resp(User, #{})};
        {error, Msg} ->
            {error, Msg}
    end.

login_resp(User, Resp) ->
    Id = maps:get(<<"id">>, User),
    maps:merge(#{
       <<"uid">> => imboy_hashids:encode(Id),
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
