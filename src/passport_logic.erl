-module(passport_logic).
%%%
% passport_logic 是 passport application logic 缩写
%%%

-export([send_email_code/1]).
-export([do_login/3]).
-export([do_signup/5]).
-export([find_password/5]).

-include_lib("imboy/include/log.hrl").

% password 给元素顺序不能够随意修改，
% login_success_transfer 按顺序匹配了
% 密码校验取第4位为密码数据 lists:nth(4, Row)
-define (LOGIN_COLUMN, <<"id,account,mobile,password,
        nickname,avatar,gender,region,sign">>).

-spec send_email_code(ToEmail :: binary()) ->
          {error, Msg :: list()} | {ok, any()}.
send_email_code(undefined) ->
    {error, "Email必须"};
% send_email_code(ToEmail) ->
%     {error, "Email必须"};
send_email_code(ToEmail) ->
    Now = imboy_dt:millisecond(),
    case verification_code_repo:get_by_id(ToEmail) of
        % 60000 = 60 * 1000 = 1分钟
        {ok, _Col, [[_, _, _, CreatedAt]]}
          when (Now - CreatedAt) < 60000 ->
            {ok, "一分钟内重复请求不发送Email"};
        {ok, _Col, [[ToEmail, Code, ValidityAt, _]]}
          when Now < ValidityAt ->
            CodeLi = binary_to_list(Code),
            Msg = "Code is " ++ CodeLi ++ " will expire in 10 minutes.",
            % ?LOG(Msg),
            % {ok, Msg};
            imboy_func:send_email(ToEmail, Msg);
        % {ok, _Col, []} ->
        _ ->
            VerifyCode = imboy_func:num_random(6),
            % 600000 = 600 * 1000 = 10分钟
            verification_code_repo:save(ToEmail,
                                        VerifyCode,
                                        Now + 600000,
                                        Now),
            CodeLi = integer_to_list(VerifyCode),
            Msg = "Code is " ++ CodeLi ++ " will expire in 10 minutes.",
            % ?LOG(Msg),
            % {ok, Msg}
            imboy_func:send_email(ToEmail, Msg)
    end.


-spec do_login(Type :: binary(), Email :: binary(), Pwd :: binary()) ->
          {ok, any()} | {error, any()}.
do_login(Type, Email, Pwd) when Type == <<"email">> ->
    case imboy_func:is_email(Email) of
        true ->
            {Check, User} = case user_repo:find_by_email(Email, ?LOGIN_COLUMN) of
                {ok, _, [Row]} when is_tuple(Row) ->
                    % 第四个元素为password
                    case imboy_password:verify(Pwd, element(4, Row)) of
                        {ok, _} ->
                            {true, Row};
                        {error, Msg} ->
                            {false, Msg}
                    end;
                _ ->
                    {false, []}
            end,
            login_success_transfer(Check, User);
        false ->
            {error, "Email格式有误"}
    end;
do_login(Type, Mobile, Pwd) when Type == <<"mobile">> ->
    Res = case imboy_func:is_mobile(Mobile) of
        true ->
            user_repo:find_by_mobile(Mobile, ?LOGIN_COLUMN);
        false ->
            user_repo:find_by_account(Mobile, ?LOGIN_COLUMN)
    end,
    % ?LOG(Res),
    {Check, User} = case Res of
        {ok, _, [Row]} when is_tuple(Row) ->
            % 第四个元素为password
            case imboy_password:verify(Pwd, element(4, Row)) of
                {ok, _} ->
                    {true, Row};
                {error, Msg} ->
                    {false, Msg}
            end;
        _ ->
            % io:format("res is ~p~n", [Res]),
            {false, []}
    end,
    login_success_transfer(Check, User).


-spec do_signup(Type :: binary(),
                EmailOrMobile :: binary(),
                Pwd :: binary(),
                Code :: binary(),
                PostVals :: list()) ->
          {ok, Msg :: list()} |
          {error, Msg :: list()} |
          {error, Msg :: list(), Code :: integer()}.
do_signup(Type, Email, Pwd, Code, PostVals) when Type == <<"email">> ->
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
do_signup(Type, Mobile, Pwd, Code, PostVals)
  when Type == <<"mobile">> ->
    case imboy_func:is_mobile(Mobile) of
        true ->
            do_signup_by_mobile(Mobile, Pwd, Code, PostVals);
        false ->
            {error, "Email格式有误"}
    end;
do_signup(_Type, _Account, _Pwd, _Code, _PostVals) ->
    {error, "不支持的注册类型"}.


-spec find_password(Type :: binary(),
                    EmailOrMobile :: binary(),
                    Pwd :: binary(),
                    Code :: binary(),
                    PostVals :: list()) ->
          {ok, Msg :: list()} |
          {error, Msg :: list()} |
          {error, Msg :: list(), Code :: integer()}.
find_password(Type, Email, Pwd, Code, PostVals)
  when Type == <<"email">> ->
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
% find_password(Type, Mobile, Pwd, Code, PostVals) when Type == <<"mobile">> ->
%     case imboy_func:is_mobile(Mobile) of
%         true ->
%             find_password_by_mobile(Mobile, Pwd, Code, PostVals);
%         false ->
%             {error, "电话号码格式有误"}
%     end;
find_password(_Type, _Account, _Pwd, _Code, _PostVals) ->
    {error, "不支持的注册类型"}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 校验验证码
-spec verify_code(Id :: binary(), VerifyCode :: binary()) ->
          {error, Msg :: list()} | {ok, any()}.
verify_code(Id, Code) ->
    Now = imboy_dt:millisecond(),
    case verification_code_repo:get_by_id(Id) of
        {ok, _Col, [[_, Code, ValidityAt, _]]} when Now < ValidityAt ->
            {ok, "验证码有效"};
        {ok, _Col, [[_ToEmail, _Code, _ValidityAt, _CreatedAt]]} ->
            {error, "验证码无效"};
        {ok, _Col, []} ->
            {error, "验证码无效"}
    end.


-spec do_signup_by_email(binary(), binary(), list()) ->
          {ok, Msg :: list()} |
          {error, Msg :: list()} |
          {error, Msg :: list(), Code :: integer()}.
do_signup_by_email(Email, Pwd, PostVals) ->
    % ?LOG([do_signup_by_email, Email, Pwd, PostVals]),
    case user_repo:find_by_email(Email, <<"email">>) of
        {ok, _Col, [_]} ->
            {error, "Email已经被占用了"};
        {ok, _Col, []} ->
            Password = imboy_cipher:rsa_decrypt(Pwd),
            Now = imboy_dt:millisecond(),
            Table = <<"user">>,
            Column = <<"(account,email,password,ref_user_id,
                reg_ip,reg_cosv,status,created_at)">>,
            Pwd2 = imboy_password:generate(Password),
            Now2 = integer_to_binary(Now),
            Status = integer_to_binary(1),
            Ip = proplists:get_value(<<"ip">>, PostVals, <<"{}">>),
            Cosv = proplists:get_value(<<"cosv">>, PostVals, <<"">>),
            Uid0 = imboy_hashids:uid_encode(0),
            RefUid = proplists:get_value(<<"ref_uid">>, PostVals, Uid0),
            RefUid2 = case bit_size(RefUid) > 5 of
                true ->
                   integer_to_binary(imboy_hashids:uid_decode(RefUid));
                _ ->
                   <<"0">>
            end,
            % ?LOG(["RefUid2", RefUid2]),
            Account = integer_to_binary(account_server:allocate()),
            % ?LOG(["Email", Email]),
            % ?LOG(["Pwd2", Pwd2]),
            % ?LOG(["PostVals", PostVals]),
            % ?LOG(["Ip", Ip]),
            % ?LOG(["Cosv", Cosv]),
            Value = <<"('", Account/binary,
                     "', '", Email/binary,
                     "', '", Pwd2/binary,
                     "', '", RefUid2/binary,
                     "', '", Ip/binary,
                     "', '", Cosv/binary,
                     "', '", Status/binary,
                     "', '", Now2/binary,
                     "')">>,
            imboy_db:insert_into(Table, Column, Value),
            % 注册成功
            {ok, #{}}
    end.


-spec do_signup_by_mobile(Account :: binary(),
                          Pwd :: binary(),
                          Code :: binary(),
                          PostVals :: list()) ->
          {ok, Msg :: list()} |
          {error, Msg :: list()} |
          {error, Msg :: list(), Code :: integer()}.
do_signup_by_mobile(_Account, _Pwd, _Code, _PostVals) ->
    % Column = <<"id,account,password,mobile">>,
    {error, "暂时不支持手机号码注册"}.


-spec find_password_by_email(Email :: binary(),
                             Pwd :: binary(),
                             PostVals :: list()) ->
          {ok, Msg :: list()} |
          {error, Msg :: list()} |
          {error, Msg :: list(), Code :: integer()}.
find_password_by_email(Email, Pwd, _PostVals) ->
    case user_repo:find_by_email(Email, <<"id,email">>) of
        {ok, _Col, []} ->
            {error, "Email不存在或已被删除"};
        {ok, _Col, [{Id, _Email}]} ->
            Password = imboy_cipher:rsa_decrypt(Pwd),
            % Now = imboy_dt:millisecond(),
            Tb2 = user_repo:tablename(),
            Pwd2 = imboy_password:generate(Password),
           Res = imboy_db:update(Tb2, Id, <<"password">>, Pwd2),
           case Res of
               {ok, _} ->
                   {ok, #{}};
               Res ->
                   Res
           end
    end.


-spec login_success_transfer(boolean(), tuple()) ->
    {ok, any()} | {error, any()}.
login_success_transfer(true, {Id, Account, _, _, Nickname, Avatar, Gender, Region, Sign}) ->
    {ok, [
        {<<"token">>, token_ds:encrypt_token(Id)},
        {<<"refreshtoken">>, token_ds:encrypt_refreshtoken(Id)},
        {<<"uid">>, imboy_hashids:uid_encode(Id)},
        {<<"nickname">>, Nickname},
        {<<"avatar">>, Avatar},
        {<<"account">>, Account},
        {<<"gender">>, Gender},
        {<<"region">>, Region},
        {<<"sign">>, Sign},
        {<<"role">>, 1}]
    };
login_success_transfer(_, User) ->
    ?LOG([User]),
    {error, "账号或密码错误"}.
