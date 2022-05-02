-module(passport_logic).
%%%
% passport_logic 是 passport_logic application logic 缩写
%%%

-export ([send_email_code/1]).
-export ([do_login/3]).
-export ([do_signup/5]).
-export ([find_password/5]).

-include("common.hrl").

-spec send_email_code(ToEmail::binary()) -> {error, Msg::list()} | {ok, any()}.
send_email_code(undefined) ->
    {error, "Email必须"};
% send_email_code(ToEmail) ->
%     {error, "Email必须"};
send_email_code(ToEmail) ->
    Now = dt_util:milliseconds(),
    case verification_code_repo:get_by_id(ToEmail) of
        % 60000 = 60 * 1000 = 1分钟
        {ok, _Col, [[_ToEmail, _Code, _ValidityAt, CreatedAt]]} when (Now - CreatedAt) < 60000 ->
            {ok, "一分钟内重复请求不发送Email"};
        {ok, _Col, [[ToEmail, Code, ValidityAt, _CreatedAt]]} when Now < ValidityAt ->
            func:send_email(ToEmail, "Code is " ++ binary_to_list(Code) ++ " will expire in 10 minutes.");
        % {ok, _Col,[]} ->
        _ ->
            VerifyCode = func:num_random(6),
            % 600000 = 600 * 1000 = 10分钟
            verification_code_repo:save(ToEmail, VerifyCode, Now + 600000, Now),
            func:send_email(ToEmail, "Code is " ++ integer_to_list(VerifyCode) ++ " will expire in 10 minutes.")
    end.

-spec do_login(Type::binary(), Email::binary(), Pwd::binary()) -> {ok, any()} | {error, any()}.
do_login(Type, Email, Pwd) when Type == <<"email">> ->
    Column = <<"`id`,`account`,`email`,`password`,`nickname`,`avatar`,`gender`">>,
    case func:is_email(Email) of
        true ->
            {Check, User} = case user_repo:find_by_email(Email, Column) of
                {ok, _FieldList, [[Id, Account, Email, Password, Nickname, Avatar, Gender]]} ->
                    % ?LOG([Pwd, Password]),
                    case password_util:verify(Pwd, Password) of
                        {ok, _} ->
                            {true, [Id, Account, Email, Nickname, Avatar, Gender]};
                        {error, Msg} ->
                            {false, Msg}
                    end;
                _ ->
                    % io:format("res is ~p~n",[Res]),
                    {false, []}
            end,
            % ?LOG([Check, User]),
            if Check == true ->
                    {ok, login_success_aas:data(User)};
                true ->
                    {error, "账号或密码错误"}
            end;
        false ->
            {error, "Email格式有误"}
    end;
do_login(Type, Mobile, Pwd) when Type == <<"mobile">> ->
    Column = <<"`id`,`account`,`mobile`,`password`,`nickname`,`avatar`,`gender`">>,
    Res = case func:is_mobile(Mobile) of
        true ->
            user_repo:find_by_mobile(Mobile, Column);
        false ->
            user_repo:find_by_account(Mobile, Column)
    end,
    % ?LOG(Res),
    {Check, User} = case Res of
        {ok, _FieldList, [[Id, Account, Password, Nickname, Avatar, Gender]]} ->
            ?LOG([Pwd, Password]),
            case password_util:verify(Pwd, Password) of
                {ok, _} ->
                    {true, [Id, Account, Nickname, Avatar, Gender]};
                {error, Msg} ->
                    {false, Msg}
            end;
        _ ->
            % io:format("res is ~p~n",[Res]),
            {false, []}
    end,
    ?LOG([Check, User]),
    if Check == true ->
            {ok, login_success_aas:data(User)};
        true ->
            {error, "账号或密码错误"}
    end.


-spec do_signup(Type::binary(), EmailOrMobile::binary(), Pwd::binary(), Code::binary(), PostVals::list()) ->
    {ok, Msg::list()} |
    {error, Msg::list()} |
    {error, Msg::list(), Code::integer()}.
do_signup(Type, Email, Pwd, Code, PostVals) when Type == <<"email">> ->
    case func:is_email(Email) of
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
do_signup(Type, Mobile, Pwd, Code, PostVals) when Type == <<"mobile">> ->
    case func:is_mobile(Mobile) of
        true ->
            do_signup_by_mobile(Mobile, Pwd, Code, PostVals);
        false ->
            {error, "Email格式有误"}
    end;
do_signup(_Type, _Account, _Pwd, _Code, _PostVals) ->
    {error, "不支持的注册类型"}.


-spec find_password(Type::binary(), EmailOrMobile::binary(), Pwd::binary(), Code::binary(), PostVals::list()) ->
    {ok, Msg::list()} |
    {error, Msg::list()} |
    {error, Msg::list(), Code::integer()}.
find_password(Type, Email, Pwd, Code, PostVals) when Type == <<"email">> ->
    case func:is_email(Email) of
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
%     case func:is_mobile(Mobile) of
%         true ->
%             find_password_by_mobile(Mobile, Pwd, Code, PostVals);
%         false ->
%             {error, "电话号码格式有误"}
%     end;
find_password(_Type, _Account, _Pwd, _Code, _PostVals) ->
    {error, "不支持的注册类型"}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% 校验验证码
-spec verify_code(Id::binary(), VerifyCode::binary()) -> {error, Msg::list()} | {ok, any()}.
verify_code(Id, Code) ->
    Now = dt_util:milliseconds(),
    case verification_code_repo:get_by_id(Id) of
        {ok, _Col, [[_ToEmail, Code, ValidityAt, _CreatedAt]]} when Now < ValidityAt ->
            {ok, "验证码有效"};
        {ok, _Col, [[_ToEmail, _Code, _ValidityAt, _CreatedAt]]} ->
            {error, "验证码无效"};
        {ok, _Col,[]} ->
            {error, "验证码无效"}
    end.

-spec do_signup_by_email(Email::binary(), Pwd::binary(), PostVals::list()) ->
    {ok, Msg::list()} |
    {error, Msg::list()} |
    {error, Msg::list(), Code::integer()}.
do_signup_by_email(Email, Pwd, PostVals) ->
    case user_repo:find_by_email(Email, <<"`email`">>) of
        {ok,_Col,[_Email]} ->
            {error, "Email已经被占用了"};
        {ok,_Col,[]} ->
            Password = imboy_cipher:rsa_decrypt(Pwd),
            Now = dt_util:milliseconds(),
            poolboy:transaction(mysql, fun(Pid) ->
                Prefix = <<"INSERT INTO">>,
                Table = <<"`user`">>,
                Column = <<"(`account`,`email`,`password`,`ref_user_id`,`reg_ip`,`reg_cosv`,`status`,`created_at`)">>,
                Pwd2 = password_util:generate(Password),
                Now2 = integer_to_binary(Now),
                Status = integer_to_binary(1),

                Ip = proplists:get_value(<<"ip">>, PostVals, {}),
                Cosv = proplists:get_value(<<"cosv">>, PostVals, <<"">>),
                RefUid = proplists:get_value(<<"ref_uid">>, PostVals, hashids_translator:uid_encode(0)),
                RefUid2 = if
                    bit_size(RefUid) > 5 ->
                        integer_to_binary(hashids_translator:uid_decode(RefUid));
                    bit_size(RefUid) =< 5  ->
                        <<"0">>
                end,
                Account = integer_to_binary(account_server:allocate()),
                ?LOG([{"Account", Account},{"RefUid2", RefUid2}, {"Ip", Ip}, {"Cosv", Cosv}, {"PostVals", PostVals}]),
                Value = <<"('",
                    Account/binary, "', '",
                    Email/binary, "', '",
                    Pwd2/binary, "', '",
                    RefUid2/binary, "', '",
                    Ip/binary, "', '",
                    Cosv/binary, "', '",
                    Status/binary, "', '",
                    Now2/binary, "')">>,

                Sql = mysql_pool:assemble_sql(Prefix, Table, Column, Value),
                case mysql:query(Pid, Sql) of
                    ok ->
                        {ok, mysql:insert_id(Pid)};
                    Res ->
                        Res
                end
            end),
            % 注册成功
            {ok, []}
    end.


-spec do_signup_by_mobile(Account::binary(), Pwd::binary(), Code::binary(), PostVals::list()) ->
    {ok, Msg::list()} |
    {error, Msg::list()} |
    {error, Msg::list(), Code::integer()}.
do_signup_by_mobile(_Account, _Pwd, _Code, _PostVals) ->
    % Column = <<"`id`,`account`,`password`,`mobile`">>,
    {error, "暂时不支持手机号码注册"}.


-spec find_password_by_email(Email::binary(), Pwd::binary(), PostVals::list()) ->
    {ok, Msg::list()} |
    {error, Msg::list()} |
    {error, Msg::list(), Code::integer()}.
find_password_by_email(Email, Pwd, _PostVals) ->
    case user_repo:find_by_email(Email, <<"`id`,`email`">>) of
        {ok,_Col,[]} ->
            {error, "Email不存在或已被删除"};
        {ok,_Col,[[Id, _Email]]} ->
            Password = imboy_cipher:rsa_decrypt(Pwd),
            % Now = dt_util:milliseconds(),
            poolboy:transaction(mysql, fun(Pid) ->
                Pwd2 = password_util:generate(Password),
                Sql = <<"UPDATE `user` SET `password` = ? WHERE `id` = ?">>,
                case mysql:query(Pid, Sql, [Pwd2, Id]) of
                    ok ->
                        {ok, "操作成功"};
                    Res ->
                        Res
                end
            end),
            % 注册成功
            {ok, []}
    end.
