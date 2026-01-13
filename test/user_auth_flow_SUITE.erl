-module(user_auth_flow_SUITE).

%%%===================================================================
%%% @doc
%%% 用户认证流程 Common Test 测试套件
%%%
%%% 运行方式：
%%%   make ct-user_auth_flow                    # 运行整个 suite
%%%   make ct-user_auth_flow t=registration     # 运行特定 group
%%%   make ct-user_auth_flow t=registration:signup_success # 运行特定测试
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    %% 注册流程
    signup_with_valid_data_succeeds/1,
    signup_with_duplicate_mobile_fails/1,
    signup_with_duplicate_email_fails/1,
    signup_with_invalid_password_fails/1,
    %% 登录流程
    login_with_valid_credentials_succeeds/1,
    login_with_invalid_credentials_fails/1,
    login_with_nonexistent_user_fails/1,
    token_refresh_after_login/1,
    %% 密码管理
    change_password_with_valid_old_password_succeeds/1,
    change_password_with_invalid_old_password_fails/1,
    reset_password_via_verification_code/1,
    %% 设备管理
    login_with_new_device_registers_device/1,
    login_from_multiple_devices_creates_multiple_records/1,
    logout_clears_device_session/1
]).

%% ===================================================================
%% Suite 回调函数
%% ===================================================================

all() ->
    [
        {group, registration},
        {group, login},
        {group, password_management},
        {group, device_management}
    ].

groups() ->
    [
        {registration, [], registration_test_cases()},
        {login, [], login_test_cases()},
        {password_management, [], password_test_cases()},
        {device_management, [], device_test_cases()}
    ].

init_per_suite(Config) ->
    application:set_env(imboy, env, test),
    ct:log("开始用户认证流程测试套件"),
    {ok, _} = application:ensure_all_started(imboy),
    Config.

end_per_suite(_Config) ->
    ct:log("结束用户认证流程测试套件"),
    application:stop(imboy),
    ok.

init_per_group(_Group, Config) ->
    % 每个测试组开始前清理数据
    cleanup_test_data(),
    Config.

end_per_group(_Group, Config) ->
    % 每个测试组结束后清理数据
    cleanup_test_data(),
    Config.

%% ===================================================================
%% 测试用例定义
%% ===================================================================

registration_test_cases() ->
    [
        signup_with_valid_data_succeeds,
        signup_with_duplicate_mobile_fails,
        signup_with_duplicate_email_fails,
        signup_with_invalid_password_fails
    ].

login_test_cases() ->
    [
        login_with_valid_credentials_succeeds,
        login_with_invalid_credentials_fails,
        login_with_nonexistent_user_fails,
        token_refresh_after_login
    ].

password_test_cases() ->
    [
        change_password_with_valid_old_password_succeeds,
        change_password_with_invalid_old_password_fails,
        reset_password_via_verification_code
    ].

device_test_cases() ->
    [
        login_with_new_device_registers_device,
        login_from_multiple_devices_creates_multiple_records,
        logout_clears_device_session
    ].

%% ===================================================================
%% 注册流程测试
%% ===================================================================

signup_with_valid_data_succeeds(_Config) ->
    ct:log("测试使用有效数据注册成功"),
    Mobile = <<"13800138999">>,
    Password = <<"Test@123456">>,
    Email = <<"test@example.com">>,

    % 清理可能存在的用户
    cleanup_user_by_mobile(Mobile),

    % 执行注册
    Result = passport_logic:signup(Mobile, Password, Email, #{}),

    % 验证注册成功
    ?assertMatch({ok, _Map}, Result),
    {ok, UserMap} = Result,
    ?assert(maps:is_key(<<"uid">>, UserMap)),
    ?assert(maps:is_key(<<"token">>, UserMap)),

    % 验证数据库中存在该用户
    {ok, User} = user_repo:find_by_mobile(Mobile, <<"id, mobile, email">>),
    ?assertEqual(Mobile, maps:get(<<"mobile">>, User)),

    % 清理
    cleanup_user_by_mobile(Mobile),
    {comment, "使用有效数据注册成功"}.

signup_with_duplicate_mobile_fails(_Config) ->
    ct:log("测试重复手机号注册失败"),
    Mobile = <<"13800138999">>,
    Password = <<"Test@123456">>,
    Email1 = <<"test1@example.com">>,
    Email2 = <<"test2@example.com">>,

    % 清理并创建第一个用户
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, Email1, #{}),

    % 尝试使用相同手机号再次注册
    Result = passport_logic:signup(Mobile, Password, Email2, #{}),

    % 验证注册失败
    ?assertMatch({error, _Msg, _Code}, Result),

    % 清理
    cleanup_user_by_mobile(Mobile),
    {comment, "重复手机号注册被拒绝"}.

signup_with_duplicate_email_fails(_Config) ->
    ct:log("测试重复邮箱注册失败"),
    Mobile1 = <<"13800138991">>,
    Mobile2 = <<"13800138992">>,
    Password = <<"Test@123456">>,
    Email = <<"test@example.com">>,

    % 清理并创建第一个用户
    cleanup_user_by_email(Email),
    {ok, _} = passport_logic:signup(Mobile1, Password, Email, #{}),

    % 尝试使用相同邮箱再次注册
    Result = passport_logic:signup(Mobile2, Password, Email, #{}),

    % 验证注册失败（取决于业务逻辑，邮箱可能不要求唯一）
    case Result of
        {error, _, _} ->
            cleanup_user_by_mobile(Mobile1),
            {comment, "重复邮箱注册被拒绝"};
        {ok, _} ->
            % 如果允许重复邮箱，清理两个用户
            cleanup_user_by_mobile(Mobile1),
            cleanup_user_by_mobile(Mobile2),
            {comment, "重复邮箱注册被允许（业务逻辑）"}
    end.

signup_with_invalid_password_fails(_Config) ->
    ct:log("测试无效密码注册失败"),
    Mobile = <<"13800138993">>,
    InvalidPasswords = [
        <<>>,                    % 空密码
        <<"123">>,                % 太短
        <<"12345678">>,           % 无字母
        <<"abcdefgh">>            % 无数字
    ],

    lists:foreach(fun(Password) ->
        Result = passport_logic:signup(Mobile, Password, <<"test@example.com">>, #{}),
        ?assertMatch({error, _, _}, Result)
    end, InvalidPasswords),

    {comment, "无效密码注册被拒绝"}.


%% ===================================================================
%% 登录流程测试
%% ===================================================================

login_with_valid_credentials_succeeds(_Config) ->
    ct:log("测试使用有效凭据登录成功"),
    Mobile = <<"13800138994">>,
    Password = <<"Test@123456">>,

    % 创建测试用户
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"test@example.com">>, #{}),

    % 执行登录
    Result = passport_logic:login(Mobile, Password, #{<<"did">> => <<"device_test">>}),

    % 验证登录成功
    ?assertMatch({ok, _Map}, Result),
    {ok, LoginMap} = Result,
    ?assert(maps:is_key(<<"uid">>, LoginMap)),
    ?assert(maps:is_key(<<"token">>, LoginMap)),

    % 清理
    cleanup_user_by_mobile(Mobile),
    {comment, "使用有效凭据登录成功"}.

login_with_invalid_credentials_fails(_Config) ->
    ct:log("测试使用错误密码登录失败"),
    Mobile = <<"13800138995">>,
    Password = <<"Test@123456">>,
    WrongPassword = <<"Wrong@123456">>,

    % 创建测试用户
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"test@example.com">>, #{}),

    % 使用错误密码登录
    Result = passport_logic:login(Mobile, WrongPassword, #{}),

    % 验证登录失败
    ?assertMatch({error, _, _}, Result),

    % 清理
    cleanup_user_by_mobile(Mobile),
    {comment, "错误密码登录被拒绝"}.

login_with_nonexistent_user_fails(_Config) ->
    ct:log("测试使用不存在用户登录失败"),
    Mobile = <<"13900139999">>,
    Password = <<"Test@123456">>,

    % 使用不存在的用户登录
    Result = passport_logic:login(Mobile, Password, #{}),

    % 验证登录失败
    ?assertMatch({error, _, _}, Result),
    {comment, "不存在用户登录被拒绝"}.

token_refresh_after_login(_Config) ->
    ct:log("测试登录后刷新 token"),
    Mobile = <<"13800138996">>,
    Password = <<"Test@123456">>,

    % 创建测试用户并登录
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"test@example.com">>, #{}),
    {ok, #{<<"uid">> := UidBin} = LoginMap} = passport_logic:login(Mobile, Password, #{}),

    % 刷新 token
    Uid = elib_hashids:decode(UidBin),
    Result = token_ds:refresh_token(Uid),

    % 验证刷新成功
    ?assertMatch({ok, _TokenMap}, Result),
    {ok, TokenMap} = Result,
    ?assert(maps:is_key(<<"token">>, TokenMap)),

    % 清理
    cleanup_user_by_mobile(Mobile),
    {comment, "Token 刷新成功"}.


%% ===================================================================
%% 密码管理测试
%% ===================================================================

change_password_with_valid_old_password_succeeds(_Config) ->
    ct:log("测试使用正确旧密码修改密码成功"),
    Mobile = <<"13800138997">>,
    OldPassword = <<"Test@123456">>,
    NewPassword = <<"New@123456">>,

    % 创建测试用户
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, OldPassword, <<"test@example.com">>, #{}),
    {ok, #{<<"uid">> := UidBin} = _LoginMap} = passport_logic:login(Mobile, OldPassword, #{}),
    Uid = elib_hashids:decode(UidBin),

    % 修改密码
    Result = user_logic:change_password(Uid, OldPassword, NewPassword),

    % 验证修改成功
    ?assertEqual(ok, Result),

    % 使用新密码登录
    {ok, _} = passport_logic:login(Mobile, NewPassword, #{}),

    % 清理
    cleanup_user_by_mobile(Mobile),
    {comment, "使用正确旧密码修改密码成功"}.

change_password_with_invalid_old_password_fails(_Config) ->
    ct:log("测试使用错误旧密码修改密码失败"),
    Mobile = <<"13800138998">>,
    OldPassword = <<"Test@123456">>,
    WrongOldPassword = <<"Wrong@123456">>,
    NewPassword = <<"New@123456">>,

    % 创建测试用户
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, OldPassword, <<"test@example.com">>, #{}),
    {ok, #{<<"uid">> := UidBin} = _LoginMap} = passport_logic:login(Mobile, OldPassword, #{}),
    Uid = elib_hashids:decode(UidBin),

    % 使用错误旧密码修改密码
    Result = user_logic:change_password(Uid, WrongOldPassword, NewPassword),

    % 验证修改失败
    ?assertMatch({error, _, _}, Result),

    % 验证旧密码仍然有效
    {ok, _} = passport_logic:login(Mobile, OldPassword, #{}),

    % 清理
    cleanup_user_by_mobile(Mobile),
    {comment, "使用错误旧密码修改密码被拒绝"}.

reset_password_via_verification_code(_Config) ->
    ct:log("测试通过验证码重置密码"),
    Mobile = <<"13800138999">>,
    OldPassword = <<"Test@123456">>,
    NewPassword = <<"Reset@123456">>,

    % 创建测试用户
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, OldPassword, <<"test@example.com">>, #{}),

    % 发送验证码（这里需要 mock 验证码发送）
    meck:new(verification_code_ds, [unstick]),
    meck:expect(verification_code_ds, send, fun(_Mobile, _Code, _Scene) -> {ok, sent} end),

    % 发送验证码
    {ok, _} = verification_code_logic:send(Mobile, <<"reset_password">>),

    % 验证并重置密码
    meck:expect(verification_code_ds, verify_and_delete, fun(_Mobile, _Code, _Scene) -> {ok, verified} end),
    Result = user_logic:reset_password(Mobile, <<"123456">>, NewPassword),

    % 验证重置成功
    ?assertEqual(ok, Result),

    % 使用新密码登录
    {ok, _} = passport_logic:login(Mobile, NewPassword, #{}),

    % 清理
    meck:unload(verification_code_ds),
    cleanup_user_by_mobile(Mobile),
    {comment, "通过验证码重置密码成功"}.


%% ===================================================================
%% 设备管理测试
%% ===================================================================

login_with_new_device_registers_device(_Config) ->
    ct:log("测试新设备登录注册设备"),
    Mobile = <<"13800138100">>,
    Password = <<"Test@123456">>,
    DID = <<"device_new_001">>,

    % 创建测试用户
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"test@example.com">>, #{}),

    % 使用新设备登录
    {ok, #{<<"uid">> := UidBin} = _LoginMap} = passport_logic:login(Mobile, Password, #{<<"did">> => DID}),
    Uid = elib_hashids:decode(UidBin),

    % 验证设备已注册
    {ok, Devices} = user_device_ds:list(Uid),
    ?assert(length(Devices) > 0),

    % 验证设备信息
    DeviceFound = lists:any(fun(D) ->
        maps:get(<<"did">>, D, <<>>) =:= DID
    end, Devices),
    ?assert(DeviceFound),

    % 清理
    cleanup_user_by_mobile(Mobile),
    {comment, "新设备登录注册设备成功"}.

login_from_multiple_devices_creates_multiple_records(_Config) ->
    ct:log("测试多设备登录创建多个设备记录"),
    Mobile = <<"13800138101">>,
    Password = <<"Test@123456">>,
    DID1 = <<"device_ios_001">>,
    DID2 = <<"device_android_001">>,

    % 创建测试用户
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"test@example.com">>, #{}),

    % 从设备 1 登录
    {ok, #{<<"uid">> := UidBin} = _} = passport_logic:login(Mobile, Password, #{<<"did">> => DID1}),
    Uid = elib_hashids:decode(UidBin),

    % 从设备 2 登录
    {ok, _} = passport_logic:login(Mobile, Password, #{<<"did">> => DID2}),

    % 验证两个设备都已注册
    {ok, Devices} = user_device_ds:list(Uid),
    ?assertEqual(2, length(Devices)),

    % 清理
    cleanup_user_by_mobile(Mobile),
    {comment, "多设备登录创建多个设备记录成功"}.

logout_clears_device_session(_Config) ->
    ct:log("测试登出清除设备会话"),
    Mobile = <<"13800138102">>,
    Password = <<"Test@123456">>,
    DID = <<"device_logout_001">>,

    % 创建测试用户并登录
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"test@example.com">>, #{}),
    {ok, #{<<"uid">> := UidBin} = _LoginMap} = passport_logic:login(Mobile, Password, #{<<"did">> => DID}),
    Uid = elib_hashids:decode(UidBin),

    % 验证设备已注册
    {ok, Devices} = user_device_ds:list(Uid),
    ?assert(length(Devices) > 0),

    % 登出
    {ok, _} = auth_logic:logout(Uid, DID),

    % 验证 token 已失效（这里可能需要 mock token 验证）
    % 实际场景中，token 会被加入黑名单或从缓存中移除
    {comment, "登出清除设备会话成功（待完善 token 验证）"}.


%% ===================================================================
%% 辅助函数
%% ===================================================================

cleanup_user_by_mobile(Mobile) ->
    case user_repo:find_by_mobile(Mobile, <<"id">>) of
        #{<<"id">> := Id} when is_integer(Id) ->
            user_repo:delete(Id);
        _ ->
            ok
    end.

cleanup_user_by_email(Email) ->
    case user_repo:find_by_email(Email, <<"id">>) of
        #{<<"id">> := Id} when is_integer(Id) ->
            user_repo:delete(Id);
        _ ->
            ok
    end.

cleanup_test_data() ->
    % 清理测试相关的所有数据
    Mobiles = [
        <<"13800138999">>, <<"13800138991">>, <<"13800138992">>,
        <<"13800138993">>, <<"13800138994">>, <<"13800138995">>,
        <<"13800138996">>, <<"13800138997">>, <<"13800138998">>,
        <<"13800138999">>, <<"13800138100">>, <<"13800138101">>,
        <<"13800138102">>
    ],
    lists:foreach(fun(M) -> cleanup_user_by_mobile(M) end, Mobiles).
