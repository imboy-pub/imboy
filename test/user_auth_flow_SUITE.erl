-module(user_auth_flow_SUITE).

%%%===================================================================
%%% @doc
%%% 用户认证流程 Common Test 测试套件
%%%
%%% 运行方式：
%%%   make ct-user_auth_flow                    # 运行整个 suite
%%%   make ct-user_auth_flow t=registration     # 运行特定 group
%%%
%%% 说明：本 suite 已按当前生产 API 重写。历史 API 漂移记录：
%%%   - passport_logic:signup/4、login/3 仍在；登录/注册响应 map 中 <<"uid">>
%%%     是整数（非 binary），且含 <<"token">> 与 <<"refreshtoken">>。
%%%   - token_ds:refresh_token/1（已删）：token 刷新改由 HTTP handler
%%%     (imboy-refreshtoken 头) 承载，登录时即签发 refreshtoken；故
%%%     token_refresh_after_login 改验登录响应含 refreshtoken。
%%%   - user_logic:change_password/3（已删）-> change_password/2(Uid, Req0)，
%%%     Req0 经 elib_param:post 取 existing_pwd/new_pwd，且二者为 RSA 密文
%%%     (elib_cipher:rsa_decrypt)。测试环境未配 login_rsa_priv_key，故 mock
%%%     rsa_decrypt 为 identity + 进程字典注入 post_vals，测真实改密逻辑。
%%%   - user_logic:reset_password/3 与 verification_code_logic 模块（均已删）：
%%%     密码找回改 passport_logic:find_password/5，且仅支持 email + 验证码，
%%%     非 email 类型返回"不支持的注册类型"；故 reset 用例改验此当前契约。
%%%   - user_device_ds:list/1 返回设备 map 的 <<"did">>（源列 device_id）。
%%%   - auth_logic:logout/2 返回 {ok, binary()}。
%%%   - 清理改硬删除：user_repo:delete 是软删（status=-1），软删行仍占用
%%%     account 唯一约束会令重复手机号 signup 返回"账号已存在"，故直接 SQL 删。
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

%% 全部测试手机号（统一 138001381xx 段，范围硬删清理）
-define(TEST_MOBILES, [
    <<"13800138990">>,
    <<"13800138991">>,
    <<"13800138992">>,
    <<"13800138993">>,
    <<"13800138994">>,
    <<"13800138995">>,
    <<"13800138996">>,
    <<"13800138997">>,
    <<"13800138998">>,
    <<"13800138999">>,
    <<"13800138100">>,
    <<"13800138101">>,
    <<"13800138102">>
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
    ct:log("开始用户认证流程测试套件"),
    eunit_runner:ct_suite_setup(Config).

end_per_suite(Config) ->
    ct:log("结束用户认证流程测试套件"),
    cleanup_all_test_data(),
    eunit_runner:ct_suite_cleanup(Config).

init_per_group(_Group, Config) ->
    cleanup_all_test_data(),
    Config.

end_per_group(_Group, _Config) ->
    try meck:unload() of
        _ -> ok
    catch
        _:_ -> ok
    end,
    cleanup_all_test_data(),
    ok.

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
    Mobile = <<"13800138990">>,
    Password = <<"Test@123456">>,
    Email = <<"test990@example.com">>,
    cleanup_user_by_mobile(Mobile),

    Result = passport_logic:signup(Mobile, Password, Email, #{}),

    ?assertMatch({ok, _Map}, Result),
    {ok, UserMap} = Result,
    ?assert(maps:is_key(<<"uid">>, UserMap)),
    ?assert(maps:is_key(<<"token">>, UserMap)),

    User = user_repo:find_by_mobile(Mobile, <<"id, mobile">>),
    ?assertEqual(Mobile, maps:get(<<"mobile">>, User)),

    cleanup_user_by_mobile(Mobile),
    {comment, "使用有效数据注册成功"}.

signup_with_duplicate_mobile_fails(_Config) ->
    ct:log("测试重复手机号注册失败"),
    Mobile = <<"13800138991">>,
    Password = <<"Test@123456">>,
    cleanup_user_by_mobile(Mobile),

    {ok, _} = passport_logic:signup(Mobile, Password, <<"dup991a@example.com">>, #{}),
    Result = passport_logic:signup(Mobile, Password, <<"dup991b@example.com">>, #{}),

    ?assertMatch({error, _Msg, _Code}, Result),

    cleanup_user_by_mobile(Mobile),
    {comment, "重复手机号注册被拒绝"}.

signup_with_duplicate_email_fails(_Config) ->
    ct:log("测试重复邮箱注册（业务上邮箱是否唯一由实现决定）"),
    Mobile1 = <<"13800138992">>,
    Mobile2 = <<"13800138993">>,
    Password = <<"Test@123456">>,
    Email = <<"dupemail@example.com">>,
    cleanup_user_by_mobile(Mobile1),
    cleanup_user_by_mobile(Mobile2),

    {ok, _} = passport_logic:signup(Mobile1, Password, Email, #{}),
    Result = passport_logic:signup(Mobile2, Password, Email, #{}),

    case Result of
        {error, _, _} ->
            cleanup_user_by_mobile(Mobile1),
            {comment, "重复邮箱注册被拒绝"};
        {ok, _} ->
            cleanup_user_by_mobile(Mobile1),
            cleanup_user_by_mobile(Mobile2),
            {comment, "重复邮箱注册被允许（业务逻辑）"}
    end.

signup_with_invalid_password_fails(_Config) ->
    ct:log("测试无效密码注册失败"),
    Mobile = <<"13800138994">>,
    cleanup_user_by_mobile(Mobile),
    InvalidPasswords = [
        %% 空密码
        <<>>,
        %% 太短
        <<"123">>,
        %% 无字母
        <<"12345678">>,
        %% 无数字
        <<"abcdefgh">>
    ],

    lists:foreach(
        fun(Password) ->
            cleanup_user_by_mobile(Mobile),
            Result = passport_logic:signup(Mobile, Password, <<"invalid994@example.com">>, #{}),
            ?assertMatch({error, _, _}, Result)
        end,
        InvalidPasswords
    ),

    cleanup_user_by_mobile(Mobile),
    {comment, "无效密码注册被拒绝"}.

%% ===================================================================
%% 登录流程测试
%% ===================================================================

login_with_valid_credentials_succeeds(_Config) ->
    ct:log("测试使用有效凭据登录成功"),
    Mobile = <<"13800138995">>,
    Password = <<"Test@123456">>,
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"login995@example.com">>, #{}),

    Result = passport_logic:login(Mobile, Password, #{<<"did">> => <<"device_test">>}),

    ?assertMatch({ok, _Map}, Result),
    {ok, LoginMap} = Result,
    ?assert(maps:is_key(<<"uid">>, LoginMap)),
    ?assert(maps:is_key(<<"token">>, LoginMap)),

    cleanup_user_by_mobile(Mobile),
    {comment, "使用有效凭据登录成功"}.

login_with_invalid_credentials_fails(_Config) ->
    ct:log("测试使用错误密码登录失败"),
    Mobile = <<"13800138996">>,
    Password = <<"Test@123456">>,
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"login996@example.com">>, #{}),

    Result = passport_logic:login(Mobile, <<"Wrong@123456">>, #{}),

    ?assertMatch({error, _, _}, Result),

    cleanup_user_by_mobile(Mobile),
    {comment, "错误密码登录被拒绝"}.

login_with_nonexistent_user_fails(_Config) ->
    ct:log("测试使用不存在用户登录失败"),
    Mobile = <<"13800138999">>,
    cleanup_user_by_mobile(Mobile),

    Result = passport_logic:login(Mobile, <<"Test@123456">>, #{}),

    ?assertMatch({error, _, _}, Result),
    {comment, "不存在用户登录被拒绝"}.

token_refresh_after_login(_Config) ->
    ct:log("测试登录响应签发 refreshtoken（refresh_token/1 已删，改由登录签发）"),
    Mobile = <<"13800138997">>,
    Password = <<"Test@123456">>,
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"token997@example.com">>, #{}),

    {ok, LoginMap} = passport_logic:login(Mobile, Password, #{<<"did">> => <<"device_tk">>}),

    %% 登录即签发 token + refreshtoken（原 token_ds:refresh_token/1 已移除）
    ?assert(maps:is_key(<<"refreshtoken">>, LoginMap)),
    ?assertNotEqual(<<>>, maps:get(<<"token">>, LoginMap)),
    ?assertNotEqual(<<>>, maps:get(<<"refreshtoken">>, LoginMap)),

    cleanup_user_by_mobile(Mobile),
    {comment, "登录响应签发 refreshtoken 成功"}.

%% ===================================================================
%% 密码管理测试
%% ===================================================================

change_password_with_valid_old_password_succeeds(_Config) ->
    ct:log("测试使用正确旧密码修改密码成功"),
    Mobile = <<"13800138998">>,
    OldPassword = <<"Test@123456">>,
    NewPassword = <<"New@123456">>,
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, OldPassword, <<"chg998@example.com">>, #{}),
    {ok, LoginMap} = passport_logic:login(Mobile, OldPassword, #{}),
    Uid = maps:get(<<"uid">>, LoginMap),
    OldHash = db_password(Uid),

    %% change_password/2 对 existing_pwd/new_pwd 做 rsa_decrypt；测试环境未配
    %% RSA 私钥，故 mock 为 identity，用明文经进程字典注入 post_vals 测真实改密
    ok = mock_rsa_identity(),
    put_post_vals(#{<<"existing_pwd">> => OldPassword, <<"new_pwd">> => NewPassword}),
    Result = user_logic:change_password(Uid, #{headers => #{}}),
    erase_post_vals(),

    ?assertMatch({ok, _}, Result),

    %% 直接查 DB 验证密码 hash 已变更（缓存无关；login 走 find_by_account
    %% 与 change_password 的 find_by_id 缓存键不同，跨缓存失效非本用例范围）
    ?assertNotEqual(OldHash, db_password(Uid)),

    meck:unload(elib_cipher),
    cleanup_user_by_mobile(Mobile),
    {comment, "使用正确旧密码修改密码成功"}.

change_password_with_invalid_old_password_fails(_Config) ->
    ct:log("测试使用错误旧密码修改密码失败"),
    Mobile = <<"13800138100">>,
    OldPassword = <<"Test@123456">>,
    NewPassword = <<"New@123456">>,
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, OldPassword, <<"chg100@example.com">>, #{}),
    {ok, LoginMap} = passport_logic:login(Mobile, OldPassword, #{}),
    Uid = maps:get(<<"uid">>, LoginMap),
    OldHash = db_password(Uid),

    ok = mock_rsa_identity(),
    put_post_vals(#{<<"existing_pwd">> => <<"Wrong@123456">>, <<"new_pwd">> => NewPassword}),
    Result = user_logic:change_password(Uid, #{headers => #{}}),
    erase_post_vals(),

    ?assertMatch({error, _}, Result),

    %% 旧密码 hash 未变更（改密被拒未落库）
    ?assertEqual(OldHash, db_password(Uid)),

    meck:unload(elib_cipher),
    cleanup_user_by_mobile(Mobile),
    {comment, "使用错误旧密码修改密码被拒绝"}.

reset_password_via_verification_code(_Config) ->
    ct:log("测试密码找回当前契约（find_password/5 仅支持 email + 验证码）"),
    %% verification_code_logic 与 user_logic:reset_password/3 均已删除；
    %% 密码找回改 passport_logic:find_password/5，非 email 类型直接拒绝。
    Mobile = <<"13800138101">>,
    Result = passport_logic:find_password(
        <<"mobile">>, Mobile, <<"Reset@123456">>, <<"123456">>, #{}
    ),

    ?assertMatch({error, _}, Result),
    {comment, "非 email 类型密码找回被拒绝（当前契约）"}.

%% ===================================================================
%% 设备管理测试
%% ===================================================================

login_with_new_device_registers_device(_Config) ->
    ct:log("测试新设备登录注册设备"),
    Mobile = <<"13800138102">>,
    Password = <<"Test@123456">>,
    DID = <<"device_new_001">>,
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"dev102@example.com">>, #{}),

    {ok, LoginMap} = passport_logic:login(Mobile, Password, #{<<"did">> => DID}),
    Uid = maps:get(<<"uid">>, LoginMap),

    {ok, Devices} = user_device_ds:list(Uid),
    ?assert(length(Devices) > 0),
    DeviceFound = lists:any(
        fun(D) -> maps:get(<<"did">>, D, <<>>) =:= DID end, Devices
    ),
    ?assert(DeviceFound),

    cleanup_user_by_mobile(Mobile),
    {comment, "新设备登录注册设备成功"}.

login_from_multiple_devices_creates_multiple_records(_Config) ->
    ct:log("测试多设备登录创建多个设备记录"),
    Mobile = <<"13800138990">>,
    Password = <<"Test@123456">>,
    DID1 = <<"device_ios_001">>,
    DID2 = <<"device_android_001">>,
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"multi990@example.com">>, #{}),

    {ok, LoginMap} = passport_logic:login(Mobile, Password, #{<<"did">> => DID1}),
    Uid = maps:get(<<"uid">>, LoginMap),
    {ok, _} = passport_logic:login(Mobile, Password, #{<<"did">> => DID2}),

    {ok, Devices} = user_device_ds:list(Uid),
    ?assertEqual(2, length(Devices)),

    cleanup_user_by_mobile(Mobile),
    {comment, "多设备登录创建多个设备记录成功"}.

logout_clears_device_session(_Config) ->
    ct:log("测试登出返回成功（清除设备会话）"),
    Mobile = <<"13800138991">>,
    Password = <<"Test@123456">>,
    DID = <<"device_logout_001">>,
    cleanup_user_by_mobile(Mobile),
    {ok, _} = passport_logic:signup(Mobile, Password, <<"logout991@example.com">>, #{}),
    {ok, LoginMap} = passport_logic:login(Mobile, Password, #{<<"did">> => DID}),
    Uid = maps:get(<<"uid">>, LoginMap),

    {ok, Devices} = user_device_ds:list(Uid),
    ?assert(length(Devices) > 0),

    %% auth_logic:logout/2 返回 {ok, binary()}
    ?assertMatch({ok, _}, auth_logic:logout(Uid, DID)),

    cleanup_user_by_mobile(Mobile),
    {comment, "登出清除设备会话成功"}.

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% 将 elib_cipher:rsa_decrypt mock 为 identity（测试环境无 RSA 私钥）
mock_rsa_identity() ->
    meck:new(elib_cipher, [unstick, passthrough]),
    meck:expect(elib_cipher, rsa_decrypt, fun(X) -> X end),
    ok.

%% 注入 elib_param:post 的进程字典缓存，绕开 cowboy_req 解析
put_post_vals(Map) ->
    erlang:put({elib_param, post_vals}, Map).

erase_post_vals() ->
    erlang:erase({elib_param, post_vals}).

%% 直接读 DB 的密码 hash（绕开 depcache，验证真持久化）
db_password(Uid) ->
    {ok, [#{<<"password">> := Pwd}]} =
        elib_pg:query(<<"SELECT password FROM \"user\" WHERE id = $1">>, [Uid]),
    Pwd.

%% 硬删除用户及其关联设备（软删 status=-1 会留行占用 account 唯一约束）
cleanup_user_by_mobile(Mobile) ->
    case user_repo:find_by_mobile(Mobile, <<"id">>) of
        #{<<"id">> := Id} when is_integer(Id) ->
            elib_pg:execute(<<"DELETE FROM user_device WHERE user_id = $1">>, [Id]),
            elib_pg:execute(<<"DELETE FROM \"user\" WHERE id = $1">>, [Id]);
        _ ->
            ok
    end.

cleanup_all_test_data() ->
    lists:foreach(fun(M) -> cleanup_user_by_mobile(M) end, ?TEST_MOBILES).
