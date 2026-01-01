-module(test_config).

-export([
    get_test_db_config/0,
    get_test_timeout/0,
    get_test_batch_size/0,
    is_test_env/0,
    test_table_prefix/0,
    test_user_data/0,
    test_user_data/1,
    generate_test_users/1
]).

%% ===================================================================
%% 测试配置
%% ===================================================================

%% @doc 获取测试数据库配置
get_test_db_config() ->
    #{
        host => application:get_env(imboy, test_db_host, "localhost"),
        port => application:get_env(imboy, test_db_port, 4323),
        database => application:get_env(imboy, test_db_name, "imboy_v1"),
        username => application:get_env(imboy, test_db_user, "imboy_user"),
        password => application:get_env(imboy, test_db_password, "V6uucUwhU9pUAYis"),
        pool_size => application:get_env(imboy, test_db_pool_size, 5),
        timeout => get_test_timeout()
    }.

%% @doc 获取测试超时时间（毫秒）
get_test_timeout() ->
    application:get_env(imboy, test_timeout, 30000).

%% @doc 获取测试批次大小
get_test_batch_size() ->
    application:get_env(imboy, test_batch_size, 100).

%% @doc 检查是否为测试环境
is_test_env() ->
    application:get_env(imboy, env, development) =:= test orelse
    os:getenv("IMBOY_ENV") =:= "test".

%% @doc 获取测试表前缀
test_table_prefix() ->
    <<"test_">>.

%% ===================================================================
%% 测试数据常量
%% ===================================================================

-define(TEST_USER_ID, 999999).
-define(TEST_MOBILE, <<"13800138000">>).
-define(TEST_EMAIL, <<"test@example.com">>).
-define(TEST_NICKNAME, <<"Test User"/utf8>>).

%% @doc 获取测试用户数据
test_user_data() ->
    #{
        id => ?TEST_USER_ID,
        mobile => ?TEST_MOBILE,
        email => ?TEST_EMAIL,
        nickname => ?TEST_NICKNAME,
        status => 1
    }.

%% @doc 获取测试用户数据（带ID变体）
test_user_data(Id) ->
    (test_user_data())#{
        id => Id,
        mobile => <<(binary:copy(<<"1">>, 8))/binary, (integer_to_binary(Id rem 100000000))/binary>>,
        email => <<"test", (integer_to_binary(Id))/binary, "@example.com">>,
        nickname => <<"Test User ", (integer_to_binary(Id))/binary>>
    }.

%% @doc 生成多个测试用户数据
generate_test_users(Count) ->
    [test_user_data(?TEST_USER_ID + I) || I <- lists:seq(1, Count)].
