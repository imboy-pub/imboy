-module(passport_logic_console2_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

send_code_invalid_type_returns_clear_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = passport_logic:send_code(<<"13800138000">>, <<"push">>),
        ?assertEqual(
            {error, <<"验证码类型不支持，仅支持 sms 或 email"/utf8>>},
            Result)
    end).
