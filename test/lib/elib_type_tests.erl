-module(elib_type_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

is_numeric_with_native_numbers_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(true, elib_type:is_numeric(123)),
        ?assertEqual(true, elib_type:is_numeric(-123.45))
    end).

is_numeric_with_string_like_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(true, elib_type:is_numeric("123e2")),
        ?assertEqual(true, elib_type:is_numeric(<<"-.45">>))
    end).

is_numeric_rejects_non_numeric_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(false, elib_type:is_numeric("12a3")),
        ?assertEqual(false, elib_type:is_numeric(atom)),
        ?assertEqual(false, elib_type:is_numeric([1, 2, 3]))
    end).

is_proplist_accepts_key_value_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(true, elib_type:is_proplist([{a, 1}, {b, <<"2">>}])),
        ?assertEqual(true, elib_type:is_proplist([]))
    end).

is_proplist_rejects_non_proplist_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(false, elib_type:is_proplist([a, b])),
        ?assertEqual(false, elib_type:is_proplist(#{a => 1}))
    end).

is_mobile_matches_mainland_number_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(true, elib_type:is_mobile(<<"13800138000">>)),
        ?assertEqual(true, elib_type:is_mobile("19912345678"))
    end).

is_mobile_rejects_invalid_number_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(false, elib_type:is_mobile(<<"23800138000">>)),
        ?assertEqual(false, elib_type:is_mobile("1380013800"))
    end).

is_email_matches_valid_addresses_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(true, elib_type:is_email(<<"test_user@example.com">>)),
        ?assertEqual(true, elib_type:is_email("dev-team@test.example.com"))
    end).

is_email_rejects_invalid_inputs_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(false, elib_type:is_email(undefined)),
        ?assertEqual(false, elib_type:is_email(<<"invalid-email">>)),
        ?assertEqual(false, elib_type:is_email("bad@@example.com"))
    end).
