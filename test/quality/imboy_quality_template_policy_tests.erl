-module(imboy_quality_template_policy_tests).
-include_lib("eunit/include/eunit.hrl").

template_conflict_is_never_replaced_test() ->
    ?assertEqual(conflict, imboy_quality_resources:template_action(true, false)),
    ?assertEqual(reuse, imboy_quality_resources:template_action(true, true)),
    ?assertEqual(create, imboy_quality_resources:template_action(false, false)),
    ?assertException(error, function_clause, imboy_quality_resources:template_action(false, true)).
