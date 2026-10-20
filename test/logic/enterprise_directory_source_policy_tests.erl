-module(enterprise_directory_source_policy_tests).
-include_lib("eunit/include/eunit.hrl").

explicit_oa_capabilities_test() ->
    Directory = [<<"identities:read">>, <<"identities:write">>],
    Combined = Directory ++ [<<"oa:login:issue">>, <<"sso:exchange">>, <<"application:read">>],
    ?assert(enterprise_directory_source_policy:allowed(Directory)),
    ?assert(enterprise_directory_source_policy:allowed(Combined)),
    ?assertEqual(Directory, enterprise_directory_source_policy:directory_scopes(Combined)),
    lists:foreach(
        fun(Scope) ->
            ?assertNot(enterprise_directory_source_policy:allowed(Directory ++ [Scope]))
        end,
        [<<"messages:send">>, <<"groups:write">>, <<"files:write">>, <<"*">>]
    ),
    ?assertNot(enterprise_directory_source_policy:allowed([<<"oa:login:issue">>])),
    ?assertNot(enterprise_directory_source_policy:allowed(undefined)).
