-module(cs_quick_reply_validation_tests).
-moduledoc "快捷回复写入参数验证；不依赖数据库，纳入 PR 回归门。".

-include_lib("eunit/include/eunit.hrl").

quick_reply_create_requires_authenticated_admin_test_() ->
    Params = valid_params(),
    MissingAdmin = maps:remove(adm_user_id, Params),
    [
        ?_assertEqual({error, invalid_argument}, cs_quick_reply_app:create(Input))
     || Input <- [
            MissingAdmin,
            MissingAdmin#{actor_id => 900001},
            Params#{adm_user_id => 0},
            Params#{adm_user_id => -1},
            Params#{adm_user_id => <<"900001">>}
        ]
    ].

quick_reply_invalid_sort_test_() ->
    Params = valid_params(),
    lists:append([
        [
            ?_assertEqual({error, invalid_argument}, cs_quick_reply_app:create(Params#{sort => Sort})),
            ?_assertEqual(
                {error, invalid_argument}, cs_quick_reply_app:update(Params#{id => 42, sort => Sort})
            )
        ]
     || Sort <- [1.5, <<"1">>, undefined, []]
    ]).

valid_params() ->
    #{
        organization_id => 996101,
        workspace_id => 996201,
        adm_user_id => 900001,
        title => <<"问候语"/utf8>>,
        content => <<"您好"/utf8>>
    }.
