-module(retired_product_tests).
-include_lib("eunit/include/eunit.hrl").

retired_attachment_access_is_denied_test() ->
    lists:foreach(
        fun(Scope) ->
            Record = #{<<"scope">> => Scope, <<"creator_user_id">> => 7},
            ?assertEqual(false, attach_logic:authorize(7, Record)),
            ?assertEqual(
                {error, forbidden},
                attach_logic:presign(7, <<"photo.jpg">>, <<"image/jpeg">>, Scope, undefined)
            )
        end,
        [<<"teaching">>, <<"moya">>]
    ).

retired_routes_are_absent_test() ->
    {ok, Router} = file:read_file("src/imboy_router.erl"),
    ?assertEqual(nomatch, binary:match(Router, <<"/api/v1/moya/">>)),
    ?assertEqual(nomatch, binary:match(Router, <<"/api/v1/wechat/mini/events">>)),
    ?assertNotEqual(nomatch, binary:match(Router, <<"wechat_mini_auth_handler">>)),
    ?assert(lists:member(<<"/api/v1/auth/wechat-mini/login">>, imboy_router:open())).
