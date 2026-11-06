-module(rtc_room_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

group_disabled_uses_existing_feature_error_test_() ->
    Req = #{request_id => <<"rtc-test">>},
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {method, 1, fun(_) -> <<"POST">> end}
            ]},
            {elib_req, [
                {post_params, 1, fun(_) ->
                    #{<<"kind">> => <<"group">>, <<"target_id">> => 100, <<"did">> => <<"dev1">>}
                end}
            ]},
            {rtc_room_logic, [
                {join, 4, fun(1, <<"dev1">>, <<"group">>, 100) ->
                    {error, feature_disabled}
                end}
            ]},
            {elib_response, [
                {error, 3, fun(Req0, Msg, Code) -> Req0#{message => Msg, code => Code} end}
            ]}
        ],
        fun() ->
            {ok, Response, #{current_uid := 1}} = rtc_room_handler:init(
                Req, #{action => join, current_uid => 1}
            ),
            ?assertEqual(?ERR_FEATURE_DISABLED, maps:get(code, Response)),
            ?assertEqual(imboy_error:error_msg(?ERR_FEATURE_DISABLED), maps:get(message, Response))
        end
    ).
