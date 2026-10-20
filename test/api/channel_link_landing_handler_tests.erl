-module(channel_link_landing_handler_tests).
-include_lib("eunit/include/eunit.hrl").

safe_resource_path_test() ->
    ?assertEqual(
        {ok, <<"/channel/42">>}, channel_link_landing_handler:app_path(<<"42">>, undefined)
    ),
    ?assertEqual(
        {ok, <<"/channel/42/article/100">>},
        channel_link_landing_handler:app_path(<<"42">>, <<"100">>)
    ),
    lists:foreach(
        fun(Id) ->
            ?assertEqual(error, channel_link_landing_handler:app_path(Id, undefined)),
            ?assertEqual(error, channel_link_landing_handler:app_path(<<"42">>, Id))
        end,
        [<<>>, <<"0">>, <<"-1">>, <<"1' onclick='x">>, <<"9223372036854775808">>]
    ).
