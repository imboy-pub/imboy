-module(moment_attachment_ticket_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

moment_ticket_rechecks_visibility_before_signing_test_() ->
    [moment_ticket_case(Key) || Key <- [<<"u1001/moment/image.png">>, <<"/legacy/image.png">>]].

moment_ticket_case(Key) ->
    State = atomics:new(1, []),
    atomics:put(State, 1, 1),
    Secret = <<"synthetic-moment-ticket-secret">>,
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (jwt_key, _) -> Secret;
                    (base_url, _) -> <<"http://127.0.0.1:29831">>
                end}
            ]},
            {attachment_ds, [
                {'find_by_path', 1, fun(K) when K =:= Key ->
                    {ok, #{
                        <<"scope">> => <<"moment">>,
                        <<"scope_ref">> => <<"555">>,
                        <<"status">> => 1,
                        <<"url">> => <<"https://synthetic-signed">>
                    }}
                end}
            ]},
            {moment_ds, [
                {'get_post', 1, fun(555) -> #{<<"status">> => atomics:get(State, 1)} end},
                {'can_view_post', 2, fun(1001, Post) -> maps:get(<<"status">>, Post) =:= 1 end}
            ]},
            {elib_oss, [
                {'presign_get_for_key', 3, fun(_, K, _) when K =:= Key ->
                    <<"https://synthetic-signed">>
                end}
            ]}
        ],
        fun() ->
            {ok, Url} = attach_logic:view_url(1001, Key),
            #{path := <<"/api/v1/attachment/content">>, query := Query} = uri_string:parse(Url),
            Ticket = proplists:get_value(<<"ticket">>, uri_string:dissect_query(Query)),
            ?assertEqual(0, meck:num_calls(elib_oss, presign_get_for_key, '_')),
            ?assertEqual({ok, <<"https://synthetic-signed">>}, attach_logic:download(Ticket)),
            atomics:put(State, 1, -1),
            ?assertEqual({error, forbidden}, attach_logic:download(Ticket)),
            ExpectedSigns =
                case Key of
                    <<"/", _/binary>> -> 0;
                    _ -> 1
                end,
            ?assertEqual(ExpectedSigns, meck:num_calls(elib_oss, presign_get_for_key, '_'))
        end
    ).
