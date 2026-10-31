-module(app_manifest_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%% J27.b server-side conditional response only: client cache ownership,
%% unknown-contract gates and request_id preservation remain separate vectors.
j27_b_manifest_304_has_no_body_and_etag_miss_returns_entity_test_() ->
    j27_manifest_fixture(fun() ->
        First = j27_manifest_request(undefined),
        ?assertEqual(200, maps:get(status, First)),
        Manifest = jsone:decode(maps:get(body, First)),
        ?assert(is_map(maps:get(<<"features">>, Manifest))),
        Etag = maps:get(<<"etag">>, maps:get(headers, First)),
        Cached = j27_manifest_request(Etag),
        ?assertEqual(304, maps:get(status, Cached)),
        ?assertEqual(<<>>, maps:get(body, Cached)),
        ?assertEqual(Etag, maps:get(<<"etag">>, maps:get(headers, Cached))),
        Miss = j27_manifest_request(<<"\"foreign-origin-etag\"">>),
        ?assertEqual(200, maps:get(status, Miss)),
        ?assert(is_map(jsone:decode(maps:get(body, Miss)))),
        ?assertNotEqual(
            Etag,
            app_manifest_handler:compute_etag(
                Manifest#{<<"list_contracts">> => #{<<"cursor">> => 999}}
            )
        )
    end).

j27_f_disabled_channel_manifest_removes_entry_points_test_() ->
    j27_manifest_fixture(fun() ->
        Response = j27_manifest_request(undefined),
        Manifest = jsone:decode(maps:get(body, Response)),
        ?assertEqual(false, maps:get(<<"channel">>, maps:get(<<"features">>, Manifest))),
        ?assertEqual([], maps:get(<<"app_entries">>, Manifest)),
        ?assertEqual([], maps:get(<<"admin_entries">>, Manifest)),
        ?assertEqual([], maps:get(<<"plugins">>, Manifest))
    end).

j27_manifest_fixture(Test) ->
    Mocks = [
        {cowboy_req, [
            {method, 1, fun(_) -> <<"GET">> end},
            {header, 2, fun(<<"if-none-match">>, Req) ->
                maps:get(if_none_match, Req, undefined)
            end},
            {reply, 4, fun(Status, Headers, Body, Req) ->
                Req#{status => Status, headers => Headers, body => Body}
            end}
        ]},
        {imboy_feature, [
            {all, 0, fun() ->
                #{
                    <<"channel">> => false,
                    <<"channel_discover">> => false,
                    <<"channel_invitation">> => false,
                    <<"channel_order">> => false
                }
            end}
        ]},
        {imboy_policy, [{effective_view, 0, fun() -> #{} end}]}
    ],
    {setup,
        fun() ->
            lists:foreach(
                fun({Mod, Exp}) ->
                    {ok, _} = meck_helper:setup_mock(Mod, Exp)
                end,
                Mocks
            )
        end,
        fun(_) ->
            meck_helper:cleanup_mocks([Mod || {Mod, _} <- Mocks])
        end,
        fun(_) -> ?_test(Test()) end}.

j27_manifest_request(Etag) ->
    {ok, Req, #{}} = app_manifest_handler:init(
        #{if_none_match => Etag},
        #{action => manifest}
    ),
    Req.

%% ===================================================================
%% P5-B2: Manifest structure test
%% P5-B1: Etag + 304 test
%% ===================================================================

setup() ->
    application:set_env(imboy, env, test),
    ok.

cleanup(_) ->
    ok.

manifest_builds_correct_structure_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Manifest = app_manifest_handler:build_manifest(),
            ?assert(is_map(Manifest)),
            ?assert(maps:is_key(<<"features">>, Manifest)),
            ?assertEqual(imboy_feature:manifest_hash(), maps:get(<<"manifest_hash">>, Manifest)),
            ?assertEqual(
                imboy_feature:manifest_schema_version(),
                maps:get(<<"manifest_schema_version">>, Manifest)
            ),
            ?assertEqual(
                [atom_to_binary(Feature, utf8) || Feature <- imboy_feature:compiled_features()],
                maps:get(<<"compiled_features">>, Manifest)
            ),
            ?assert(maps:is_key(<<"policy">>, Manifest)),
            ?assert(maps:is_key(<<"app_entries">>, Manifest)),
            ?assert(maps:is_key(<<"admin_entries">>, Manifest)),
            ?assert(maps:is_key(<<"plugins">>, Manifest)),
            ?assert(maps:is_key(<<"generated_at">>, Manifest)),
            Features = maps:get(<<"features">>, Manifest),
            ?assert(is_map(Features)),
            ?assert(is_list(maps:get(<<"app_entries">>, Manifest))),
            ?assert(is_list(maps:get(<<"admin_entries">>, Manifest))),
            ?assert(is_list(maps:get(<<"plugins">>, Manifest)))
        end
    end}.

etag_is_consistent_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            M1 = app_manifest_handler:build_manifest(),
            Etag1 = app_manifest_handler:compute_etag(M1),
            M2 = app_manifest_handler:build_manifest(),
            Etag2 = app_manifest_handler:compute_etag(M2),
            ?assertEqual(Etag1, Etag2),
            ?assertMatch(<<"\"", _/binary>>, Etag1),
            ?assert(byte_size(Etag1) > 2)
        end
    end}.

etag_changes_with_content_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Etag1 = app_manifest_handler:compute_etag(#{<<"v">> => 1}),
            Etag2 = app_manifest_handler:compute_etag(#{<<"v">> => 2}),
            ?assertNotEqual(Etag1, Etag2)
        end
    end}.
