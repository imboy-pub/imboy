-module(app_manifest_handler_tests).

-include_lib("eunit/include/eunit.hrl").

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
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            Manifest = app_manifest_handler:build_manifest(),
            ?assert(is_map(Manifest)),
            ?assert(maps:is_key(<<"features">>, Manifest)),
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
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
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
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            Etag1 = app_manifest_handler:compute_etag(#{<<"v">> => 1}),
            Etag2 = app_manifest_handler:compute_etag(#{<<"v">> => 2}),
            ?assertNotEqual(Etag1, Etag2)
        end
     end}.
