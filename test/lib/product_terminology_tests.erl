-module(product_terminology_tests).

-include_lib("eunit/include/eunit.hrl").

generic_profile_loads_test() ->
    ?assertEqual(ok, product_terminology:init()),
    assert_public_profile(<<"generic">>).

moya_profile_loads_test() ->
    {ok, Json} = file:read_file(filename:join(terminology_dir(), "moya.json")),
    Document = json:decode(Json),
    Public = product_terminology:public(<<"moya">>),
    ?assertEqual(maps:get(<<"terms">>, Document), maps:get(<<"terms">>, Public)).

unknown_profile_falls_back_to_generic_test() ->
    ?assertEqual(<<"generic">>, maps:get(<<"profile">>, product_terminology:public(<<"other">>))),
    ?assertEqual(<<"generic">>, maps:get(<<"profile">>, product_terminology:public(<<"../moya">>))),
    ?assertEqual(
        <<"generic">>, maps:get(<<"profile">>, product_terminology:public(binary:copy(<<"a">>, 33)))
    ).

validate_checked_in_profiles_test() ->
    ?assertEqual(ok, product_terminology:validate_all()).

document_validation_test() ->
    Valid = valid_document(),
    ?assertEqual(ok, product_terminology:validate_document(<<"moya">>, Valid)),
    ?assertError(
        {invalid_terminology, {profile_mismatch, <<"other">>, <<"moya">>}},
        product_terminology:validate_document(<<"other">>, Valid)
    ),
    Terms = maps:remove(<<"participant">>, maps:get(<<"terms">>, Valid)),
    ?assertError(
        {invalid_terminology, {term_keys, [<<"participant">>], []}},
        product_terminology:validate_document(<<"moya">>, Valid#{<<"terms">> => Terms})
    ).

duplicate_json_keys_are_rejected_test() ->
    Json = <<"{\"schema_version\":1,\"profile\":\"moya\",\"profile\":\"other\",\"terms\":{}}">>,
    ?assertError(
        {invalid_terminology_json, memory, {duplicate_json_object_key, <<"profile">>}},
        product_terminology:validate_document(<<"moya">>, Json)
    ).

assert_public_profile(Profile) ->
    Public = product_terminology:public(Profile),
    ?assertEqual(Profile, maps:get(<<"profile">>, Public)),
    ?assertMatch(<<"sha256:", _/binary>>, maps:get(<<"hash">>, Public)),
    ?assertEqual(71, byte_size(maps:get(<<"hash">>, Public))),
    ?assert(is_map(maps:get(<<"terms">>, Public))).

valid_document() ->
    #{
        <<"schema_version">> => 1,
        <<"profile">> => <<"moya">>,
        <<"terms">> => #{
            <<"participant">> => <<"participant-label">>,
            <<"delegate">> => <<"delegate-label">>,
            <<"operator">> => <<"operator-label">>,
            <<"group">> => <<"group-label">>,
            <<"task_assignment">> => <<"assignment-label">>,
            <<"task_submission">> => <<"submission-label">>,
            <<"submission_review">> => <<"review-label">>,
            <<"review_assist">> => <<"assist-label">>
        }
    }.

terminology_dir() ->
    filename:join(code:priv_dir(imboy), "terminology").
