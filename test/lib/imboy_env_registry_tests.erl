%% IMBOY_ATTACHMENT_REGISTRY env JSON 解析/结构校验单测（2026-10-20 prod
%% 事故加固：registry 脱离 release 走环境层，显式声明必须 fail-fast）。
-module(imboy_env_registry_tests).

-include_lib("eunit/include/eunit.hrl").

-define(VALID_JSON, <<
    "{\"version\":1,\"roots\":{"
    "\"prod-main-20261020\":{\"driver\":\"garage\","
    "\"namespace\":\"prod-main-20261020\","
    "\"endpoint\":\"http://garage:3900\",\"container\":\"imboy\"},"
    "\"prod-public-20261020\":{\"driver\":\"garage\","
    "\"namespace\":\"prod-public-20261020\","
    "\"endpoint\":\"http://garage:3900\",\"container\":\"imboy-public\"}}"
    ",\"upload_namespaces\":{\"c2c\":\"prod-main-20261020\","
    "\"public\":\"prod-public-20261020\"}}"
>>).

valid_json_test() ->
    {ok, R} = imboy_env:parse_attachment_registry_json(?VALID_JSON),
    ?assertEqual(1, maps:get(version, R)),
    Roots = maps:get(roots, R),
    ?assertEqual(2, map_size(Roots)),
    Main = maps:get(<<"prod-main-20261020">>, Roots),
    ?assertEqual(
        #{
            driver => <<"garage">>,
            namespace => <<"prod-main-20261020">>,
            endpoint => <<"http://garage:3900">>,
            container => <<"imboy">>
        },
        Main
    ),
    ?assertEqual(
        #{
            <<"c2c">> => <<"prod-main-20261020">>,
            <<"public">> => <<"prod-public-20261020">>
        },
        maps:get(upload_namespaces, R)
    ).

upload_namespaces_optional_test() ->
    Json = <<
        "{\"version\":1,\"roots\":{\"r1\":{\"driver\":\"garage\","
        "\"namespace\":\"r1\",\"endpoint\":\"http://e\",\"container\":\"c\"}}}"
    >>,
    {ok, R} = imboy_env:parse_attachment_registry_json(Json),
    ?assertEqual(#{}, maps:get(upload_namespaces, R)).

bad_json_test() ->
    ?assertEqual(
        {error, bad_json},
        imboy_env:parse_attachment_registry_json(<<"{not json">>)
    ).

non_object_test() ->
    ?assertMatch(
        {error, _},
        imboy_env:parse_attachment_registry_json(<<"[1,2,3]">>)
    ).

empty_roots_test() ->
    Json = <<"{\"version\":1,\"roots\":{}}">>,
    ?assertEqual(
        {error, empty_roots},
        imboy_env:parse_attachment_registry_json(Json)
    ).

unsupported_version_test() ->
    Json = <<
        "{\"version\":2,\"roots\":{\"r\":{\"driver\":\"garage\","
        "\"namespace\":\"r\",\"endpoint\":\"http://e\",\"container\":\"c\"}}}"
    >>,
    ?assertMatch(
        {error, {unsupported_version, 2}},
        imboy_env:parse_attachment_registry_json(Json)
    ).

unsupported_driver_test() ->
    Json = <<
        "{\"version\":1,\"roots\":{\"r1\":{\"driver\":\"disk\","
        "\"namespace\":\"r1\",\"endpoint\":\"file:///srv\",\"container\":\"c\"}}}"
    >>,
    ?assertMatch(
        {error, {root_invalid, <<"r1">>, {unsupported_driver, <<"disk">>}}},
        imboy_env:parse_attachment_registry_json(Json)
    ).

missing_fields_test() ->
    %% 缺 container
    Json = <<
        "{\"version\":1,\"roots\":{\"r1\":{\"driver\":\"garage\","
        "\"namespace\":\"r1\",\"endpoint\":\"http://e\"}}}"
    >>,
    ?assertMatch(
        {error, {root_invalid, <<"r1">>, {missing_fields, [<<"container">>]}}},
        imboy_env:parse_attachment_registry_json(Json)
    ).

empty_field_test() ->
    Json = <<
        "{\"version\":1,\"roots\":{\"r1\":{\"driver\":\"garage\","
        "\"namespace\":\"r1\",\"endpoint\":\"\",\"container\":\"c\"}}}"
    >>,
    ?assertMatch(
        {error, {root_invalid, <<"r1">>, empty_field}},
        imboy_env:parse_attachment_registry_json(Json)
    ).

namespace_ref_integrity_test() ->
    %% upload_namespaces 指向不存在的 namespace
    Json = <<
        "{\"version\":1,\"roots\":{\"r1\":{\"driver\":\"garage\","
        "\"namespace\":\"r1\",\"endpoint\":\"http://e\",\"container\":\"c\"}},"
        "\"upload_namespaces\":{\"c2c\":\"no-such-namespace\"}}"
    >>,
    ?assertMatch(
        {error, {namespace_not_in_roots, <<"c2c">>, <<"no-such-namespace">>}},
        imboy_env:parse_attachment_registry_json(Json)
    ).
