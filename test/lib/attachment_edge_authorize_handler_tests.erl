-module(attachment_edge_authorize_handler_tests).
-moduledoc "边缘授权在企业功能启用/裁剪时的离线处理器回归；不启动应用或网络。".

-include_lib("eunit/include/eunit.hrl").

-define(HANDLER, attachment_edge_authorize_handler).
-define(TOKEN, <<"synthetic-edge-internal-token">>).
-define(ORIGIN, <<"https://storage.invalid/owned-asset">>).

enterprise_feature_profiles_test_() ->
    [
        {atom_to_list(Profile), {timeout, 35, ?_test(pure_mock_fixture:run(fun() ->
            check_profile(Profile)
        end))}}
     || Profile <- [enabled, pruned]
    ].

check_profile(Profile) ->
    %% Each profile is compiled to memory in a fresh peer VM. The repository's
    %% generated header and shared ebin are never changed by this fixture.
    Beam = compile_profile(Profile),
    assert_feature_imports(Profile, Beam),
    {module, ?HANDLER} = code:load_binary(?HANDLER, "edge-feature-profile", Beam),
    pure_mock_fixture:with_mocks(common_mocks() ++ feature_mocks(Profile), fun() ->
        application:set_env(imboy, edge_internal_token, ?TOKEN),
        assert_internal_token_required(),
        assert_enterprise_dispatch(Profile),
        assert_export_fallback(Profile)
    end).

compile_profile(Profile) ->
    Source = proplists:get_value(source, ?HANDLER:module_info(compile)),
    ?assert(filelib:is_regular(Source)),
    Temp = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "imboy-edge-profile-" ++ os:getpid() ++ "-" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Temp),
    try
        Header = filename:join([Temp, "generated", "imboy_product_features.hrl"]),
        ok = filelib:ensure_dir(Header),
        Contents =
            case Profile of
                enabled -> <<"-define(IMBOY_FEATURE_ENTERPRISE_BUSINESS, true).\n">>;
                pruned -> <<"%% Enterprise business is excluded in this test profile.\n">>
            end,
        ok = file:write_file(Header, Contents),
        {ok, ?HANDLER, Beam, []} = compile:file(Source, [
            binary, debug_info, return_errors, return_warnings, {i, Temp}
        ]),
        Beam
    after
        ok = file:del_dir_r(Temp)
    end.

assert_feature_imports(Profile, Beam) ->
    {ok, {?HANDLER, [{imports, Imports}]}} = beam_lib:chunks(Beam, [imports]),
    EnterpriseImports = [
        Entry
     || Entry = {Module, _, _} <- Imports,
        Module =:= eb_asset_app orelse Module =:= eb_asset_download_ticket
    ],
    Expected =
        case Profile of
            enabled ->
                [{eb_asset_app, edge_authorize_content, 1}, {eb_asset_download_ticket, verify, 1}];
            pruned -> []
        end,
    ?assertEqual(Expected, lists:sort(EnterpriseImports)).

assert_internal_token_required() ->
    lists:foreach(
        fun(Token) ->
            assert_response(404, #{<<"error">> => <<"not_found">>}, request(<<"GET">>, Token))
        end,
        [<<>>, <<"wrong-internal-token">>, <<"synthetic-edge-internal-tokeX">>]
    ),
    assert_response(
        404, #{<<"error">> => <<"not_found">>}, maps:remove(internal_token, request(<<"GET">>, ?TOKEN))
    ),
    %% An otherwise valid caller cannot use an unconfigured or empty server token.
    application:unset_env(imboy, edge_internal_token),
    assert_response(404, #{<<"error">> => <<"not_found">>}, request(<<"GET">>, ?TOKEN)),
    application:set_env(imboy, edge_internal_token, <<>>),
    assert_response(404, #{<<"error">> => <<"not_found">>}, request(<<"GET">>, ?TOKEN)),
    application:set_env(imboy, edge_internal_token, ?TOKEN),
    assert_response(
        404, #{<<"error">> => <<"not_found">>}, (request(<<"GET">>, ?TOKEN))#{method => <<"GET">>}
    ),
    ?assertEqual(0, meck:num_calls(cowboy_req, read_body, 2)),
    ?assertEqual(0, meck:num_calls(attach_logic, download, 1)).

assert_enterprise_dispatch(enabled) ->
    assert_response(200, #{<<"url">> => ?ORIGIN}, request(<<"HEAD">>, ?TOKEN)),
    ?assertEqual(1, meck:num_calls(eb_asset_download_ticket, verify, 1)),
    ?assertEqual(1, meck:num_calls(eb_asset_app, edge_authorize_content, [claims()])),
    ?assertEqual(0, meck:num_calls(talk_record_playback_stream, edge_target, 1)),
    %% A verified ticket still requires the current asset ACL on every request.
    meck:expect(eb_asset_app, edge_authorize_content, fun(_) -> {error, forbidden} end),
    assert_response(404, #{<<"error">> => <<"not_found">>}, request(<<"GET">>, ?TOKEN)),
    ?assertEqual(2, meck:num_calls(eb_asset_app, edge_authorize_content, [claims()]));
assert_enterprise_dispatch(pruned) ->
    assert_response(404, #{<<"error">> => <<"not_found">>}, request(<<"HEAD">>, ?TOKEN)),
    ?assertEqual(1, meck:num_calls(talk_record_playback_stream, edge_target, 1)),
    ?assertEqual(1, meck:num_calls(talk_record_export_download, edge_target, 1)).

assert_export_fallback(Profile) ->
    case Profile of
        enabled ->
            meck:expect(eb_asset_download_ticket, verify, fun(_) -> {error, invalid} end);
        pruned ->
            ok
    end,
    Filename = <<"talk-record-export.md">>,
    meck:expect(talk_record_export_download, edge_target, fun(_) ->
        {ok, {?ORIGIN, Filename}}
    end),
    assert_response(
        200, #{<<"url">> => ?ORIGIN, <<"filename">> => Filename}, request(<<"GET">>, ?TOKEN)
    ).

request(Method, Token) ->
    #{
        method => <<"POST">>,
        internal_token => Token,
        body => jsone:encode(#{<<"ticket">> => <<"owned-ticket">>, <<"method">> => Method})
    }.

assert_response(Status, Payload, Request) ->
    {ok, Reply, state} = ?HANDLER:init(Request, state),
    ?assertEqual(Status, maps:get(status, Reply)),
    ?assertEqual(Payload, maps:get(payload, Reply)),
    ?assertEqual(<<"private, no-store">>, maps:get(<<"cache-control">>, maps:get(headers, Reply))).

claims() ->
    #{org_id => 1, workspace_id => 2, asset_id => 3, actor_kind => member, actor_id => 4}.

feature_mocks(enabled) ->
    [
        {eb_asset_download_ticket, [{verify, 1, fun(_) -> {ok, claims()} end}]},
        {eb_asset_app, [{edge_authorize_content, 1, fun(_) -> {ok, ?ORIGIN} end}]}
    ];
feature_mocks(pruned) ->
    [].

common_mocks() ->
    [
        {cowboy_req, [
            {method, 1, fun(Req) -> maps:get(method, Req) end},
            {header, 3, fun(<<"x-edge-internal-token">>, Req, Default) ->
                maps:get(internal_token, Req, Default)
            end},
            {read_body, 2, fun(Req, Options) ->
                ?assertEqual(8192, maps:get(length, Options)),
                {ok, maps:get(body, Req), Req}
            end},
            {reply, 4, fun(Status, Headers, Body, Req) ->
                Req#{status => Status, headers => Headers, payload => jsone:decode(Body)}
            end}
        ]},
        {attach_logic, [{download, 1, fun(_) -> {error, invalid} end}]},
        {talk_record_playback_stream, [{edge_target, 1, fun(_) -> {error, invalid} end}]},
        {talk_record_export_download, [{edge_target, 1, fun(_) -> {error, invalid} end}]}
    ].
