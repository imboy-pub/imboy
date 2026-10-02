%% Real authenticated HTTP: one signing key, separate Human/Internal domains.
-module(enterprise_cursor_domain_http_checks).
-include_lib("eunit/include/eunit.hrl").
-define(H, intbe02_http_support).

cursor_domain_test_() -> {timeout, 180, fun run/0}.

run() ->
    S = ?H:setup_all(),
    SignKey = app_version_ds:sign_key(<<"synthetic">>, <<"orgdir-test">>, <<"synthetic.orgdir">>),
    Saved = [{K, application:get_env(imboy, K)} || K <- [jwt_key, api_auth_switch]],
    try
        application:set_env(imboy, api_auth_switch, <<"on">>),
        application:set_env(imboy, jwt_key, <<"orgdir_http_test_jwt_key_0123456789">>),
        ok = app_version_ds:set_sign_key(
            <<"synthetic">>,
            <<"orgdir-test">>,
            <<"synthetic.orgdir">>,
            <<"synthetic-orgdir-device-key-only">>
        ),
        check(S)
    after
        ok = app_version_ds:set_sign_key(
            <<"synthetic">>, <<"orgdir-test">>, <<"synthetic.orgdir">>, SignKey
        ),
        lists:foreach(fun restore/1, Saved),
        ?H:teardown_all(S),
        inttest_marker_db:release(S)
    end.

check(S) ->
    H1 = payload(human(S, 995011, <<>>)),
    HC = cursor(H1, <<"cursor">>),
    H2 = payload(human(S, 995011, HC)),
    ?assertNotEqual(first(H1, <<"list">>), first(H2, <<"list">>)),
    I1 = success(internal(S, <<>>)),
    IC = cursor(I1, <<"next_cursor">>),
    I2 = success(internal(S, IC)),
    ?assertNotEqual(first(I1, <<"items">>), first(I2, <<"items">>)),
    denied(internal(S, HC)),
    denied(human(S, 995011, IC)),
    _ = payload(human(S, 995012, <<>>)),
    denied(human(S, 995012, HC)),
    ok.

cursor(Page, Key) ->
    ?assertEqual(true, maps:get(<<"has_more">>, Page)),
    Value = maps:get(Key, Page),
    ?assert(is_binary(Value) andalso byte_size(Value) > 0),
    Value.

first(Page, Key) ->
    [Row] = maps:get(Key, Page),
    Row.

payload(R) -> maps:get(<<"payload">>, success(R)).

success(R) ->
    ?assertEqual(200, maps:get(status, R)),
    jsone:decode(maps:get(body, R)).

denied(R) ->
    ?assertEqual(400, maps:get(status, R)),
    Body = jsone:decode(maps:get(body, R)),
    ?assertEqual(<<"invalid_request">>, maps:get(<<"code">>, maps:get(<<"error">>, Body))).

human(#{port := Port}, Uid, Cursor) ->
    Path = <<"/api/v1/organizations/995101/directory/members?limit=1&cursor=", Cursor/binary>>,
    ?H:http(Port, <<"GET">>, Path, <<>>, organization_directory_http_support:bearer(Uid)).

internal(#{port := Port} = S, Cursor) ->
    Body =
        case Cursor of
            <<>> -> #{<<"page_size">> => 1};
            _ -> #{<<"page_size">> => 1, <<"cursor">> => Cursor}
        end,
    ?H:http(
        Port,
        <<"POST">>,
        <<"/api/internal/v1/directory/users">>,
        Body,
        ?H:auth(maps:get(cred_a, S))
    ).

restore({Key, undefined}) -> application:unset_env(imboy, Key);
restore({Key, {ok, Value}}) -> application:set_env(imboy, Key, Value).
