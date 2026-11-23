-module(user_region_pg_tests).

-include_lib("eunit/include/eunit.hrl").

region_http_roundtrip_test_() ->
    {timeout, 1800, fun region_http_roundtrip/0}.

region_http_roundtrip() ->
    {ok, _} = application:ensure_all_started(pooler),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(jose),
    %% An existing pool belongs to another fixture. Run this suite in its own VM.
    undefined = whereis(pgsql),
    {Pin, Selection} = pinned_release(),
    Uid = 124707737620973568,
    Old = <<"  同名地区 / 旧行政码  "/utf8>>,
    Db = inttest_marker_db:provision(#{env_prefix => <<"I02">>, migration_target => 286}),
    try
        #{conn := Conn, db := DbName, server := Server} = Db,
        {ok, 1} = epgsql:equery(
            Conn,
            <<
                "INSERT INTO public.\"user\"(id,account,nickname,password,reg_ip,reg_cosv,region,status,account_type) "
                "VALUES ($1,'i02-region-probe','i02probe','unused','127.0.0.1','qa-probe',$2,1,0)"
            >>,
            [Uid, Old]
        ),
        {ok, _} = pooler:new_pool(#{
            name => pgsql,
            init_count => 1,
            max_count => 2,
            start_mfa =>
                {epgsql, connect, [
                    Server#{database => DbName, codecs => [{epgsql_codec_rfc3339_bin, []}]}
                ]}
        }),
        try
            {NativeHeaders, WebHeaders} = authenticated_fixture(Conn, Uid),
            with_http(Pin, fun(Port) ->
                {401, _} = request(get, {url(Port, "/api/v1/web/user/me"), []}),
                check_http_cases(Conn, Uid, {Port, NativeHeaders, WebHeaders}, Pin, Selection, Old)
            end)
        after
            ok = pooler:rm_pool(pgsql)
        end
    after
        inttest_marker_db:release(Db)
    end.

pinned_release() ->
    {ok, Data} = file:read_file(os:getenv("I02_VECTORS")),
    #{
        <<"data_root">> := Root,
        <<"manifest_sha256">> := Digest,
        <<"cases">> := [#{<<"label">> := <<"full-path">>, <<"selection">> := Selection} | _]
    } = jsone:decode(Data),
    {#{data_root => Root, manifest_sha256 => Digest}, Selection}.

authenticated_fixture(Conn, Uid) ->
    ok = application:set_env(imboy, api_auth_switch, <<"on">>),
    ok = application:set_env(imboy, jwt_key, crypto:strong_rand_bytes(32)),
    ok = application:set_env(imboy, postgre_aes_key, base64:encode(crypto:strong_rand_bytes(24))),
    {ok, _} = imboy_cache:start_link([{depcache_memory_max, 8}]),
    ok = elib_tsid:init(#{dc_id => 0, node_id => 1, names => [user_device]}),
    NativeDid = <<"qa-i02-android-device">>,
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO public.user_device(id,user_id,device_id,device_type) VALUES ($1,$2,$3,'android')">>,
        [Uid + 1, Uid, NativeDid]
    ),
    Key = base64:encode(crypto:strong_rand_bytes(32)),
    Pkg = <<"pub.imboy.i02.qa">>,
    ok = app_version_ds:set_sign_key(<<"android">>, <<"1">>, Pkg, Key),
    ?assertEqual(Key, app_version_ds:sign_key(<<"android">>, <<"1">>, Pkg)),
    Sign = elib_hasher:hmac_sha256(<<NativeDid/binary, "|1|android|", Pkg/binary>>, Key),
    NativeToken = token_ds:encrypt_token(Uid, NativeDid),
    NativeHeaders = [
        bearer(NativeToken),
        {"did", binary_to_list(NativeDid)},
        {"vsn", "1"},
        {"cos", "android"},
        {"pkg", binary_to_list(Pkg)},
        {"sk", "1"},
        {"method", "sha256"},
        {"sign", binary_to_list(Sign)}
    ],
    WebDid = <<"qa-i02-web-device">>,
    {ok, WebDevice} = user_device_ds:register_web(elib_dt:now(), Uid, WebDid, #{
        <<"ip">> => <<"127.0.0.1">>
    }),
    {ok, Epoch} = auth_session_ds:current_epoch(Uid),
    WebToken = token_ds:encrypt_web_token(Uid, WebDid, Epoch, WebDevice),
    {NativeHeaders, [bearer(WebToken)]}.

bearer(Token) ->
    {"authorization", binary_to_list(<<"Bearer ", Token/binary>>)}.

with_http(Pin, Run) ->
    Routes = cowboy_router:compile(imboy_router:get_routes()),
    {ok, _} = cowboy:start_clear(
        i02_region_http,
        [{ip, {127, 0, 0, 1}}, {port, 0}],
        #{
            env => #{dispatch => Routes},
            middlewares => [cowboy_router, auth_middleware, cowboy_handler]
        }
    ),
    Previous = application:get_env(imboy, geo_regions_pin),
    try
        ok = application:set_env(imboy, geo_regions_pin, Pin),
        Run(ranch:get_port(i02_region_http))
    after
        ok = cowboy:stop_listener(i02_region_http),
        case Previous of
            {ok, Setting} -> application:set_env(imboy, geo_regions_pin, Setting);
            undefined -> application:unset_env(imboy, geo_regions_pin)
        end
    end.

check_http_cases(Conn, Uid, Port, Pin, Selection, Old) ->
    New = <<"China 北京市 市辖区 东城区"/utf8>>,
    LegacyEdit = <<"  同名新区  "/utf8>>,
    Payload = #{<<"region">> => New, <<"selection">> => Selection},
    expect_region(Port, Old, null, <<"unresolved">>),
    {200, #{<<"code">> := 0}} = update(Port, <<"region">>, Old),
    expect_region(Port, Old, null, <<"unresolved">>),
    {200, #{<<"code">> := 0}} = update(Port, <<"regionSelection">>, Payload),
    expect_region(Port, New, Selection, <<"resolved">>),
    check_rejections(Port, Payload, Selection),
    expect_region(Port, New, Selection, <<"resolved">>),
    ok = application:unset_env(imboy, geo_regions_pin),
    {503, _} = update(Port, <<"regionSelection">>, Payload),
    expect_region(Port, New, Selection, <<"unresolved">>),
    ok = application:set_env(imboy, geo_regions_pin, Pin),
    {200, #{<<"code">> := 0}} = update(Port, <<"region">>, New),
    expect_region(Port, New, Selection, <<"resolved">>),
    {200, #{<<"code">> := 0}} = update(Port, <<"region">>, LegacyEdit),
    expect_region(Port, LegacyEdit, null, <<"unresolved">>),
    {ok, _, [{LegacyEdit, null, null}]} = epgsql:equery(
        Conn,
        <<"SELECT region,region_selection,region_selection_legacy FROM public.\"user\" WHERE id=$1">>,
        [Uid]
    ),
    {200, #{<<"code">> := 0}} = update(Port, <<"region">>, <<>>),
    expect_region(Port, <<>>, null, <<"empty">>).

check_rejections(Port, Payload, Selection) ->
    [Root | Rest] = maps:get(<<"pathIds">>, Selection),
    [_, _ | _] = Rest,
    Rejections = [
        Selection#{<<"pathIds">> => [Root, lists:last(Rest)]},
        Selection#{<<"dataRevision">> => <<"s-0000000000000000">>},
        Selection#{<<"datasetId">> => <<"unknown-dataset">>}
    ],
    lists:foreach(
        fun(Rejected) ->
            {200, #{<<"code">> := Code}} =
                update(Port, <<"regionSelection">>, Payload#{<<"selection">> => Rejected}),
            ?assertNotEqual(0, Code)
        end,
        Rejections
    ),
    RejectEnvelopes = [
        Payload#{<<"pin">> => #{}},
        Payload#{<<"region">> := <<"A different display label">>},
        Payload#{<<"selection">> := Selection#{<<"displayLocale">> => <<"en", 0>>}},
        Payload#{<<"selection">> := Selection#{<<"displaySnapshot">> => [<<"China", 0>>]}}
    ],
    lists:foreach(
        fun(Envelope) ->
            {200, #{<<"code">> := Code}} = update(Port, <<"regionSelection">>, Envelope),
            ?assertNotEqual(0, Code)
        end,
        RejectEnvelopes
    ).

expect_region({Port, _, WebHeaders}, Legacy, Selection, Status) ->
    Req = {url(Port, "/api/v1/web/user/me"), WebHeaders},
    {200, #{<<"code">> := 0, <<"payload">> := Profile}} = request(get, Req),
    ?assertMatch(
        #{
            <<"region">> := Legacy,
            <<"regionSelection">> := Selection,
            <<"regionSelectionStatus">> := Status
        },
        Profile
    ).

update({Port, NativeHeaders, _}, Field, Value) ->
    Body = jsone:encode(#{<<"field">> => Field, <<"value">> => Value}),
    request(post, {url(Port, "/api/v1/user/update"), NativeHeaders, "application/json", Body}).

request(Method, Req) ->
    {ok, {{_, Status, _}, _, Body}} =
        httpc:request(Method, Req, [{timeout, 5000}], [{body_format, binary}]),
    {Status, jsone:decode(Body)}.

url(Port, Path) ->
    "http://127.0.0.1:" ++ integer_to_list(Port) ++ Path.
