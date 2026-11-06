-module(attach_direct_avatar_pg_garage_tests).
-include_lib("eunit/include/eunit.hrl").

%% Dedicated, freshly migrated marker DB and unique private/public Garage buckets only.
avatar_pg_garage_test_() ->
    %% 共享全量裸跑不经 C08 专用车道：缺钉名 env 时在生成器入口让位
    %% （先例：TALK_OWNED/W403_STORAGE 的入口 yield）；车道在场（C08
    %% record 门）语义零删减，env 在场但错值仍由 setup fail-closed。
    case avatar_lane_env_set("c08_avatar_") of
        true ->
            {timeout, 240, {setup, fun setup/0, fun teardown/1, fun(S) -> ?_test(run(S)) end}};
        false ->
            io:format(
                user,
                "C08_AVATAR_EUNIT_YIELD module=attach_direct_avatar_pg_garage_tests"
                " missing_env=\"c08_avatar_ lane\"~n",
                []
            ),
            []
    end.

avatar_lane_env_set(DbPrefix) ->
    Db = os:getenv("T05_PG_DB"),
    is_list(Db) andalso lists:prefix(DbPrefix, Db) andalso
        lists:all(
            fun(Name) ->
                V = os:getenv(Name),
                is_list(V) andalso V =/= ""
            end,
            ["C08_AVATAR_PRIVATE", "C08_AVATAR_PUBLIC", "A0_ENDPOINT", "A0_AK", "A0_SK"]
        ).

setup() ->
    Db = os:getenv("T05_PG_DB"),
    true = is_list(Db) andalso lists:prefix("c08_avatar_", Db),
    S = talk_record_t05_support:setup_all(),
    C = maps:get(conn, S),
    {ok, [#{<<"comment">> := <<"c08-avatar-isolated">>}]} = elib_pg:query(
        C,
        <<"SELECT shobj_description(oid,'pg_database') AS comment FROM pg_database WHERE datname=current_database()">>,
        []
    ),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Priv = list_to_binary(os:getenv("C08_AVATAR_PRIVATE")),
    Public = list_to_binary(os:getenv("C08_AVATAR_PUBLIC")),
    true = Priv =/= Public,
    Endpoint = list_to_binary(os:getenv("A0_ENDPOINT")),
    Ak = list_to_binary(os:getenv("A0_AK")),
    Sk = list_to_binary(os:getenv("A0_SK")),
    Config = #{
        endpoint => Endpoint,
        bucket => Priv,
        public_bucket => Public,
        region => <<"garage">>,
        access_key => Ak,
        secret_key => Sk
    },
    Ns = <<"c08-avatar-pg">>,
    Root = #{
        driver => <<"garage">>,
        namespace => Ns,
        endpoint => Endpoint,
        container => Priv,
        access_key => Ak,
        secret_key => Sk,
        region => <<"garage">>
    },
    ok = application:set_env(imboy, garage, Config),
    ok = application:set_env(
        imboy,
        attachment_storage_registry,
        #{version => 1, roots => #{Ns => Root}, upload_namespaces => #{<<"public">> => Ns}}
    ),
    S#{config => Config}.

teardown(S) -> talk_record_t05_support:teardown_all(S).

run(S) ->
    Cfg = maps:get(config, S),
    Body = base64:decode(
        <<"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/aX8AAAAASUVORK5CYII=">>
    ),
    {Key, App} = upload(1, Body),
    ?assertEqual(404, public_get(Cfg, Key)),
    {ok, Pub} = attach_direct_logic:publish_avatar(Key, App, <<>>),
    ?assertEqual(200, public_get(Cfg, Key)),
    ?assertEqual({ok, Pub}, attach_direct_logic:publish_avatar(Key, App, <<>>)),
    ?assertMatch(
        {error, _}, elib_pg:query(<<"UPDATE attachment SET status=0 WHERE path=$1">>, [Key])
    ),
    {Disabled, DisabledApp} = upload(2, Body),
    {ok, 1} = elib_pg:execute(<<"UPDATE attachment SET status=0 WHERE path=$1">>, [Disabled]),
    ?assertEqual(
        {error, avatar_not_validatable},
        attach_direct_logic:publish_avatar(Disabled, DisabledApp, <<>>)
    ),
    ?assertEqual(404, public_get(Cfg, Disabled)),
    {Unconfirmed, UnconfirmedApp} = register(3),
    ?assertEqual(
        {error, not_confirmed},
        attach_direct_logic:publish_avatar(Unconfirmed, UnconfirmedApp, <<>>)
    ),
    {SameBucket, _} = upload(4, Body),
    ?assertEqual(
        {error, avatar_bucket_not_private},
        attachment_avatar_publication_repo:reserve(SameBucket, maps:get(bucket, Cfg))
    ),
    delayed_copy(Body),
    unknown_copy(Body),
    not_dispatched_retry(Cfg, Body),
    stale_dispatch_tokens(Cfg, Body),
    ok.

register(N) ->
    App = list_to_binary(io_lib:format("00000000-0000-4000-8000-~12..0B", [N])),
    Cid = list_to_binary(io_lib:format("10000000-0000-4000-8000-~12..0B", [N])),
    {ok, #{object_key := Key}} = attach_direct_logic:register_application(
        App, <<"0">>, <<"public">>, Cid, <<"avatar.png">>
    ),
    {Key, App}.

upload(N, Body) ->
    {Key, App} = register(N),
    Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
    ok = attach_direct_logic:declare(Key, Sha, byte_size(Body), <<"image/png">>),
    {ok, #{grant := Grant, attempt_id := Attempt}} = attach_direct_logic:grant(Key, Sha),
    Headers = maps:from_list(maps:get(required_headers, Grant)),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {
            binary_to_list(maps:get(url, Grant)),
            [
                {"x-amz-checksum-sha256",
                    binary_to_list(maps:get(<<"x-amz-checksum-sha256">>, Headers))}
            ],
            "image/png",
            Body
        },
        [{autoredirect, false}],
        [{body_format, binary}]
    ),
    {ok, #{state := <<"CONFIRMED">>}} = attach_direct_logic:finalize(Key, Attempt),
    {Key, App}.

public_get(Cfg, Key) ->
    Bucket = maps:get(public_bucket, Cfg),
    Url = elib_s3_sign:presign_signed_url_at(
        <<"GET">>,
        maps:get(endpoint, Cfg),
        Bucket,
        Key,
        [],
        calendar:universal_time(),
        300,
        Cfg#{container => Bucket}
    ),
    {ok, {{_, Status, _}, _, _}} = httpc:request(
        get,
        {binary_to_list(Url), []},
        [{autoredirect, false}],
        [{body_format, binary}]
    ),
    Status.

delayed_copy(Body) ->
    {Key, App} = upload(5, Body),
    Parent = self(),
    meck:new(attachment_storage_garage, [passthrough, no_link]),
    meck:expect(attachment_storage_garage, copy_cross, fun(Ns, C, Src, Dst, Dest) ->
        Parent ! {copy_started, self()},
        receive
            proceed -> meck:passthrough([Ns, C, Src, Dst, Dest])
        after 30000 -> {error, timeout}
        end
    end),
    try
        Pid = spawn(fun() ->
            Parent ! {copy_result, attach_direct_logic:publish_avatar(Key, App, <<>>)}
        end),
        receive
            {copy_started, Pid} -> ok
        after 30000 -> error(copy_not_started)
        end,
        ?assertEqual(
            {error, avatar_publication_unsettled},
            attach_direct_logic:publish_avatar(Key, App, <<>>)
        ),
        ?assertMatch(
            {error, _}, elib_pg:query(<<"UPDATE attachment SET status=0 WHERE path=$1">>, [Key])
        ),
        Pid ! proceed,
        receive
            {copy_result, Result} -> ?assertMatch({ok, _}, Result)
        after 30000 -> error(copy_not_finished)
        end,
        ?assertEqual(1, meck:num_calls(attachment_storage_garage, copy_cross, '_'))
    after
        meck:unload(attachment_storage_garage)
    end.

unknown_copy(Body) ->
    {Key, App} = upload(6, Body),
    meck:new(attachment_storage_garage, [passthrough, no_link]),
    meck:expect(attachment_storage_garage, copy_cross, fun(_, _, _, _, _) -> {error, timeout} end),
    try
        ?assertEqual(
            {error, {publish_failed, timeout}}, attach_direct_logic:publish_avatar(Key, App, <<>>)
        ),
        ?assertEqual(
            {error, avatar_publication_unsettled},
            attach_direct_logic:publish_avatar(Key, App, <<>>)
        ),
        ?assertEqual(1, meck:num_calls(attachment_storage_garage, copy_cross, '_')),
        ?assertMatch(
            {error, _}, elib_pg:query(<<"UPDATE attachment SET status=0 WHERE path=$1">>, [Key])
        )
    after
        meck:unload(attachment_storage_garage)
    end.

not_dispatched_retry(Cfg, Body) ->
    {Key, App} = upload(7, Body),
    meck:new(attachment_storage_garage, [passthrough, no_link]),
    try
        meck:expect(attachment_storage_garage, copy_cross, fun(_, _, _, _, _) ->
            {error, {not_dispatched, location_changed}}
        end),
        ?assertEqual(
            {error, {publish_failed, {not_dispatched, location_changed}}},
            attach_direct_logic:publish_avatar(Key, App, <<>>)
        ),
        ?assertEqual(404, public_get(Cfg, Key)),
        meck:expect(attachment_storage_garage, copy_cross, fun(Ns, C, Src, Dst, Dest) ->
            meck:passthrough([Ns, C, Src, Dst, Dest])
        end),
        ?assertMatch({ok, _}, attach_direct_logic:publish_avatar(Key, App, <<>>)),
        ?assertEqual(200, public_get(Cfg, Key)),
        ?assertEqual(2, meck:num_calls(attachment_storage_garage, copy_cross, '_'))
    after
        meck:unload(attachment_storage_garage)
    end.

stale_dispatch_tokens(Cfg, Body) ->
    {Key, _} = upload(8, Body),
    Bucket = maps:get(public_bucket, Cfg),
    {ok, #{<<"id">> := Aid}, <<"reserved">>} =
        attachment_avatar_publication_repo:reserve(Key, Bucket),
    {ok, OldToken} = attachment_avatar_publication_repo:start_copy(Aid),
    ?assertEqual(
        {error, {not_dispatched, location_changed}},
        attachment_avatar_publication_repo:finish_copy(
            Aid,
            OldToken,
            {error, {not_dispatched, location_changed}}
        )
    ),
    {ok, NewToken} = attachment_avatar_publication_repo:start_copy(Aid),
    ?assertNotEqual(OldToken, NewToken),
    ?assertEqual(
        {error, avatar_publication_unsettled},
        attachment_avatar_publication_repo:finish_copy(Aid, OldToken, {ok, #{}})
    ),
    ?assertEqual(
        {error, avatar_publication_unsettled},
        attachment_avatar_publication_repo:finish_copy(
            Aid,
            OldToken,
            {error, {not_dispatched, location_changed}}
        )
    ),
    {ok, [#{<<"state">> := <<"unknown">>, <<"dispatch_token">> := NewToken}]} =
        elib_pg:query(
            <<"SELECT state,dispatch_token::text AS dispatch_token FROM attachment_avatar_publication WHERE attachment_id=$1">>,
            [Aid]
        ),
    ?assertEqual(
        {error, timeout},
        attachment_avatar_publication_repo:finish_copy(Aid, NewToken, {error, timeout})
    ),
    ?assertEqual(
        {error, avatar_publication_not_retryable},
        attachment_avatar_publication_repo:start_copy(Aid)
    ),
    ?assertEqual(404, public_get(Cfg, Key)).
