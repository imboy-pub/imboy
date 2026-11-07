-module(attach_direct_avatar_real_tests).
-include_lib("eunit/include/eunit.hrl").

avatar_real_test_() ->
    %% 真 Garage 端到端（A0_ENDPOINT/A0_AK/A0_SK/A0_PUBLIC_BUCKET）：缺 env
    %% 时 list_to_binary(false) 直接 badarg 崩溃——按仓内让位约定打标记空跑
    %% （与 ATTACH_BIND_EUNIT_YIELD 同构），真 fixture 环境全语义执行。
    case
        [
            N
         || N <- ["A0_ENDPOINT", "A0_AK", "A0_SK", "A0_PUBLIC_BUCKET"],
            os:getenv(N) =:= false
        ]
    of
        [] ->
            {timeout, 120, fun avatar_real/0};
        Missing ->
            io:format(user, "AVATAR_REAL_EUNIT_YIELD missing_env=~p~n", [Missing]),
            []
    end.

avatar_real() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    Cfg = #{
        endpoint => list_to_binary(os:getenv("A0_ENDPOINT")),
        access_key => list_to_binary(os:getenv("A0_AK")),
        secret_key => list_to_binary(os:getenv("A0_SK")),
        bucket => <<"imboy">>,
        public_bucket => list_to_binary(os:getenv("A0_PUBLIC_BUCKET")),
        region => <<"garage">>
    },
    PublicCfg = Cfg#{bucket => maps:get(public_bucket, Cfg)},
    Prev = application:get_env(imboy, garage),
    ok = application:set_env(imboy, garage, Cfg),
    Suffix = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    Prefix = <<"closeout-c08/avatar-", Suffix/binary>>,
    Source = <<Prefix/binary, "/private">>,
    Target = <<"appagent/avatar/", Prefix/binary, "/public">>,
    Body = base64:decode(
        <<"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/aX8AAAAASUVORK5CYII=">>
    ),
    Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
    ok = put(Cfg, Source, Body),
    Mods = [
        elib_pg,
        attachment_repo,
        attachment_avatar_alias_repo,
        attachment_edge_redirect,
        attachment_storage
    ],
    [meck:new(M, [passthrough, no_link]) || M <- Mods],
    try
        meck:expect(elib_pg, query, fun(_, _) ->
            {ok, [
                #{
                    <<"direct_state">> => <<"CONFIRMED">>,
                    <<"upload_registration">> => #{
                        <<"subject">> => #{
                            <<"kind">> => <<"application">>,
                            <<"org_id">> => <<"0">>,
                            <<"principal">> => <<"agent">>
                        }
                    }
                }
            ]}
        end),
        meck:expect(attachment_edge_redirect, edge_ready, fun() -> true end),
        meck:expect(attachment_avatar_alias_repo, reserve, fun(Key) ->
            case attachment_repo:find_by_path_for_update_tx(mock_conn, Key) of
                {ok, Row0} -> {ok, Row0, #{<<"state">> => <<"unpublished">>}};
                Error -> Error
            end
        end),
        meck:expect(attachment_avatar_alias_repo, publish, fun(_, _) ->
            {ok, <<"11111111-2222-4333-8444-555555555555">>}
        end),
        meck:expect(attachment_avatar_alias_repo, public_url, fun(_) ->
            {ok, <<"http://localhost/_avatar/11111111-2222-4333-8444-555555555555">>}
        end),
        Row = #{
            <<"id">> => 42,
            <<"status">> => 1,
            <<"lifecycle_state">> => <<"live">>,
            <<"mime_type">> => <<"image/png">>,
            <<"file_hash256">> => Sha,
            <<"size">> => byte_size(Body)
        },
        meck:expect(attachment_repo, find_by_path_for_update_tx, fun(_, _) -> {ok, Row} end),
        meck:expect(attachment_storage, resolve, fun(_) ->
            {ok, #{driver => <<"garage">>, endpoint => maps:get(endpoint, Cfg)}}
        end),
        meck:expect(attachment_storage, frozen_location, fun(_, _) ->
            {ok, #{
                namespace => <<"fixture">>,
                container => <<"imboy">>,
                key => Source,
                garage_config => Cfg#{container => <<"imboy">>}
            }}
        end),
        ?assertEqual(404, get_status(PublicCfg, Target)),
        meck:expect(attachment_repo, find_by_path_for_update_tx, fun(_, _) ->
            {ok, Row#{<<"size">> => byte_size(Body) + 1}}
        end),
        ?assertEqual(
            {error, avatar_proof_mismatch},
            attach_direct_logic:publish_avatar(Target, <<"agent">>, <<>>)
        ),
        ?assertEqual(404, get_status(PublicCfg, Target)),
        meck:expect(attachment_repo, find_by_path_for_update_tx, fun(_, _) -> {ok, Row} end),
        ?assertMatch({ok, _}, attach_direct_logic:publish_avatar(Target, <<"agent">>, <<>>)),
        ?assertEqual(404, get_status(PublicCfg, Target))
    after
        [meck:unload(M) || M <- Mods],
        case Prev of
            undefined -> application:unset_env(imboy, garage);
            {ok, V} -> application:set_env(imboy, garage, V)
        end
    end.

put(Cfg, Key, Body) ->
    Checksum = base64:encode(crypto:hash(sha256, Body)),
    Headers = [{<<"content-type">>, <<"image/png">>}, {<<"x-amz-checksum-sha256">>, Checksum}],
    Url = signed_url(<<"PUT">>, Cfg, Key, Headers),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {
            binary_to_list(Url),
            [{"x-amz-checksum-sha256", binary_to_list(Checksum)}],
            "image/png",
            Body
        },
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ok.

get_status(Cfg, Key) -> element(1, get(Cfg, Key)).
get(Cfg, Key) ->
    Url = signed_url(<<"GET">>, Cfg, Key, []),
    {ok, {{_, Status, _}, _, Body}} = httpc:request(
        get,
        {binary_to_list(Url), []},
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    {Status, Body}.

signed_url(Method, Cfg, Key, Headers) ->
    elib_s3_sign:presign_signed_url_at(
        Method,
        maps:get(endpoint, Cfg),
        maps:get(bucket, Cfg),
        Key,
        Headers,
        calendar:universal_time(),
        300,
        Cfg#{container => maps:get(bucket, Cfg)}
    ).
