-module(talk_record_private_garage_tests).
-include_lib("eunit/include/eunit.hrl").

%% Explicit private synthetic resource; absent configuration is not acceptance.
private_garage_test_() ->
    case os:getenv("TALK_RECORD_PRIVATE_GARAGE_CONFIG") of
        false ->
            [];
        Path ->
            {timeout, 60,
                {setup, fun() -> setup(Path) end, fun cleanup/1, fun(C) ->
                    [?_test(full_object(C)), ?_test(exact_range(C)), ?_test(integrity_reject(C))]
                end}}
    end.
setup(Path) ->
    {ok, Bytes} = file:read_file(Path),
    J = jsone:decode(Bytes),
    Endpoint = maps:get(<<"endpoint">>, J),
    ?assertMatch(<<"http://127.0.0.1:", _/binary>>, Endpoint),
    ?assertEqual(<<"talk-record-relay">>, maps:get(<<"bucket">>, J)),
    application:ensure_all_started(inets),
    application:ensure_all_started(crypto),
    Saved = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry, attachment_gateway_tmp_dir]
    ],
    C = #{
        endpoint => Endpoint,
        region => maps:get(<<"region">>, J),
        bucket => maps:get(<<"bucket">>, J),
        access_key => maps:get(<<"access_key">>, J),
        secret_key => maps:get(<<"secret_key">>, J)
    },
    Root = #{
        root_id => <<"relay-private">>,
        driver => <<"garage">>,
        namespace => <<"relay-private">>,
        endpoint => Endpoint,
        container => maps:get(bucket, C)
    },
    application:set_env(imboy, garage, C),
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1, roots => #{<<"relay-private">> => Root}
    }),
    application:set_env(
        imboy, attachment_gateway_tmp_dir, binary_to_list(maps:get(<<"spool_dir">>, J))
    ),
    #{saved => Saved, root => Root, config => C}.
cleanup(#{saved := Saved}) ->
    [
        case V of
            {ok, X} -> application:set_env(imboy, K, X);
            undefined -> application:unset_env(imboy, K)
        end
     || {K, V} <- Saved
    ],
    ok.
with_asset(#{root := Root, config := C}, Fun) ->
    Key = <<"synthetic/", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    B = binary:copy(<<"synthetic-record-audio-0123456789">>, 20000),
    I = #{
        endpoint => maps:get(endpoint, C),
        container => maps:get(bucket, C),
        key => Key,
        garage_config => C
    },
    Now = calendar:universal_time(),
    Exp = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Now) + 120),
    ok = attachment_storage_garage:put_snapshot(I, B, <<"audio/aac">>, #{
        issued_at => Now, expires_at => Exp
    }),
    Row = #{
        <<"path">> => Key,
        <<"size">> => byte_size(B),
        <<"mime_type">> => <<"audio/aac">>,
        <<"file_hash256">> => binary:encode_hex(crypto:hash(sha256, B), lowercase),
        <<"storage_driver">> => <<"garage">>,
        <<"storage_namespace">> => <<"relay-private">>,
        <<"storage_container">> => maps:get(bucket, C),
        <<"storage_locator_sha256">> => attachment_storage:locator_digest(<<"relay-private">>, Root)
    },
    try
        Fun(Row, B)
    after
        ok = attachment_storage_garage:delete_snapshot(I),
        ?assertEqual({error, not_found}, attachment_storage_garage:read_snapshot(I))
    end.
full_object(C) ->
    with_asset(C, fun(Row, B) ->
        ?assertEqual(
            {ok, B},
            attachment_artifact_reader:with_source(
                Row,
                1000000,
                fun(S) -> {ok, read_source(S, 0, [])} end
            )
        )
    end).
exact_range(C) ->
    with_asset(C, fun(Row, B) ->
        Offset = 4097,
        Length = 128003,
        ?assertEqual(
            {ok, binary:part(B, Offset, Length)},
            attachment_artifact_reader:with_range(
                Row,
                Offset,
                Length,
                fun(S) -> {ok, read_source(S, 0, [])} end
            )
        )
    end).
integrity_reject(C) ->
    with_asset(C, fun(Row, _) ->
        ?assertEqual(
            {error, artifact_integrity_mismatch},
            attachment_artifact_reader:with_source(
                Row#{<<"file_hash256">> => binary:copy(<<"0">>, 64)}, 1000000, fun(_) ->
                    erlang:error(delivered_bad_bytes)
                end
            )
        ),
        ?assertEqual(
            {error, artifact_integrity_mismatch},
            attachment_artifact_reader:with_source(
                Row, 1, fun(_) -> erlang:error(delivered_oversize_bytes) end
            )
        )
    end).
read_source(S, N, Acc) ->
    case attachment_upload_spool:read(S, N) of
        {ok, B} -> read_source(S, N + byte_size(B), [B | Acc]);
        eof -> iolist_to_binary(lists:reverse(Acc))
    end.
