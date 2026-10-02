%%% Synthetic legacy metadata with real Garage bytes; no existing databases.
-module(enterprise_legacy_group_file_pg_checks).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").
-define(S, intbe02_http_support).

run() ->
    {ok, _} = application:ensure_all_started(inets),
    H = ?S:setup_all(),
    try
        configure(H),
        legacy_journey(H)
    after
        ?S:teardown_all(H),
        inttest_marker_db:release(H)
    end.

configure(#{conn := C, port := Port}) ->
    application:set_env(imboy, garage, #{
        endpoint => env("IMBOY_TEST_ENDPOINT"),
        bucket => <<"synthetic-enterprise-assets">>,
        region => <<"garage">>,
        access_key => env("IMBOY_TEST_ACCESS"),
        secret_key => env("IMBOY_TEST_SECRET"),
        key_prefix => <<>>
    }),
    application:set_env(imboy, jwt_key, <<"synthetic-legacy-file-gateway-secret">>),
    application:set_env(
        imboy,
        base_url,
        iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port)])
    ),
    ok = ?S:sql_exec(
        C,
        <<"INSERT INTO group_member_generation(group_id,user_id,generation_no,start_seq) VALUES (995301,995011,1,1)">>
    ).

legacy_journey(#{conn := C}) ->
    Bytes = <<"synthetic legacy enterprise file bytes">>,
    Name = <<"legacy-report.txt">>,
    {ok, OldUrl, FileKey} = elib_oss:upload(Bytes, Name, #{mime_type => <<"text/plain">>}),
    ObjectKey = <<FileKey/binary, "/", Name/binary>>,
    Bucket = elib_oss:get_bucket(<<"group">>),
    try
        FileId = seed_legacy(C, FileKey, Name, OldUrl, ObjectKey, Bytes),
        ?assertEqual({error, not_found}, group_file_logic:download(FileId, 995011)),
        run_original_backfill(C),
        #{
            <<"group_file_id">> := FileId,
            <<"anchor_msg_id">> := null,
            <<"anchor_conv_seq">> := null
        } = ?S:one(
            C,
            <<"SELECT group_file_id,anchor_msg_id,anchor_conv_seq FROM attachment WHERE path=$1">>,
            [ObjectKey]
        ),
        {ok, Url} = group_file_logic:download(FileId, 995011),
        ?assertEqual({200, Bytes}, gateway_get(Url)),
        ?assertEqual({error, not_member}, group_file_logic:download(FileId, 995021)),
        assert_unknown_file(C, Name, Bytes),
        {ok, _} = organization_member_logic:suspend(995001, 995101, 995011),
        ?assertEqual({error, not_member}, group_file_logic:download(FileId, 995011)),
        {403, _} = gateway_get(Url),
        {ok, _} = organization_member_logic:restore(995001, 995101, 995011),
        ?assertEqual({200, Bytes}, gateway_get(Url)),
        ok = group_file_logic:delete(FileId, 995011),
        ?assertEqual({error, not_found}, group_file_logic:download(FileId, 995011)),
        {403, _} = gateway_get(Url),
        save_proof(Bytes)
    after
        ok = elib_oss:delete_object(Bucket, ObjectKey)
    end.

seed_legacy(C, FileKey, Name, OldUrl, ObjectKey, Bytes) ->
    {ok, FileId} = group_file_repo:insert(file_data(FileKey, Name, OldUrl, Bytes)),
    %% Pre-108 legacy row shape: anchors/link absent; no current upload helper.
    ok = attachment_ds:save(C, elib_dt:now(), 995011, [
        #{
            <<"file_hash256">> => binary:encode_hex(erlang:md5(Bytes)),
            <<"mime_type">> => <<"text/plain">>,
            <<"name">> => Name,
            <<"path">> => ObjectKey,
            <<"url">> => OldUrl,
            <<"size">> => byte_size(Bytes),
            <<"scope">> => <<"group">>,
            <<"scope_ref">> => <<"995301">>
        }
    ]),
    #{<<"group_file_id">> := null} = ?S:one(
        C,
        <<"SELECT group_file_id FROM attachment WHERE path=$1">>,
        [ObjectKey]
    ),
    FileId.

file_data(FileKey, Name, Url, Bytes) ->
    #{
        group_id => 995301,
        file_id => FileKey,
        file_name => Name,
        file_size => byte_size(Bytes),
        file_type => <<"text/plain">>,
        file_category => <<"document">>,
        file_url => Url,
        file_hash => binary:encode_hex(erlang:md5(Bytes)),
        uploader_id => 995011,
        download_count => 0,
        status => 1,
        created_at => elib_dt:now(),
        updated_at => elib_dt:now()
    }.

run_original_backfill(C) ->
    {ok, Sql} = file:read_file("priv/migrations/00000108_group_attachment_anchor.up.sql"),
    %% Use both original DML statements; latest schema already has the columns.
    {match, [[Link], [Chat]]} = re:run(
        Sql,
        <<"UPDATE public\\.attachment[\\s\\S]*?;">>,
        [global, {capture, first, binary}]
    ),
    ok = ?S:sql_exec(C, Link),
    ok = ?S:sql_exec(C, Chat).

assert_unknown_file(C, Name, Bytes) ->
    {ok, UnknownId} = group_file_repo:insert(
        file_data(
            <<"synthetic-unknown-legacy-key">>, Name, <<"https://invalid.example/legacy">>, Bytes
        )
    ),
    %% No matching attachment metadata: never fall back to the old bare URL.
    ?assertEqual({error, not_found}, group_file_logic:download(UnknownId, 995011)),
    #{<<"n">> := 0} = ?S:one(
        C,
        <<"SELECT count(*) AS n FROM attachment WHERE group_file_id=$1">>,
        [UnknownId]
    ).

gateway_get(Url) ->
    {ok, {{_, Status, _}, _, Body}} = httpc:request(
        get,
        {binary_to_list(Url), []},
        [{autoredirect, false}, {timeout, 10000}],
        [{body_format, binary}]
    ),
    {Status, Body}.

env(Name) ->
    list_to_binary(os:getenv(Name)).

save_proof(Bytes) ->
    Path = filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), "legacy-file-oracle.json"),
    ok = file:write_file(
        Path,
        jsone:encode(#{
            status => <<"PASS">>,
            synthetic_legacy_only => true,
            original_migration_dml => true,
            real_garage_bytes => byte_size(Bytes),
            content_sha256 => eb_asset_content:sha256_hex(Bytes),
            unknown_metadata_denied => true,
            suspended_and_deleted_ticket_denied => true,
            encrypted_legacy_objects_verified => false,
            production_verified => false
        })
    ).
