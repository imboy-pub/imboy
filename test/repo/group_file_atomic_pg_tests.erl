-module(group_file_atomic_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

run(Socket) ->
    {ok, C} = epgsql:connect(#{
        host => {local, Socket},
        port => 0,
        username => "departure_test",
        database => "postgres",
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }),
    try
        schema(C),
        mocks(C),
        eunit:test(
            [{"file operations recheck current scope", fun() -> verify_scope(C) end}] ++
                [
                    {atom_to_list(Mode), fun() -> verify(C, Mode) end}
                 || Mode <- [
                        success,
                        attachment_failure,
                        file_failure,
                        revoked_during_upload,
                        revoked_group_during_upload
                    ]
                ] ++ [{"binding index reversible", fun() -> verify_index(C) end}],
            [verbose]
        )
    after
        meck:unload(),
        epgsql:close(C)
    end.

verify_scope(C) ->
    sql(C, <<
        "DELETE FROM attachment; DELETE FROM group_file;"
        "UPDATE organization_member SET status='active';"
        "UPDATE group_member SET status=1;"
    >>),
    meck:expect(elib_oss, upload, fun(_, _, _) ->
        {ok, <<"https://storage.example.com/test-file/a.txt">>, <<"test-file">>}
    end),
    ?assertEqual(
        {ok, <<"test-file">>},
        group_file_ds:upload_file(11, 1, <<"a.txt">>, <<0, 1, 2>>, <<"text/plain">>)
    ),
    ?assertMatch(
        {ok, [#{<<"object_key">> := <<"test-file/a.txt">>}]},
        group_file_ds:list_files(11, 1, 1, 10)
    ),
    ?assertEqual({ok, <<"https://storage.example.com/signed">>}, group_file_logic:download(101, 1)),
    ?assertMatch(
        {ok, [#{<<"object_key">> := <<"test-file/a.txt">>}]},
        group_file_ds:search_files(11, <<"a">>, 1, 10, 1)
    ),
    ?assertMatch(
        {ok, [#{<<"object_key">> := <<"test-file/a.txt">>}]},
        group_file_repo:list_by_category(11, <<"document">>, 1, 10)
    ),
    lists:foreach(
        fun(Change) ->
            sql(C, Change),
            ?assertMatch(
                {ok, [#{<<"object_key">> := null}]},
                group_file_ds:list_files(11, 1, 1, 10)
            ),
            ?assertEqual({error, not_found}, group_file_logic:download(101, 1)),
            sql(C, <<"UPDATE attachment SET scope='group',scope_ref='11',status=1">>)
        end,
        [
            <<"UPDATE attachment SET scope='public'">>,
            <<"UPDATE attachment SET scope_ref='22'">>,
            <<"UPDATE attachment SET status=-1">>
        ]
    ),
    sql(C, <<"UPDATE group_member_generation SET end_seq=10">>),
    ?assertEqual({error, forbidden}, group_file_logic:download(101, 1)),
    sql(C, <<"UPDATE group_member_generation SET end_seq=NULL">>),
    ?assertMatch({ok, [_]}, group_file_logic:get_categories(<<"11">>, 1)),
    lists:foreach(
        fun(Revoke) ->
            sql(C, Revoke),
            ?assertEqual({error, not_member}, group_file_ds:list_files(11, 1, 1, 10)),
            ?assertEqual({error, not_member}, group_file_ds:search_files(11, <<"a">>, 1, 10, 1)),
            ?assertEqual({error, not_member}, group_file_logic:get_categories(<<"11">>, 1)),
            ?assertEqual({error, not_member}, group_file_ds:download_file(101, 1)),
            ?assertEqual({error, not_member}, group_file_ds:delete_file(101, 1)),
            ?assertEqual(1, count(C, <<"group_file WHERE status=1">>)),
            sql(C, <<
                "UPDATE organization_member SET status='active';"
                "UPDATE workspace_member SET status='active'; UPDATE group_member SET status=1"
            >>)
        end,
        [
            <<"UPDATE organization_member SET status='suspended'">>,
            <<"UPDATE workspace_member SET status='removed'">>,
            <<"UPDATE group_member SET status=0">>
        ]
    ),
    sql(C, <<"UPDATE workspace SET status='archived'">>),
    ?assertMatch({ok, [_]}, group_file_ds:list_files(11, 1, 1, 10)),
    ?assertMatch({error, {980, _}}, group_file_ds:delete_file(101, 1)),
    sql(C, <<"UPDATE workspace SET status='active'">>),
    ?assertEqual(ok, group_file_ds:delete_file(101, 1)),
    ?assertEqual({ok, []}, group_file_ds:list_files(11, 1, 1, 10)),
    ?assertEqual({error, not_found}, group_file_ds:download_file(101, 1)).

schema(C) ->
    sql(C, <<
        "CREATE TABLE organization(id bigint PRIMARY KEY,status text);"
        "CREATE TABLE organization_member(organization_id bigint,user_id bigint,status text,role text);"
        "CREATE TABLE workspace(id bigint PRIMARY KEY,organization_id bigint,status text);"
        "CREATE TABLE workspace_member(workspace_id bigint,user_id bigint,status text);"
        "CREATE TABLE \"group\"(id bigint PRIMARY KEY,status int,scope text,workspace_id bigint);"
        "INSERT INTO organization VALUES(10,'active');"
        "INSERT INTO organization_member VALUES(10,1,'active','member');"
        "INSERT INTO workspace VALUES(100,10,'active');"
        "INSERT INTO workspace_member VALUES(100,1,'active');"
        "INSERT INTO \"group\" VALUES(11,1,'workspace',100);"
        "CREATE TABLE group_member(group_id bigint,user_id bigint,status int);"
        "INSERT INTO group_member VALUES(11,1,1);"
        "CREATE TABLE group_member_generation(group_id bigint,user_id bigint,start_seq bigint,end_seq bigint);"
        "INSERT INTO group_member_generation VALUES(11,1,1,NULL);"
        "CREATE TABLE group_file(id bigint PRIMARY KEY,group_id bigint,file_id text,file_name text,"
        "file_size bigint CHECK(file_size<>4),file_type text,file_category text,file_url text,"
        "file_hash text,uploader_id bigint,download_count int,status int,"
        "created_at timestamptz,updated_at timestamptz);"
        "CREATE TABLE attachment(id bigint PRIMARY KEY,file_hash256 text,mime_type text,ext text,"
        "name text,path text UNIQUE,url text,size bigint CHECK(size<>2),info jsonb,"
        "referer_time int,last_referer_user_id bigint,last_referer_at timestamptz,"
        "creator_user_id bigint,scope text,scope_ref text,cipher text,anchor_msg_id text,"
        "group_file_id bigint,anchor_conv_seq bigint,created_at timestamptz,updated_at timestamptz,status int);"
    >>),
    migration(C, "up").

verify_index(C) ->
    Before = count(C, <<"attachment">>),
    sql(C, <<"SET enable_seqscan=off">>),
    {ok, _, Rows} = epgsql:equery(
        C,
        <<
            "EXPLAIN SELECT path FROM attachment WHERE group_file_id=$1 AND scope='group' "
            "AND scope_ref=$2::bigint::text AND status>=0 ORDER BY id LIMIT 1"
        >>,
        [101, 11]
    ),
    Plan = iolist_to_binary([Line || {Line} <- Rows]),
    ?assertNotEqual(nomatch, binary:match(Plan, <<"idx_attachment_group_file_binding">>)),
    sql(C, <<"RESET enable_seqscan">>),
    migration(C, "down"),
    {ok, _, [{null}]} = epgsql:equery(
        C,
        <<"SELECT to_regclass('public.idx_attachment_group_file_binding')">>,
        []
    ),
    ?assertEqual(Before, count(C, <<"attachment">>)),
    migration(C, "up"),
    migration(C, "up"),
    ?assertEqual(Before, count(C, <<"attachment">>)).

%% 从仓库根运行，以执行实际迁移文件而非复制的 DDL。
migration(C, Direction) ->
    Name = "priv/migrations/00000157_group_file_attachment_binding_index." ++ Direction ++ ".sql",
    {ok, Sql} = file:read_file(Name),
    sql(C, Sql).

mocks(C) ->
    meck:new(config_ds, [non_strict, no_link]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    meck:new(pooler, [non_strict, no_link]),
    meck:expect(pooler, take_member, fun(pgsql) -> C end),
    meck:expect(pooler, return_member, fun(pgsql, C0) when C0 =:= C -> ok end),
    meck:expect(pooler, return_member, fun(pgsql, C0, _) when C0 =:= C -> ok end),
    meck:new(group_ds, [non_strict, no_link]),
    meck:expect(group_ds, is_member, fun(_, _) -> true end),
    meck:new(elib_oss, [non_strict, no_link]),
    meck:expect(elib_oss, validate_file_type, fun(_) -> true end),
    meck:expect(elib_oss, get_file_category, fun(_) -> document end),
    meck:expect(elib_oss, get_bucket, fun(<<"group">>) -> <<"private">> end),
    meck:expect(elib_oss, presign_get_for_key, fun(<<"private">>, <<"test-file/a.txt">>, 600) ->
        <<"https://storage.example.com/signed">>
    end),
    meck:new(workspace_guard, [passthrough, no_link]),
    meck:expect(workspace_guard, write_tx_or_skip, fun(_, _) -> ok end),
    meck:new(elib_tsid, [non_strict, no_link]),
    meck:expect(elib_tsid, generate, fun
        (group_file) -> 101;
        (attachment) -> 201
    end).

verify(C, Mode) ->
    sql(C, <<
        "DELETE FROM attachment; DELETE FROM group_file;"
        "UPDATE organization_member SET status='active'; UPDATE group_member SET status=1"
    >>),
    meck:expect(elib_oss, upload, fun(_, _, _) ->
        case Mode of
            revoked_during_upload ->
                sql(
                    C,
                    <<"UPDATE organization_member SET status='suspended'">>
                );
            revoked_group_during_upload ->
                sql(C, <<"UPDATE group_member SET status=0">>);
            _ ->
                ok
        end,
        {ok, <<"https://storage.example.com/test-file/a.txt">>, <<"test-file">>}
    end),
    Data =
        case Mode of
            attachment_failure -> <<0, 1>>;
            file_failure -> <<0, 1, 2, 3>>;
            _ -> <<0, 1, 2>>
        end,
    Result = group_file_ds:upload_file(11, 1, <<"a.txt">>, Data, <<"text/plain">>),
    case Mode of
        success ->
            ?assertEqual({ok, <<"test-file">>}, Result),
            ?assertEqual(1, count(C, <<"group_file">>)),
            ?assertEqual(1, count(C, <<"attachment">>)),
            {ok, _, [{101, 1, <<"group">>, <<"11">>, <<"test-file/a.txt">>}]} = epgsql:equery(
                C,
                <<"SELECT group_file_id,creator_user_id,scope,scope_ref,path FROM attachment">>,
                []
            );
        attachment_failure ->
            ?assertMatch({error, {attachment_save_failed, _}}, Result),
            assert_empty(C);
        file_failure ->
            ?assertMatch({error, _}, Result),
            assert_empty(C);
        revoked_during_upload ->
            ?assertEqual({error, forbidden}, Result),
            assert_empty(C);
        revoked_group_during_upload ->
            ?assertEqual({error, forbidden}, Result),
            assert_empty(C)
    end.

assert_empty(C) ->
    ?assertEqual(0, count(C, <<"group_file">>)),
    ?assertEqual(0, count(C, <<"attachment">>)).

count(C, Table) ->
    {ok, _, [{N}]} = epgsql:equery(C, <<"SELECT count(*) FROM ", Table/binary>>, []),
    N.

sql(C, Q) ->
    R = epgsql:squery(C, Q),
    lists:foreach(
        fun(Item) -> ?assertNotMatch({error, _}, Item) end,
        case is_list(R) of
            true -> R;
            false -> [R]
        end
    ).
