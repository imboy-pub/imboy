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
            [
                {atom_to_list(Mode), fun() -> verify(C, Mode) end}
             || Mode <- [success, attachment_failure, file_failure, revoked_during_upload]
            ],
            [verbose]
        )
    after
        meck:unload(),
        epgsql:close(C)
    end.

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
        "CREATE TABLE group_file(id bigint PRIMARY KEY,group_id bigint,file_id text,file_name text,"
        "file_size bigint CHECK(file_size<>4),file_type text,file_category text,file_url text,"
        "file_hash text,uploader_id bigint,download_count int,status int,"
        "created_at timestamptz,updated_at timestamptz);"
        "CREATE TABLE attachment(id bigint PRIMARY KEY,file_hash256 text,mime_type text,ext text,"
        "name text,path text UNIQUE,url text,size bigint CHECK(size<>2),info jsonb,"
        "referer_time int,last_referer_user_id bigint,last_referer_at timestamptz,"
        "creator_user_id bigint,scope text,scope_ref text,cipher text,anchor_msg_id text,"
        "group_file_id bigint,created_at timestamptz,updated_at timestamptz,status int);"
    >>).

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
    meck:new(elib_tsid, [non_strict, no_link]),
    meck:expect(elib_tsid, generate, fun
        (group_file) -> 101;
        (attachment) -> 201
    end).

verify(C, Mode) ->
    sql(C, <<
        "DELETE FROM attachment; DELETE FROM group_file;"
        "UPDATE organization_member SET status='active'"
    >>),
    meck:expect(elib_oss, upload, fun(_, _, _) ->
        case Mode of
            revoked_during_upload ->
                sql(
                    C,
                    <<"UPDATE organization_member SET status='suspended'">>
                );
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
