%% 无 PG、无外部解释器；Python 差分基准只在仓外验证时运行。
-module(imboy_quality_textguard_schema_tests).
-include_lib("eunit/include/eunit.hrl").

native_only_test() ->
    lists:foreach(
        fun(P) ->
            {ok, Source} = file:read_file(filename:join(repo(), P)),
            ?assertEqual(nomatch, binary:match(Source, <<"python3 -c">>))
        end,
        [
            "tools/imboy_quality/imboy_quality_env.erl",
            "tools/imboy_quality/imboy_quality_textguard_schema.erl"
        ]
    ).

schema_bytes_test_() ->
    [
        {Name, fun() ->
            with_fixture(fun(Root, Out, Expected) ->
                normalize_inputs(Root, Ending),
                ok = imboy_quality_textguard_schema:textguard_schema_subset(Root, Out),
                ?assertEqual({ok, Expected}, file:read_file(schema(Out))),
                ?assertEqual({ok, ["business-schema-subset.sql"]}, file:list_dir(Out))
            end)
        end}
     || {Name, Ending} <- [
            {"LF Unicode and ordering", <<"\n">>},
            {"CRLF normalization", <<"\r\n">>},
            {"CR normalization", <<"\r">>}
        ]
    ].

reject_test_() ->
    [
        {atom_to_list(Kind), fun() ->
            with_fixture(fun(Root, Out, _) ->
                mutate(Root, Kind),
                ?assertException(
                    error, _, imboy_quality_textguard_schema:textguard_schema_subset(Root, Out)
                ),
                ?assertNot(filelib:is_file(schema(Out)))
            end)
        end}
     || Kind <- [
            missing_file,
            missing_table,
            duplicate_table,
            missing_migration,
            duplicate_migration,
            missing_scope,
            duplicate_scope,
            invalid_utf8
        ]
    ].

literal_parent_path_test() ->
    with_fixture(fun(Root, _, _) ->
        Literal = filename:join(Root, unicode:characters_to_list(<<"空 格[目录]*?"/utf8>>)),
        {Out, Expected} = fixture(Literal),
        ok = imboy_quality_textguard_schema:textguard_schema_subset(Literal, Out),
        ?assertEqual({ok, Expected}, file:read_file(schema(Out)))
    end).

exclusive_test() ->
    with_fixture(fun(Root, Out, _) ->
        ok = file:write_file(schema(Out), <<"owned sentinel">>),
        ?assertException(
            error, _, imboy_quality_textguard_schema:textguard_schema_subset(Root, Out)
        ),
        ?assertEqual({ok, <<"owned sentinel">>}, file:read_file(schema(Out)))
    end).

with_fixture(Fun) ->
    Root = filename:join(
        "/private/tmp",
        "imboy-schema-unit-" ++ os:getpid() ++ "-" ++
            integer_to_list(erlang:unique_integer([positive, monotonic]))
    ),
    try
        {Out, Expected} = fixture(Root),
        Fun(Root, Out, Expected)
    after
        file:del_dir_r(Root)
    end.

fixture(Root) ->
    Out = filename:join(Root, "out"),
    ok = filelib:ensure_dir(filename:join(Out, "x")),
    Groups = [
        {"00000001_foundation.up.sql", ["\"user\"", "user_friend", "user_denylist"]},
        {"00000003_channel.up.sql", [
            "channel", "channel_admin", "channel_message", "channel_subscription"
        ]},
        {"00000004_social.up.sql", ["moment_post", "moment_post_acl", "moment_timeline"]}
    ],
    Tables = lists:append([
        begin
            Parts = [table_sql(T) || T <- Ts],
            write(Root, Name, lists:join("\n", lists:reverse(Parts))),
            lists:append(Parts)
        end
     || {Name, Ts} <- Groups
    ]),
    Whole = [
        begin
            Sql = unicode:characters_to_binary(["-- 迁移 ", integer_to_list(N), "\nSELECT '雪';\n"]),
            write(Root, migration(N), Sql),
            Sql
        end
     || N <- [22, 34, 39, 59, 63, 72, 197, 230, 233]
    ],
    Scope = [
        <<"ALTER TABLE channel ADD COLUMN IF NOT EXISTS scope text;">>,
        <<"ALTER TABLE channel ADD COLUMN IF NOT EXISTS workspace_id bigint;">>,
        <<"ALTER TABLE channel ADD CONSTRAINT chk_channel_scope_xor CHECK (\ntrue);">>
    ],
    write(Root, "00000077_resource_scope.up.sql", lists:join("\n", lists:reverse(Scope))),
    {Out, iolist_to_binary([lists:join("\n\n", Tables ++ Whole ++ Scope), "\n"])}.

table_sql(T) ->
    %% Python Unicode \s 包括 NBSP 和 U+001C；不能退化成 ASCII 空白匹配。
    [
        unicode:characters_to_binary([
            "CREATE TABLE public.", T, [160, 28], "(\n value text DEFAULT '中文'\n);"
        ]),
        iolist_to_binary(["ALTER TABLE ONLY public.", T, "\n ADD CONSTRAINT x CHECK (true);"]),
        iolist_to_binary(["CREATE INDEX i ON public.", T, " (value);"]),
        iolist_to_binary(["CREATE UNIQUE INDEX j ON public.", T, " (value);"])
    ].

normalize_inputs(Root, Ending) ->
    lists:foreach(
        fun(Path) ->
            {ok, B} = file:read_file(Path),
            ok = file:write_file(Path, binary:replace(B, <<"\n">>, Ending, [global]))
        end,
        filelib:wildcard(filename:join([Root, "priv/migrations", "*.sql"]))
    ).

mutate(Root, missing_file) ->
    file:delete(path(Root, "00000001_foundation.up.sql"));
mutate(Root, missing_table) ->
    write(Root, "00000001_foundation.up.sql", <<>>);
mutate(Root, duplicate_table) ->
    append(Root, "00000001_foundation.up.sql", hd(table_sql("\"user\"")));
mutate(Root, missing_migration) ->
    file:delete(path(Root, migration(22)));
mutate(Root, duplicate_migration) ->
    write(Root, "00000022_duplicate.up.sql", <<"SELECT 1;">>);
mutate(Root, missing_scope) ->
    write(Root, "00000077_resource_scope.up.sql", <<>>);
mutate(Root, duplicate_scope) ->
    append(
        Root,
        "00000077_resource_scope.up.sql",
        <<"ALTER TABLE channel ADD COLUMN IF NOT EXISTS scope text;">>
    );
mutate(Root, invalid_utf8) ->
    append(Root, migration(22), <<255>>).

append(Root, Name, B) -> file:write_file(path(Root, Name), ["\n", B], [append]).
write(Root, Name, B) ->
    Path = path(Root, Name),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, B).
path(Root, Name) -> filename:join([Root, "priv/migrations", Name]).
migration(N) -> lists:flatten(io_lib:format("~8..0B_fixture.up.sql", [N])).
schema(Out) -> filename:join(Out, "business-schema-subset.sql").
repo() -> filename:dirname(filename:dirname(filename:dirname(filename:absname(?FILE)))).
