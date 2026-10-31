%% textguard schema 子集（business-schema-subset.sql）的纯 Erlang 抽取实现。
%% 由 imboy_quality_env:textguard_files 调用；无 PG、无外部解释器依赖。
-module(imboy_quality_textguard_schema).
-export([textguard_schema_subset/2]).

textguard_schema_subset(Repo, Dir) ->
    Migrations = filename:join(Repo, "priv/migrations"),
    Groups = [
        {"00000001_foundation.up.sql", ["\"user\"", "user_friend", "user_denylist"]},
        {"00000003_channel.up.sql", [
            "channel", "channel_admin", "channel_message", "channel_subscription"
        ]},
        {"00000004_social.up.sql", ["moment_post", "moment_post_acl", "moment_timeline"]}
    ],
    Tables = lists:append([
        begin
            Text = schema_text(filename:join(Migrations, Name)),
            lists:append([schema_table(Text, T) || T <- Ts])
        end
     || {Name, Ts} <- Groups
    ]),
    Whole = [
        begin
            Pattern = lists:flatten(io_lib:format("~8..0B_*.up.sql", [N])),
            Name = schema_one(filelib:wildcard(Pattern, Migrations), {migration, N}),
            schema_text(filename:join(Migrations, Name))
        end
     || N <- [22, 34, 39, 59, 63, 72, 197, 230, 233]
    ],
    Scope = schema_text(filename:join(Migrations, "00000077_resource_scope.up.sql")),
    Tail = [
        schema_one(schema_matches(Scope, E), scope)
     || E <- [
            "ALTER TABLE channel ADD COLUMN IF NOT EXISTS scope [^;]+;",
            "ALTER TABLE channel ADD COLUMN IF NOT EXISTS workspace_id [^;]+;",
            "ALTER TABLE channel ADD CONSTRAINT chk_channel_scope_xor CHECK \\([\\s\\S]*?\\);"
        ]
    ],
    Sql = [lists:join("\n\n", Tables ++ Whole ++ Tail), "\n"],
    ok = file:write_file(filename:join(Dir, "business-schema-subset.sql"), Sql, [exclusive]).

schema_table(Text, Table) ->
    %% 固定表名仅含字母、下划线、双引号，均非正则元字符。
    Create = schema_one(
        schema_matches(
            Text,
            "CREATE TABLE public\\." ++ Table ++ "\\s*\\([\\s\\S]*?\n\\);"
        ),
        {table, Table}
    ),
    [Create] ++
        schema_matches(
            Text,
            "ALTER TABLE ONLY public\\." ++ Table ++ "\\s+ADD CONSTRAINT [^;]+;"
        ) ++
        schema_matches(
            Text,
            "CREATE (?:UNIQUE )?INDEX [^;]+ ON public\\." ++ Table ++ "\\s+[^;]+;"
        ).

schema_matches(Text, Expression) ->
    %% Python str 正则的 Unicode 空白集（PCRE UCP 仍缺 U+001C..001F）。
    Space = <<
        "[\\x{9}-\\x{D}\\x{1C}-\\x{20}\\x{85}\\x{A0}\\x{1680}"
        "\\x{2000}-\\x{200A}\\x{2028}\\x{2029}\\x{202F}\\x{205F}\\x{3000}]"
    >>,
    Any = binary:replace(list_to_binary(Expression), <<"[\\s\\S]">>, <<"(?s:.)">>, [global]),
    Pattern = binary:replace(Any, <<"\\s">>, Space, [global]),
    case re:run(Text, Pattern, [unicode, global, {capture, first, binary}]) of
        {match, Matches} -> [B || [B] <- Matches];
        nomatch -> []
    end.

schema_one([Value], _) -> Value;
schema_one(_, What) -> erlang:error({textguard_schema_subset_failed, What}).

schema_text(Path) ->
    {ok, Bytes} = file:read_file(Path),
    Utf8 = unicode:characters_to_binary(Bytes, utf8, utf8),
    true = is_binary(Utf8) orelse erlang:error({invalid_schema_utf8, Path}),
    %% pathlib.read_text 默认 universal newlines：CRLF 与孤立 CR 都变 LF。
    binary:replace(
        binary:replace(Utf8, <<"\r\n">>, <<"\n">>, [global]),
        <<"\r">>,
        <<"\n">>,
        [global]
    ).
