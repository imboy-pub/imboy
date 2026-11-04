%% Runtime receipts are fsynced; unknown files after a host crash are retained.
-module(imboy_quality_config_receipts).
-include_lib("kernel/include/file.hrl").
-export([begin_run/2, begin_run/3, write/2, cleanup/0]).
-define(TABLE, imboy_quality_config_receipts).
-define(MAX_CONFIG, 1048576).

begin_run(Ctrl, RunId) ->
    {ok, Repo} = file:get_cwd(),
    begin_run(Ctrl, RunId, Repo).

%% Explicit Repo is used only by isolated synthetic tests; production uses begin_run/2.
begin_run(Ctrl, RunId0, Repo0) ->
    RunId = unicode:characters_to_binary(RunId0),
    true = byte_size(RunId) > 0 andalso byte_size(RunId) =< 128,
    true = lists:all(fun safe_char/1, binary_to_list(RunId)),
    Repo = filename:absname(Repo0),
    ConfigDir = filename:join(Repo, "config"),
    ConfigIdentity = directory_identity(ConfigDir),
    Evidence = filename:join(filename:absname(Ctrl), "evidence"),
    _ = directory_identity(Evidence),
    Base = filename:join(Evidence, "config-receipts"),
    ok = ensure_dir(Base),
    Table = ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
    try
        Dir = filename:join(Base, binary_to_list(RunId)),
        ok = file:make_dir(Dir),
        ok = file:change_mode(Dir, 8#700),
        JournalIdentity = directory_identity(Dir),
        true = ets:insert(
            Table,
            {state, #{
                journal => Dir,
                journal_identity => JournalIdentity,
                run_id => RunId,
                config_dir => ConfigDir,
                config_identity => ConfigIdentity,
                closing => false
            }}
        ),
        ok
    catch
        Class:Reason:Stack ->
            ets:delete(Table),
            erlang:raise(Class, Reason, Stack)
    end.

write(Path0, Bytes) when is_binary(Bytes), byte_size(Bytes) =< ?MAX_CONFIG ->
    S = state(),
    false = maps:get(closing, S),
    ok = verify_journal(S),
    Path = filename:absname(Path0),
    ok = valid_path(Path, S),
    true = ets:insert_new(?TABLE, {{path, Path}, pending}),
    false = maps:get(closing, state()),
    Stem = receipt_stem(Path, S),
    RunId = maps:get(run_id, S),
    Intent = #{kind => intent, run_id => RunId, path => unicode:characters_to_binary(Path)},
    ok = write_exclusive(Stem ++ ".intent", term_to_binary(Intent)),
    case publish_config(Stem, Path, Bytes) of
        ok -> ok;
        {error, Why} -> error({quality_config_write_failed, Path, Why})
    end,
    Meta = config_metadata(Path),
    true = maps:get(sha256, Meta) =:= crypto:hash(sha256, Bytes),
    Done = Meta#{kind => done, run_id => RunId, path => unicode:characters_to_binary(Path)},
    ok = write_exclusive(Stem ++ ".done", term_to_binary(Done)),
    true = ets:insert(?TABLE, {{path, Path}, done}),
    ok.

publish_config(Stem, Path, Bytes) ->
    Temp = Stem ++ ".configtmp",
    case write_exclusive(Temp, Bytes) of
        ok ->
            case file:make_link(Temp, Path) of
                ok -> file:delete(Temp);
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

cleanup() ->
    try
        S0 = state(),
        S = S0#{closing => true},
        true = ets:insert(?TABLE, {state, S}),
        Errors = cleanup_journal(S),
        case Errors of
            [] -> ok;
            _ -> {error, Errors}
        end
    catch
        Class:Reason -> {error, {config_receipts_cleanup_failed, Class, Reason}}
    end.

cleanup_journal(S) ->
    Dir = maps:get(journal, S),
    ok = verify_journal(S),
    {ok, Names} = file:list_dir(Dir),
    Intents = lists:sort([N || N <- Names, filename:extension(N) =:= ".intent"]),
    DoneNames = [filename:rootname(N) || N <- Names, filename:extension(N) =:= ".done"],
    IntentNames = [filename:rootname(N) || N <- Intents],
    RemovedNames = [filename:rootname(N) || N <- Names, filename:extension(N) =:= ".removed"],
    Orphans = [
        {orphan_receipt, N}
     || N <- DoneNames ++ RemovedNames, not lists:member(N, IntentNames)
    ],
    Pending = [{pending_registration, Path} || {{path, Path}, pending} <- ets:tab2list(?TABLE)],
    Unknown = [
        {unknown_journal_entry, N}
     || N <- Names,
        not lists:member(filename:extension(N), [".intent", ".done", ".removed"])
    ],
    Pending ++ Orphans ++ Unknown ++
        lists:append([cleanup_one(filename:join(Dir, N), S) || N <- Intents]).

cleanup_one(IntentPath, S) ->
    try
        Intent = read_term(IntentPath),
        #{kind := intent, run_id := RunId, path := PathBin} = Intent,
        RunId = maps:get(run_id, S),
        Path = unicode:characters_to_list(PathBin),
        ok = valid_path(Path, S),
        Stem = filename:rootname(IntentPath),
        Stem = receipt_stem(Path, S),
        DonePath = Stem ++ ".done",
        case ets:lookup(?TABLE, {path, Path}) of
            [{{path, Path}, pending}] ->
                [{pending_registration, Path}];
            _ ->
                case file:read_link_info(DonePath) of
                    {error, enoent} -> [{unconfirmed_intent, Path}];
                    {ok, _} -> cleanup_done(Path, DonePath, Stem ++ ".removed", S)
                end
        end
    catch
        Class:Reason -> [{invalid_receipt, IntentPath, Class, Reason}]
    end.

cleanup_done(Path, DonePath, RemovedPath, S) ->
    Done = read_term(DonePath),
    #{kind := done, run_id := RunId, path := PathBin} = Done,
    RunId = maps:get(run_id, S),
    PathBin = unicode:characters_to_binary(Path),
    case file:read_link_info(RemovedPath) of
        {ok, _} ->
            Done = read_term(RemovedPath),
            case file:read_link_info(Path) of
                {error, enoent} -> [];
                _ -> [{removed_path_reappeared, Path}]
            end;
        {error, enoent} ->
            Actual = config_metadata(Path),
            Expected = maps:without([kind, run_id, path], Done),
            case Actual =:= Expected of
                false ->
                    [{config_identity_changed, Path}];
                true ->
                    case file:delete(Path) of
                        ok ->
                            case write_exclusive(RemovedPath, term_to_binary(Done)) of
                                ok -> [];
                                {error, Why} -> [{deletion_receipt_failed, Path, Why}]
                            end;
                        {error, Why} ->
                            [{config_delete_failed, Path, Why}]
                    end
            end
    end.

valid_path(Path, S) ->
    ConfigDir = maps:get(config_dir, S),
    ConfigDir = filename:dirname(Path),
    true = lists:prefix("sys.quality.", filename:basename(Path)),
    Identity = maps:get(config_identity, S),
    Identity = directory_identity(ConfigDir),
    ok.

config_metadata(Path) ->
    {ok, Before = #file_info{type = regular, links = 1, size = Size}} = file:read_link_info(Path),
    true = Size =< ?MAX_CONFIG,
    {ok, F} = file:open(Path, [read, binary, raw]),
    Bytes =
        try
            case file:read(F, ?MAX_CONFIG + 1) of
                eof -> <<>>;
                {ok, B} when byte_size(B) =< ?MAX_CONFIG -> B
            end
        after
            file:close(F)
        end,
    {ok, After = #file_info{type = regular, links = 1}} = file:read_link_info(Path),
    Identity = file_id(Before),
    Identity = file_id(After),
    true = byte_size(Bytes) =:= Before#file_info.size,
    #{identity => Identity, sha256 => crypto:hash(sha256, Bytes)}.

file_id(#file_info{major_device = Major, minor_device = Minor, inode = Inode}) when
    is_integer(Inode), Inode > 0
->
    {Major, Minor, Inode}.

directory_identity(Dir) ->
    {ok, I = #file_info{type = directory}} = file:read_link_info(Dir),
    file_id(I).

verify_journal(S) ->
    Identity = maps:get(journal_identity, S),
    Identity = directory_identity(maps:get(journal, S)),
    ok.

ensure_dir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            _ = directory_identity(Dir),
            ok;
        {error, Why} ->
            error({journal_directory_failed, Dir, Why})
    end.

state() ->
    [{state, S}] = ets:lookup(?TABLE, state),
    S.

receipt_stem(Path, S) ->
    Hex = string:lowercase(
        binary:encode_hex(crypto:hash(sha256, unicode:characters_to_binary(Path)))
    ),
    filename:join(maps:get(journal, S), binary_to_list(Hex)).

read_term(Path) ->
    {ok, #file_info{type = regular, links = 1, size = Size}} = file:read_link_info(Path),
    true = Size =< 65536,
    {ok, Bytes} = file:read_file(Path),
    binary_to_term(Bytes, [safe]).

write_exclusive(Path, Bytes) ->
    case file:open(Path, [write, binary, raw, exclusive]) of
        {ok, F} ->
            try
                ok = file:change_mode(Path, 8#600),
                ok = file:write(F, Bytes),
                file:sync(F)
            after
                file:close(F)
            end;
        {error, Why} ->
            {error, Why}
    end.

safe_char(C) ->
    (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse C =:= $- orelse C =:= $_.
