-module(imboy_quality_env_cleanup).
-include_lib("kernel/include/file.hrl").
-export([
    reserve_dirs/2,
    cleanup_owned/1,
    garage_resource_root/1,
    verified_volume_root/3,
    verify_postgres/3,
    verified_pg_binding/5,
    cleanup_result/3,
    prepare_cleanup/6,
    verified_reconnect/3,
    archive_garage/5,
    garage_cleanup_ok/1,
    copy_exclusive/2,
    journal_open/2,
    journal_close/1,
    with_transaction/4,
    transaction_intent/2,
    transaction_confirm/3,
    release/1,
    release_with/4,
    release_with/5,
    release_once/1,
    finish_postgres_cleanup/2
]).

reserve_dirs(Ctrl, Pid) ->
    true =
        is_list(Pid) andalso Pid =/= [] andalso
            lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Pid),
    Runtime = filename:join(Ctrl, "runtime"),
    ok = directory(Runtime),
    reserve_dirs(Runtime, Pid, ["attv5", "hird", "textguard", "talk_owned"], []).

reserve_dirs(_Runtime, _Pid, [], Acc) ->
    lists:reverse(Acc);
reserve_dirs(Runtime, Pid, [Name | Rest], Acc) ->
    try
        Parent = filename:join(Runtime, Name),
        ok = directory(Parent),
        Dir = filename:join(Parent, Pid),
        ok = file:make_dir(Dir),
        Info = identity(Dir),
        Token = crypto:strong_rand_bytes(32),
        Marker = filename:join(Dir, ".imboy-quality-owner"),
        {ok, F} = file:open(Marker, [write, binary, raw, exclusive]),
        try
            ok = file:write(F, Token),
            ok = file:sync(F)
        after
            file:close(F)
        end,
        ok = file:change_mode(Marker, 8#600),
        Entry = #{path => Dir, identity => Info, token => Token},
        reserve_dirs(Runtime, Pid, Rest, [Entry | Acc])
    catch
        Class:Reason:Stack ->
            _ = [cleanup_owned(E) || E <- Acc],
            erlang:raise(Class, Reason, Stack)
    end.

directory(Path) ->
    case file:make_dir(Path) of
        ok ->
            ok;
        {error, eexist} ->
            {ok, #file_info{type = directory}} = file:read_link_info(Path),
            ok;
        {error, Reason} ->
            error({directory_create_failed, Path, Reason})
    end.

identity(Path) ->
    {ok, #file_info{type = directory, inode = Inode, major_device = Major, minor_device = Minor}} =
        file:read_link_info(Path),
    true = is_integer(Inode) andalso Inode > 0,
    {Major, Minor, Inode}.

cleanup_owned(#{path := Path, identity := Identity, token := Token}) ->
    try
        Identity = identity(Path),
        Marker = filename:join(Path, ".imboy-quality-owner"),
        {ok, #file_info{type = regular, links = 1}} = file:read_link_info(Marker),
        {ok, Token} = file:read_file(Marker),
        Identity = identity(Path),
        file:del_dir_r(Path)
    catch
        _:_ -> {error, ownership_not_verified}
    end;
cleanup_owned(_) ->
    {error, ownership_not_verified}.

garage_resource_root(Ctrl) ->
    {ok, Bytes} = file:read_file(filename:join(Ctrl, "STATE.json")),
    {ok, Hash} = file:read_file(filename:join(Ctrl, "STATE.json.sha256")),
    true = string:lowercase(binary:encode_hex(crypto:hash(sha256, Bytes))) =:= string:trim(Hash),
    State = json:decode(Bytes),
    case imboy_quality_backend:is_state(State) of
        true -> imboy_quality_backend:resource_root(filename:absname(Ctrl), State);
        false -> legacy_resource_root(Ctrl, State)
    end.

legacy_resource_root(Ctrl, State) ->
    Volumes = [
        V
     || #{<<"kind">> := <<"ISOLATED_APFS_VOLUME">>} = V <-
            maps:get(<<"owned_resources">>, State)
    ],
    [Volume] = Volumes,
    Root = binary_to_list(maps:get(<<"path">>, Volume)),
    CtrlAbs = filename:absname(Ctrl),
    CtrlAbs = filename:dirname(Root),
    Marker = filename:join(Root, ".imboy-quality-owned"),
    Marker = binary_to_list(maps:get(<<"ownership_marker">>, Volume)),
    RootInfo = directory_stat(Root),
    ParentInfo = directory_stat(CtrlAbs),
    {ok, #file_info{type = regular, links = 1}} = file:read_link_info(Marker),
    {ok, MarkerBytes} = file:read_file(Marker),
    MarkerHash = string:lowercase(binary:encode_hex(crypto:hash(sha256, MarkerBytes))),
    {ok, Root} = verified_volume_root(Volume, CtrlAbs, {MarkerHash, RootInfo, ParentInfo}),
    Root.

directory_stat(Path) ->
    {ok, #file_info{type = directory, inode = Inode, major_device = Major, minor_device = Minor}} =
        file:read_link_info(Path),
    #{<<"major_device">> => Major, <<"minor_device">> => Minor, <<"inode">> => Inode}.

verified_volume_root(Volume, Ctrl, {MarkerHash, RootInfo, ParentInfo}) ->
    try
        <<"ISOLATED_APFS_VOLUME">> = maps:get(<<"kind">>, Volume),
        true = is_binary(MarkerHash) andalso byte_size(MarkerHash) =:= 64,
        MarkerHash = maps:get(<<"ownership_marker_sha256">>, Volume),
        Root = binary_to_list(maps:get(<<"path">>, Volume)),
        Image = binary_to_list(maps:get(<<"image">>, Volume)),
        Ctrl = filename:dirname(Root),
        true = lists:prefix("isolated-resources-", filename:basename(Root)),
        Image = Root ++ ".sparsebundle",
        Marker = filename:join(Root, ".imboy-quality-owned"),
        Marker = binary_to_list(maps:get(<<"ownership_marker">>, Volume)),
        #{<<"root">> := RootInfo, <<"parent">> := ParentInfo} =
            maps:get(<<"filesystem_identity">>, Volume),
        RootDevice = directory_device(RootInfo),
        ParentDevice = directory_device(ParentInfo),
        true = RootDevice =/= ParentDevice,
        {ok, Root}
    catch
        _:_ -> {error, volume_identity_not_verified}
    end.

directory_device(#{<<"major_device">> := Major, <<"minor_device">> := Minor, <<"inode">> := Inode}) when
    is_integer(Major),
    Major >= 0,
    is_integer(Minor),
    Minor >= 0,
    is_integer(Inode),
    Inode > 0
->
    {Major, Minor}.

%% A registered PID without its live port owner cannot establish process identity.
verify_postgres(_Ctrl, _Args, _Root) ->
    imboy_quality_pg_owner:verify(_Ctrl, _Args, _Root).

verified_pg_binding(PG, Args, Root, Identity, {Socket, Executable}) ->
    try
        <<"ISOLATED_POSTGRESQL">> = maps:get(<<"kind">>, PG),
        <<"RUNNING">> = maps:get(<<"status">>, PG),
        Pid = maps:get(<<"pid">>, PG),
        true = is_integer(Pid) andalso Pid > 0,
        <<"127.0.0.1">> = maps:get(<<"host">>, PG),
        <<"127.0.0.1">> = iolist_to_binary(maps:get(host, Args)),
        Port = maps:get(<<"port">>, PG),
        true = is_integer(Port) andalso Port > 0 andalso Port < 65536,
        Port = maps:get(port, Args),
        Data = filename:join(Root, "pgdata"),
        Data = binary_to_list(maps:get(<<"data_dir">>, PG)),
        Exec = filename:join([Root, "postgres-toolchain", "bin", "postgres"]),
        Exec = binary_to_list(maps:get(<<"executable">>, PG)),
        Start = maps:get(<<"start_identity">>, PG),
        true = string:trim(Identity) =:= string:trim(Start),
        Words = string:lexemes(binary_to_list(Identity), " \t\r\n"),
        true = argument_pair("-D", Data, Words),
        true = lists:member(
            list_to_binary("n" ++ Exec), binary:split(Executable, <<"\n">>, [global])
        ),
        true = lists:member(
            list_to_binary("n127.0.0.1:" ++ integer_to_list(Port)),
            binary:split(Socket, <<"\n">>, [global])
        ),
        ok
    catch
        _:_ -> {error, postgres_identity_not_verified}
    end.

argument_pair(Key, Value, [Key, Value | _]) -> true;
argument_pair(Key, Value, [_ | Rest]) -> argument_pair(Key, Value, Rest);
argument_pair(_, _, []) -> false.

cleanup_result(DbResults, GarageResult, RuntimeResults) ->
    Failed = [
        iolist_to_binary(io_lib:format("~s: ~0p", [N, R]))
     || {N, R} <- DbResults, R =/= ok
    ],
    #{
        <<"clones_dropped">> => [list_to_binary(N) || {N, ok} <- DbResults],
        <<"clone_drop_failed">> => Failed,
        <<"garage">> => fmt(GarageResult),
        <<"runtime_cleanup_failed">> => [fmt({D, R}) || {D, R} <- RuntimeResults, R =/= ok],
        <<"ok">> => Failed =:= [] andalso garage_cleanup_ok(GarageResult) andalso
            lists:all(fun({_, R}) -> R =:= ok end, RuntimeResults)
    }.

garage_cleanup_ok({{ok, _}, #{exit := Exit, cleanup := ok}}) -> Exit =:= 0 orelse Exit =:= 143;
garage_cleanup_ok(_) -> false.

fmt(Term) -> iolist_to_binary(io_lib:format("~0p", [Term])).

%% A private fsynced journal makes ambiguous post-crash resources visible without deleting them.
journal_open(Ctrl, PG) ->
    Evidence = filename:join(Ctrl, "evidence"),
    {ok, #file_info{type = directory}} = file:read_link_info(Evidence),
    Id = binary_to_list(string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(16)))),
    Path = filename:join(Evidence, "prepare-resources-" ++ Id ++ ".jsonl"),
    {ok, F} = file:open(Path, [write, binary, raw, exclusive]),
    try
        ok = file:change_mode(Path, 8#600),
        J = #{
            fd => F,
            path => Path,
            pg => maps:with(
                [
                    <<"pid">>,
                    <<"port">>,
                    <<"host">>,
                    <<"data_dir">>,
                    <<"executable">>,
                    <<"start_identity">>,
                    <<"system_identifier">>
                ],
                PG
            )
        },
        journal_append(J, #{event => <<"begin">>, pg => maps:get(pg, J)}),
        J
    catch
        Class:Reason:Stack ->
            _ = file:close(F),
            erlang:raise(Class, Reason, Stack)
    end.

journal_close(#{fd := F}) -> file:close(F).

journal_append(#{fd := F}, Event) ->
    ok = file:write(F, [json:encode(Event), <<"\n">>]),
    ok = file:sync(F).

with_transaction(J, Cleanup, Finalize, Fun) ->
    undefined = get(imboy_prepare_transaction),
    put(imboy_prepare_transaction, #{
        journal => J, cleanup => Cleanup, tracked => [], pending => #{}
    }),
    imboy_quality_backend:track_prepare(get(imboy_prepare_transaction)),
    try
        Value = Fun(),
        Tx = get(imboy_prepare_transaction),
        true = map_size(maps:get(pending, Tx)) =:= 0,
        ok = Finalize(),
        put(imboy_prepare_finalized, true),
        journal_append(J, #{event => <<"prepare_complete">>}),
        Value
    catch
        Class:Reason:Stack ->
            Tx0 = get(imboy_prepare_transaction),
            Pending = maps:keys(maps:get(pending, Tx0)),
            Write1 = safe_journal(J, #{
                event => <<"prepare_failed">>,
                class => atom_to_binary(Class),
                pending => [journal_label(K) || K <- Pending]
            }),
            Results = [recover_item(J, Cleanup, Item) || Item <- maps:get(tracked, Tx0)],
            Write2 = safe_journal(J, #{event => <<"recovery_complete">>, outcomes => Results}),
            erlang:raise(
                Class,
                {prepare_failed, Reason, #{
                    journal => maps:get(path, J),
                    pending => [journal_label(K) || K <- Pending],
                    recovery => Results,
                    journal_failed => Write1 =/= ok orelse Write2 =/= ok
                }},
                Stack
            )
    after
        imboy_quality_backend:clear_prepare(),
        erase(imboy_prepare_transaction),
        case erase(imboy_prepare_finalized) of
            true -> ok;
            _ -> _ = safe_call(Finalize)
        end,
        _ = journal_close(J)
    end.

transaction_intent(Type, Name) when is_atom(Type), is_binary(Name), byte_size(Name) > 0 ->
    Tx = get(imboy_prepare_transaction),
    Pending = maps:get(pending, Tx),
    Key = {Type, Name},
    false = maps:is_key(Key, Pending),
    J = maps:get(journal, Tx),
    journal_append(J, #{
        event => <<"intent">>,
        type => atom_to_binary(Type),
        name => Name,
        pg => maps:get(pg, J)
    }),
    put(imboy_prepare_transaction, Tx#{pending := Pending#{Key => true}}),
    imboy_quality_backend:track_prepare(get(imboy_prepare_transaction)),
    ok.

transaction_confirm(Type, Name, Resource) ->
    Tx = get(imboy_prepare_transaction),
    Key = {Type, Name},
    Pending = maps:get(pending, Tx),
    true = maps:is_key(Key, Pending),
    J = maps:get(journal, Tx),
    journal_append(J, #{
        event => <<"confirmed">>,
        type => atom_to_binary(Type),
        name => Name,
        resource => resource_description(Type, Resource),
        identity_digest => string:lowercase(
            binary:encode_hex(crypto:hash(sha256, term_to_binary(Resource)))
        )
    }),
    put(imboy_prepare_transaction, Tx#{
        pending := maps:remove(Key, Pending),
        tracked := [{Type, Name, Resource} | maps:get(tracked, Tx)]
    }),
    imboy_quality_backend:track_prepare(get(imboy_prepare_transaction)),
    ok.

recover_item(J, Cleanup, {Type, Name, Resource}) ->
    Outcome = safe_call(fun() -> Cleanup(Type, Resource) end),
    State =
        case Outcome of
            ok -> <<"cleaned">>;
            retained -> <<"retained">>;
            _ -> <<"unconfirmed">>
        end,
    _ = safe_journal(J, #{
        event => <<"recovery_item">>,
        type => atom_to_binary(Type),
        name => Name,
        status => State,
        reason => safe_outcome(Outcome),
        outcome_digest => string:lowercase(
            binary:encode_hex(crypto:hash(sha256, term_to_binary(Outcome)))
        )
    }),
    #{type => atom_to_binary(Type), name => Name, status => State}.

safe_journal(J, Event) -> safe_call(fun() -> journal_append(J, Event) end).
journal_label({Type, Name}) -> #{type => atom_to_binary(Type), name => Name}.

resource_description(runtime, #{path := Path, identity := Identity, token := Token}) ->
    #{
        path => list_to_binary(Path),
        identity => fmt(Identity),
        marker_sha256 => string:lowercase(binary:encode_hex(crypto:hash(sha256, Token)))
    };
resource_description(
    garage, #{dir := Dir, os_pid := Pid, dir_identity := Identity, dir_token := Token} = G
) ->
    #{
        path => list_to_binary(Dir),
        os_pid => Pid,
        identity => fmt(Identity),
        marker_sha256 => string:lowercase(binary:encode_hex(crypto:hash(sha256, Token))),
        process_identity => fmt(maps:get(process_identity, G, missing))
    };
resource_description(template, #{name := Name}) ->
    #{name => list_to_binary(Name)};
resource_description(clone, Name) when is_list(Name) -> #{name => list_to_binary(Name)};
resource_description(_, _) ->
    #{}.

safe_outcome(ok) ->
    <<"ok">>;
safe_outcome(retained) ->
    <<"retained">>;
safe_outcome({error, Reason}) when is_atom(Reason) -> atom_to_binary(Reason);
safe_outcome({error, {Class, Reason}}) when is_atom(Class), is_atom(Reason) ->
    <<(atom_to_binary(Class))/binary, ":", (atom_to_binary(Reason))/binary>>;
safe_outcome(_) ->
    <<"complex_error_redacted">>.

release(#{pg_args := _, clones := _} = Prepared) ->
    Result = release_with(
        Prepared,
        fun drop_databases/1,
        fun imboy_quality_resources:stop_garage/1,
        fun copy_exclusive/2
    ),
    Postgres =
        case maps:find(pg_handle, Prepared) of
            {ok, Handle} ->
                imboy_quality_pg_owner:stop(Handle, fun imboy_quality_pg_register:record_exit/3);
            error ->
                {error, postgres_handle_missing}
        end,
    Complete = finish_postgres_cleanup(Result, Postgres),
    put(resources_released, {completed, Complete}),
    Complete;
release(_) ->
    #{
        <<"clones_dropped">> => [],
        <<"clone_drop_failed">> => [],
        <<"ok">> => false,
        <<"error">> => <<"invalid_prepared_resources">>
    }.

postgres_stopped({ok, #{postgres_exit := Exit, pg_ctl_exit := 0, log_ok := true}}) ->
    Exit =:= 0;
postgres_stopped(_) ->
    false.

finish_postgres_cleanup(Result, Postgres) ->
    Result#{
        <<"postgres">> => fmt(Postgres),
        <<"ok">> := maps:get(<<"ok">>, Result) andalso postgres_stopped(Postgres)
    }.

release_with(Prepared, Drop, Stop, CopyFile) ->
    release_with(
        Prepared,
        Drop,
        Stop,
        CopyFile,
        fun imboy_quality_resources:cleanup_stopped_garage/2
    ).

release_with(Prepared, Drop, Stop, CopyFile, CleanupGarage) ->
    DbResults =
        case safe_call(fun() -> Drop(Prepared) end) of
            DbList when is_list(DbList) -> DbList;
            Error -> [{"postgres_connection", Error}]
        end,
    GarageResult =
        case maps:get(garage, Prepared, undefined) of
            {ok, G} ->
                Evidence = filename:join([
                    maps:get(ctrl, Prepared), "evidence", "garage-" ++ os:getpid() ++ ".log"
                ]),
                archive_garage(G, Evidence, Stop, CopyFile, CleanupGarage);
            Other ->
                {not_started, Other}
        end,
    Runtime =
        case
            safe_call(fun() ->
                [{runtime_path(D), runtime_cleanup(D)} || D <- maps:get(runtime_dirs, Prepared, [])]
            end)
        of
            Results when is_list(Results) -> Results;
            RuntimeError -> [{"runtime_dirs", RuntimeError}]
        end,
    Result = cleanup_result(DbResults, GarageResult, Runtime),
    put(resources_released, {completed, Result}),
    Result.

archive_garage(G, Evidence, Stop, CopyFile, CleanupGarage) ->
    StopResult = safe_call(fun() -> Stop(G#{keep_dir => true}) end),
    case StopResult of
        #{exit := Code, cleanup := retained, receipt := _} when is_integer(Code) ->
            Copy = safe_call(fun() -> CopyFile(maps:get(log, G), Evidence) end),
            Clean =
                case Copy of
                    {ok, _} -> safe_call(fun() -> CleanupGarage(G, StopResult) end);
                    _ -> {error, log_not_preserved}
                end,
            {Copy, #{exit => Code, cleanup => Clean}};
        OtherStop ->
            {not_copied, OtherStop}
    end.

copy_exclusive(Source, Destination) ->
    {ok, #file_info{type = regular, links = 1}} = file:read_link_info(Source),
    {ok, In} = file:open(Source, [read, binary, raw]),
    try
        {ok, Out} = file:open(Destination, [write, binary, raw, exclusive]),
        try
            {ok, Count} = file:copy(In, Out),
            ok = file:sync(Out),
            {ok, Count}
        after
            file:close(Out)
        end
    after
        file:close(In)
    end.

safe_call(Fun) ->
    try
        Fun()
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

drop_databases(
    #{
        pg_args := Args,
        clones := Clones,
        ctrl := Ctrl,
        pg_identity := Expected,
        resource_root := Root
    } = Prepared
) ->
    Root = garage_resource_root(Ctrl),
    {ok, Expected} = verify_postgres(Ctrl, Args, Root),
    {ok, C} = imboy_quality_fixture:connect(Args#{database => "postgres"}),
    try
        Root = garage_resource_root(Ctrl),
        {ok, Actual} = verify_postgres(Ctrl, Args, Root),
        SystemId = list_to_binary(imboy_quality_fixture:system_identifier(C)),
        ok = verified_reconnect(Expected, Actual, SystemId),
        [
            {Name, safe_call(fun() -> imboy_quality_fixture:drop_owned(C, Args, Name) end)}
         || Name <-
                [N || {_, N} <- lists:sort(maps:to_list(Clones))] ++
                    maps:get(drop_dbs, Prepared, [])
        ]
    after
        close_quiet(C)
    end.

verified_reconnect(Expected, Actual, SystemId) ->
    try
        true = is_map(Expected) andalso Expected =:= Actual,
        true = is_binary(SystemId) andalso byte_size(SystemId) > 0,
        SystemId = maps:get(<<"system_identifier">>, Expected),
        ok
    catch
        _:_ -> {error, postgres_reconnect_identity_mismatch}
    end.

%% Retain paths until their ownership is independently proven.
runtime_cleanup(Entry) -> imboy_quality_env_cleanup:cleanup_owned(Entry).

runtime_path(#{path := Path}) -> Path;
runtime_path(Path) when is_list(Path) -> Path;
runtime_path(_) -> "unverified_runtime_path".

%% after 子句的兜底回收：正常路径已回收则跳过；异常路径尽力回收不再抛错。
release_once(Prepared) ->
    case get(resources_released) of
        {completed, #{<<"ok">> := true}} ->
            ok;
        {completed, Result} ->
            cleanup_incomplete(Result);
        true ->
            cleanup_incomplete(missing_cleanup_result);
        _ ->
            try
                case release(Prepared) of
                    #{<<"ok">> := true} -> ok;
                    Result -> cleanup_incomplete(Result)
                end
            catch
                Class:Reason -> cleanup_incomplete({Class, Reason})
            end
    end.

cleanup_incomplete(Reason) ->
    io:format(
        standard_error,
        "quality cleanup incomplete; resources retained where ownership was unverified~n",
        []
    ),
    {error, {cleanup_incomplete, Reason}}.

close_quiet(C) ->
    try
        epgsql:close(C)
    catch
        _:_ -> ok
    end.

prepare_cleanup(Ctrl, Args, Root, PG, clone, Name) ->
    {ok, PG} = imboy_quality_env_cleanup:verify_postgres(Ctrl, Args, Root),
    {ok, C} = imboy_quality_fixture:connect(Args#{database => "postgres"}),
    try
        Expected = maps:get(<<"system_identifier">>, PG),
        Expected = list_to_binary(imboy_quality_fixture:system_identifier(C)),
        imboy_quality_fixture:drop_owned(C, Args, Name)
    after
        close_quiet(C)
    end;
prepare_cleanup(_Ctrl, _Args, _Root, _PG, runtime, Entry) ->
    imboy_quality_env_cleanup:cleanup_owned(Entry);
prepare_cleanup(Ctrl, _Args, _Root, _PG, garage, G) ->
    Evidence = filename:join([
        Ctrl,
        "evidence",
        "garage-prepare-failure-" ++ integer_to_list(maps:get(os_pid, G)) ++ ".log"
    ]),
    Result = imboy_quality_env_cleanup:archive_garage(
        G,
        Evidence,
        fun imboy_quality_resources:stop_garage/1,
        fun imboy_quality_env_cleanup:copy_exclusive/2,
        fun imboy_quality_resources:cleanup_stopped_garage/2
    ),
    case imboy_quality_env_cleanup:garage_cleanup_ok(Result) of
        true -> ok;
        false -> {error, garage_cleanup_unconfirmed}
    end;
prepare_cleanup(_Ctrl, _Args, _Root, _PG, template, _Template) ->
    retained.
