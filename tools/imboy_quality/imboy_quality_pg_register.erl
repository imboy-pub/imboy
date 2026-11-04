%% Registration changes only the task-owned PostgreSQL entry under both task locks.
-module(imboy_quality_pg_register).
-export([register/3, record_exit/3, transition/3, verify_port_receipt/6, persist_then_commit/3]).
-include_lib("kernel/include/file.hrl").

register(Pid, Meta, Proof) -> transaction(register, {Pid, Meta}, Proof).
record_exit(Pid, Exit, Proof) -> transaction(record_exit, {Pid, Exit}, Proof).

transition(Action, State, Payload) ->
    try
        Resources = maps:get(<<"owned_resources">>, State),
        true = is_list(Resources),
        [PG] = [R || #{<<"kind">> := <<"ISOLATED_POSTGRESQL">>} = R <- Resources],
        [Volume] = [R || #{<<"kind">> := <<"ISOLATED_APFS_VOLUME">>} = R <- Resources],
        NewPG = update(Action, PG, Volume, Payload),
        {ok, State#{
            <<"owned_resources">> := [
                case R =:= PG of
                    true -> NewPG;
                    false -> R
                end
             || R <- Resources
            ]
        }}
    catch
        _:_ -> {error, invalid_pg_registration_transition}
    end.

update(register, #{<<"status">> := <<"RUNNING">>} = PG, Volume, {Pid, Meta}) ->
    Pid = maps:get(<<"pid">>, PG),
    PG = update(register, PG#{<<"status">> := <<"PREPARED">>}, Volume, {Pid, Meta}),
    PG;
update(register, PG, Volume, {Pid, Meta}) ->
    <<"PREPARED">> = maps:get(<<"status">>, PG),
    true = is_integer(Pid) andalso Pid > 0,
    true = is_map(Meta),
    Keys = [
        <<"host">>,
        <<"port">>,
        <<"data_dir">>,
        <<"executable">>,
        <<"executable_sha256">>,
        <<"system_identifier">>,
        <<"executable_identity">>,
        <<"data_dir_identity">>,
        <<"log_path">>,
        <<"owner_nonce_sha256">>
    ],
    true = lists:sort(maps:keys(Meta)) =:= lists:sort(Keys),
    lists:foreach(
        fun(K) -> true = maps:get(K, PG) =:= maps:get(K, Meta) end,
        [
            <<"host">>,
            <<"port">>,
            <<"data_dir">>,
            <<"executable">>,
            <<"executable_sha256">>,
            <<"system_identifier">>
        ]
    ),
    <<"127.0.0.1">> = maps:get(<<"host">>, Meta),
    Port = maps:get(<<"port">>, Meta),
    true = is_integer(Port) andalso Port > 0 andalso Port < 65536,
    Root = maps:get(<<"path">>, Volume),
    true = is_binary(Root) andalso byte_size(Root) > 0,
    DataDir = <<Root/binary, "/pgdata">>,
    DataDir = maps:get(<<"data_dir">>, Meta),
    Executable = <<Root/binary, "/postgres-toolchain/bin/postgres">>,
    Executable = maps:get(<<"executable">>, Meta),
    Log = maps:get(<<"log_path">>, Meta),
    true = is_binary(Log),
    Prefix = <<Root/binary, "/postgres-owner-">>,
    {0, PrefixSize} = binary:match(Log, Prefix),
    Tail = binary:part(Log, PrefixSize, byte_size(Log) - PrefixSize),
    true = byte_size(Tail) > 4,
    true = binary:match(Tail, <<"/">>) =:= nomatch,
    true = hex64(maps:get(<<"executable_sha256">>, Meta)),
    true = hex64(maps:get(<<"owner_nonce_sha256">>, Meta)),
    true = digits(maps:get(<<"system_identifier">>, Meta)),
    true = identity(maps:get(<<"executable_identity">>, Meta)),
    true = identity(maps:get(<<"data_dir_identity">>, Meta)),
    maps:merge(PG, Meta#{<<"status">> => <<"RUNNING">>, <<"pid">> => Pid});
update(record_exit, #{<<"status">> := Status} = PG, Volume, {Pid, Exit}) when
    Status =:= <<"STOPPED">>; Status =:= <<"STOPPED_WITH_EVIDENCE_ERROR">>
->
    PG = update(record_exit, PG#{<<"status">> := <<"RUNNING">>}, Volume, {Pid, Exit}),
    PG;
update(
    record_exit, PG, _Volume, {Pid, #{postgres_exit := P, pg_ctl_exit := C, log_ok := LogOk} = Exit}
) ->
    <<"RUNNING">> = maps:get(<<"status">>, PG),
    Pid = maps:get(<<"pid">>, PG),
    true = is_integer(Pid) andalso Pid > 0,
    true = map_size(Exit) =:= 3,
    true = is_integer(P) andalso P >= 0 andalso P =< 255,
    true = (is_integer(C) andalso C >= 0 andalso C =< 255) orelse C =:= <<"NOT_INVOKED">>,
    true = is_boolean(LogOk),
    Status =
        case P =:= 0 andalso C =:= 0 andalso LogOk of
            true -> <<"STOPPED">>;
            false -> <<"STOPPED_WITH_EVIDENCE_ERROR">>
        end,
    PG#{
        <<"status">> => Status,
        <<"postgres_exit">> => P,
        <<"pg_ctl_exit">> => C,
        <<"log_ok">> => LogOk
    }.

hex64(B) when is_binary(B), byte_size(B) =:= 64 ->
    lists:all(
        fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) end,
        binary_to_list(B)
    );
hex64(_) ->
    false.
digits(B) when is_binary(B), byte_size(B) > 0 ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(B));
digits(_) ->
    false.
identity(#{<<"major_device">> := A, <<"minor_device">> := B, <<"inode">> := I} = M) ->
    map_size(M) =:= 3 andalso is_integer(A) andalso A >= 0 andalso
        is_integer(B) andalso B >= 0 andalso is_integer(I) andalso I > 0;
identity(_) ->
    false.

transaction(Action, Payload, Proof) ->
    try
        {ok, Repo} = file:get_cwd(),
        {0, Common0} = imboy_quality_guard:git(Repo, ["rev-parse", "--git-common-dir"]),
        Common = string:trim(binary_to_list(Common0)),
        absolute = filename:pathtype(Common),
        Ctrl = filename:join(Common, "zcode-tasks/IMBOY_ERLANG_QUALITY"),
        Package =
            "/Users/leeyi/project/imboy.pub/_task_packages/IMBOY_ERLANG_QUALITY/.package.lock",
        Lock = filename:join(Ctrl, "owner.lock"),
        ok = file:make_dir(Package),
        try
            ok = file:make_dir(Lock),
            try
                locked(Ctrl, Action, Payload, Proof)
            after
                ok = file:del_dir(Lock)
            end
        after
            ok = file:del_dir(Package)
        end
    catch
        Class:Reason -> {error, {pg_registration_failed, Class, Reason}}
    end.

locked(Ctrl, Action, Payload, Proof) ->
    Names = ["STATE.json", "STATE.json.sha256", "checkpoint.json", "checkpoint.json.sha256"],
    Files = maps:from_list([{N, read_regular(filename:join(Ctrl, N))} || N <- Names]),
    Bytes = maps:get("STATE.json", Files),
    Bytes = maps:get("checkpoint.json", Files),
    Sha = hex(crypto:hash(sha256, Bytes)),
    Sha = string:trim(maps:get("STATE.json.sha256", Files)),
    Sha = string:trim(maps:get("checkpoint.json.sha256", Files)),
    State = json:decode(Bytes),
    true = imboy_quality_full_budget:owner_verified(State),
    Ctx = (imboy_quality_state:trusted_context(State))#{owner_verified => true},
    %% An already registered process must remain recoverable after resource admission closes.
    Operation =
        case Action of
            register -> eunit_group;
            record_exit -> inspect
        end,
    ok = imboy_quality_cli:permit(Ctx, Operation),
    {ok, Changed} = transition(Action, State, Payload),
    {Pid, Details} = Payload,
    {OwnerPid, Ref} = Proof,
    true = is_pid(OwnerPid) andalso node(OwnerPid) =:= node() andalso is_reference(Ref),
    [PG] = [
        R
     || #{<<"kind">> := <<"ISOLATED_POSTGRESQL">>} = R <-
            maps:get(<<"owned_resources">>, State)
    ],
    Claim = imboy_quality_pg_owner:claim_receipt(OwnerPid, Ref, Action, Pid, Details),
    case Claim of
        {ok, Port, Nonce} ->
            ok = verify_port_receipt(Action, Pid, Details, PG, {OwnerPid, Port}, Nonce),
            ok = verify_source_receipt(Ctrl, Action, Pid, Details, Nonce),
            Commit = fun() ->
                {ok, Port, Nonce} = imboy_quality_pg_owner:commit_receipt(
                    OwnerPid, Ref, Action, Pid, Details
                ),
                ok
            end,
            case Changed =:= State of
                true ->
                    ok = Commit();
                false ->
                    Next = Changed#{
                        <<"checkpoint_sequence">> := maps:get(<<"checkpoint_sequence">>, State) + 1
                    },
                    NewBytes = iolist_to_binary(json:encode(Next)),
                    NewSha = hex(crypto:hash(sha256, NewBytes)),
                    Write = fun() ->
                        lists:foreach(
                            fun({Name, Data}) ->
                                atomic_existing(filename:join(Ctrl, Name), Data)
                            end,
                            [
                                {"STATE.json", NewBytes},
                                {"STATE.json.sha256", NewSha},
                                {"checkpoint.json", NewBytes},
                                {"checkpoint.json.sha256", NewSha}
                            ]
                        ),
                        ok
                    end,
                    ok = persist_then_commit(fun() -> backup(Ctrl, Names, Files) end, Write, Commit)
            end;
        {error, invalid_or_consumed_receipt} when Changed =:= State ->
            {ok, Port, Nonce} = imboy_quality_pg_owner:commit_receipt(
                OwnerPid, Ref, Action, Pid, Details
            ),
            ok = verify_port_receipt(Action, Pid, Details, PG, {OwnerPid, Port}, Nonce),
            ok = verify_source_receipt(Ctrl, Action, Pid, Details, Nonce);
        _ ->
            erlang:error(invalid_pg_owner_receipt)
    end,
    case {Action, Payload} of
        {record_exit, {_, #{postgres_exit := P, pg_ctl_exit := C, log_ok := LogOk}}} when
            P =/= 0; C =/= 0; LogOk =:= false
        ->
            {error, postgres_cleanup_incomplete};
        _ ->
            ok
    end.

persist_then_commit(Backup, Write, Commit) ->
    try
        ok = Backup(),
        ok = Write(),
        Commit()
    catch
        Class:Reason -> {error, {registration_persistence_failed, Class, Reason}}
    end.

verify_source_receipt(Ctrl, Action, Pid, Details, Nonce) ->
    Evidence = filename:join(Ctrl, "evidence"),
    {ok, #file_info{type = directory}} = file:read_link_info(Evidence),
    Hash = binary_to_list(hex(crypto:hash(sha256, Nonce))),
    Prefix =
        case Action of
            register -> "postgres-launch-";
            record_exit -> "postgres-exit-"
        end,
    Data = json:decode(read_regular(filename:join(Evidence, Prefix ++ Hash ++ ".json"))),
    Pid = maps:get(<<"pid">>, Data),
    case Action of
        register ->
            <<"LAUNCHED_EXIT_UNVERIFIED">> = maps:get(<<"status">>, Data),
            Details = maps:get(<<"metadata">>, Data);
        record_exit ->
            P = maps:get(postgres_exit, Details),
            P = maps:get(<<"postgres_exit">>, Data),
            C = maps:get(pg_ctl_exit, Details),
            C = maps:get(<<"pg_ctl_exit">>, Data),
            LogOk = maps:get(log_ok, Details),
            LogOk = maps:get(<<"log_ok">>, Data)
    end,
    ok.

verify_port_receipt(Action, Pid, Details, PG, {OwnerPid, Port}, Nonce) ->
    try
        true = is_pid(OwnerPid) andalso node(OwnerPid) =:= node(),
        true = is_port(Port),
        true = is_binary(Nonce) andalso byte_size(Nonce) =:= 32,
        Digest = hex(crypto:hash(sha256, Nonce)),
        case Action of
            register ->
                Digest = maps:get(<<"owner_nonce_sha256">>, Details),
                Exec = binary_to_list(maps:get(<<"executable">>, PG)),
                ok = imboy_quality_pg_owner:validate_port(Port, OwnerPid, Exec, Pid);
            record_exit ->
                Digest = maps:get(<<"owner_nonce_sha256">>, PG),
                Pid = maps:get(<<"pid">>, PG),
                undefined = erlang:port_info(Port)
        end,
        ok
    catch
        _:_ -> {error, invalid_port_receipt}
    end.

backup(Ctrl, Names, Files) ->
    Id = binary_to_list(hex(crypto:strong_rand_bytes(16))),
    Rel = filename:join("state-patches", "pg-registration-" ++ Id),
    {ok, Dir} = imboy_quality_state:new_path(Ctrl, Rel),
    ok = file:make_dir(Dir),
    ok = file:change_mode(Dir, 8#700),
    lists:foreach(
        fun(Name) -> exclusive(filename:join(Dir, Name), maps:get(Name, Files)) end, Names
    ).

read_regular(Path) ->
    {ok, #file_info{type = regular, links = 1}} = file:read_link_info(Path),
    {ok, Bytes} = file:read_file(Path),
    Bytes.

atomic_existing(Path, Bytes) ->
    _ = read_regular(Path),
    Temp = Path ++ ".pg-register." ++ binary_to_list(hex(crypto:strong_rand_bytes(16))),
    ok = exclusive(Temp, Bytes),
    _ = read_regular(Path),
    ok = file:rename(Temp, Path).

exclusive(Path, Bytes) ->
    {ok, F} = file:open(Path, [write, binary, raw, exclusive]),
    try
        ok = file:change_mode(Path, 8#600),
        ok = file:write(F, Bytes),
        ok = file:sync(F)
    after
        ok = file:close(F)
    end,
    ok.

hex(B) -> string:lowercase(binary:encode_hex(B)).
