-module(attachment_upload_spool_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

private_unlinked_roundtrip_test() ->
    with_directory(fun(Dir) ->
        {ok, Writer = {upload_spool_writer, Fh}} = attachment_upload_spool:new(Dir),
        try
            ?assertEqual({ok, []}, file:list_dir(Dir)),
            {ok, #file_info{mode = Mode, links = Links}} = file:read_file_info(Fh),
            ?assertEqual(8#600, Mode band 8#777),
            ?assertEqual(0, Links),
            {ok, Writer} = attachment_upload_spool:append(Writer, <<"first">>),
            {ok, Writer} = attachment_upload_spool:append(Writer, <<"second">>),
            {ok, Source} = attachment_upload_spool:seal(Writer),
            ?assertEqual(11, attachment_upload_spool:size(Source)),
            ?assertEqual(hex_hash(<<"firstsecond">>), attachment_upload_spool:sha256(Source)),
            ?assertEqual({ok, <<"firstsecond">>}, attachment_upload_spool:read(Source, 0)),
            ?assertEqual(eof, attachment_upload_spool:read(Source, 11))
        after
            ok = attachment_upload_spool:close(Writer)
        end,
        ?assertMatch({error, _}, file:pread(Fh, 0, 1))
    end).

insecure_temporary_base_is_rejected_test() ->
    with_directory(fun(Dir) ->
        ok = file:change_mode(Dir, 8#777),
        ?assertEqual({error, insecure_temporary_directory}, attachment_upload_spool:new(Dir)),
        %% An untrusted path is deliberately not used for failure cleanup.
        [Probe] = filelib:wildcard(filename:join([Dir, "*", "probe"])),
        ?assertEqual({ok, <<>>}, file:read_file(Probe)),
        ?assertEqual([], filelib:wildcard(filename:join([Dir, "*", "body"])))
    end).

untrusted_base_has_no_chmod_or_cleanup_test() ->
    with_directory(fun(Dir) ->
        ok = file:change_mode(Dir, 8#777),
        ok = meck:new(file, [unstick, passthrough]),
        try
            Result = attachment_upload_spool:new(Dir),
            ?assertEqual({error, insecure_temporary_directory}, Result),
            ?assertEqual(0, meck:num_calls(file, change_mode, '_')),
            ?assertEqual(0, meck:num_calls(file, delete, '_')),
            ?assertEqual(0, meck:num_calls(file, del_dir, '_')),
            ?assertEqual(1, meck:num_calls(file, close, '_'))
        after
            meck:unload(file)
        end
    end).

payload_never_uses_exposed_probe_inode_test() ->
    with_directory(fun(Dir) ->
        erase(retained_probe_reader),
        ok = meck:new(file, [unstick, passthrough]),
        try
            ok = meck:expect(file, open, fun(Path, Modes) ->
                Result = meck:passthrough([Path, Modes]),
                case filename:basename(Path) of
                    "probe" ->
                        {ok, ProbeReader} = meck:passthrough([Path, [read, binary]]),
                        put(retained_probe_reader, ProbeReader);
                    _ ->
                        ok
                end,
                Result
            end),
            {ok, Writer = {upload_spool_writer, Fh}} = attachment_upload_spool:new(Dir),
            try
                Probe = get(retained_probe_reader),
                ?assert(is_pid(Probe)),
                {ok, #file_info{inode = ProbeInode}} = file:read_file_info(Probe),
                {ok, #file_info{inode = BodyInode}} = file:read_file_info(Fh),
                ?assertNotEqual(ProbeInode, BodyInode),
                {ok, Writer} = attachment_upload_spool:append(Writer, <<"secret payload">>),
                {ok, Source} = attachment_upload_spool:seal(Writer),
                ?assertEqual(eof, file:pread(Probe, 0, 65536)),
                ?assertEqual({ok, <<"secret payload">>}, attachment_upload_spool:read(Source, 0)),
                ?assertEqual({ok, []}, file:list_dir(Dir))
            after
                attachment_upload_spool:close(Writer)
            end
        after
            case erase(retained_probe_reader) of
                undefined -> ok;
                Reader -> file:close(Reader)
            end,
            meck:unload(file)
        end
    end).

trusted_symlink_temporary_base_test() ->
    with_directory(fun(Dir) ->
        Real = filename:join(Dir, "real"),
        Link = filename:join(Dir, "link"),
        ok = file:make_dir(Real),
        ok = file:make_symlink("real", Link),
        {ok, Writer} = attachment_upload_spool:new(Link),
        try
            {ok, Writer} = attachment_upload_spool:append(Writer, <<"private">>),
            {ok, Source} = attachment_upload_spool:seal(Writer),
            ?assertEqual({ok, <<"private">>}, attachment_upload_spool:read(Source, 0))
        after
            attachment_upload_spool:close(Writer)
        end,
        ?assertEqual({ok, []}, file:list_dir(Real)),
        ok = file:change_mode(Real, 8#777),
        ?assertEqual({error, insecure_temporary_directory}, attachment_upload_spool:new(Link))
    end).

bounded_independent_readers_test() ->
    Bytes = binary:copy(<<"chunk-content">>, 20000),
    attachment_upload_spool:with_source(Bytes, fun(Source) ->
        ?assertEqual(byte_size(Bytes), attachment_upload_spool:size(Source)),
        {ok, Chunk} = attachment_upload_spool:read_at(Source, 0, byte_size(Bytes)),
        ?assertEqual(65536, byte_size(Chunk)),
        Parent = self(),
        {Pid, Ref} = spawn_monitor(fun() ->
            Parent ! {self(), read_all(Source, 0, [])}
        end),
        receive
            {Pid, Read} -> ?assertEqual(Bytes, Read)
        after 1000 -> error(read_timeout)
        end,
        receive
            {'DOWN', Ref, process, Pid, normal} -> ok
        after 1000 -> error(reader_timeout)
        end,
        %% The other process and a replay do not share a mutable read position.
        ?assertEqual({ok, Chunk}, attachment_upload_spool:read(Source, 0))
    end).

legacy_path_replacement_cannot_change_verified_bytes_test() ->
    with_directory(fun(Dir) ->
        Path = filename:join(Dir, "input"),
        ok = file:write_file(Path, <<"verified">>),
        attachment_upload_spool:with_source({file, Path}, fun(Source) ->
            ok = file:delete(Path),
            ok = file:write_file(Path, <<"replacement">>),
            ?assertEqual(hex_hash(<<"verified">>), attachment_upload_spool:sha256(Source)),
            ?assertEqual({ok, <<"verified">>}, attachment_upload_spool:read(Source, 0))
        end),
        ?assertEqual({ok, <<"replacement">>}, file:read_file(Path))
    end).

source_ownership_and_exception_cleanup_test() ->
    {ok, Writer} = attachment_upload_spool:new(),
    try
        {ok, Writer} = attachment_upload_spool:append(Writer, <<"kept">>),
        {ok, Source} = attachment_upload_spool:seal(Writer),
        ?assertException(
            error,
            expected,
            attachment_upload_spool:with_source(
                Source,
                fun(_) -> error(expected) end
            )
        ),
        ?assertEqual({ok, <<"kept">>}, attachment_upload_spool:read(Source, 0)),
        Self = self(),
        ?assertException(
            error,
            expected,
            attachment_upload_spool:with_source(
                <<"owned">>,
                fun({upload_spool, Fh, _, _}) ->
                    Self ! {owned_file, Fh},
                    error(expected)
                end
            )
        ),
        receive
            {owned_file, Fh} -> ?assertMatch({error, _}, file:pread(Fh, 0, 1))
        after 1000 -> error(missing_file)
        end
    after
        attachment_upload_spool:close(Writer)
    end.

bounded_copy_test() ->
    with_directory(fun(Dir) ->
        Bytes = binary:copy(<<"0123456789">>, 25000),
        Path = filename:join(Dir, "copy"),
        {ok, Out} = file:open(Path, [write, raw, binary]),
        try
            attachment_upload_spool:with_source(Bytes, fun(Source) ->
                ?assertEqual(ok, attachment_upload_spool:copy_to(Source, Out))
            end)
        after
            file:close(Out)
        end,
        ?assertEqual({ok, Bytes}, file:read_file(Path))
    end).

disk_recheck_rejection_never_publishes_test() ->
    with_directory(fun(Dir) ->
        I = disk_identity(Dir),
        ?assertEqual(
            {error, upload_closed},
            attachment_storage_disk:put_snapshot(
                I,
                <<"rejected">>,
                erlang:system_time(second) + 60,
                fun() -> {error, upload_closed} end
            )
        ),
        ?assertEqual({error, not_found}, attachment_storage_disk:read_snapshot(I)),
        ?assertEqual(
            {error, invalid_signature_context},
            attachment_storage_disk:put_snapshot(
                I,
                <<"expired">>,
                erlang:system_time(second) - 1,
                fun() -> error(expired_recheck_must_not_run) end
            )
        )
    end).

disk_snapshot_private_and_atomic_test() ->
    with_directory(fun(Dir) ->
        I = disk_identity(Dir),
        Path = filename:join([Dir, "bucket", "object"]),
        Bytes = binary:copy(<<"disk-stream">>, 20000),
        Result = attachment_storage_disk:put_snapshot(
            I,
            Bytes,
            erlang:system_time(second) + 60,
            fun() ->
                ?assertEqual(false, filelib:is_regular(Path)),
                ok
            end
        ),
        ?assertEqual(ok, Result),
        ?assertEqual({ok, Bytes}, attachment_storage_disk:read_snapshot(I)),
        {ok, #file_info{mode = Mode}} = file:read_file_info(Path),
        ?assertEqual(8#600, Mode band 8#777),
        ?assertEqual([], filelib:wildcard(Path ++ ".v5tmp.*"))
    end).

disk_recheck_runs_inside_shared_os_lock_test() ->
    with_directory(fun(Dir) ->
        I = disk_identity(Dir),
        Expiry = erlang:system_time(second) + 60,
        Parent = self(),
        {First, FirstRef} = spawn_monitor(fun() ->
            Result = attachment_storage_disk:put_snapshot(I, <<"first">>, Expiry, fun() ->
                Parent ! {self(), locked},
                receive
                    release -> ok
                after 2000 -> error(release_timeout)
                end
            end),
            Parent ! {self(), result, Result}
        end),
        receive
            {First, locked} -> ok
        after 2000 -> error(lock_timeout)
        end,
        {Second, SecondRef} = spawn_monitor(fun() ->
            Result = attachment_storage_disk:put_snapshot(I, <<"second">>, Expiry, fun() ->
                Parent ! {self(), recheck, attachment_storage_disk:read_snapshot(I)},
                {error, upload_closed}
            end),
            Parent ! {self(), result, Result}
        end),
        try
            receive
                {Second, recheck, _} -> error(recheck_before_lock)
            after 50 -> ok
            end,
            First ! release,
            receive
                {First, result, ok} -> ok
            after 2000 -> error(first_timeout)
            end,
            receive
                {Second, recheck, Result} -> ?assertEqual({ok, <<"first">>}, Result)
            after 2000 -> error(recheck_timeout)
            end,
            receive
                {Second, result, {error, upload_closed}} -> ok
            after 2000 -> error(second_timeout)
            end,
            ?assertEqual({ok, <<"first">>}, attachment_storage_disk:read_snapshot(I))
        after
            exit(First, kill),
            exit(Second, kill),
            erlang:demonitor(FirstRef, [flush]),
            erlang:demonitor(SecondRef, [flush])
        end
    end).

garage_real_httpc_stream_test() ->
    {ok, _} = application:ensure_all_started(inets),
    Bytes = binary:copy(<<"bounded-http-body">>, 20000),
    {ok, Listen} = gen_tcp:listen(0, [
        binary, {active, false}, {ip, {127, 0, 0, 1}}, {reuseaddr, true}
    ]),
    {ok, {_, Port}} = inet:sockname(Listen),
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() -> receive_upload(Listen, Parent) end),
    try
        Endpoint = list_to_binary("http://127.0.0.1:" ++ integer_to_list(Port)),
        Identity = #{
            endpoint => Endpoint,
            container => <<"bucket">>,
            key => <<"object">>,
            garage_config => #{access_key => <<"fixture">>, secret_key => <<"fixture-secret">>}
        },
        Now = erlang:system_time(second),
        Ctx = #{issued_at => datetime(Now), expires_at => datetime(Now + 60)},
        ?assertEqual(
            ok,
            attachment_storage_garage:put_snapshot(
                Identity, Bytes, <<"application/octet-stream">>, Ctx
            )
        ),
        receive
            {Pid, received, Headers, Size, Sha} ->
                ?assertEqual(byte_size(Bytes), Size),
                ?assertEqual(hex_hash(Bytes), Sha),
                ?assertNotEqual(
                    nomatch,
                    binary:match(
                        Headers,
                        <<"content-length: ", (integer_to_binary(Size))/binary>>
                    )
                ),
                ?assertNotEqual(nomatch, binary:match(Headers, <<"X-Amz-Date=">>)),
                ?assertEqual(nomatch, binary:match(Headers, <<"transfer-encoding:">>))
        after 2000 -> error(no_upload)
        end,
        receive
            {'DOWN', Ref, process, Pid, normal} -> ok
        after 2000 -> error(server_timeout)
        end
    after
        gen_tcp:close(Listen),
        exit(Pid, kill),
        erlang:demonitor(Ref, [flush])
    end.

receive_upload(Listen, Parent) ->
    {ok, Socket} = gen_tcp:accept(Listen, 3000),
    try
        {Headers, Initial} = receive_headers(Socket, <<>>),
        {match, [LengthBin]} = re:run(
            Headers,
            <<"content-length: ([0-9]+)">>,
            [caseless, {capture, [1], binary}]
        ),
        Length = binary_to_integer(LengthBin),
        Hash = receive_bytes(
            Socket,
            Length - byte_size(Initial),
            crypto:hash_update(
                crypto:hash_init(sha256), Initial
            )
        ),
        Parent !
            {
                self(),
                received,
                Headers,
                Length,
                binary:encode_hex(crypto:hash_final(Hash), lowercase)
            },
        ok = gen_tcp:send(
            Socket, <<"HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n">>
        )
    after
        gen_tcp:close(Socket)
    end.

receive_headers(Socket, Acc) ->
    case binary:split(Acc, <<"\r\n\r\n">>) of
        [Headers, Body] ->
            {Headers, Body};
        [_] ->
            {ok, Bytes} = gen_tcp:recv(Socket, 0, 3000),
            receive_headers(Socket, <<Acc/binary, Bytes/binary>>)
    end.

receive_bytes(_, 0, Hash) ->
    Hash;
receive_bytes(Socket, Left, Hash) when Left > 0 ->
    {ok, Bytes} = gen_tcp:recv(Socket, min(65536, Left), 3000),
    receive_bytes(Socket, Left - byte_size(Bytes), crypto:hash_update(Hash, Bytes)).

disk_identity(Dir) ->
    #{endpoint => list_to_binary("file://" ++ Dir), container => <<"bucket">>, key => <<"object">>}.
datetime(Unix) -> calendar:gregorian_seconds_to_datetime(Unix + 62167219200).
hex_hash(Bytes) -> binary:encode_hex(crypto:hash(sha256, Bytes), lowercase).
read_all(Source, Offset, Acc) ->
    case attachment_upload_spool:read(Source, Offset) of
        {ok, Bytes} ->
            ?assert(byte_size(Bytes) =< 65536),
            read_all(Source, Offset + byte_size(Bytes), [Bytes | Acc]);
        eof ->
            iolist_to_binary(lists:reverse(Acc))
    end.
with_directory(Fun) ->
    Dir = filename:join(
        tmp_root(),
        "imboy-spool-test-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))
    ),
    ok = file:make_dir(Dir),
    try
        Fun(Dir)
    after
        file:del_dir_r(Dir)
    end.

%% macOS 将 /tmp 软链到 /private/tmp；no_symlinks 逐段校验会拒绝 /tmp 路径。
tmp_root() ->
    case file:read_link_all("/tmp") of
        {ok, Real} -> filename:join("/", unicode:characters_to_list(Real));
        {error, _} -> "/tmp"
    end.
