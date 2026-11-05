-module(talk_record_audio_worker_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-export([garage_validation/0]).

worker_validation_test_() -> {timeout, 90, fun worker_validation/0}.
worker_upload_timeout_test_() -> {timeout, 30, fun worker_upload_timeout/0}.

derived_write_unknown_test() ->
    meck:new(elib_pg, [passthrough]),
    Self = self(),
    meck:expect(elib_pg, execute, fun(_Sql, Params) ->
        Self ! {params, Params},
        {ok, 1}
    end),
    try
        ?assertEqual(
            {error, worker_outcome_unknown},
            attachment_derived_artifact:finish_write(42, {error, worker_outcome_unknown})
        ),
        receive
            {params, Params} -> ?assertEqual([42, <<"unknown">>], Params)
        after 1000 -> error(missing_derived_write)
        end
    after
        meck:unload(elib_pg)
    end.

worker_validation() ->
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(inets),
    Frame = <<16#ff, 16#f1, 16#60, 16#40, 4, 16#e0, 0, 0:256>>,
    Bytes = <<Frame/binary, Frame/binary>>,
    Sha = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    Part = #{<<"sha256">> => Sha, <<"byte_size">> => byte_size(Bytes)},
    Final = #{
        <<"sha256">> => Sha,
        <<"byte_size">> => byte_size(Bytes),
        <<"sample_rate">> => 16000,
        <<"channels">> => 1,
        <<"sample_count">> => 2048,
        <<"duration_ms">> => 128
    },
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, {_, Port}} = inet:sockname(Listen),
    Server = spawn(fun() -> serve(Listen, Bytes) end),
    Url = iolist_to_binary(io_lib:format("http://127.0.0.1:~B/audio", [Port])),
    Task = #{
        url => Url,
        size => byte_size(Bytes),
        sha256 => Sha,
        parts => [Part],
        final => Final,
        limits => #{max_bytes => 4096, max_parts => 2000},
        binding => crypto:strong_rand_bytes(32)
    },
    try
        ?assertMatch({ok, #{frames := 2}}, attachment_worker_port:validate_audio(Task)),
        ?assertMatch(
            {error, audio_part_integrity_mismatch},
            attachment_worker_port:validate_audio(Task#{
                parts => [Part#{<<"sha256">> => <<"wrong">>}]
            })
        ),
        ?assertMatch(
            {error, audio_identity_mismatch},
            attachment_worker_port:validate_audio(Task#{
                final => Final#{
                    <<"sample_count">> => 1024,
                    <<"duration_ms">> => 64
                }
            })
        ),
        ?assertMatch(
            {error, artifact_integrity_mismatch},
            attachment_worker_port:validate_audio(Task#{sha256 => binary:copy(<<"0">>, 64)})
        ),
        timeout_is_failure(Task)
    after
        gen_tcp:close(Listen),
        exit(Server, shutdown)
    end.

timeout_is_failure(Task) ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, {_, Port}} = inet:sockname(Listen),
    Server = spawn(fun() ->
        {ok, Socket} = gen_tcp:accept(Listen),
        {ok, _} = gen_tcp:recv(Socket, 0, 5000),
        receive
            stop -> gen_tcp:close(Socket)
        end
    end),
    Url = iolist_to_binary(io_lib:format("http://127.0.0.1:~B/audio", [Port])),
    try
        ?assertEqual(
            {error, timeout},
            attachment_worker_port:validate_audio(
                Task#{url => Url, deadline_ms => 1000}
            )
        )
    after
        Server ! stop,
        gen_tcp:close(Listen)
    end.

serve(Listen, Bytes) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            {ok, _} = gen_tcp:recv(Socket, 0, 5000),
            ok = gen_tcp:send(Socket, [
                "HTTP/1.1 200 OK\r\nContent-Length: ",
                integer_to_list(byte_size(Bytes)),
                "\r\nConnection: close\r\n\r\n",
                Bytes
            ]),
            gen_tcp:close(Socket),
            serve(Listen, Bytes);
        {error, closed} ->
            ok
    end.

worker_upload_timeout() ->
    Dir = filename:join(
        "/tmp",
        "c09-worker-test-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))
    ),
    ok = file:make_dir(Dir),
    ok = file:change_mode(Dir, 8#700),
    Source = filename:join(Dir, "source"),
    ok = file:write_file(Source, <<"temporary fixture">>),
    Prior = os:getenv("IMBOY_WORKER_TMPDIR"),
    PriorTimeout = application:get_env(imboy, attachment_worker_port_timeout_ms),
    ok = application:set_env(imboy, attachment_worker_port_timeout_ms, 2000),
    true = os:putenv("IMBOY_WORKER_TMPDIR", Dir),
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, {_, Port}} = inet:sockname(Listen),
    Parent = self(),
    Server = spawn(fun() ->
        {ok, Socket} = gen_tcp:accept(Listen),
        {ok, Request} = gen_tcp:recv(Socket, 0, 5000),
        Parent ! {put_started, Request},
        receive
            commit -> Parent ! remote_write_completed
        end,
        gen_tcp:close(Socket)
    end),
    Endpoint = iolist_to_binary(io_lib:format("http://127.0.0.1:~B", [Port])),
    Task = #{
        src_path => Source,
        endpoint => Endpoint,
        ak => <<"key">>,
        sk => <<"secret">>,
        bucket => <<"bucket">>,
        region => <<"garage">>,
        object_key => <<"unique-staging-key">>,
        deadline_ms => 20000
    },
    spawn(fun() -> Parent ! {upload_result, attachment_worker_port:upload_file(Task)} end),
    try
        receive
            {put_started, <<"PUT ", _/binary>>} -> ok
        after 5000 -> error(put_not_started)
        end,
        [TaskFile] = filelib:wildcard(filename:join([Dir, "imboy-worker-*", "worker.task"])),
        {ok, #file_info{mode = Mode}} = file:read_file_info(filename:dirname(TaskFile)),
        ?assertEqual(8#700, Mode band 8#777),
        {ok, Encoded} = file:read_file(TaskFile),
        ?assertEqual(2000, maps:get(deadline_ms, binary_to_term(base64:decode(Encoded)))),
        Pid = worker_pid(TaskFile),
        receive
            {upload_result, Result} -> ?assertEqual({error, worker_outcome_unknown}, Result)
        after 5000 -> error(worker_timeout_not_reported)
        end,
        ?assertEqual(false, process_exists(Pid, 20)),
        Server ! commit,
        receive
            remote_write_completed -> ok
        after 1000 -> error(remote_commit_not_seen)
        end,
        ?assertEqual(
            {error, worker_outcome_unknown},
            attachment_worker_port:upload_file(
                Task#{src_path => filename:join(Dir, "absent")}
            )
        )
    after
        gen_tcp:close(Listen),
        exit(Server, shutdown),
        case Prior of
            false -> os:unsetenv("IMBOY_WORKER_TMPDIR");
            _ -> os:putenv("IMBOY_WORKER_TMPDIR", Prior)
        end,
        case PriorTimeout of
            undefined ->
                application:unset_env(imboy, attachment_worker_port_timeout_ms);
            {ok, Previous} ->
                application:set_env(imboy, attachment_worker_port_timeout_ms, Previous)
        end,
        file:delete(Source),
        file:del_dir(Dir)
    end.

worker_pid(TaskFile) ->
    Lines = string:split(os:cmd("ps -axo pid=,args="), "\n", all),
    [Line] = [L || L <- Lines, string:find(L, TaskFile) =/= nomatch],
    [Pid | _] = string:lexemes(Line, " "),
    list_to_integer(Pid).

process_exists(Pid, Attempts) ->
    Command = "ps -p " ++ integer_to_list(Pid) ++ " -o stat=",
    case string:trim(os:cmd(Command)) of
        "" ->
            false;
        _ when Attempts > 0 ->
            timer:sleep(100),
            process_exists(Pid, Attempts - 1);
        State ->
            {alive, State}
    end.

%% Explicitly invoked with isolated Garage fixture; never silently skipped.
garage_validation() ->
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Endpoint = list_to_binary(os:getenv("C09_GARAGE_ENDPOINT")),
    Bucket = list_to_binary(os:getenv("C09_GARAGE_BUCKET")),
    Cfg = #{
        endpoint => Endpoint,
        container => Bucket,
        access_key => list_to_binary(os:getenv("C09_GARAGE_AK")),
        secret_key => list_to_binary(os:getenv("C09_GARAGE_SK")),
        region => <<"garage">>
    },
    Frame = <<16#ff, 16#f1, 16#60, 16#40, 4, 16#e0, 0, 0:256>>,
    Bytes = <<Frame/binary, Frame/binary>>,
    Sha = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    Checksum = base64:encode(crypto:hash(sha256, Bytes)),
    Key =
        <<"c09-worker-validation/",
            (binary:encode_hex(crypto:strong_rand_bytes(16), lowercase))/binary>>,
    Headers = [{<<"content-type">>, <<"audio/aac">>}, {<<"x-amz-checksum-sha256">>, Checksum}],
    Put = elib_s3_sign:presign_signed_url_at(
        <<"PUT">>,
        Endpoint,
        Bucket,
        Key,
        Headers,
        calendar:universal_time(),
        300,
        Cfg
    ),
    {ok, {{_, Status, _}, _, _}} = httpc:request(
        put,
        {
            binary_to_list(Put),
            [{"Content-Type", "audio/aac"}, {"x-amz-checksum-sha256", binary_to_list(Checksum)}],
            "audio/aac",
            Bytes
        },
        [{timeout, 30000}],
        [{body_format, binary}]
    ),
    true = Status >= 200 andalso Status < 300,
    Url = elib_s3_sign:presign_signed_url_at(
        <<"GET">>,
        Endpoint,
        Bucket,
        Key,
        [],
        calendar:universal_time(),
        300,
        Cfg
    ),
    Final = #{
        <<"sha256">> => Sha,
        <<"byte_size">> => byte_size(Bytes),
        <<"sample_rate">> => 16000,
        <<"channels">> => 1,
        <<"sample_count">> => 2048,
        <<"duration_ms">> => 128
    },
    Task = #{
        url => Url,
        size => byte_size(Bytes),
        sha256 => Sha,
        parts => [#{<<"sha256">> => Sha, <<"byte_size">> => byte_size(Bytes)}],
        final => Final,
        limits => #{max_bytes => 4096, max_parts => 2000},
        binding => crypto:strong_rand_bytes(32)
    },
    {ok, #{frames := 2}} = attachment_worker_port:validate_audio(Task),
    ok.
