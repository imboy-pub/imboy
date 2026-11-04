-module(attachment_artifact_reader_tests).
-include_lib("eunit/include/eunit.hrl").

%% V3-20 S3-only：artifact reader 的 disk 路径 fail-closed（copy/3 恒
%% {error, disk_driver_retired}）。spool 完整性/hash/篡改拒绝义务由下方的
%% garage_stream_integrity_test 在现行 garage 路径完整承载；本用例冻结
%% disk 路径的拒绝合同（零存储 I/O）。
bounded_disk_integrity_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun({Root, Bytes, I}) ->
        ?_test(begin
            Row = row(Bytes),
            meck:expect(attachment_storage, frozen_location, fun(_, _) -> {ok, I} end),
            ?assertEqual(
                {error, disk_driver_retired},
                attachment_artifact_reader:with_source(Row, 200000, fun(_) -> error(unreachable) end)
            ),
            ?assertEqual(
                {error, disk_driver_retired},
                attachment_artifact_reader:with_range(Row, 65537, 9, fun(_) ->
                    error(unreachable)
                end)
            ),
            %% fail-closed 不产生任何存储读/写：源文件字节保持原样
            ?assertEqual(
                {ok, Bytes},
                file:read_file(filename:join([Root, "bucket", "audio.aac"]))
            )
        end)
    end}.

garage_stream_integrity_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun({_Root, Bytes, _I}) ->
        ?_test(begin
            {ok, _} = application:ensure_all_started(inets),
            ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
            try
                meck:expect(attachment_storage, frozen_location, fun(_, _) ->
                    {ok, #{driver => garage}}
                end),
                Check = fun(Body, Status, Expected) ->
                    with_http(Body, Status, fun(Url) ->
                        meck:expect(attachment_storage_garage, presign_snapshot, fun(_, _) ->
                            {ok, Url}
                        end),
                        Result = attachment_artifact_reader:with_source(
                            row(Bytes),
                            200000,
                            fun(S) -> {ok, attachment_upload_spool:size(S)} end
                        ),
                        ?assertEqual(Expected, Result)
                    end)
                end,
                Check(Bytes, 200, {ok, byte_size(Bytes)}),
                Check(<<Bytes/binary, 0>>, 200, {error, artifact_integrity_mismatch}),
                Check(<<"denied">>, 403, {error, {storage_http_status, 403}}),
                Partial = binary:part(Bytes, 2, 4),
                with_http(Partial, 206, [{"Content-Range", "bytes 2-5/150000"}], fun(Url) ->
                    meck:expect(attachment_storage_garage, presign_snapshot, fun(_, _) ->
                        {ok, Url}
                    end),
                    Read = attachment_artifact_reader:with_range(
                        row(Bytes),
                        2,
                        4,
                        fun(S) -> attachment_upload_spool:read(S, 0) end
                    ),
                    ?assertEqual({ok, Partial}, Read),
                    receive
                        {fixture_request, Request} ->
                            ?assertNotEqual(
                                nomatch,
                                binary:match(string:lowercase(Request), <<"range: bytes=2-5">>)
                            )
                    after 1000 -> error(no_range_request)
                    end
                end),
                with_http(Partial, 200, fun(Url) ->
                    meck:expect(attachment_storage_garage, presign_snapshot, fun(_, _) ->
                        {ok, Url}
                    end),
                    ?assertEqual(
                        {error, storage_range_mismatch},
                        attachment_artifact_reader:with_range(
                            row(Bytes), 2, 4, fun(_) -> error(unreachable) end
                        )
                    )
                end)
            after
                meck:unload(attachment_storage_garage)
            end
        end)
    end}.

with_http(Bytes, Status, Fun) -> with_http(Bytes, Status, [], Fun).
with_http(Bytes, Status, ExtraHeaders, Fun) ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}, {ip, {127, 0, 0, 1}}]),
    {ok, {_, Port}} = inet:sockname(L),
    Caller = self(),
    Pid = spawn(fun() ->
        case gen_tcp:accept(L, 5000) of
            {ok, S} ->
                {ok, Request} = gen_tcp:recv(S, 0, 5000),
                case ExtraHeaders of
                    [] -> ok;
                    _ -> Caller ! {fixture_request, Request}
                end,
                gen_tcp:send(S, [
                    "HTTP/1.1 ",
                    integer_to_list(Status),
                    " Fixture\r\nContent-Length: ",
                    integer_to_list(byte_size(Bytes)),
                    "\r\n",
                    [[K, ": ", V, "\r\n"] || {K, V} <- ExtraHeaders],
                    "Connection: close\r\n\r\n"
                ]),
                gen_tcp:send(S, Bytes),
                gen_tcp:close(S);
            _ ->
                ok
        end
    end),
    try
        Fun(list_to_binary("http://127.0.0.1:" ++ integer_to_list(Port) ++ "/private-audio"))
    after
        gen_tcp:close(L),
        exit(Pid, kill)
    end.

setup() ->
    Base =
        case filelib:is_dir("/private/tmp") of
            true -> "/private/tmp";
            false -> "/tmp"
        end,
    Root = filename:join(
        Base, "talk-artifact-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Root),
    ok = file:change_mode(Root, 8#700),
    ok = file:make_dir(filename:join(Root, "bucket")),
    ok = file:change_mode(filename:join(Root, "bucket"), 8#700),
    Bytes = binary:copy(<<"synthetic-audio">>, 10000),
    ok = file:write_file(filename:join([Root, "bucket", "audio.aac"]), Bytes),
    ok = meck:new(attachment_storage, [passthrough, no_link]),
    I = #{
        driver => disk,
        endpoint => <<"file://", (list_to_binary(Root))/binary>>,
        container => <<"bucket">>,
        key => <<"audio.aac">>
    },
    {Root, Bytes, I}.
cleanup({Root, _, _}) ->
    meck:unload(attachment_storage),
    file:del_dir_r(Root).
row(Bytes) ->
    #{
        <<"path">> => <<"logical/audio.aac">>,
        <<"size">> => byte_size(Bytes),
        <<"file_hash256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase)
    }.
