-module(attachment_upload_body_tests).
-include_lib("eunit/include/eunit.hrl").

final_chunk_limit_test() ->
    with_reader(fun(_, _) -> {ok, <<"12345">>, done} end, fun() ->
        ?assertEqual({error, {invalid_param, payload}}, attachment_upload_body:read(start, 4)),
        assert_closed()
    end).

cumulative_final_chunk_limit_test() ->
    with_reader(fun
        (start, _) -> {more, <<"123">>, next};
        (next, _) -> {ok, <<"45">>, done}
    end, fun() ->
        ?assertEqual({error, {invalid_param, payload}}, attachment_upload_body:read(start, 4)),
        assert_closed()
    end).

bounded_exact_limit_and_ownership_test() ->
    with_reader(fun
        (start, #{length := 65536, period := 10000}) -> {more, <<"12">>, next};
        (next, #{length := 65536, period := 10000}) -> {ok, <<"34">>, done}
    end, fun() ->
        {ok, Source, done} = attachment_upload_body:read(start, 4),
        try
            ?assertEqual(4, attachment_upload_spool:size(Source)),
            ?assertEqual({ok, <<"1234">>}, attachment_upload_spool:read(Source, 0))
        after attachment_upload_spool:close(Source) end,
        assert_closed()
    end).

reader_exception_closes_descriptor_test() ->
    with_reader(fun(_, _) -> error(reader_failed) end, fun() ->
        ?assertError(reader_failed, attachment_upload_body:read(start, 4)),
        assert_closed()
    end).

reader_error_closes_descriptor_test() ->
    with_reader(fun(_, _) -> {error, timeout} end, fun() ->
        ?assertEqual({error, timeout}, attachment_upload_body:read(start, 4)),
        assert_closed()
    end).

with_reader(Reader, Test) ->
    ok = meck:new(cowboy_req, [passthrough, no_link]),
    ok = meck:new(attachment_upload_spool, [passthrough, no_link]),
    Self = self(),
    try
        ok = meck:expect(cowboy_req, read_body, Reader),
        ok = meck:expect(attachment_upload_spool, new, fun() ->
            Result = meck:passthrough([]),
            {ok, {upload_spool_writer, Fh}} = Result,
            Self ! {writer, Fh},
            Result
        end),
        Test()
    after
        meck:unload(attachment_upload_spool),
        meck:unload(cowboy_req)
    end.

assert_closed() ->
    receive {writer, Fh} -> ?assertMatch({error, _}, file:pread(Fh, 0, 1))
    after 1000 -> error(missing_writer) end.
