-module(imboy_quality_compile_tests).
-include_lib("eunit/include/eunit.hrl").

framework_test_helpers_are_compiled_test() ->
    Dir = filename:join("/tmp", "imboy-quality-test-macro-" ++ os:getpid()),
    ok = file:make_dir(Dir),
    Src = filename:join(Dir, "quality_macro_fixture.erl"),
    Beam = filename:join(Dir, "quality_macro_fixture.beam"),
    try
        ok = file:write_file(
            Src,
            <<
                "-module(quality_macro_fixture).\n"
                "-ifdef(TEST).\n-export([test_helper/0]).\n"
                "test_helper() -> ok.\n-endif.\n"
            >>
        ),
        ?assertEqual(0, imboy_quality_compile:erlc(Src, Dir)),
        {ok, {quality_macro_fixture, [{exports, Exports}]}} = beam_lib:chunks(Beam, [exports]),
        ?assert(lists:member({test_helper, 0}, Exports))
    after
        file:delete(Beam),
        file:delete(Src),
        file:del_dir(Dir)
    end.

restored_timestamp_is_not_a_cache_hit_test() ->
    Dir = filename:join("/tmp", "imboy-quality-compile-" ++ os:getpid()),
    ok = file:make_dir(Dir),
    Src = filename:join(Dir, "source.erl"),
    Beam = filename:join(Dir, "cached.beam"),
    try
        ok = file:write_file(Src, <<"source">>),
        ok = file:write_file(Beam, <<"old beam">>),
        ok = file:change_time(Src, {{2000, 1, 1}, {0, 0, 0}}),
        ?assert(imboy_quality_compile:needs_compile(Src, Beam)),
        ?assertException(
            error,
            {missing_source, _},
            imboy_quality_compile:needs_compile(filename:join(Dir, "missing.erl"), Beam)
        )
    after
        file:delete(Src),
        file:delete(Beam),
        file:del_dir(Dir)
    end.
