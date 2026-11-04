-module(imboy_quality_compile_tests).
-include_lib("eunit/include/eunit.hrl").

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
