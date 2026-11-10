%% 合成 fixture：只验证加载器的准入绑定语义，不触碰 imboy/config 或 evidence。
%% 每个用例用独立临时目录（/tmp/<模块>_<pid>/<用例>），结束即删。
-module(imboy_quality_u_leaves_tests).
-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "u-leaves-r5.term").

leaves() ->
    #{
        <<"imboy_quality_budget_tests">> => [<<"imboy_quality_budget_tests">>],
        <<"imboy_quality_ci_gate_tests">> => [<<"imboy_quality_ci_gate_tests">>]
    }.

hex(Bin) ->
    string:lowercase(binary:encode_hex(crypto:hash(sha256, Bin))).

with_ctrl(Name, Fun) ->
    Base = filename:join(["/tmp", "imboy_quality_u_leaves_" ++ os:getpid(), Name]),
    ok = filelib:ensure_dir(filename:join([Base, "evidence", "dummy"])),
    try
        Fun(Base, filename:join([Base, "evidence", ?FILENAME]))
    after
        _ = file:del_dir_r(Base)
    end.

write(Path, Bytes) ->
    ok = file:write_file(Path, Bytes).

%% 文件缺失 / 不可读：两种读失败都必须具名（D2）。
%% 并且「读文件」先于「校验 dynamic_U」——admission 为空时仍报 missing。
read_failure_test() ->
    with_ctrl("read_failure", fun(Ctrl, Path) ->
        ?assertEqual(
            {error, {u_leaves_missing, Path}},
            imboy_quality_u_leaves:load(Ctrl, #{})
        ),
        ok = file:make_dir(Path),
        ?assertEqual(
            {error, {u_leaves_unreadable, Path, eisdir}},
            imboy_quality_u_leaves:load(Ctrl, #{<<"dynamic_U">> => hex(term_to_binary(leaves()))})
        )
    end).

%% admission 无 dynamic_U 或形状不合规：未绑定，不得降级成 mismatch/pass。
unbound_test() ->
    with_ctrl("unbound", fun(Ctrl, Path) ->
        Bin = term_to_binary(leaves()),
        ok = write(Path, Bin),
        True = hex(Bin),
        Unbound = [
            #{},
            #{<<"dynamic_U">> => <<>>},
            #{<<"dynamic_U">> => <<"2bc8f348">>},
            #{<<"dynamic_U">> => list_to_binary(string:uppercase(binary_to_list(True)))},
            #{<<"dynamic_U">> => binary:copy(<<"z">>, 64)},
            #{<<"dynamic_U">> => 42},
            #{<<"dynamic_U">> => undefined}
        ],
        lists:foreach(
            fun(Admission) ->
                ?assertEqual(
                    {error, {u_leaves_fingerprint_unbound, Ctrl}},
                    imboy_quality_u_leaves:load(Ctrl, Admission)
                )
            end,
            Unbound
        ),
        %% 非 map 的 admission 同样算未绑定。
        ?assertEqual(
            {error, {u_leaves_fingerprint_unbound, Ctrl}},
            imboy_quality_u_leaves:load(Ctrl, not_a_map)
        )
    end).

%% 字节可解码但 sha 与 admission 不符：另一份合法 ETF → fingerprint_mismatch（D1）。
fingerprint_mismatch_test() ->
    with_ctrl("mismatch", fun(Ctrl, Path) ->
        Admitted = term_to_binary(leaves()),
        Other = maps:put(
            <<"imboy_quality_extra_tests">>,
            [<<"imboy_quality_extra_tests">>],
            leaves()
        ),
        OtherBin = term_to_binary(Other),
        ok = write(Path, OtherBin),
        ?assertEqual(
            {error,
                {u_leaves_fingerprint_mismatch, #{
                    expected => hex(Admitted),
                    actual => hex(OtherBin),
                    path => Path
                }}},
            imboy_quality_u_leaves:load(Ctrl, #{<<"dynamic_U">> => hex(Admitted)})
        )
    end).

%% 损坏字节：badarg 必须被包装（D3），且 sha 不匹配时也不能被误判成 mismatch。
%% 同时覆盖「不是 map」与「形状不是 K => [K]」两种解码失败。
decode_failed_test() ->
    with_ctrl("decode_failed", fun(Ctrl, Path) ->
        Corrupt = <<"not-an-etf">>,
        ok = write(Path, Corrupt),
        ?assertEqual(
            {error, {u_leaves_decode_failed, badarg}},
            imboy_quality_u_leaves:load(Ctrl, #{<<"dynamic_U">> => hex(Corrupt)})
        ),
        ?assertEqual(
            {error, {u_leaves_decode_failed, badarg}},
            imboy_quality_u_leaves:load(
                Ctrl, #{<<"dynamic_U">> => hex(term_to_binary(leaves()))}
            )
        ),
        NotMap = term_to_binary([1, 2, 3]),
        ok = write(Path, NotMap),
        ?assertEqual(
            {error, {u_leaves_decode_failed, {not_a_map, [1, 2, 3]}}},
            imboy_quality_u_leaves:load(Ctrl, #{<<"dynamic_U">> => hex(NotMap)})
        ),
        BadShape = term_to_binary(#{<<"root">> => [<<"other">>]}),
        ok = write(Path, BadShape),
        ?assertEqual(
            {error, {u_leaves_decode_failed, {shape_invalid, <<"root">>}}},
            imboy_quality_u_leaves:load(Ctrl, #{<<"dynamic_U">> => hex(BadShape)})
        )
    end).

%% 正例：sha256(term_to_binary(LeafMap)) == admission.dynamic_U → 返回同一份叶表。
bound_fingerprint_test() ->
    with_ctrl("bound", fun(Ctrl, Path) ->
        Bin = term_to_binary(leaves()),
        ok = write(Path, Bin),
        {ok, LeafMap} = imboy_quality_u_leaves:load(Ctrl, #{<<"dynamic_U">> => hex(Bin)}),
        ?assertEqual(leaves(), LeafMap),
        ?assertEqual(
            [<<"imboy_quality_budget_tests">>, <<"imboy_quality_ci_gate_tests">>],
            lists:sort(maps:keys(LeafMap))
        )
    end).
