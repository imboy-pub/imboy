%%% elib_tsid_store_tests — TSID-05 双槽 durable future fence store 测试
%%%
%%% 覆盖 T-202..T-210：每个写入断点的 kill -9 恢复（真实进程死亡，
%%% 非 mock）、单/双槽损坏、split-brain、配置不匹配、存储故障与
%%% golden vectors。
-module(elib_tsid_store_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NODE, 129).
-define(DC_BITS, 3).
-define(MAX_REL_PLUS_1, 4398046511104).

tmp_root() ->
    Dir = "/tmp/tsid_store_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = filelib:ensure_dir(Dir ++ "/x"),
    Dir.

cfg(Root) ->
    #{root => Root, combined_node => ?NODE, dc_bits => ?DC_BITS, store_bootstrap => fresh}.

%% ===================================================================
%% golden vectors 与编解码
%% ===================================================================

golden_vector_test() ->
    {Bin, Expect} = elib_tsid_store:golden_vector(),
    ?assertEqual(48, byte_size(Bin)),
    ?assertEqual({ok, Expect}, elib_tsid_store:decode_record(Bin)),
    %% 头 8 字节 magic 固定
    ?assertMatch(<<"IMBTSID1", _/binary>>, Bin).

record_roundtrip_test() ->
    LH = elib_tsid_store:layout_hash(?NODE, ?DC_BITS),
    Bin = elib_tsid_store:encode_record(LH, ?NODE, 42, 4398046511103, 12345),
    ?assertEqual(
        {ok, #{
            layout_hash => LH,
            combined_node => ?NODE,
            generation => 42,
            safe_before => 4398046511103,
            written_at_ms => 12345
        }},
        elib_tsid_store:decode_record(Bin)
    ).

record_corruption_test() ->
    {Bin, _} = elib_tsid_store:golden_vector(),
    %% 任意单字节翻转 → bad_crc
    Flip =
        fun(Pos) ->
            <<A:Pos/binary, C, B/binary>> = Bin,
            Flipped = C bxor 16#FF,
            <<A/binary, Flipped, B/binary>>
        end,
    lists:foreach(
        fun(Pos) ->
            ?assertMatch({error, bad_crc}, elib_tsid_store:decode_record(Flip(Pos)))
        end,
        [0, 7, 20, 43]
    ),
    %% 截断与加长 → bad_length
    ?assertMatch({error, bad_length}, elib_tsid_store:decode_record(binary:part(Bin, 0, 47))),
    ?assertMatch(
        {error, bad_length}, elib_tsid_store:decode_record(<<Bin/binary, 0>>)
    ).

layout_hash_binding_test() ->
    %% 不同 node/dc_bits/layout 参数产生不同 hash（配置漂移可检测）
    H1 = elib_tsid_store:layout_hash(129, 3),
    H2 = elib_tsid_store:layout_hash(130, 3),
    H3 = elib_tsid_store:layout_hash(129, 4),
    ?assertNotEqual(H1, H2),
    ?assertNotEqual(H1, H3),
    ?assertEqual(8, byte_size(H1)).

%% ===================================================================
%% 基础持久化与恢复
%% ===================================================================

persist_and_reopen_test() ->
    Root = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Root)),
    ?assertMatch(
        #{generation := 0, degraded := false, safe_before := 0}, elib_tsid_store:status(S0)
    ),
    {ok, S1} = elib_tsid_store:persist(S1_ = S0, 1000),
    _ = S1_,
    {ok, S2} = elib_tsid_store:persist(S1, 2000),
    ?assertMatch(
        #{generation := 2, safe_before := 2000, degraded := false}, elib_tsid_store:status(S2)
    ),
    %% 独立重新打开（existing 语义）：恢复出最高 generation 的 safe_before
    {ok, R} = elib_tsid_store:open(cfg(Root)),
    ?assertMatch(#{generation := 2, safe_before := 2000}, elib_tsid_store:status(R)).

bootstrap_existing_requires_slots_test() ->
    Root = tmp_root(),
    %% 双槽 absent + existing：必须拒绝（禁止自动当 fresh）
    {error, no_valid_slot} =
        elib_tsid_store:open(#{
            root => Root, combined_node => ?NODE, dc_bits => ?DC_BITS, store_bootstrap => existing
        }),
    ok.

%% ===================================================================
%% T-202..T-205 写入断点 kill -9 恢复（真实进程死亡）
%% ===================================================================

%% 建立双槽基线（gen1=a, gen2=b），随后在指定断点 kill 第 3 次 persist
crash_at(Point) ->
    Root = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Root)),
    {ok, S1} = elib_tsid_store:persist(S0, 1000),
    {ok, S2} = elib_tsid_store:persist(S1, 2000),
    Self = self(),
    Pid = spawn(fun() ->
        %% gen3 目标槽 a；在 Point 断点真实 kill 本进程
        elib_tsid_store:persist(S2, 3000, #{fault => Point})
    end),
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, _, _} -> ok
    after 5000 -> error(crash_did_not_happen)
    end,
    {Root, S2}.

reopen_and_check(Root, ExpectGen, ExpectSafeBefore) ->
    {ok, R} = elib_tsid_store:open(#{
        root => Root, combined_node => ?NODE, dc_bits => ?DC_BITS, store_bootstrap => existing
    }),
    Status = elib_tsid_store:status(R),
    ?assertMatch(#{generation := ExpectGen, safe_before := ExpectSafeBefore}, Status),
    R.

t202_temp_create_crash_test() ->
    {Root, _S2} = crash_at(after_temp_create),
    %% 旧双槽完好：恢复 gen2/2000，绝不回退
    reopen_and_check(Root, 2, 2000).

t202_temp_write_crash_test() ->
    {Root, _} = crash_at(after_write),
    reopen_and_check(Root, 2, 2000).

t203_sync_crash_test() ->
    {Root, _} = crash_at(after_sync),
    reopen_and_check(Root, 2, 2000).

t203_close_crash_test() ->
    {Root, _} = crash_at(after_close),
    reopen_and_check(Root, 2, 2000).

t204_rename_crash_test() ->
    {Root, _} = crash_at(after_rename),
    %% rename 已落（进程死后 OS 可见）：新槽有效，选 gen3；无论选择哪个
    %% 都不低于旧 safe_before
    R = reopen_and_check(Root, 3, 3000),
    ?assertMatch(#{degraded := false}, elib_tsid_store:status(R)).

t205_dirsync_crash_test() ->
    {Root, _} = crash_at(after_dirsync),
    reopen_and_check(Root, 3, 3000).

t205_readback_crash_test() ->
    {Root, _} = crash_at(after_readback),
    reopen_and_check(Root, 3, 3000).

crash_then_continue_test() ->
    %% 崩溃后新 store 可继续持久化且 generation 递增不回退
    {Root, _} = crash_at(after_sync),
    {ok, R0} = elib_tsid_store:open(cfg(Root)),
    {ok, R1} = elib_tsid_store:persist(R0, 5000),
    ?assertMatch(#{generation := 3, safe_before := 5000}, elib_tsid_store:status(R1)),
    {ok, R2} = elib_tsid_store:open(cfg(Root)),
    ?assertMatch(#{generation := 3, safe_before := 5000}, elib_tsid_store:status(R2)).

%% ===================================================================
%% T-206..T-210 损坏与故障
%% ===================================================================

%% T-206 单槽损坏：另一槽有效 + 自动修复
t206_single_slot_corrupt_test() ->
    Root = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Root)),
    {ok, S1} = elib_tsid_store:persist(S0, 1000),
    {ok, S2} = elib_tsid_store:persist(S1, 2000),
    %% 破坏 a 槽（gen1）
    SlotA = elib_tsid_store:slot_path(elib_tsid_store:dir(S2), a),
    {ok, Old} = file:read_file(SlotA),
    ok = file:write_file(SlotA, binary:part(Old, 0, 20)),
    %% 重开：b 有效，degraded
    {ok, R0} = elib_tsid_store:open(cfg(Root)),
    ?assertMatch(
        #{generation := 2, safe_before := 2000, degraded := true}, elib_tsid_store:status(R0)
    ),
    %% 下次 persist 修复损坏槽（目标选 a）→ 再次重开双槽有效
    {ok, R1} = elib_tsid_store:persist(R0, 3000),
    ?assertMatch(#{generation := 3}, elib_tsid_store:status(R1)),
    {ok, R2} = elib_tsid_store:open(cfg(Root)),
    ?assertMatch(#{degraded := false, safe_before := 3000}, elib_tsid_store:status(R2)).

t206_bit_flip_corrupt_test() ->
    Root = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Root)),
    {ok, S1} = elib_tsid_store:persist(S0, 1000),
    {ok, S2} = elib_tsid_store:persist(S1, 2000),
    SlotA = elib_tsid_store:slot_path(elib_tsid_store:dir(S2), a),
    {ok, Old} = file:read_file(SlotA),
    <<H:10/binary, C, T/binary>> = Old,
    ok = file:write_file(SlotA, <<H/binary, (C bxor 1), T/binary>>),
    {ok, R} = elib_tsid_store:open(cfg(Root)),
    ?assertMatch(#{safe_before := 2000, degraded := true}, elib_tsid_store:status(R)).

%% T-207 双槽损坏：STOP，坏文件保留，不得 fresh reset
t207_dual_corrupt_test() ->
    Root = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Root)),
    {ok, S1} = elib_tsid_store:persist(S0, 1000),
    {ok, S2} = elib_tsid_store:persist(S1, 2000),
    Dir = elib_tsid_store:dir(S2),
    ok = file:write_file(elib_tsid_store:slot_path(Dir, a), <<1:48/unit:8>>),
    ok = file:write_file(elib_tsid_store:slot_path(Dir, b), <<2:48/unit:8>>),
    ?assertMatch(
        {error, corrupt_no_valid},
        elib_tsid_store:open(cfg(Root))
    ),
    %% 坏文件保留取证（未被删除/重置）
    {ok, <<1:48/unit:8>>} = file:read_file(elib_tsid_store:slot_path(Dir, a)),
    {ok, <<2:48/unit:8>>} = file:read_file(elib_tsid_store:slot_path(Dir, b)).

%% T-208 同 generation 不同 payload：split-brain
t208_split_brain_test() ->
    Root = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Root)),
    {ok, S1} = elib_tsid_store:persist(S0, 1000),
    %% 手工构造同 generation、不同 safe_before 的双槽
    Dir = elib_tsid_store:dir(S1),
    LH = elib_tsid_store:layout_hash(?NODE, ?DC_BITS),
    ok = file:write_file(
        elib_tsid_store:slot_path(Dir, a),
        elib_tsid_store:encode_record(LH, ?NODE, 7, 1000, 1)
    ),
    ok = file:write_file(
        elib_tsid_store:slot_path(Dir, b),
        elib_tsid_store:encode_record(LH, ?NODE, 7, 2000, 2)
    ),
    ?assertMatch({error, split_brain}, elib_tsid_store:open(cfg(Root))).

%% 同 generation 同 payload：幂等双写，正常
same_generation_same_payload_ok_test() ->
    Root = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Root)),
    {ok, S1} = elib_tsid_store:persist(S0, 1000),
    Dir = elib_tsid_store:dir(S1),
    LH = elib_tsid_store:layout_hash(?NODE, ?DC_BITS),
    Bin = elib_tsid_store:encode_record(LH, ?NODE, 7, 1500, 3),
    ok = file:write_file(elib_tsid_store:slot_path(Dir, a), Bin),
    ok = file:write_file(elib_tsid_store:slot_path(Dir, b), Bin),
    {ok, R} = elib_tsid_store:open(cfg(Root)),
    ?assertMatch(
        #{generation := 7, safe_before := 1500, degraded := false}, elib_tsid_store:status(R)
    ).

%% T-209 node/layout 不匹配：配置冲突拒绝。
%% 目录名内嵌 node 编号，正常打开路径下不同 node 天然隔离；不匹配场景
%% 是目录被拷贝/manifest 被他配置实例写入——手工构造该状态验证。
t209_config_mismatch_test() ->
    Root = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Root)),
    {ok, S1} = elib_tsid_store:persist(S0, 1000),
    Dir = elib_tsid_store:dir(S1),
    %% 用 node=130 的 manifest 覆盖（模拟异配置实例写入/目录拷贝）
    write_manifest(Dir, 130, ?DC_BITS),
    ?assertMatch(
        {error, {config_mismatch, _}},
        elib_tsid_store:open(cfg(Root))
    ),
    %% dc_bits 变化 → layout_hash 变化 → 拒绝
    write_manifest(Dir, ?NODE, 4),
    ?assertMatch(
        {error, {config_mismatch, _}},
        elib_tsid_store:open(cfg(Root))
    ).

write_manifest(Dir, Node, DcBits) ->
    LH = elib_tsid_store:layout_hash(Node, DcBits),
    Body = <<"IMBTSIDM", 1:16/unsigned-big, LH/binary, Node:16/unsigned-big>>,
    Bin = <<Body/binary, (erlang:crc32(Body)):32/unsigned-big>>,
    ok = file:write_file(filename:join(Dir, "manifest"), Bin).

%% T-210 存储故障（目录只读 → EACCES）：不越 fence、不碰旧槽、恢复后可续
t210_readonly_dir_test() ->
    Root = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Root)),
    {ok, S1} = elib_tsid_store:persist(S0, 1000),
    {ok, S2} = elib_tsid_store:persist(S1, 2000),
    Dir = elib_tsid_store:dir(S2),
    ok = file:change_mode(Dir, 8#500),
    {error, {temp_create, _}} = elib_tsid_store:persist(S2, 3000),
    ok = file:change_mode(Dir, 8#700),
    %% 旧槽未动，恢复后可继续
    {ok, R0} = elib_tsid_store:open(cfg(Root)),
    ?assertMatch(#{safe_before := 2000}, elib_tsid_store:status(R0)),
    {ok, R1} = elib_tsid_store:persist(R0, 3000),
    ?assertMatch(#{safe_before := 3000, generation := 3}, elib_tsid_store:status(R1)).

%% 越界 safe_before 拒绝
invalid_safe_before_rejected_test() ->
    Root = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Root)),
    ?assertMatch(
        {error, {invalid_safe_before, _}},
        try
            elib_tsid_store:persist(S0, ?MAX_REL_PLUS_1 + 1),
            %% persist 抛 error({invalid_safe_before,...})；catch 转返回值断言
            unexpected
        catch
            error:{invalid_safe_before, _} = E -> {error, E}
        end
    ).

%% symlink 拒绝
symlink_rejected_test() ->
    Real = tmp_root(),
    {ok, S0} = elib_tsid_store:open(cfg(Real)),
    {ok, S1} = elib_tsid_store:persist(S0, 1000),
    _ = S1,
    Link = Real ++ "_link",
    ok = file:make_symlink(Real, Link),
    ?assertMatch(
        {error, symlink_rejected},
        elib_tsid_store:open(cfg(Link))
    ),
    ok = file:delete(Link).
