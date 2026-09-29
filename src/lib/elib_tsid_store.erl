%%% elib_tsid_store — TSID 双槽 durable future fence store（TSID-05）
%%%
%%% 职责：把 safe_before 上界以 crash-safe 协议持久化到双槽文件，
%%% 供 guard 在任何写入断点后恢复出不低于历史的 durable floor。
%%% 本模块不含锁、不做发布——guard（TSID-06）先持锁再调用本模块，
%%% 且只在 persist 成功返回后才把 safe_before 发布进内存。
%%%
%%% 目录布局（每 CombinedNode 独立目录）：
%%%   <root>/node-<NNNN>/manifest
%%%   <root>/node-<NNNN>/clock.a
%%%   <root>/node-<NNNN>/clock.b
%%%
%%% canonical record（固定 48 字节，v1，禁止 binary_to_term）：
%%%   magic            8B  "IMBTSID1"
%%%   format_version   u16 = 1
%%%   layout_hash      8B  绑定 EPOCH/42/10/11/dc_bits/编码版本
%%%   combined_node    u16
%%%   generation       u32 每次成功持久化递增（双槽择新依据）
%%%   safe_before      u64 0..MAX_REL_TS+1
%%%   written_at_ms    u64 诊断字段，不参与唯一性判断
%%%   payload_length   u32 = 0（v1 无 payload；严格长度校验）
%%%   crc32            u32 覆盖前述全部字节
%%%
%%% 写入协议（任一步失败：不发布、只清理本次 temp、已存在有效槽不动）：
%%%   1. 选择 generation 较旧/无效的槽为目标
%%%   2. 同目录 exclusive 创建唯一 temp
%%%   3. 写完整 record；file:sync(temp)；close 错误同样检查
%%%   4. 文件权限收紧 0600
%%%   5. rename(temp, target)
%%%   6. 父目录 directory sync（file:open(Dir,[read,directory,raw])+sync）
%%%   7. 重读 target 校验 exact bytes/CRC/generation/safe_before
%%%
%%% 恢复协议（open/2）：
%%%   - manifest 校验（layout_hash + combined_node 匹配配置，拒绝 symlink）
%%%   - clock.a / clock.b 独立读取：valid | corrupt | absent
%%%   - 0 valid + 双 absent → {error, no_valid_slot}（bootstrap 决策在上层）
%%%   - 0 valid + 任一 corrupt → {error, corrupt_no_valid}（保留坏文件取证）
%%%   - 1 valid → 选用并标记 degraded（下次 persist 优先修复另一槽）
%%%   - 2 valid → generation 大者胜；同 generation 不同 payload →
%%%     {error, split_brain}
-module(elib_tsid_store).

-export([
    open/1,
    persist/2,
    persist/3,
    status/1,
    dir/1,
    layout_hash/2,
    encode_record/5,
    decode_record/1
]).

-export_type([store/0, fault_point/0]).

%% 测试/观测 seam（非公开 API 契约）
-export([slot_path/2, record_size/0, golden_vector/0]).

%% fault injection 点（仅测试经 persist/3 注入）
-type fault_point() ::
    after_temp_create
    | after_write
    | after_sync
    | after_close
    | after_rename
    | after_dirsync
    | after_readback.

-record(slot, {
    name :: a | b,
    state :: absent | valid | corrupt,
    generation = 0 :: non_neg_integer(),
    safe_before = 0 :: non_neg_integer()
}).

-record(store, {
    dir :: file:filename_all(),
    combined_node :: 0..1023,
    layout_hash :: binary(),
    slots = {#slot{name = a, state = absent}, #slot{name = b, state = absent}} ::
        {#slot{}, #slot{}},
    generation = 0 :: non_neg_integer()
}).

-opaque store() :: #store{}.

%% 常量
-define(MAGIC, <<"IMBTSID1">>).
-define(FORMAT_VERSION, 1).
-define(RECORD_SIZE, 48).
-define(MAX_REL_TS_PLUS_1, 4398046511104).
-define(FILE_MODE, 8#600).
-define(DIR_MODE, 8#700).

%% ===================================================================
%% 打开 / 恢复
%% ===================================================================

%% @doc 打开（或初始化）一个 CombinedNode 的 durable store。
%% Config:
%%   root          - store 根目录（每节点目录 = root/node-<NNNN>）
%%   combined_node - 0..1023，必须与启动配置一致
%%   dc_bits       - 0..10，参与 layout_hash
%%   bootstrap     - fresh | existing | undefined
%%     * 双槽 absent 且 bootstrap=fresh：创建 manifest 并允许空恢复
%%     * 双槽 absent 且 bootstrap=existing：{error, no_valid_slot}
%%       （上层必须走停写高水位扫描，禁止自动当 fresh）
%%     * 任一槽 corrupt 且无有效槽：{error, corrupt_no_valid}
-spec open(map()) -> {ok, store()} | {error, term()}.
open(Config) ->
    Root = maps:get(root, Config),
    CombinedNode = maps:get(combined_node, Config),
    DcBits = maps:get(dc_bits, Config),
    Bootstrap = maps:get(store_bootstrap, Config, undefined),
    case is_integer(CombinedNode) andalso CombinedNode >= 0 andalso CombinedNode =< 1023 of
        false ->
            {error, {invalid_combined_node, CombinedNode}};
        true ->
            LayoutHash = layout_hash(CombinedNode, DcBits),
            Dir = filename:join(Root, node_dir_name(CombinedNode)),
            case ensure_dir(Dir) of
                ok ->
                    case validate_no_symlink(Root, Dir) of
                        ok ->
                            open_store(Dir, CombinedNode, LayoutHash, Bootstrap);
                        {error, _} = E ->
                            E
                    end;
                {error, _} = E ->
                    E
            end
    end.

open_store(Dir, CombinedNode, LayoutHash, Bootstrap) ->
    case read_manifest(Dir) of
        {error, Reason} ->
            {error, {manifest, Reason}};
        absent ->
            case write_manifest(Dir, CombinedNode, LayoutHash) of
                ok ->
                    recover(Dir, CombinedNode, LayoutHash, Bootstrap);
                {error, _} = E ->
                    {error, {manifest_write, E}}
            end;
        {ok, Manifest} ->
            case Manifest of
                #{combined_node := CombinedNode, layout_hash := LayoutHash} ->
                    recover(Dir, CombinedNode, LayoutHash, Bootstrap);
                _ ->
                    {error,
                        {config_mismatch, #{
                            manifest => Manifest,
                            expected => #{combined_node => CombinedNode, layout_hash => LayoutHash}
                        }}}
            end
    end.

recover(Dir, CombinedNode, LayoutHash, Bootstrap) ->
    SlotA = read_slot(Dir, a, LayoutHash, CombinedNode),
    SlotB = read_slot(Dir, b, LayoutHash, CombinedNode),
    {Valid, _Corrupt, Absent} = classify([SlotA, SlotB]),
    case {Valid, Absent} of
        {[], [_, _]} ->
            case Bootstrap of
                fresh ->
                    {ok, #store{
                        dir = Dir,
                        combined_node = CombinedNode,
                        layout_hash = LayoutHash,
                        slots = {slot_state(SlotA), slot_state(SlotB)},
                        generation = 0
                    }};
                _ ->
                    %% 双槽 absent 且未显式 fresh：existing 语义必须走
                    %% 停写高水位扫描，禁止自动当 fresh
                    {error, no_valid_slot}
            end;
        {[], _} ->
            {error, corrupt_no_valid};
        {[S1], _} ->
            {ok, #store{
                dir = Dir,
                combined_node = CombinedNode,
                layout_hash = LayoutHash,
                slots = {slot_state(SlotA), slot_state(SlotB)},
                generation = S1#slot.generation
            }};
        {[S1, S2], _} ->
            case {S1#slot.generation, S2#slot.generation} of
                {G, G} when
                    S1#slot.safe_before =:= S2#slot.safe_before
                ->
                    %% 同 generation 同值：双写幂等，正常
                    {ok, #store{
                        dir = Dir,
                        combined_node = CombinedNode,
                        layout_hash = LayoutHash,
                        slots = {slot_state(SlotA), slot_state(SlotB)},
                        generation = G
                    }};
                {G, G} ->
                    {error, split_brain};
                {G1, G2} ->
                    Max = max(G1, G2),
                    {ok, #store{
                        dir = Dir,
                        combined_node = CombinedNode,
                        layout_hash = LayoutHash,
                        slots = {slot_state(SlotA), slot_state(SlotB)},
                        generation = Max
                    }}
            end
    end.

%% ===================================================================
%% 持久化
%% ===================================================================

%% @doc 写入新的 safe_before fence（无 fault injection）。
-spec persist(store(), non_neg_integer()) ->
    {ok, store()} | {error, term()}.
persist(Store, SafeBefore) ->
    persist(Store, SafeBefore, #{}).

%% @doc 写入新的 safe_before fence。
%% Opts:
%%   fault - 测试 fault injection：在指定断点 kill 当前进程（模拟
%%           kill -9），验证各断点后的恢复不变量
%%   written_at_ms - 诊断字段覆盖（测试确定性）
%%
%% 成功返回的 store 携带新 generation；失败绝不触碰已存在有效槽。
-spec persist(store(), non_neg_integer(), map()) ->
    {ok, store()} | {error, term()}.
persist(
    #store{dir = Dir, combined_node = Node, layout_hash = LH, generation = Gen} = S0,
    SafeBefore,
    Opts
) ->
    true =
        is_integer(SafeBefore) andalso SafeBefore >= 0 andalso
            SafeBefore =< ?MAX_REL_TS_PLUS_1 orelse error({invalid_safe_before, SafeBefore}),
    NewGen = Gen + 1,
    WrittenAt = maps:get(written_at_ms, Opts, os:system_time(millisecond)),
    Target = older_slot(S0),
    TargetPath = slot_path(Dir, Target),
    TempPath = temp_path(Dir, NewGen),
    maybe_fault(Opts, before_temp_create),
    case file:open(TempPath, [write, exclusive, raw]) of
        {ok, Fd} ->
            maybe_fault(Opts, after_temp_create),
            Record = encode_record(LH, Node, NewGen, SafeBefore, WrittenAt),
            case file:write(Fd, Record) of
                ok ->
                    maybe_fault(Opts, after_write),
                    case file:sync(Fd) of
                        ok ->
                            maybe_fault(Opts, after_sync),
                            case file:close(Fd) of
                                ok ->
                                    maybe_fault(Opts, after_close),
                                    persist_renamed(
                                        S0, TempPath, TargetPath, Target, NewGen, SafeBefore, Opts
                                    );
                                {error, _} = E ->
                                    _ = file:close(Fd),
                                    _ = file:delete(TempPath),
                                    {error, {close, E}}
                            end;
                        {error, _} = E ->
                            _ = file:close(Fd),
                            _ = file:delete(TempPath),
                            {error, {sync, E}}
                    end;
                {error, _} = E ->
                    _ = file:close(Fd),
                    _ = file:delete(TempPath),
                    {error, {write, E}}
            end;
        {error, _} = E ->
            %% ENOSPC/EACCES/EROFS 等：不越 fence、不碰旧槽
            {error, {temp_create, E}}
    end.

persist_renamed(S0, TempPath, TargetPath, Target, NewGen, SafeBefore, Opts) ->
    case file:change_mode(TempPath, ?FILE_MODE) of
        ok ->
            persist_rename_commit(S0, TempPath, TargetPath, Target, NewGen, SafeBefore, Opts);
        {error, _} = E ->
            _ = file:delete(TempPath),
            {error, {chmod, E}}
    end.

persist_rename_commit(S0, TempPath, TargetPath, Target, NewGen, SafeBefore, Opts) ->
    Dir = S0#store.dir,
    case file:rename(TempPath, TargetPath) of
        ok ->
            maybe_fault(Opts, after_rename),
            case dir_sync(Dir) of
                ok ->
                    maybe_fault(Opts, after_dirsync),
                    verify_readback(S0, Target, NewGen, SafeBefore, Opts);
                {error, _} = E ->
                    {error, {dir_sync, E}}
            end;
        {error, _} = E ->
            _ = file:delete(TempPath),
            {error, {rename, E}}
    end.

verify_readback(
    #store{dir = Dir, layout_hash = LH, combined_node = Node} = S0, Target, NewGen, SafeBefore, Opts
) ->
    Path = slot_path(Dir, Target),
    case file:read_file(Path) of
        {ok, Bin} ->
            maybe_fault(Opts, after_readback),
            case decode_record(Bin) of
                {ok, #{
                    layout_hash := LH,
                    combined_node := Node,
                    generation := NewGen,
                    safe_before := SafeBefore
                }} ->
                    reopen_updated(S0, Target, NewGen, SafeBefore);
                Other ->
                    {error, {readback_mismatch, Other}}
            end;
        {error, _} = E ->
            {error, {readback_read, E}}
    end.

reopen_updated(#store{slots = {A, B}} = S0, Target, NewGen, SafeBefore) ->
    New = #slot{name = Target, state = valid, generation = NewGen, safe_before = SafeBefore},
    Slots =
        case Target of
            a -> {New, B};
            b -> {A, New}
        end,
    {ok, S0#store{slots = Slots, generation = NewGen}}.

%% ===================================================================
%% 状态
%% ===================================================================

%% @doc 恢复状态摘要（不含路径等敏感信息之外的字段）
-spec status(store()) -> map().
status(#store{slots = {A, B}, generation = Gen}) ->
    #{
        generation => Gen,
        slots => #{
            a => slot_summary(A),
            b => slot_summary(B)
        },
        degraded => (A#slot.state =:= valid) xor (B#slot.state =:= valid),
        safe_before => max(A#slot.safe_before, B#slot.safe_before)
    }.

%% @doc store 目录（诊断/测试用）
-spec dir(store()) -> file:filename_all().
dir(#store{dir = D}) ->
    D.

slot_summary(#slot{state = absent}) ->
    absent;
slot_summary(#slot{state = corrupt}) ->
    corrupt;
slot_summary(#slot{state = valid, generation = G, safe_before = SB}) ->
    #{generation => G, safe_before => SB}.

%% ===================================================================
%% canonical record 编解码
%% ===================================================================

%% @doc layout_hash：绑定 EPOCH、42/10/11、combined_node、dc_bits 与
%% 编码版本。确定性 crc32 组合，无外部依赖，跨 OTP 版本稳定。
-spec layout_hash(0..1023, 0..10) -> binary().
layout_hash(CombinedNode, DcBits) ->
    EpochMs = 1735689600000,
    Bin = <<
        EpochMs:64/unsigned-big,
        42:16/unsigned-big,
        10:16/unsigned-big,
        11:16/unsigned-big,
        DcBits:16/unsigned-big,
        CombinedNode:16/unsigned-big,
        ?FORMAT_VERSION:16/unsigned-big
    >>,
    <<
        (erlang:crc32(Bin)):32/unsigned-big, (erlang:crc32(<<"imboy-tsid-layout">>)):32/unsigned-big
    >>.

%% @doc 编码 canonical record（固定 48 字节）
-spec encode_record(binary(), 0..1023, non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    binary().
encode_record(LayoutHash, CombinedNode, Generation, SafeBefore, WrittenAtMs) ->
    Body = <<
        ?MAGIC/binary,
        ?FORMAT_VERSION:16/unsigned-big,
        LayoutHash/binary,
        CombinedNode:16/unsigned-big,
        Generation:32/unsigned-big,
        SafeBefore:64/unsigned-big,
        WrittenAtMs:64/unsigned-big,
        0:32/unsigned-big
    >>,
    <<Body/binary, (erlang:crc32(Body)):32/unsigned-big>>.

%% @doc 严格解码：长度、magic、版本、crc 逐项校验；绝不掩码截断
-spec decode_record(binary()) ->
    {ok, #{
        layout_hash := binary(),
        combined_node := 0..1023,
        generation := non_neg_integer(),
        safe_before := non_neg_integer(),
        written_at_ms := non_neg_integer()
    }}
    | {error, bad_length | bad_magic | bad_version | bad_crc | bad_range}.
decode_record(Bin) when byte_size(Bin) =:= ?RECORD_SIZE ->
    <<Body:44/binary, Crc:32/unsigned-big>> = Bin,
    case erlang:crc32(Body) =:= Crc of
        false ->
            {error, bad_crc};
        true ->
            <<
                Magic:8/binary,
                Version:16/unsigned-big,
                LayoutHash:8/binary,
                CombinedNode:16/unsigned-big,
                Generation:32/unsigned-big,
                SafeBefore:64/unsigned-big,
                WrittenAt:64/unsigned-big,
                0:32/unsigned-big
            >> = Body,
            case Magic of
                ?MAGIC ->
                    case Version of
                        ?FORMAT_VERSION ->
                            case CombinedNode =< 1023 andalso SafeBefore =< ?MAX_REL_TS_PLUS_1 of
                                true ->
                                    {ok, #{
                                        layout_hash => LayoutHash,
                                        combined_node => CombinedNode,
                                        generation => Generation,
                                        safe_before => SafeBefore,
                                        written_at_ms => WrittenAt
                                    }};
                                false ->
                                    {error, bad_range}
                            end;
                        _ ->
                            {error, bad_version}
                    end;
                _ ->
                    {error, bad_magic}
            end
    end;
decode_record(_) ->
    {error, bad_length}.

%% @doc golden vector（跨实现一致性锚点）
-spec golden_vector() -> {binary(), map()}.
golden_vector() ->
    LH = layout_hash(129, 3),
    Bin = encode_record(LH, 129, 7, 4398046511103, 1735689600123),
    {Bin, #{
        layout_hash => LH,
        combined_node => 129,
        generation => 7,
        safe_before => 4398046511103,
        written_at_ms => 1735689600123
    }}.

%% ===================================================================
%% 内部
%% ===================================================================

node_dir_name(CombinedNode) ->
    lists:flatten(io_lib:format("node-~4..0B", [CombinedNode])).

slot_path(Dir, a) -> filename:join(Dir, "clock.a");
slot_path(Dir, b) -> filename:join(Dir, "clock.b").

temp_path(Dir, Gen) ->
    Unique = erlang:unique_integer([positive]),
    filename:join(Dir, io_lib:format("clock.tmp.~B.~B", [Gen, Unique])).

record_size() ->
    ?RECORD_SIZE.

slot_state(#slot{name = N, state = St, generation = G, safe_before = SB}) ->
    #slot{name = N, state = St, generation = G, safe_before = SB}.

classify(Slots) ->
    Valid = [S || #slot{state = valid} = S <- Slots],
    Corrupt = [S || #slot{state = corrupt} = S <- Slots],
    Absent = [S || #slot{state = absent} = S <- Slots],
    {Valid, Corrupt, Absent}.

read_slot(Dir, Name, LayoutHash, CombinedNode) ->
    Path = slot_path(Dir, Name),
    case file:read_file(Path) of
        {error, enoent} ->
            #slot{name = Name, state = absent};
        {ok, Bin} ->
            case decode_record(Bin) of
                {ok, #{
                    layout_hash := LayoutHash,
                    combined_node := CombinedNode,
                    generation := Gen,
                    safe_before := SB
                }} ->
                    #slot{name = Name, state = valid, generation = Gen, safe_before = SB};
                _Other ->
                    #slot{name = Name, state = corrupt}
            end;
        {error, _} ->
            %% 读失败按 corrupt 处理（保留文件取证）
            #slot{name = Name, state = corrupt}
    end.

%% 选择目标槽：无效槽优先（修复 DEGRADED），否则 generation 较旧者
older_slot(#store{slots = {A, B}}) ->
    case {A#slot.state, B#slot.state} of
        {valid, valid} ->
            case A#slot.generation =< B#slot.generation of
                true -> a;
                false -> b
            end;
        {valid, _} ->
            b;
        {_, valid} ->
            a;
        _ ->
            a
    end.

ensure_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            ok;
        false ->
            _ = filelib:ensure_dir(Dir),
            case file:make_dir(Dir) of
                ok -> ok = file:change_mode(Dir, ?DIR_MODE);
                {error, eexist} -> ok;
                {error, _} = E -> E
            end
    end.

%% 拒绝 symlink：根目录、节点目录、槽文件、manifest 任一是链接即拒绝。
%% 槽文件必须在内——read_slot 会跟随链接，替换 clock.a 指向旧
%% generation record 可把 durable floor 回退到旧 fence（审计探针实证）。
validate_no_symlink(Root, Dir) ->
    Paths = [
        Root,
        Dir,
        filename:join(Dir, "manifest"),
        slot_path(Dir, a),
        slot_path(Dir, b)
    ],
    case lists:any(fun is_symlink/1, Paths) of
        true ->
            {error, symlink_rejected};
        false ->
            ok
    end.

is_symlink(Path) ->
    case file:read_link(Path) of
        {ok, _} -> true;
        {error, _} -> false
    end.

read_manifest(Dir) ->
    Path = filename:join(Dir, "manifest"),
    case file:read_file(Path) of
        {error, enoent} ->
            absent;
        {ok, Bin} ->
            case decode_manifest(Bin) of
                {ok, M} -> {ok, M};
                {error, _} -> {error, bad_manifest}
            end;
        {error, _} = E ->
            {error, E}
    end.

decode_manifest(<<
    "IMBTSIDM",
    1:16/unsigned-big,
    LayoutHash:8/binary,
    CombinedNode:16/unsigned-big,
    Crc:32/unsigned-big
>>) when byte_size(LayoutHash) =:= 8 ->
    Body = <<"IMBTSIDM", 1:16/unsigned-big, LayoutHash/binary, CombinedNode:16/unsigned-big>>,
    case erlang:crc32(Body) =:= Crc of
        true -> {ok, #{layout_hash => LayoutHash, combined_node => CombinedNode}};
        false -> {error, bad_crc}
    end;
decode_manifest(_) ->
    {error, bad_length}.

write_manifest(Dir, CombinedNode, LayoutHash) ->
    Body = <<"IMBTSIDM", 1:16/unsigned-big, LayoutHash/binary, CombinedNode:16/unsigned-big>>,
    Bin = <<Body/binary, (erlang:crc32(Body)):32/unsigned-big>>,
    Path = filename:join(Dir, "manifest"),
    Temp = filename:join(Dir, "manifest.tmp"),
    case file:open(Temp, [write, exclusive, raw]) of
        {ok, Fd} ->
            R =
                case
                    file:write(Fd, Bin) =:= ok andalso file:sync(Fd) =:= ok andalso
                        file:close(Fd) =:= ok
                of
                    true ->
                        _ = file:change_mode(Temp, ?FILE_MODE),
                        case file:rename(Temp, Path) of
                            ok -> dir_sync(Dir);
                            {error, _} = E -> E
                        end;
                    false ->
                        _ = file:close(Fd),
                        {error, manifest_write_failed}
                end,
            case R of
                ok ->
                    ok;
                {error, _} ->
                    _ = file:delete(Temp),
                    R
            end;
        {error, _} = E ->
            E
    end.

%% 父目录 directory sync：file:open(Dir, [read, directory, raw]) + sync
%% （EXT-03 实测：macOS APFS 与 Linux 均返回 ok；NFS 等远程 FS 不可靠，
%%  目标 PVC crash 验证留待 EXT-04 真实存储门）
-spec dir_sync(file:filename_all()) -> ok | {error, term()}.
dir_sync(Dir) ->
    case file:open(Dir, [read, directory, raw]) of
        {ok, Fd} ->
            R = file:sync(Fd),
            _ = file:close(Fd),
            case R of
                ok -> ok;
                {error, _} = E -> {error, {dir_sync_failed, E}}
            end;
        {error, _} = E ->
            {error, {dir_open_failed, E}}
    end.

%% fault injection：仅在测试显式注入时 kill 当前进程（模拟 kill -9）
maybe_fault(Opts, Point) ->
    case maps:get(fault, Opts, undefined) of
        Point ->
            erlang:exit(self(), kill);
        _ ->
            ok
    end.
