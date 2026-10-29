%%% 固定 registry root；同物理对象的写/删由系统内核锁串行化。
-module(attachment_storage_disk).
-behaviour(attachment_storage_driver).
-export([
    put/4,
    put_ctx/5,
    head/3,
    head_bounded/3,
    copy_dispatch/4,
    delete/3,
    presign_put/4,
    presign_get/4,
    read/3,
    put_snapshot/3,
    put_snapshot/4,
    read_snapshot/1,
    head_snapshot/1,
    fold_snapshot/3,
    fold_range_snapshot/6,
    delete_snapshot/1
]).
-include_lib("kernel/include/file.hrl").

put(Ns, Container, Key, Body) ->
    put_ctx(Ns, Container, Key, Body, infinity).
put_ctx(Ns, Container, Key, Body, Expiry) ->
    Deadline = erlang:monotonic_time(millisecond) + 30000,
    Check = fun() -> write_window(Expiry, Deadline) end,
    attachment_upload_spool:with_source(Body, fun(Source) ->
        with_path(Ns, Container, Key, fun(Root, Path) ->
            with_locked_action(Root, Container, Key, Path, fun() ->
                atomic_write(Path, Source, Check)
            end)
        end)
    end).

put_snapshot(I, Body, Expiry) ->
    put_snapshot(I, Body, Expiry, fun() -> ok end).

%% Acquire the physical-object lock before the short live DB recheck. The callback
%% must finish its transaction before returning; no DB lock spans filesystem I/O.
%% 先取物理对象 OS 锁，再短事务复核；回调返回前必须结束事务。
put_snapshot(#{container := C, key := K} = I, Body, Expiry, Recheck) when
    is_function(Recheck, 0)
->
    Deadline = erlang:monotonic_time(millisecond) + 30000,
    Check = fun() -> write_window(Expiry, Deadline) end,
    attachment_upload_spool:with_source(Body, fun(Source) ->
        with_snapshot_path(I, fun(Root, Path) ->
            with_locked_action(Root, C, K, Path, fun() ->
                rechecked_write(Path, Source, Check, Recheck)
            end)
        end)
    end).

with_locked_action(Root, Container, Key, Path, Fun) ->
    with_os_lock(Root, Container, Key, fun() -> locked_action(Root, Path, Fun) end).

rechecked_write(Path, Source, Check, Recheck) ->
    case Check() of
        ok ->
            case Recheck() of
                ok -> atomic_write(Path, Source, Check);
                Error -> Error
            end;
        Error ->
            Error
    end.

write_window(Expiry, Deadline) ->
    case Expiry =:= infinity orelse erlang:system_time(second) < Expiry of
        false ->
            {error, invalid_signature_context};
        true ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true -> ok;
                false -> {error, timeout}
            end
    end.

delete_snapshot(#{container := C, key := K} = I) ->
    with_snapshot_path(I, fun(Root, Path) ->
        with_os_lock(Root, C, K, fun() ->
            locked_action(Root, Path, fun() ->
                case file:delete(Path) of
                    {error, enoent} -> {error, not_found};
                    E -> E
                end
            end)
        end)
    end).
read_snapshot(I) ->
    with_snapshot_path(I, fun(_, Path) ->
        case file:read_file(Path) of
            {error, enoent} -> {error, not_found};
            E -> E
        end
    end).
%% O(1) 元数据 stat（W2-02 confirm 锁后复核用）：与 read_snapshot 同一冻结
%% 身份 I（with_snapshot_path 含 no_symlinks 复验）。不进 OS 锁——调用方
%% confirm 已持 DB pin 行锁，OS 锁会形成 DB→OS 嵌套，与 put_snapshot 的
%% recheck 路径（OS 锁内开短事务）构成潜在死锁对；read_link_info 单次读
%% inode 元数据无撕裂，威胁模型与 read_snapshot 一致。read_link_info 不
%% 跟随符号链接——与既有 head/3 同语义（等价 read_file_info 且更严）；
%% mtime 取 posix 秒与 garage HEAD 的 last-modified 同刻度。disk 无 etag
%% 概念，恒 undefined，对照层按 size+mtime 判据执行。
head_snapshot(I) ->
    with_snapshot_path(I, fun(_, Path) ->
        case file:read_link_info(Path, [{time, posix}]) of
            {ok, #file_info{type = regular, size = Size, mtime = Mtime}} when
                is_integer(Size), Size >= 0, is_integer(Mtime)
            ->
                {ok, #{size => Size, mtime => Mtime, etag => undefined}};
            {ok, #file_info{}} ->
                {error, path_escape};
            {error, enoent} ->
                {error, not_found};
            {error, R} ->
                {error, R};
            _ ->
                {error, path_escape}
        end
    end).
%% Read one already-verified physical inode in bounded chunks under its storage lock.
fold_snapshot(#{container := C, key := K} = I, Fun, Acc) ->
    with_snapshot_path(I, fun(Root, Path) ->
        with_locked_action(Root, C, K, Path, fun() ->
            case file:open(Path, [read, raw, binary]) of
                {ok, Fh} ->
                    try
                        fold_file(Fh, Fun, Acc)
                    after
                        file:close(Fh)
                    end;
                {error, enoent} ->
                    {error, not_found};
                Error ->
                    Error
            end
        end)
    end).
fold_range_snapshot(#{container := C, key := K} = I, Offset, Length, ExpectedSize, Fun, Acc) ->
    with_snapshot_path(I, fun(Root, Path) ->
        with_locked_action(Root, C, K, Path, fun() ->
            case file:open(Path, [read, raw, binary]) of
                {ok, Fh} ->
                    try
                        case file:read_file_info(Fh) of
                            {ok, #file_info{type = regular, size = ExpectedSize}} ->
                                fold_range(Fh, Offset, Length, Fun, Acc);
                            _ ->
                                {error, artifact_integrity_mismatch}
                        end
                    after
                        file:close(Fh)
                    end;
                Error ->
                    Error
            end
        end)
    end).
fold_range(_, _, 0, _, Acc) ->
    {ok, Acc};
fold_range(Fh, Offset, Left, Fun, Acc) ->
    N = min(65536, Left),
    case file:pread(Fh, Offset, N) of
        {ok, B} when byte_size(B) =:= N ->
            case Fun(B, Acc) of
                {ok, Next} -> fold_range(Fh, Offset + N, Left - N, Fun, Next);
                Error -> Error
            end;
        _ ->
            {error, artifact_integrity_mismatch}
    end.

fold_file(Fh, Fun, Acc) ->
    case file:read(Fh, 65536) of
        eof ->
            {ok, Acc};
        {ok, Bytes} ->
            case Fun(Bytes, Acc) of
                {ok, Next} -> fold_file(Fh, Fun, Next);
                Error -> Error
            end;
        Error ->
            Error
    end.

with_snapshot_path(#{endpoint := <<"file://", RootBin/binary>>, container := C, key := K}, Fun) ->
    Root = binary_to_list(RootBin),
    case filename:pathtype(Root) =:= absolute andalso safe_segments(C) andalso safe_segments(K) of
        true ->
            Path = filename:join([Root, binary_to_list(C), binary_to_list(K)]),
            case no_symlinks(Path) of
                ok -> Fun(Root, Path);
                E -> E
            end;
        false ->
            {error, invalid_path}
    end;
with_snapshot_path(_, _) ->
    {error, location_unverified}.

%% 等锁期间路径可能变化，锁内必须重验。根及子目录不可对组/其他用户开放写入。
%% 同 UID 任意文件系统 writer 可在检查后改路径，不属隔离边界；根须归专用服务 UID。
locked_action(Root, Path, Fun) ->
    case locked_path_check(Root, Path) of
        ok -> Fun();
        E -> E
    end.
locked_path_check(Root, Path) ->
    case no_symlinks(Path) of
        ok -> check_directory_permissions(Root, filename:dirname(Path));
        E -> E
    end.
check_directory_permissions(Root, Path) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = directory, mode = Mode}} when Mode band 8#022 =:= 0 ->
            case Path =:= Root of
                true -> ok;
                false -> check_directory_permissions(Root, filename:dirname(Path))
            end;
        {error, enoent} ->
            case Path =:= Root of
                true -> ok;
                false -> check_directory_permissions(Root, filename:dirname(Path))
            end;
        _ ->
            {error, insecure_directory}
    end.

atomic_write(Path, Source, Check) ->
    case Check() of
        ok -> atomic_write_open(Path, Source, Check);
        Error -> Error
    end.

atomic_write_open(Path, Source, Check) ->
    ok = filelib:ensure_dir(Path),
    Tmp = Path ++ ".v5tmp." ++ binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12))),
    case file:open(Tmp, [write, exclusive, raw, binary]) of
        {ok, Fh} ->
            try
                Result =
                    try
                        ok = file:change_mode(Tmp, 8#600),
                        case attachment_upload_spool:copy_to(Source, Fh, Check) of
                            ok -> file:sync(Fh);
                            Error -> Error
                        end
                    after
                        file:close(Fh)
                    end,
                case Result of
                    ok ->
                        case Check() of
                            ok -> file:rename(Tmp, Path);
                            Error2 -> Error2
                        end;
                    Error2 ->
                        Error2
                end
            after
                file:delete(Tmp)
            end;
        Error ->
            Error
    end.

head(Ns, Container, Key) ->
    with_path(Ns, Container, Key, fun(_, Path) ->
        case file:read_link_info(Path) of
            {ok, #file_info{type = regular, size = Size}} ->
                {ok, #{size => Size, content_type => <<"application/octet-stream">>}};
            {error, enoent} ->
                {error, not_found};
            {error, R} ->
                {error, R};
            _ ->
                {error, path_escape}
        end
    end).
%% direct-v1 控制面：disk 是历史兼容 driver（新 Disk 数据面已移除），
%% 真 HEAD 复用既有元数据语义；copy 一次 file copy，无重试。
head_bounded(Ns, Container, Key) ->
    head(Ns, Container, Key).

copy_dispatch(Ns, Container, SourceKey, TargetKey) ->
    with_path(Ns, Container, TargetKey, fun(Root, TargetPath) ->
        with_path(Ns, Container, SourceKey, fun(_, SourcePath) ->
            with_os_lock(Root, Container, TargetKey, fun() ->
                case file:read_file_info(SourcePath) of
                    {ok, #file_info{type = regular}} ->
                        case filelib:ensure_dir(TargetPath) of
                            ok ->
                                %% 返回形态对齐 driver callback（{ok, map()}）
                                case file:copy(SourcePath, TargetPath) of
                                    {ok, N} -> {ok, #{bytes_copied => N}};
                                    {error, R} -> {error, R}
                                end;
                            {error, R} ->
                                {error, R}
                        end;
                    {error, enoent} ->
                        {error, not_found};
                    {error, R} ->
                        {error, R};
                    _ ->
                        {error, path_escape}
                end
            end)
        end)
    end).

read(Ns, Container, Key) ->
    with_path(Ns, Container, Key, fun(_, Path) ->
        case file:read_file(Path) of
            {error, enoent} -> {error, not_found};
            E -> E
        end
    end).
delete(Ns, Container, Key) ->
    with_path(Ns, Container, Key, fun(Root, Path) ->
        with_os_lock(Root, Container, Key, fun() ->
            locked_action(Root, Path, fun() ->
                case file:delete(Path) of
                    {error, enoent} -> {error, not_found};
                    E -> E
                end
            end)
        end)
    end).

%% Disk 能力必须指向应用网关，driver 不产生伪 disk://能力。
presign_put(_, _, _, _) -> {error, use_controlled_gateway}.
presign_get(_, _, _, _) -> {error, use_authorized_gateway}.

with_path(Ns, Container, Key, Fun) ->
    case attachment_storage:resolve(Ns) of
        {ok, #{driver := <<"disk">>, endpoint := <<"file://", RootBin/binary>>}} ->
            Root = binary_to_list(RootBin),
            case
                filename:pathtype(Root) =:= absolute andalso safe_segments(Container) andalso
                    safe_segments(Key)
            of
                true ->
                    Path = filename:join([Root, binary_to_list(Container), binary_to_list(Key)]),
                    case no_symlinks(Path) of
                        ok -> Fun(Root, Path);
                        E -> E
                    end;
                false ->
                    {error, invalid_path}
            end;
        _ ->
            {error, location_unverified}
    end.
safe_segments(B) when is_binary(B), byte_size(B) > 0 ->
    lists:all(
        fun(S) ->
            S =/= <<>> andalso S =/= <<".">> andalso S =/= <<"..">> andalso
                lists:all(fun(C) -> C >= 32 andalso C =/= 127 end, binary_to_list(S))
        end,
        binary:split(B, <<"/">>, [global])
    );
safe_segments(_) ->
    false.

no_symlinks(Path) -> check_parts(filename:split(filename:absname(Path)), "").
check_parts([], _) ->
    ok;
check_parts([Part | Rest], Prefix) ->
    Current =
        case Prefix of
            "" -> Part;
            _ -> filename:join(Prefix, Part)
        end,
    case file:read_link_info(Current) of
        {ok, #file_info{type = symlink}} -> {error, path_escape};
        {ok, #file_info{type = directory}} -> check_parts(Rest, Current);
        {ok, #file_info{type = regular}} when Rest =:= [] -> ok;
        {ok, _} -> {error, invalid_path};
        {error, enoent} -> check_parts(Rest, Current);
        {error, R} -> {error, R}
    end.

%% 锁文件不删除；端口 stdin EOF（包含 BEAM 崩溃）使固定 read 命令退出释放内核锁。
with_os_lock(Root, Container, Key, Fun) ->
    LockDir = filename:join(Root, ".v5-locks"),
    Digest = binary_to_list(
        binary:encode_hex(crypto:hash(sha256, term_to_binary({Container, Key})), lowercase)
    ),
    Lock = filename:join(LockDir, Digest),
    case no_symlinks(Lock) of
        ok ->
            ok = filelib:ensure_dir(Lock),
            ok = file:change_mode(LockDir, 8#700),
            case lock_command(Lock) of
                {ok, Exe, Args} -> hold_lock(Exe, Args, Fun);
                E -> E
            end;
        E ->
            E
    end.
lock_command(Lock) ->
    Script = "printf 'V5_LOCK_READY\\n'; IFS= read -r release",
    case os:find_executable("lockf") of
        false ->
            case os:find_executable("flock") of
                false -> {error, os_lock_unavailable};
                Exe -> {ok, Exe, ["-x", "-w", "5", Lock, "/bin/sh", "-c", Script]}
            end;
        Exe ->
            {ok, Exe, ["-k", "-t", "5", Lock, "/bin/sh", "-c", Script]}
    end.
hold_lock(Exe, Args, Fun) ->
    Port = open_port({spawn_executable, Exe}, [
        binary, exit_status, use_stdio, stderr_to_stdout, {line, 1024}, {args, Args}
    ]),
    receive
        {Port, {data, {eol, <<"V5_LOCK_READY">>}}} ->
            try
                Fun()
            after
                release_lock(Port)
            end;
        {Port, {exit_status, _}} ->
            {error, os_lock_failed};
        {Port, {data, _}} ->
            port_close(Port),
            {error, os_lock_failed}
    after 7000 ->
        port_close(Port),
        {error, os_lock_timeout}
    end.
release_lock(Port) ->
    try
        port_command(Port, <<"release\n">>)
    catch
        error:badarg -> ok
    end,
    receive
        {Port, {exit_status, _}} -> ok
    after 2000 ->
        try
            port_close(Port)
        catch
            error:badarg -> ok
        end,
        ok
    end.
