%%% Owned upload bytes: bounded I/O, private inode, no pathname reopen after verification.
%%% 上传使用私有、立即 unlink 的文件；校验和转存始终复用同一打开的文件描述符。
-module(attachment_upload_spool).

-export([new/0, new/1, append/2, seal/1, close/1, size/1, sha256/1,
         read/2, read_at/3, copy_to/2, copy_to/3, with_source/2]).
-include_lib("kernel/include/file.hrl").
-define(CHUNK, 65536).

new() ->
    new(case os:getenv("TMPDIR") of false -> "/tmp"; Dir -> Dir end).

new(Base) ->
    case filelib:ensure_dir(filename:join(Base, "placeholder")) of
        ok ->
            Dir = filename:join(Base, "imboy-upload-" ++
                binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(16), lowercase))),
            case file:make_dir(Dir) of
                ok -> prepare_private_directory(Base, Dir);
                PrepareError -> PrepareError
            end;
        Error -> Error
    end.

prepare_private_directory(Base, Dir) ->
    Probe = filename:join(Dir, "probe"),
    open_exclusive(Probe, fun(Fh) ->
        Prepared = try prepare_probe(Base, Dir, Fh) after file:close(Fh) end,
        case Prepared of
            {ok, ProbeInfo, PrivateInfo} ->
                try
                    remove_owned_path(Probe, {ok, ProbeInfo}, fun file:delete/1),
                    %% A reader could retain the probe FD before chmod700.
                    %% Only a fresh inode in the protected directory receives payload.
                    %% 探针可能被其他 UID 打开；内容只写权限收紧后创建的新 inode。
                    open_private(Dir)
                after
                    remove_owned_path(Dir, {ok, PrivateInfo}, fun file:del_dir/1)
                end;
            PrepareError -> PrepareError
        end
    end).

prepare_probe(Base, Dir, Fh) ->
    {ok, #file_info{type = regular, uid = Uid} = Info} = file:read_file_info(Fh),
    %% Never chmod or clean paths before trust is established; the probe stays empty.
    %% 目录链可信前不 chmod/清路径，探针始终为空。
    case trusted_path(Base, Uid, 40) of
        ok ->
            case protect_directory(Dir, Uid) of
                {ok, DirInfo} -> {ok, Info, DirInfo};
                Error -> Error
            end;
        Error -> Error
    end.

protect_directory(Dir, Uid) ->
    case file:read_link_info(Dir) of
        {ok, #file_info{type = directory, uid = Uid} = DirInfo} ->
            case file:change_mode(Dir, 8#700) of
                ok -> {ok, DirInfo};
                Error -> Error
            end;
        _ -> {error, insecure_temporary_directory}
    end.

open_private(Dir) ->
    Path = filename:join(Dir, "body"),
    open_exclusive(Path, fun(Fh) ->
        Info = file:read_file_info(Fh),
        try
            ok = file:change_mode(Path, 8#600),
            {ok, #file_info{type = regular, mode = Mode}} = file:read_file_info(Fh),
            true = Mode band 8#777 =:= 8#600,
            ok = file:delete(Path),
            {ok, #file_info{links = 0}} = file:read_file_info(Fh),
            {ok, {upload_spool_writer, Fh}}
        catch
            Class:Reason:Stack ->
                file:close(Fh),
                erlang:raise(Class, Reason, Stack)
        after
            remove_owned_path(Path, Info, fun file:delete/1)
        end
    end).

open_exclusive(Path, Fun) ->
    %% Non-raw is intentional: httpc reads from its own process using pread/3.
    %% 不使用 raw：httpc 工作进程必须能读取此 io_device。
    case file:open(Path, [read, write, exclusive, binary]) of
        {ok, Fh} -> Fun(Fh);
        Error -> Error
    end.

%% Sticky shared roots such as /tmp are safe only when root/service-owned.
%% Trusted symlinks retain macOS /tmp and /var support; all targets are rechecked.
%% 仅信任 root/服务 UID 的目录；共享可写目录必须有 sticky 位，符号链接目标同样重验。
trusted_path(Path, Uid, LinksLeft) ->
    trusted_parts(filename:split(filename:absname(Path)), "", Uid, LinksLeft).

trusted_parts([], _, _, _) -> ok;
trusted_parts([Part | Rest], Prefix, Uid, LinksLeft) ->
    Current = case Prefix of "" -> Part; _ -> filename:join(Prefix, Part) end,
    case file:read_link_info(Current) of
        {ok, #file_info{type = directory, uid = Owner, mode = Mode}} when
            (Owner =:= Uid orelse Owner =:= 0) andalso
            (Mode band 8#022 =:= 0 orelse Mode band 8#1000 =/= 0)
        ->
            trusted_parts(Rest, Current, Uid, LinksLeft);
        {ok, #file_info{type = symlink, uid = Owner}} when
            (Owner =:= Uid orelse Owner =:= 0) andalso LinksLeft > 0
        ->
            case file:read_link(Current) of
                {ok, Target} ->
                    Absolute = filename:absname(Target, filename:dirname(Current)),
                    trusted_path(filename:join([Absolute | Rest]), Uid, LinksLeft - 1);
                Error -> Error
            end;
        {error, _} = Error -> Error;
        _ -> {error, insecure_temporary_directory}
    end.

remove_owned_path(Path, {ok, #file_info{inode = Inode, major_device = Device}}, Remove) ->
    case file:read_link_info(Path) of
        {ok, #file_info{inode = Inode, major_device = Device}} -> Remove(Path);
        _ -> ok
    end;
remove_owned_path(_, _, _) -> ok.

append({upload_spool_writer, Fh} = Writer, Bytes) when is_binary(Bytes) ->
    case write_chunks(Fh, Bytes) of
        ok -> {ok, Writer};
        Error -> Error
    end.

write_chunks(Fh, Bytes) when byte_size(Bytes) =< ?CHUNK -> file:write(Fh, Bytes);
write_chunks(Fh, <<Chunk:?CHUNK/binary, Rest/binary>>) ->
    case file:write(Fh, Chunk) of
        ok -> write_chunks(Fh, Rest);
        Error -> Error
    end.

seal({upload_spool_writer, Fh}) ->
    hash(Fh, 0, crypto:hash_init(sha256)).

hash(Fh, Offset, Hash) ->
    case file:pread(Fh, Offset, ?CHUNK) of
        {ok, Bytes} -> hash(Fh, Offset + byte_size(Bytes), crypto:hash_update(Hash, Bytes));
        eof ->
            Sha = binary:encode_hex(crypto:hash_final(Hash), lowercase),
            {ok, {upload_spool, Fh, Offset, Sha}};
        Error -> Error
    end.

size({upload_spool, _, Size, _}) -> Size.
sha256({upload_spool, _, _, Sha}) -> Sha.

close({upload_spool_writer, Fh}) -> file:close(Fh);
close({upload_spool, Fh, _, _}) -> file:close(Fh).

read(Source, Offset) -> read_at(Source, Offset, ?CHUNK).

read_at({upload_spool, Fh, Size, _}, Offset, Max) when
    is_integer(Offset), Offset >= 0, is_integer(Max), Max > 0
->
    case Offset >= Size of
        true -> eof;
        false ->
            Count = min(min(Max, ?CHUNK), Size - Offset),
            case file:pread(Fh, Offset, Count) of
                {ok, Bytes} when byte_size(Bytes) =:= Count -> {ok, Bytes};
                {error, _} = Error -> Error;
                _ -> {error, truncated_upload_spool}
            end
    end.

copy_to(Source, Fh) -> copy_to(Source, Fh, fun() -> ok end).
copy_to(Source, Fh, Check) -> copy_to(Source, Fh, Check, 0).
copy_to(Source, Fh, Check, Offset) ->
    case Check() of
        ok -> copy_chunk(Source, Fh, Check, Offset);
        Error -> Error
    end.
copy_chunk(Source, Fh, Check, Offset) ->
    case read(Source, Offset) of
        {ok, Bytes} ->
            case file:write(Fh, Bytes) of
                ok -> copy_to(Source, Fh, Check, Offset + byte_size(Bytes));
                Error -> Error
            end;
        eof -> ok;
        Error -> Error
    end.

%% Caller-owned sealed sources stay open; temporary copies close on every exit.
%% 调用方的已校验句柄不由 driver 关闭；兼容输入的私有副本在所有退出路径关闭。
with_source({upload_spool, _, _, _} = Source, Fun) -> Fun(Source);
with_source(Body, Fun) when is_binary(Body); element(1, Body) =:= file ->
    case new() of
        {ok, Writer} ->
            try
                case populate(Writer, Body) of
                    ok ->
                        case seal(Writer) of
                            {ok, Source} -> Fun(Source);
                            Error -> Error
                        end;
                    Error -> Error
                end
            after
                close(Writer)
            end;
        Error -> Error
    end;
with_source(_, _) -> {error, invalid_payload}.

populate(Writer, Bytes) when is_binary(Bytes) ->
    case append(Writer, Bytes) of {ok, _} -> ok; Error -> Error end;
populate(Writer, {file, Path}) ->
    case file:open(Path, [read, raw, binary]) of
        {ok, Input} ->
            try copy_input(Input, Writer) after file:close(Input) end;
        Error -> Error
    end;
populate(_, _) -> {error, invalid_payload}.

copy_input(Input, Writer) ->
    case file:read(Input, ?CHUNK) of
        {ok, Bytes} ->
            case append(Writer, Bytes) of
                {ok, _} -> copy_input(Input, Writer);
                Error -> Error
            end;
        eof -> ok;
        Error -> Error
    end.
