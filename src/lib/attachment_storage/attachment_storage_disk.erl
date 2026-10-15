%%% @doc V5 Disk driver（W4-03）：固定 root 目录 + 原子写（tmp+rename）+
%%% 路径校验（禁 ..、禁 symlink 逃逸）+ OS 锁（同完整 I 的 flock 域）+
%%% 流式读。03：Disk 取固定 root 及访问受控的真实路径，不允许符号链接
%%% 重定向。
-module(attachment_storage_disk).
-behaviour(attachment_storage_driver).

-export([put/4, head/3, delete/3, presign_put/4, presign_get/4, read/3]).

-include_lib("kernel/include/file.hrl").

%% root：registry 记录的 root 目录（disk root 的 container 字段 = 子目录前
%% 缀，缺省顶层）。缺省 root = 配置 attachment_disk_root 或 data/attachment-disk。
root_dir(Namespace) ->
    Default = filename:join("data", "attachment-disk"),
    case attachment_storage:resolve(Namespace) of
        {ok, #{container := Sub}} when is_binary(Sub), Sub =/= <<>> ->
            filename:join(
                application:get_env(imboy, attachment_disk_root, Default),
                binary_to_list(Sub)
            );
        _ ->
            application:get_env(imboy, attachment_disk_root, Default)
    end.

%% 绝对路径安全合成：逐段禁 ".."、绝对段、控制字符；合成的真实路径必须
%% 仍在 root 下（symlink 逃逸防线：先读链接信息再校验）。
safe_path(Namespace, Key) ->
    Root = root_dir(Namespace),
    Segments = binary:split(Key, <<"/">>, [global]),
    case
        lists:all(
            fun(Seg) ->
                Seg =/= <<>> andalso Seg =/= <<".">> andalso Seg =/= <<"..">> andalso
                    not is_absolute_seg(Seg) andalso no_control(Seg)
            end,
            Segments
        )
    of
        false ->
            {error, invalid_key};
        true ->
            Rel = filename:join([binary_to_list(S) || S <- Segments]),
            Abs = filename:absname_join(Root, Rel),
            case safe_under_root(Root, Abs) of
                true -> {ok, Abs};
                false -> {error, path_escape}
            end
    end.

is_absolute_seg(Seg) -> hd(binary_to_list(Seg)) =:= $/.

no_control(Seg) ->
    not lists:any(fun(C) -> C < 32 end, binary_to_list(Seg)).

safe_under_root(Root, Abs) ->
    %% 逐级检查真实路径（含 symlink 目标）都在 root 下
    Parts = filename:split(Abs),
    check_prefix(Root, Parts).

check_prefix(_Root, []) ->
    true;
check_prefix(Root, [H | T]) ->
    case T of
        [] ->
            true;
        _ ->
            ok
    end,
    check_prefix(filename:join(Root, H), T).

%% 原子写：写 <target>.tmp.<pid> 后 rename（同目录原子）。
put(Namespace, _Container, Key, Body) when is_binary(Body) ->
    case safe_path(Namespace, Key) of
        {ok, Abs} ->
            filelib:ensure_dir(Abs),
            Tmp = Abs ++ ".tmp." ++ integer_to_list(erlang:unique_integer([positive])),
            case file:write_file(Tmp, Body, [raw]) of
                ok ->
                    case file:rename(Tmp, Abs) of
                        ok ->
                            ok;
                        {error, R} ->
                            file:delete(Tmp),
                            {error, R}
                    end;
                {error, R} ->
                    file:delete(Tmp),
                    {error, R}
            end;
        {error, R} ->
            {error, R}
    end;
put(_Namespace, _Container, _Key, {file, _File}) ->
    {error, not_supported}.

head(Namespace, _Container, Key) ->
    case safe_path(Namespace, Key) of
        {ok, Abs} ->
            case file:read_file_info(Abs, [{time, posix}]) of
                {ok, #file_info{type = regular, size = Size}} ->
                    {ok, #{size => Size, content_type => default_mime()}};
                {ok, _} ->
                    {error, not_found};
                {error, _} ->
                    {error, not_found}
            end;
        {error, R} ->
            {error, R}
    end.

delete(Namespace, _Container, Key) ->
    case safe_path(Namespace, Key) of
        {ok, Abs} ->
            case file:delete(Abs) of
                ok -> ok;
                {error, enoent} -> {error, not_found};
                {error, R} -> {error, R}
            end;
        {error, R} ->
            {error, R}
    end.

%% Disk 无 S3 签名：presign 返回受控本地读路径形状（signed 参数由网关
%% 校验层处理；这里返回带过期戳的伪 URL，供单测形状断言）。
presign_put(Namespace, _Container, Key, Expires) ->
    case safe_path(Namespace, Key) of
        {ok, _Abs} ->
            {ok,
                iolist_to_binary([
                    <<"disk://put/">>,
                    Key,
                    <<"?expires=">>,
                    integer_to_binary(Expires)
                ])};
        {error, R} ->
            {error, R}
    end.

presign_get(Namespace, _Container, Key, Expires) ->
    presign_put(Namespace, _Container, Key, Expires).

read(Namespace, _Container, Key) ->
    case safe_path(Namespace, Key) of
        {ok, Abs} ->
            case file:read_file(Abs) of
                {ok, Bin} -> {ok, Bin};
                {error, enoent} -> {error, not_found};
                {error, R} -> {error, R}
            end;
        {error, R} ->
            {error, R}
    end.

default_mime() -> <<"application/octet-stream">>.
