-module(attachment_disk_lock_nif).
-on_load(init/0).
-export([acquire/2, release/1, rename/3, unlink/2]).

init() ->
    case priv_dir() of
        {error, _} = E -> E;
        Dir -> erlang:load_nif(filename:join(Dir, "attachment_disk_lock_nif"), 0)
    end.

%% code:priv_dir(imboy) 只在 release/boot 形态可解析；-pa ebin 的裸 VM（含测试
%% holder 子 VM）即使 application:load 后仍 bad_name（app controller 无 directory
%% 记录）。此时从本模块 beam 位置推导：ebin 的上级即仓库根，so 在同级 priv/ 下。
priv_dir() ->
    case code:priv_dir(imboy) of
        {error, _} ->
            case code:which(?MODULE) of
                Beam when is_list(Beam) ->
                    filename:join(filename:dirname(filename:dirname(Beam)), "priv");
                _ ->
                    {error, no_priv_dir}
            end;
        Dir ->
            Dir
    end.

acquire(_, _) -> erlang:nif_error(nif_not_loaded).
release(_) -> erlang:nif_error(nif_not_loaded).
rename(_, _, _) -> erlang:nif_error(nif_not_loaded).
unlink(_, _) -> erlang:nif_error(nif_not_loaded).
