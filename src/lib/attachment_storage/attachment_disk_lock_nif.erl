-module(attachment_disk_lock_nif).
-on_load(init/0).
-export([acquire/2, release/1, rename/3, unlink/2]).

init() ->
    case code:priv_dir(imboy) of
        {error, _} -> {error, no_priv_dir};
        Dir -> erlang:load_nif(filename:join(Dir, "attachment_disk_lock_nif"), 0)
    end.

acquire(_, _) -> erlang:nif_error(nif_not_loaded).
release(_) -> erlang:nif_error(nif_not_loaded).
rename(_, _, _) -> erlang:nif_error(nif_not_loaded).
unlink(_, _) -> erlang:nif_error(nif_not_loaded).
