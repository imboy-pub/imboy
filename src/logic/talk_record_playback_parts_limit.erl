-module(talk_record_playback_parts_limit).
-moduledoc "Bound concurrent source-part spools on this node.".
-export([with_slot/1]).

-define(SLOTS, 4).

with_slot(Fun) ->
    case acquire(1) of
        {ok, Lock} ->
            try
                Fun()
            after
                global:del_lock(Lock, [node()])
            end;
        busy ->
            {error, playback_busy}
    end.

acquire(N) when N =< ?SLOTS ->
    Lock = {{?MODULE, N}, self()},
    case global:set_lock(Lock, [node()], 0) of
        true -> {ok, Lock};
        false -> acquire(N + 1)
    end;
acquire(_) ->
    busy.
