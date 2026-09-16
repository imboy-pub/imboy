%%% @doc cs_id_port 的 fake 实现（test-only）：进程内递增计数，确定性。
-module(cs_fake_id).

-export([new_id/1, reset/0]).

-define(TAB, cs_fake_id_tab).

new_id(_Kind) ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, public]);
        _ -> ok
    end,
    [{n, N}] = ets:lookup(?TAB, n),
    ets:insert(?TAB, {n, N + 1}),
    900000000000000 + N.

reset() ->
    catch ets:delete(?TAB),
    ets:new(?TAB, [named_table, public]),
    ets:insert(?TAB, {n, 1}),
    ok.
