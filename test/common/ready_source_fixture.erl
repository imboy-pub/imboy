-module(ready_source_fixture).
-export([snapshot/6, finish/3]).
-include("imboy_cache_sync_state.hrl").

snapshot(Peer, Boot, Epoch, Seq, Entries, Round) ->
    Saved = ets:tab2list(imboy_cache_meta),
    Values = [{K, depcache:get(K, imboy_cache)} || {K, _, _, _} <- Entries],
    try
        ets:delete_all_objects(imboy_cache_meta),
        ets:insert(imboy_cache_meta, [
            {status, ready},
            {node_boot, {Peer, Boot}},
            {sync_epoch, Epoch},
            {sync_origin_seq, Seq},
            {sync_counter, 1000}
        ]),
        lists:foreach(
            fun({Key, Value, Exp, Rev}) ->
                ok = imboy_cache_sync_apply:store_revision(
                    Key, Rev, Exp, live, Epoch, {Peer, Boot, Seq}
                ),
                ok = imboy_cache_sync_apply:index_replicate(Key),
                ok = depcache:set(Key, Value, 60, [], imboy_cache)
            end,
            Entries
        ),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        Page =
            case Entries of
                [] ->
                    [];
                _ ->
                    receive
                        {cache_snapshot_page, _, Round, 1, Wire, _, false} -> Wire
                    after 1000 -> error(no_source_page)
                    end
            end,
        Cut =
            receive
                {cache_snapshot_cut, Peer, Round, _, _, _, _, _, _, _} = F -> F
            after 1000 -> error(no_source_cut)
            end,
        Done =
            receive
                {cache_snapshot_done, _, Round, Watermark, Total} -> {Watermark, Total}
            after 1000 -> error(no_source_done)
            end,
        {Page, Cut, Done, imboy_cache_sync_coverage:content_census()}
    after
        ets:delete_all_objects(imboy_cache_meta),
        ets:insert(imboy_cache_meta, Saved),
        lists:foreach(
            fun
                ({K, {ok, V}}) -> depcache:set(K, V, 60, [], imboy_cache);
                ({K, undefined}) -> depcache:flush(K, imboy_cache)
            end,
            Values
        ),
        receive
            {cache_invalidation_watermark, Peer, _, _, _, _} -> ok
        after 0 -> ok
        end
    end.

finish(Peer, {Cut, {Watermark, Total}}, R) ->
    Peers = maps:get(peers, R),
    P = maps:get(Peer, Peers),
    {cache_snapshot_cut, Peer, _, Boot, Epoch, Seq, _, _, _, _} = Cut,
    R0 = R#{
        peers := Peers#{
            Peer := P#{
                source_pid => self(),
                origin_target => {Peer, Boot, Epoch, Seq}
            }
        }
    },
    {ok, R1} = imboy_cache_sync_recover:accept_source_cut(Cut, R0),
    imboy_cache_sync_snapshot:handle_done(Peer, maps:get(round, R), Watermark, Total, R1).
