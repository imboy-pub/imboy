

map_fold_test() ->
    Map = #{"k1" => 1, "k2" => 2, "k3" => 3},
    maps:fold(fun(K,V, Li) ->
        [{1, K, V} | Li]
    end, [], Map)
 ok.

map_fold_test2() ->
    Map = #{"k1" => 1, "k2" => 2, "k3" => 3},
    C = 3,
    maps:fold(fun(K,V, Li) ->
        case V of
            C ->
                Li;
            _ ->
                [{1, K, V} | Li]
        end
    end, [], Map)
 ok.


map_fold0_test() ->
    maps:fold(fun(K,V, Li) -> [{1, K, V} | Li] end, [], #{})
 ok.
