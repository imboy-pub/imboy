-module (conversation_mine_aas).
%%%
% conversation_mine_aas 是 conversation_mine application assembler 缩写
%%%
-export ([data/1]).

%%%
% 我的会话记录
%%%
data(List) ->
    [
        [
          {<<"id">>, proplists:get_value(<<"id">>, Msg)}
          | jsx:decode(proplists:get_value(<<"payload">>, Msg), [{return_maps, false}])
        ]
        || Msg <- List
    ].
