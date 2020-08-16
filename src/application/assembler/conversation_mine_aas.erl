-module (conversation_mine_aas).
%%%
% conversation_mine_aas 是 conversation_mine application assembler 缩写
%%%
-export ([data/1]).

%%%
% 我的会话记录
%%%
data(List) ->
    % msg_md5 传递过去，避免APP重新计算消耗CPU
    [
        [{<<"msg_md5">>, proplists:get_value(<<"msg_md5">>, Msg)} | jsx:decode(proplists:get_value(<<"payload">>, Msg))]
    || Msg <- List].
