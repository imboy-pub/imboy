-module (config_as).
%%%
% config_as 是 config application service 缩写
%%%
-export ([get/1]).
-export ([get/2]).

get(Key) ->
    get(Key, "").

get(Key, Defalut) ->
    case config_repo:get_by_key(Key) of
        {ok, _FieldList, []} ->
            Defalut;
        {ok, _FieldList, [[Val]]}->
            Val
    end.
