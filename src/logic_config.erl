-module(logic_config).
%%%
%  config 业务逻辑模块
%%%
-export([get/1]).
-export([get/2]).


get(Key) ->
    get(Key, "").


get(Key, Defalut) ->
    case repo_config:get_by_key(Key) of
        {ok, _FieldList, []} ->
            Defalut;
        {ok, _FieldList, [[Val]]} ->
            Val
    end.
