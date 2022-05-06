-module(aas_group_member).
%%%
% aas_group_member 是 group_member application assembler 缩写
%%%

-export([data/1]).


data(Members) ->
    [{<<"list">>, [hashids_translator:replace_id(M) || M <- Members]}].
