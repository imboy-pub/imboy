-module (group_member_aas).
%%%
% group_member_aas 是 group_member application assembler 缩写
%%%

-export ([data/1]).

data(Members) ->
    [{<<"list">>, [hashids_tlt:replace_id(M) || M <- Members]}].
