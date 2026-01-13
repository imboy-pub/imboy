-module(group_member_transfer).

-export([member_list/1]).

%% @doc 转换群组成员列表
%% 将群组成员数据进行ID编码转换
%%
%% @param Li 原始成员列表
%% @return 转换后的成员列表
%% @end
-spec member_list(list(map())) -> list(map()).
member_list(Li) ->
    [elib_hashids:replace_id(elib_hashids:replace_id(M, <<"group_id">>), <<"user_id">>) || M <- Li].
