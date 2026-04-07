-module(group_member_transfer).

-export([member_list/1]).

%% @doc 转换群组成员列表
%% 将群组成员数据进行ID编码转换
%% TSID 大整数超过 JS 安全范围，所有 ID 必须转为 binary 字符串
%%
%% @param Li 原始成员列表
%% @return 转换后的成员列表
%% @end
-spec member_list(list(map())) -> list(map()).
member_list(Li) ->
    [convert_member_ids(M) || M <- Li].

-spec convert_member_ids(map()) -> map().
convert_member_ids(M) ->
    Fields = [<<"id">>, <<"user_id">>, <<"group_id">>],
    lists:foldl(fun(Field, Acc) ->
        case maps:find(Field, Acc) of
            {ok, V} when is_integer(V) ->
                maps:put(Field, V, Acc);
            _ ->
                Acc
        end
    end, M, Fields).
