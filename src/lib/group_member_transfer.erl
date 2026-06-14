-module(group_member_transfer).

-export([member_list/1]).

%% @doc 转换群组成员列表中的 ID 字段（TSID 大整数保持 integer 传输）
-spec member_list(list(map())) -> list(map()).
member_list(Li) ->
    [convert_member_ids(M) || M <- Li].

-spec convert_member_ids(map()) -> map().
convert_member_ids(M) ->
    Fields = [<<"id">>, <<"user_id">>, <<"group_id">>],
    lists:foldl(
        fun(Field, Acc) ->
            case maps:find(Field, Acc) of
                {ok, V} when is_integer(V) ->
                    maps:put(Field, V, Acc);
                _ ->
                    Acc
            end
        end,
        M,
        Fields
    ).
