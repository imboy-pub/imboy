-module(group_member_transfer).

-export([member_list/1]).

%% P1-7b 注：本模块当前为恒等式 transfer（仅做 ID 字段整数校验，不移除字段）。
%% 防御深度由 SQL 层显式列（group_member_ds:list_member/2、
%% group_member_repo 等）保证；表新增敏感列时应在 SQL/Repo 层剔除外，
%% 而不是在这里剥离（避免破坏 member_list 的"透传其他字段"契约）。
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
