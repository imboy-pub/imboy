%%% @doc CSB-02R 批量摘要读面的鸭子型 store fake（仅实现被测 callback；
%%% 不声明 eb_store_port behaviour——测试替身不参与 port 契约冻结检查）。
-module(eb_fake_preview_store).

-export([
    init/0,
    set_rows/1,
    set_error/1,
    fetch_message_rows_by_ids/2
]).

init() ->
    persistent_term:erase({?MODULE, rows}),
    persistent_term:erase({?MODULE, error}).

set_rows(Rows) ->
    ById = maps:from_list([{maps:get(id, R), R} || R <- Rows]),
    persistent_term:put({?MODULE, rows}, ById).

set_error(Error) ->
    persistent_term:put({?MODULE, error}, Error).

fetch_message_rows_by_ids(_OrgId, Ids) ->
    case persistent_term:get({?MODULE, error}, undefined) of
        undefined ->
            All = persistent_term:get({?MODULE, rows}, #{}),
            {ok, [maps:get(Id, All) || Id <- Ids, maps:is_key(Id, All)]};
        Error ->
            Error
    end.
