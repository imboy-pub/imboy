-module(mention_handler_contract_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% mention_handler 契约测试（无外部依赖）
%%%
%%% 目标：
%%% - 保证 action 分发与路由一致
%%% - 保证 App 兼容参数（group_id/mention_id/all/items）不被回退删除
%%%===================================================================

action_contract_contains_actions_test() ->
    Source = read_mention_handler_source(),
    lists:foreach(fun(Action) ->
        Pattern = <<Action/binary, ", Req, State) ->">>,
        ?assert(binary:match(Source, Pattern) =/= nomatch)
    end, [
        <<"list">>,
        <<"unread">>,
        <<"mark_read">>,
        <<"suggest">>
    ]).

list_mentions_supports_group_filter_compat_test() ->
    Source = read_mention_handler_source(),
    ?assert(binary:match(Source, <<"get_param([<<\"gid\">>, <<\"group_id\">>]">>) =/= nomatch).

mark_read_supports_mention_id_and_all_test() ->
    Source = read_mention_handler_source(),
    ?assert(binary:match(Source, <<"<<\"mention_id\">>">>) =/= nomatch),
    ?assert(binary:match(Source, <<"<<\"all\">>">>) =/= nomatch).

response_contains_items_alias_test() ->
    Source = read_mention_handler_source(),
    ?assert(binary:match(Source, <<"<<\"items\">>">>) =/= nomatch).

read_mention_handler_source() ->
    {ok, Bin} = file:read_file("src/api/mention_handler.erl"),
    Bin.
