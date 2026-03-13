-module(group_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

action_contract_contains_current_actions_test() ->
    Source = read_group_handler_source(),
    lists:foreach(fun(Action) ->
        Pattern = <<"handle_action(", Action/binary, ", Req, State) ->">>,
        ?assert(binary:match(Source, Pattern) =/= nomatch)
    end, [
        <<"face2face">>,
        <<"face2face_save">>,
        <<"add">>,
        <<"edit">>,
        <<"dissolve">>,
        <<"detail">>,
        <<"transfer">>,
        <<"page">>,
        <<"msg_page">>,
        <<"qrcode">>
    ]).

action_contract_excludes_legacy_actions_test() ->
    Source = read_group_handler_source(),
    ?assertEqual(nomatch, binary:match(Source, <<"handle_action(create, Req, State) ->">>)),
    ?assertEqual(nomatch, binary:match(Source, <<"handle_action(update, Req, State) ->">>)),
    ?assertEqual(nomatch, binary:match(Source, <<"handle_action(list, Req, State) ->">>)).

page_manager_uses_joined_query_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(req0) -> [{<<"attr">>, <<"manager">>}] end}
        ]},
        {elib_param, [
            {'page', 1, fun(req0) -> {1, 20} end}
        ]},
        {group_repo, [
            {'tablename', 0, fun() -> <<"public.group">> end}
        ]},
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"public.group_member">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(Tb, Columns, Where, Order, Page, Size) ->
                self() ! {page_query, Tb, Columns, Where, Order, Page, Size},
                {ok, #{total => 1, list => [#{<<"id">> => 1001}]}}
            end}
        ]},
        {group_logic, [
            {'group_transfer', 1, fun(Item) -> Item#{<<"converted">> => true} end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> #{response_status => 200, payload => Payload} end}
        ]}
    ], fun() ->
        Req = group_handler:handle_action(page, req0, #{current_uid => 12345}),
        Payload = maps:get(payload, Req),
        [Item] = maps:get(list, Payload),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(true, maps:get(<<"converted">>, Item)),
        receive
            {page_query, Tb, Columns, Where, Order, Page, Size} ->
                ?assertEqual(<<"public.group g LEFT JOIN public.group_member m ON g.id = m.group_id">>, Tb),
                ?assertEqual(<<"g.*">>, Columns),
                ?assertEqual(<<"g.id desc">>, Order),
                ?assertEqual(1, Page),
                ?assertEqual(20, Size),
                ?assert(maps:is_key(<<"__or">>, Where))
        after 1000 ->
            ?assert(false)
        end
    end).

page_unknown_attr_falls_back_to_owner_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(req0) -> [{<<"attr">>, <<"unknown">>}] end}
        ]},
        {elib_param, [
            {'page', 1, fun(req0) -> {2, 10} end}
        ]},
        {group_repo, [
            {'tablename', 0, fun() -> <<"public.group">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 4, fun(Tb, Where, Page, Size) ->
                self() ! {owner_query, Tb, Where, Page, Size},
                {ok, #{total => 1, list => [#{<<"id">> => 2001}]}}
            end}
        ]},
        {group_logic, [
            {'group_transfer', 1, fun(Item) -> Item#{<<"owner_view">> => true} end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> #{response_status => 200, payload => Payload} end}
        ]}
    ], fun() ->
        Req = group_handler:handle_action(page, req0, #{current_uid => 12345}),
        Payload = maps:get(payload, Req),
        [Item] = maps:get(list, Payload),
        ?assertEqual(true, maps:get(<<"owner_view">>, Item)),
        receive
            {owner_query, Tb, Where, Page, Size} ->
                ?assertEqual(<<"public.group">>, Tb),
                ?assertEqual(#{status => 1, owner_uid => 12345}, Where),
                ?assertEqual(2, Page),
                ?assertEqual(10, Size)
        after 1000 ->
            ?assert(false)
        end
    end).

msg_page_preserves_atom_list_payload_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(req0) -> [{<<"gid">>, <<"g_101">>}] end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"g_101">>) -> 101 end}
        ]},
        {group_member_repo, [
            {'find', 3, fun(101, 12345, <<"id">>) -> #{<<"id">> => 1} end}
        ]},
        {elib_param, [
            {'int', 3, fun(last_time, req0, 0) -> {ok, 0} end},
            {'page', 1, fun(req0) -> {1, 20} end}
        ]},
        {msg_c2g_repo, [
            {'tablename', 0, fun() -> <<"public.msg_c2g">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 4, fun(Tb, Where, Page, Size) ->
                self() ! {msg_query, Tb, Where, Page, Size},
                {ok, #{total => 1, list => [#{<<"msg_id">> => <<"m1">>}]}}
            end}
        ]},
        {group_logic, [
            {'group_transfer', 1, fun(Item) -> Item#{<<"converted">> => true} end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> #{response_status => 200, payload => Payload} end}
        ]}
    ], fun() ->
        Req = group_handler:handle_action(msg_page, req0, #{current_uid => 12345}),
        Payload = maps:get(payload, Req),
        [Item] = maps:get(list, Payload),
        ?assertEqual(true, maps:get(<<"converted">>, Item)),
        receive
            {msg_query, Tb, Where, Page, Size} ->
                ?assertEqual(<<"public.msg_c2g">>, Tb),
                ?assertEqual(#{to_groupid => 101}, Where),
                ?assertEqual(1, Page),
                ?assertEqual(20, Size)
        after 1000 ->
            ?assert(false)
        end
    end).

read_group_handler_source() ->
    {ok, Bin} = file:read_file("src/api/group_handler.erl"),
    Bin.
