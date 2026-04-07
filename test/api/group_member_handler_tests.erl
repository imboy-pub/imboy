-module(group_member_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

action_contract_contains_current_actions_test() ->
    Source = read_group_member_handler_source(),
    lists:foreach(fun(Action) ->
        Pattern = <<Action/binary, " ->">>,
        ?assert(binary:match(Source, Pattern) =/= nomatch)
    end, [
        <<"join">>,
        <<"leave">>,
        <<"alias">>,
        <<"page">>,
        <<"same_group">>,
        <<"mute">>,
        <<"role">>
    ]).

action_contract_excludes_legacy_actions_test() ->
    Source = read_group_member_handler_source(),
    ?assertEqual(nomatch, binary:match(Source, <<"add ->">>)),
    ?assertEqual(nomatch, binary:match(Source, <<"remove ->">>)),
    ?assertEqual(nomatch, binary:match(Source, <<"list ->">>)),
    ?assertEqual(nomatch, binary:match(Source, <<"transfer_owner ->">>)).

mute_accepts_string_duration_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"gid">> => <<"g_101">>, <<"user_id">> => <<"u_202">>, <<"duration">> => <<"60">>}
            end}
        ]},
        {throttle, [
            {'check', 2, fun(three_second_once, {group_member_mute, 12345}) -> ok end}
        ]},
        {group_member_logic, [
            {'mute', 4, fun(12345, 101, 202, 60) -> ok end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload, _Msg) -> #{response_status => 200, payload => Payload} end}
        ]}
    ], fun() ->
        {ok, Req, _State} = group_member_handler:init(#{}, #{action => mute, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(<<"g_101">>, maps:get(<<"gid">>, maps:get(payload, Req)))
    end).

role_accepts_string_role_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"gid">> => <<"g_101">>, <<"user_id">> => <<"u_202">>, <<"role">> => <<"3">>}
            end}
        ]},
        {throttle, [
            {'check', 2, fun(three_second_once, {group_member_role, 12345}) -> ok end}
        ]},
        {group_member_logic, [
            {'update_role', 4, fun(12345, 101, 202, 3) -> ok end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload, _Msg) -> #{response_status => 200, payload => Payload} end}
        ]}
    ], fun() ->
        {ok, Req, _State} = group_member_handler:init(#{}, #{action => role, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(<<"u_202">>, maps:get(<<"user_id">>, maps:get(payload, Req)))
    end).

page_preserves_atom_list_payload_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(req0) -> [{<<"gid">>, <<"g_101">>}] end}
        ]},
        {group_member_repo, [
            {'find', 3, fun(101, 12345, <<"id">>) -> #{<<"id">> => 1} end},
            {'tablename', 0, fun() -> <<"public.group_member">> end}
        ]},
        {user_repo, [
            {'tablename', 0, fun() -> <<"public.user">> end}
        ]},
        {elib_param, [
            {'page', 1, fun(req0) -> {1, 20} end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, _Fields, _Where, _Order, _Page, _Size) ->
                {ok, #{total => 1, list => [#{<<"user_id">> => 202}]}}
            end}
        ]},
        {group_member_transfer, [
            {'member_list', 1, fun(List) -> List end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> #{response_status => 200, payload => Payload} end}
        ]}
    ], fun() ->
        {ok, Req, _State} = group_member_handler:init(req0, #{action => page, current_uid => 12345}),
        Payload = maps:get(payload, Req),
        [Item] = maps:get(list, Payload),
        ?assertEqual(202, maps:get(<<"user_id">>, Item))
    end).

read_group_member_handler_source() ->
    {ok, Bin} = file:read_file("src/api/group_member_handler.erl"),
    Bin.
