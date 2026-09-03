-module(group_notice_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

action_contract_contains_current_actions_test() ->
    Source = read_group_notice_handler_source(),
    lists:foreach(
        fun(Action) ->
            Pattern = <<Action/binary, " ->">>,
            ?assert(binary:match(Source, Pattern) =/= nomatch)
        end,
        [
            <<"add">>,
            <<"edit">>,
            <<"delete">>,
            <<"page">>,
            <<"publish">>,
            <<"latest">>,
            <<"list">>,
            <<"detail">>,
            <<"pin">>,
            <<"unpin">>,
            <<"mark_read">>
        ]
    ).

action_contract_excludes_legacy_actions_test() ->
    Source = read_group_notice_handler_source(),
    ?assertEqual(nomatch, binary:match(Source, <<"create ->">>)),
    ?assertEqual(nomatch, binary:match(Source, <<"update ->">>)).

add_rejects_out_of_range_status_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(req0) -> <<"POST">> end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"gid">> => <<"g_101">>,
                        <<"title">> => <<"n">>,
                        <<"body">> => <<"b">>,
                        <<"status">> => <<"3">>,
                        <<"expired_at">> => <<"2026-03-13T00:00:00Z">>
                    }
                end}
            ]},
            {elib_dt, [
                {'rfc3339_to', 2, fun(_ExpiredAt, millisecond) -> 1 end},
                {'now', 0, fun() -> <<"2026-03-13T00:00:00Z">> end}
            ]},
            {throttle, [
                {'check', 2, fun(three_second_once, 12345) -> ok end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, _Code) -> #{response_status => 400} end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = group_notice_handler:init(req0, #{
                action => add, current_uid => 12345
            }),
            ?assertEqual(400, maps:get(response_status, Req))
        end
    ).

edit_rejects_out_of_range_status_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(req0) -> <<"POST">> end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"gid">> => <<"g_101">>,
                        <<"notice_id">> => <<"n_201">>,
                        <<"title">> => <<"n">>,
                        <<"body">> => <<"b">>,
                        <<"status">> => <<"9">>,
                        <<"expired_at">> => <<"2026-03-13T00:00:00Z">>
                    }
                end}
            ]},
            {throttle, [
                {'check', 2, fun(three_second_once, 12345) -> ok end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, _Code) -> #{response_status => 400} end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = group_notice_handler:init(req0, #{
                action => edit, current_uid => 12345
            }),
            ?assertEqual(400, maps:get(response_status, Req))
        end
    ).

edit_passes_rfc3339_expired_at_to_logic_test_() ->
    ExpiredAt = <<"2026-03-13T00:00:00Z">>,
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(req0) -> <<"POST">> end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"gid">> => <<"101">>,
                        <<"notice_id">> => <<"201">>,
                        <<"title">> => <<"n">>,
                        <<"body">> => <<"b">>,
                        <<"status">> => <<"1">>,
                        <<"expired_at">> => ExpiredAt
                    }
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-03-12T00:00:00Z">> end}
            ]},
            {throttle, [
                {'check', 2, fun(three_second_once, 12345) -> ok end}
            ]},
            {workspace_resolver, [
                {'guard_group_gid', 2, fun(12345, 101) -> ok end}
            ]},
            {group_notice_logic, [
                {'update', 3, fun(12345, 201, Data) ->
                    ?assertEqual(ExpiredAt, maps:get(expired_at, Data)),
                    {ok, 201}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Payload) -> #{response_status => 200} end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = group_notice_handler:init(
                req0, #{action => edit, current_uid => 12345}
            ),
            ?assertEqual(200, maps:get(response_status, Req))
        end
    ).

read_group_notice_handler_source() ->
    {ok, Bin} = file:read_file("src/api/group_notice_handler.erl"),
    Bin.
