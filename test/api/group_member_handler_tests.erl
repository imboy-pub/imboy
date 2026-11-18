-module(group_member_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

action_contract_contains_current_actions_test() ->
    Source = read_group_member_handler_source(),
    lists:foreach(
        fun(Action) ->
            Pattern = <<Action/binary, " ->">>,
            ?assert(binary:match(Source, Pattern) =/= nomatch)
        end,
        [
            <<"join">>,
            <<"leave">>,
            <<"alias">>,
            <<"page">>,
            <<"same_group">>,
            <<"mute">>,
            <<"role">>
        ]
    ).

action_contract_excludes_legacy_actions_test() ->
    Source = read_group_member_handler_source(),
    ?assertEqual(nomatch, binary:match(Source, <<"add ->">>)),
    ?assertEqual(nomatch, binary:match(Source, <<"remove ->">>)),
    ?assertEqual(nomatch, binary:match(Source, <<"list ->">>)),
    ?assertEqual(nomatch, binary:match(Source, <<"transfer_owner ->">>)).

mute_accepts_string_duration_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"gid">> => <<"101">>,
                        <<"user_id">> => <<"202">>,
                        <<"duration">> => <<"60">>
                    }
                end}
            ]},
            {throttle, [
                {'check', 2, fun(three_second_once, {group_member_mute, 12345}) -> ok end}
            ]},
            {group_member_logic, [
                {'mute', 4, fun(12345, 101, 202, 60) -> ok end}
            ]},
            {elib_response, [
                {'success', 3, fun(_Req, Payload, _Msg) ->
                    #{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = group_member_handler:init(#{}, #{
                action => mute, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, Req)),
            ?assertEqual(<<"101">>, maps:get(<<"gid">>, maps:get(payload, Req)))
        end
    ).

role_accepts_string_role_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"gid">> => <<"101">>, <<"user_id">> => <<"202">>, <<"role">> => <<"3">>}
                end}
            ]},
            {throttle, [
                {'check', 2, fun(three_second_once, {group_member_role, 12345}) -> ok end}
            ]},
            {group_member_logic, [
                {'update_role', 4, fun(12345, 101, 202, 3) -> ok end}
            ]},
            {elib_response, [
                {'success', 3, fun(_Req, Payload, _Msg) ->
                    #{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = group_member_handler:init(#{}, #{
                action => role, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, Req)),
            ?assertEqual(<<"202">>, maps:get(<<"user_id">>, maps:get(payload, Req)))
        end
    ).

page_preserves_atom_list_payload_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(req0) -> [{<<"gid">>, <<"101">>}] end}
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
                {'success', 2, fun(_Req, Payload) ->
                    #{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = group_member_handler:init(req0, #{
                action => page, current_uid => 12345
            }),
            Payload = maps:get(payload, Req),
            [Item] = maps:get(list, Payload),
            ?assertEqual(202, maps:get(<<"user_id">>, Item))
        end
    ).

page_datasource_error_returns_error_not_empty_success_test_() ->
    %% 数据层故障不得伪装成「空群 success」（曾致客户端停在不完整第一页
    %% 且 hasMore=false，第二页成员静默丢失）
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(req0) -> [{<<"gid">>, <<"101">>}] end}
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
            {group_member_logic, [
                {'page_with_user_info', 3, fun(_Gid, _Page, _Size) ->
                    {error, simulated_ds_down}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) ->
                    #{response_status => 200, kind => success, payload => Payload}
                end},
                {'error', 2, fun(_Req, _Msg) ->
                    #{response_status => 200, kind => error}
                end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = group_member_handler:init(req0, #{
                action => page, current_uid => 12345
            }),
            ?assertEqual(error, maps:get(kind, Req)),
            ?assertNot(maps:is_key(payload, Req))
        end
    ).

page_web_privacy_and_native_compatibility_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [{'parse_qs', 1, fun(req0) -> [{<<"gid">>, <<"101">>}] end}]},
            {elib_param, [{'page', 1, fun(req0) -> {2, 20} end}]},
            {group_member_logic, [
                {'find_by_gid_and_uid', 3, fun(101, 12345, <<"id">>) -> #{<<"id">> => 1} end},
                {'page_with_user_info', 3, fun(101, 2, 20) ->
                    {ok, #{total => 21, list => [privacy_member()]}}
                end}
            ]},
            {elib_response, [{'success', 2, fun(_Req, Payload) -> #{payload => Payload} end}]}
        ],
        fun() ->
            lists:foreach(
                fun(Surface) ->
                    State = #{action => page, current_uid => 12345, surface => Surface},
                    {ok, Req, _} = group_member_handler:init(req0, State),
                    #{total := 21, page := 2, size := 20, list := [Item]} = maps:get(payload, Req),
                    ?assertEqual(9007199254740993123, maps:get(<<"user_id">>, Item)),
                    ?assertEqual(<<"PUBLIC_ALIAS">>, maps:get(<<"alias">>, Item)),
                    ?assertEqual(<<"PUBLIC_NICKNAME">>, maps:get(<<"nickname">>, Item)),
                    ?assertEqual(1, maps:get(<<"status">>, Item)),
                    lists:foreach(
                        fun(Key) ->
                            ?assertEqual(Surface =/= web_human, maps:is_key(Key, Item))
                        end,
                        [<<"account">>, <<"mobile">>, <<"email">>, account, mobile, email]
                    )
                end,
                [web_human, native]
            )
        end
    ).

privacy_member() ->
    #{
        <<"group_id">> => 101,
        <<"user_id">> => 9007199254740993123,
        <<"alias">> => <<"PUBLIC_ALIAS">>,
        <<"nickname">> => <<"PUBLIC_NICKNAME">>,
        <<"status">> => 1,
        <<"account">> => <<"PRIVATE_SENTINEL">>,
        <<"mobile">> => <<"PRIVATE_SENTINEL">>,
        <<"email">> => <<"PRIVATE_SENTINEL">>,
        account => <<"PRIVATE_SENTINEL">>,
        mobile => <<"PRIVATE_SENTINEL">>,
        email => <<"PRIVATE_SENTINEL">>
    }.

page_web_nonmember_cannot_read_metadata_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [{'parse_qs', 1, fun(req0) -> [{<<"gid">>, <<"101">>}] end}]},
            {group_member_logic, [
                {'find_by_gid_and_uid', 3, fun(101, 12345, <<"id">>) -> #{} end},
                {'page_with_user_info', 3, fun(_, _, _) ->
                    error(metadata_read_without_membership)
                end}
            ]},
            {elib_response, [{'error', 2, fun(_Req, _Message) -> #{denied => true} end}]}
        ],
        fun() ->
            {ok, Req, _} = group_member_handler:init(req0, #{
                action => page, current_uid => 12345, surface => web_human
            }),
            ?assertEqual(#{denied => true}, Req),
            ?assertEqual(0, meck:num_calls(group_member_logic, page_with_user_info, '_'))
        end
    ).

read_group_member_handler_source() ->
    {ok, Bin} = file:read_file("src/api/group_member_handler.erl"),
    Bin.
