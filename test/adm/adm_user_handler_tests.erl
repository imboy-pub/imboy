-module(adm_user_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_user_handler 模块的 EUnit 测试
%%%
%%% 目标：覆盖用户分页、详情与禁用/解禁敏感动作
%%%===================================================================

init_list_with_status_and_keyword_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) -> {3, 25} end},
            {'int', 3, fun(status, _Req, _Default) -> {ok, 1} end},
            {'binary', 3, fun(keyword, _Req, _Default) -> {ok, <<"alice">>} end}
        ]},
        {user_repo, [
            {'page', 4, fun(Page, Size, Where, OrderBy) ->
                ?assertEqual(3, Page),
                ?assertEqual(25, Size),
                ?assertEqual(<<"created_at DESC">>, OrderBy),
                ?assertEqual(1, maps:get(status, Where)),
                AndWhere = maps:get('and', Where),
                OrClauses = maps:get('or', AndWhere),
                ?assertEqual(4, length(OrClauses)),
                {ok, #{list => [#{<<"id">> => 1001}], total => 1, page => 3, size => 25}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => list}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload)),
        ?assertEqual(1, length(maps:get(list, Payload))),
        ?assertEqual(false, maps:is_key(items, Payload))
    end).

init_detail_success_includes_relation_counters_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(uid, _Req, _Default) -> {ok, 1001} end}
        ]},
        {user_repo, [
            {'find_by_id', 2, fun(1001, Column) ->
                ?assertNotEqual(nomatch, binary:match(Column, <<"nickname">>)),
                #{
                    <<"id">> => 1001,
                    <<"account">> => <<"u1001">>,
                    <<"nickname">> => <<"alice">>
                }
            end}
        ]},
        {user_device_repo, [
            {'count_by_uid', 1, fun(1001) -> 2 end}
        ]},
        {friend_repo, [
            {'count_by_uid', 1, fun(1001) -> 8 end}
        ]},
        {group_member_repo, [
            {'count_by_uid', 1, fun(1001) -> 3 end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => detail}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(2, maps:get(device_count, Payload)),
        ?assertEqual(8, maps:get(friend_count, Payload)),
        ?assertEqual(3, maps:get(group_count, Payload))
    end).

init_detail_accepts_hashid_uid_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(uid, _Req, _Default) -> {ok, 0} end},
            {'binary', 3, fun(uid, _Req, _Default) -> {ok, <<"uid_hash_1001">>} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_1001">>) -> 1001 end}
        ]},
        {user_repo, [
            {'find_by_id', 2, fun(1001, _Column) ->
                #{
                    <<"id">> => 1001,
                    <<"account">> => <<"u1001">>,
                    <<"nickname">> => <<"alice">>
                }
            end}
        ]},
        {user_device_repo, [
            {'count_by_uid', 1, fun(1001) -> 2 end}
        ]},
        {friend_repo, [
            {'count_by_uid', 1, fun(1001) -> 8 end}
        ]},
        {group_member_repo, [
            {'count_by_uid', 1, fun(1001) -> 3 end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => detail}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(2, maps:get(device_count, Payload)),
        ?assert(is_binary(maps:get(<<"id">>, Payload)))
    end).

init_ban_updates_status_to_zero_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(uid, _Req, _Default) -> {ok, 1001} end}
        ]},
        {user_repo, [
            {'update', 2, fun(Uid, Data) ->
                ?assertEqual(1001, Uid),
                ?assertEqual(0, maps:get(status, Data)),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => ban}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq))
    end).

init_unban_updates_status_to_one_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(uid, _Req, _Default) -> {ok, 1001} end}
        ]},
        {user_repo, [
            {'update', 2, fun(Uid, Data) ->
                ?assertEqual(1001, Uid),
                ?assertEqual(1, maps:get(status, Data)),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => unban}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq))
    end).

init_search_requires_keyword_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'binary', 3, fun(keyword, _Req, _Default) -> {ok, <<>>} end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, Msg) ->
                Req#{response_status => 400, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => search}),
        ?assertEqual(400, maps:get(response_status, RespReq)),
        ?assertEqual("请输入搜索关键词", maps:get(error_msg, RespReq))
    end).
