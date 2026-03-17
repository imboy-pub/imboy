-module(adm_group_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_group_handler 模块的 EUnit 测试
%%%
%%% 目标：覆盖群组管理分页、详情与解散等高风险动作
%%%===================================================================

group_task_feature_enabled_mocks() ->
    [
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, group_task) -> ok end}
        ]}
    ].

group_schedule_feature_enabled_mocks() ->
    [
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, group_schedule) -> ok end}
        ]}
    ].

group_vote_feature_enabled_mocks() ->
    [
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, group_vote) -> ok end}
        ]}
    ].

init_list_with_pagination_filters_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end},
            {'int', 3, fun(Key, _Req, _Default) ->
                case Key of
                    status -> {ok, 1};
                    type -> {ok, 2}
                end
            end}
        ]},
        {group_repo, [
            {'page', 4, fun(Page, Size, Where, OrderBy) ->
                ?assertEqual(1, Page),
                ?assertEqual(20, Size),
                ?assertEqual(#{status => 1, type => 2}, Where),
                ?assertEqual(<<"created_at DESC">>, OrderBy),
                {ok, #{list => [#{<<"id">> => 11}], total => 1, page => 1, size => 20}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => list, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload)),
        ?assertEqual(1, length(maps:get(list, Payload))),
        ?assertEqual(false, maps:is_key(items, Payload))
    end).

init_list_permission_denied_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_repo, [
            {'page', 4, fun(_, _, _, _) ->
                erlang:error(should_not_query_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => list, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_repo, page, 4))
    end).

init_detail_success_with_owner_profile_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(gid, _Req, _Default) -> {ok, 66} end}
        ]},
        {group_repo, [
            {'find_by_id', 2, fun(Gid, Column) ->
                ?assertEqual(66, Gid),
                ?assertNotEqual(nomatch, binary:match(Column, <<"owner_uid">>)),
                #{
                    <<"id">> => 66,
                    <<"title">> => <<"研发群">>,
                    <<"owner_uid">> => 9001
                }
            end}
        ]},
        {user_repo, [
            {'find_by_id', 2, fun(9001, _Column) ->
                #{<<"id">> => 9001, <<"nickname">> => <<"owner">>}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => detail, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        Owner = maps:get(owner, Payload),
        OwnerId = maps:get(<<"id">>, Owner),
        ?assert(is_binary(OwnerId)),
        ?assert(byte_size(OwnerId) > 0)
    end).

init_detail_accepts_hashid_gid_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'binary', 3, fun(gid, _Req, _Default) -> {ok, <<"gid_hash_66">>} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"gid_hash_66">>) -> 66 end}
        ]},
        {group_repo, [
            {'find_by_id', 2, fun(66, _Column) ->
                #{
                    <<"id">> => 66,
                    <<"title">> => <<"研发群">>,
                    <<"owner_uid">> => 9001
                }
            end}
        ]},
        {user_repo, [
            {'find_by_id', 2, fun(9001, _Column) ->
                #{<<"id">> => 9001, <<"nickname">> => <<"owner">>}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => detail, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(<<"研发群">>, maps:get(<<"title">>, Payload))
    end).

init_dissolve_marks_group_disabled_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(gid, _Req, _Default) -> {ok, 66} end}
        ]},
        {group_repo, [
            {'update', 1, fun(Data) ->
                ?assertEqual(66, maps:get(id, Data)),
                ?assertEqual(-1, maps:get(status, Data)),
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
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => dissolve, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq))
    end).

init_dissolve_permission_denied_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_repo, [
            {'update', 1, fun(_Data) ->
                erlang:error(should_not_update_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => dissolve, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_repo, update, 1))
    end).

init_members_returns_paged_members_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(gid, _Req, _Default) -> {ok, 66} end},
            {'page', 1, fun(_Req) -> {2, 10} end}
        ]},
        {group_member_repo, [
            {'page_by_gid', 4, fun(Gid, Page, Size, Column) ->
                ?assertEqual(66, Gid),
                ?assertEqual(2, Page),
                ?assertEqual(10, Size),
                ?assertEqual(<<"user_id,nickname,avatar,role,joined_at">>, Column),
                {ok, #{list => [#{<<"user_id">> => 1001}], total => 1, page => 2, size => 10}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => members, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload)),
        ?assertEqual(1, length(maps:get(list, Payload))),
        ?assertEqual(false, maps:is_key(items, Payload))
    end).

init_search_requires_non_empty_keyword_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
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
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => search, adm_user_id => 9001}),
        ?assertEqual(400, maps:get(response_status, RespReq)),
        ?assertEqual("请输入搜索关键词", maps:get(error_msg, RespReq))
    end).

init_vote_list_success_test_() ->
    ?WITH_MECKS(group_vote_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(gid, _Req, _Default) -> {ok, 66} end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {group_vote_logic, [
            {'list_votes', 3, fun(66, 1, 10) ->
                {ok, #{list => [#{<<"vote_id">> => <<"vote_1">>}], total => 1, page => 1, size => 10}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => vote_list, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload))
    end).

init_vote_close_success_writes_audit_test_() ->
    ?WITH_MECKS(group_vote_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"vote_id">> => <<"vote_1">>} end}
        ]},
        {group_vote_logic, [
            {'close_vote', 1, fun(<<"vote_1">>) -> ok end}
        ]},
        {group_vote_repo, [
            {'find_by_vote_id', 1, fun(<<"vote_1">>) -> {ok, #{<<"group_id">> => 66}} end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => vote_close, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        ?assertEqual(9001, maps:get(uid, AuditData)),
        ?assertEqual(<<"adm_group_governance">>, maps:get(remark, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"close_vote">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(<<"vote_1">>, maps:get(<<"target_id">>, AuditBody)),
        Extra = maps:get(<<"extra">>, AuditBody),
        ?assertEqual(<<"vote">>, maps:get(<<"scope">>, Extra))
    end).

init_vote_close_permission_denied_test_() ->
    ?WITH_MECKS(group_vote_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_vote_logic, [
            {'close_vote', 1, fun(_VoteId) ->
                erlang:error(should_not_close_vote_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => vote_close, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_vote_logic, close_vote, 1))
    end).

init_notice_list_success_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(gid, _Req, _Default) -> {ok, 66} end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {group_notice_repo, [
            {'list_by_group_id', 3, fun(66, 1, 10) ->
                {ok, [#{<<"id">> => 123, <<"title">> => <<"群规公告">>}]}
            end},
            {'count_by_group_id', 1, fun(66) ->
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => notice_list, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload)),
        [Notice] = maps:get(list, Payload),
        ?assertEqual(false, maps:is_key(items, Payload)),
        ?assertEqual(123, maps:get(<<"notice_id">>, Notice))
    end).

init_notice_detail_success_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'binary', 3, fun(notice_id, _Req, _Default) -> {ok, <<"123">>} end}
        ]},
        {group_notice_repo, [
            {'find_by_id', 1, fun(123) ->
                {ok, #{
                    <<"id">> => 123,
                    <<"group_id">> => 66,
                    <<"title">> => <<"群规公告">>
                }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => notice_detail, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(123, maps:get(<<"notice_id">>, Payload))
    end).

init_notice_delete_success_writes_audit_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"notice_id">> => <<"123">>} end}
        ]},
        {group_notice_repo, [
            {'find_by_id', 1, fun(123) ->
                {ok, #{<<"id">> => 123, <<"group_id">> => 66}}
            end},
            {'soft_delete', 1, fun(123) ->
                {ok, 1}
            end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(notice_delete_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(notice_delete_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => notice_delete, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(notice_delete_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        ?assertEqual(9001, maps:get(uid, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"delete_notice">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(123, maps:get(<<"target_id">>, AuditBody)),
        Extra = maps:get(<<"extra">>, AuditBody),
        ?assertEqual(<<"notice">>, maps:get(<<"scope">>, Extra))
    end).

init_notice_delete_permission_denied_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_notice_repo, [
            {'soft_delete', 1, fun(_NoticeId) ->
                erlang:error(should_not_delete_notice_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => notice_delete, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_notice_repo, soft_delete, 1))
    end).

init_category_list_success_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'binary', 3, fun(Key, _Req, _Default) ->
                case Key of
                    uid -> {ok, <<"1001">>};
                    keyword -> {ok, <<>>}
                end
            end},
            {'int', 3, fun(gid, _Req, _Default) -> {ok, 66} end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {group_category_logic, [
            {'list', 1, fun(1001) ->
                {ok, [
                    #{<<"id">> => 0, <<"category_name">> => <<"未分类"/utf8>>, <<"sort_order">> => 0},
                    #{<<"id">> => 9, <<"category_name">> => <<"工作"/utf8>>, <<"sort_order">> => 1}
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => category_list, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1001, maps:get(uid, Payload)),
        ?assertEqual(66, maps:get(gid, Payload)),
        ?assertEqual(2, maps:get(total, Payload)),
        [First | _] = maps:get(list, Payload),
        ?assertEqual(false, maps:is_key(items, Payload)),
        ?assertEqual(0, maps:get(<<"category_id">>, First))
    end).

init_category_delete_success_writes_audit_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"uid">> => <<"1001">>,
                    <<"gid">> => 66,
                    <<"category_id">> => <<"9">>
                }
            end}
        ]},
        {group_category_logic, [
            {'delete', 2, fun(1001, 9) ->
                ok
            end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(category_delete_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(category_delete_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => category_delete, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(category_delete_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"delete_category">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(9, maps:get(<<"target_id">>, AuditBody)),
        Extra = maps:get(<<"extra">>, AuditBody),
        ?assertEqual(<<"category">>, maps:get(<<"scope">>, Extra)),
        ?assertEqual(1001, maps:get(<<"uid">>, Extra))
    end).

init_category_delete_permission_denied_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_category_logic, [
            {'delete', 2, fun(_Uid, _CategoryId) ->
                erlang:error(should_not_delete_category_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => category_delete, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_category_logic, delete, 2))
    end).

init_tag_list_success_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(gid, _Req, _Default) -> {ok, 66} end}
        ]},
        {group_tag_repo, [
            {'list_by_group', 2, fun(66, _Column) ->
                {ok, [#{<<"group_id">> => 66, <<"tag_name">> => <<"urgent">>}]}
            end},
            {'count_by_group', 1, fun(66) ->
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => tag_list, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload))
    end).

init_tag_delete_success_writes_audit_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"gid">> => 66, <<"tag_name">> => <<"urgent">>}
            end}
        ]},
        {group_tag_repo, [
            {'delete', 2, fun(66, <<"urgent">>) ->
                {ok, 1}
            end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(tag_delete_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(tag_delete_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => tag_delete, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(tag_delete_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        ?assertEqual(9001, maps:get(uid, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"delete_tag">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(<<"urgent">>, maps:get(<<"target_id">>, AuditBody)),
        Extra = maps:get(<<"extra">>, AuditBody),
        ?assertEqual(<<"tag">>, maps:get(<<"scope">>, Extra))
    end).

init_tag_delete_permission_denied_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_tag_repo, [
            {'delete', 2, fun(_Gid, _TagName) ->
                erlang:error(should_not_delete_tag_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => tag_delete, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_tag_repo, delete, 2))
    end).

init_file_list_success_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(gid, _Req, _Default) -> {ok, 66} end},
            {'binary', 3, fun(Key, _Req, _Default) ->
                case Key of
                    category -> {ok, <<>>};
                    keyword -> {ok, <<>>}
                end
            end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {group_file_repo, [
            {'list_by_group', 4, fun(66, 1, 10, #{}) ->
                {ok, [#{<<"id">> => 11, <<"file_id">> => <<"file_11">>}]}
            end},
            {'count_by_group', 1, fun(66) ->
                {ok, 21}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => file_list, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(21, maps:get(total, Payload)),
        ?assertEqual(3, maps:get(total_pages, Payload))
    end).

init_file_delete_success_writes_audit_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"file_id">> => <<"11">>}
            end}
        ]},
        {group_file_repo, [
            {'find_by_id', 1, fun(11) ->
                #{
                    <<"id">> => 11,
                    <<"group_id">> => 66,
                    <<"file_id">> => <<"file_11">>
                }
            end},
            {'soft_delete', 1, fun(11) ->
                {ok, 1}
            end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(file_delete_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(file_delete_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => file_delete, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(file_delete_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"delete_file">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(11, maps:get(<<"target_id">>, AuditBody))
    end).

init_file_delete_permission_denied_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_file_repo, [
            {'soft_delete', 1, fun(_FilePk) ->
                erlang:error(should_not_delete_file_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => file_delete, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_file_repo, soft_delete, 1))
    end).

init_album_list_success_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(gid, _Req, _Default) -> {ok, 66} end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {group_album_repo, [
            {'list_albums', 3, fun(66, 1, 10) ->
                {ok, #{
                    list => [#{<<"id">> => 7, <<"album_id">> => <<"alb_7">>}],
                    total => 12,
                    page => 1,
                    size => 10
                }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => album_list, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        Items = maps:get(list, Payload),
        ?assertEqual(false, maps:is_key(items, Payload)),
        ?assertEqual(1, length(Items)),
        ?assertEqual(12, maps:get(total, Payload)),
        ?assertEqual(2, maps:get(total_pages, Payload))
    end).

init_album_delete_success_writes_audit_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"album_id">> => <<"7">>}
            end}
        ]},
        {group_album_repo, [
            {'find_album_by_id', 1, fun(7) ->
                #{
                    <<"id">> => 7,
                    <<"group_id">> => 66,
                    <<"album_id">> => <<"alb_7">>
                }
            end},
            {'delete_album', 1, fun(7) ->
                {ok, 1}
            end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(album_delete_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(album_delete_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => album_delete, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(album_delete_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"delete_album">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(7, maps:get(<<"target_id">>, AuditBody))
    end).

init_album_delete_permission_denied_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_album_repo, [
            {'delete_album', 1, fun(_AlbumPk) ->
                erlang:error(should_not_delete_album_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => album_delete, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_album_repo, delete_album, 1))
    end).

init_task_list_with_status_uses_repo_count_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(Key, _Req, _Default) ->
                case Key of
                    gid -> {ok, 66};
                    status -> {ok, 2};
                    deleted -> {ok, 0}
                end
            end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {group_task_repo, [
            {'list_by_group_id', 4, fun(66, 2, 1, 10) ->
                {ok, [#{<<"id">> => 1}]}
            end},
            {'count_by_group_id', 2, fun(66, 2) ->
                {ok, 25}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_list, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(25, maps:get(total, Payload)),
        ?assertEqual(3, maps:get(total_pages, Payload))
    end).

init_task_list_deleted_view_uses_deleted_repo_count_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(Key, _Req, _Default) ->
                case Key of
                    gid -> {ok, 66};
                    status -> {ok, -1};
                    deleted -> {ok, 1}
                end
            end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {group_task_repo, [
            {'list_deleted_by_group_id', 3, fun(66, 1, 10) ->
                {ok, [#{<<"id">> => 9, <<"deleted_at">> => <<"2026-02-24T09:00:00+08:00">>}]}
            end},
            {'count_deleted_by_group_id', 1, fun(66) ->
                {ok, 6}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_list, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(6, maps:get(total, Payload)),
        ?assertEqual(1, maps:get(total_pages, Payload))
    end).

init_task_pending_review_success_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'binary', 3, fun(task_id, _Req, _Default) -> {ok, <<"99">>} end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {group_task_repo, [
            {'find_by_id', 1, fun(99) ->
                {ok, #{<<"task_id">> => <<"task_uid_9">>}}
            end}
        ]},
        {group_task_logic, [
            {'pending_review', 3, fun(<<"task_uid_9">>, 1, 10) ->
                {ok, [#{<<"id">> => 501, <<"status">> => 2}]}
            end}
        ]},
        {group_task_assignment_repo, [
            {'count_by_status', 2, fun(<<"task_uid_9">>, 2) ->
                {ok, 21}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_pending_review, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(21, maps:get(total, Payload)),
        ?assertEqual(3, maps:get(total_pages, Payload))
    end).

init_task_review_success_writes_audit_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"assignment_id">> => <<"501">>,
                    <<"score">> => 95,
                    <<"comment">> => <<"good">>
                }
            end}
        ]},
        {group_task_assignment_repo, [
            {'find_by_id', 1, fun(501) ->
                {ok, #{
                    <<"task_id">> => <<"task_uid_9">>,
                    <<"user_id">> => 1002,
                    <<"status">> => 2
                }}
            end}
        ]},
        {group_task_repo, [
            {'find_by_task_id', 1, fun(<<"task_uid_9">>) ->
                {ok, #{<<"group_id">> => 66}}
            end}
        ]},
        {group_task_logic, [
            {'review', 3, fun(501, 9001, Data) ->
                ?assertEqual(95, maps:get(score, Data)),
                ?assertEqual(<<"good">>, maps:get(comment, Data)),
                ok
            end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(task_review_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(task_review_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_review, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(task_review_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        ?assertEqual(9001, maps:get(uid, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"review_task">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(501, maps:get(<<"target_id">>, AuditBody)),
        Extra = maps:get(<<"extra">>, AuditBody),
        ?assertEqual(<<"task_assignment">>, maps:get(<<"scope">>, Extra)),
        ?assertEqual(<<"task_uid_9">>, maps:get(<<"task_id">>, Extra)),
        ?assertEqual(1002, maps:get(<<"assignee_uid">>, Extra)),
        ?assertEqual(2, maps:get(<<"previous_status">>, Extra)),
        ?assertEqual(3, maps:get(<<"target_status">>, Extra)),
        ?assertEqual(95, maps:get(<<"score">>, Extra))
    end).

init_task_review_permission_denied_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_task_logic, [
            {'review', 3, fun(_, _, _) ->
                erlang:error(should_not_review_task_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_review, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_task_logic, review, 3))
    end).

init_task_close_success_writes_audit_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"task_id">> => <<"99">>} end}
        ]},
        {group_task_repo, [
            {'find_by_id', 1, fun(99) ->
                {ok, #{<<"group_id">> => 66, <<"task_id">> => <<"task_uid_9">>, <<"status">> => 1}}
            end},
            {'update', 2, fun(99, #{status := 3}) -> {ok, 1} end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(task_close_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(task_close_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_close, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(task_close_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        ?assertEqual(9001, maps:get(uid, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"close_task">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(99, maps:get(<<"target_id">>, AuditBody)),
        Extra = maps:get(<<"extra">>, AuditBody),
        ?assertEqual(<<"task">>, maps:get(<<"scope">>, Extra)),
        ?assertEqual(<<"task_uid_9">>, maps:get(<<"task_id">>, Extra)),
        ?assertEqual(1, maps:get(<<"previous_status">>, Extra)),
        ?assertEqual(3, maps:get(<<"target_status">>, Extra))
    end).

init_task_close_permission_denied_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_task_repo, [
            {'update', 2, fun(_TaskPk, _Data) ->
                erlang:error(should_not_close_task_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_close, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_task_repo, update, 2))
    end).

init_task_restore_success_writes_audit_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"task_id">> => <<"task_uid_9">>} end}
        ]},
        {group_task_repo, [
            {'find_any_by_task_id', 1, fun(<<"task_uid_9">>) ->
                {ok, #{<<"id">> => 99}}
            end},
            {'find_any_by_id', 1, fun(99) ->
                {ok, #{
                    <<"group_id">> => 66,
                    <<"task_id">> => <<"task_uid_9">>,
                    <<"deleted_at">> => <<"2026-02-24T08:00:00+08:00">>
                }}
            end},
            {'restore', 1, fun(99) -> {ok, 1} end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(task_restore_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(task_restore_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_restore, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(task_restore_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        ?assertEqual(9001, maps:get(uid, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"restore_task">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(99, maps:get(<<"target_id">>, AuditBody)),
        Extra = maps:get(<<"extra">>, AuditBody),
        ?assertEqual(<<"task">>, maps:get(<<"scope">>, Extra)),
        ?assertEqual(<<"task_uid_9">>, maps:get(<<"task_id">>, Extra))
    end).

init_task_restore_permission_denied_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_task_repo, [
            {'restore', 1, fun(_TaskPk) ->
                erlang:error(should_not_restore_task_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_restore, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_task_repo, restore, 1))
    end).

init_task_delete_success_writes_audit_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"task_id">> => <<"99">>} end}
        ]},
        {group_task_repo, [
            {'find_by_id', 1, fun(99) ->
                {ok, #{<<"group_id">> => 66, <<"task_id">> => <<"task_uid_9">>}}
            end},
            {'soft_delete', 1, fun(99) -> {ok, 1} end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(task_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(task_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_delete, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(task_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        ?assertEqual(9001, maps:get(uid, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"delete_task">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(99, maps:get(<<"target_id">>, AuditBody)),
        Extra = maps:get(<<"extra">>, AuditBody),
        ?assertEqual(<<"task">>, maps:get(<<"scope">>, Extra)),
        ?assertEqual(<<"task_uid_9">>, maps:get(<<"task_id">>, Extra))
    end).

init_schedule_detail_supports_numeric_schedule_id_test_() ->
    ?WITH_MECKS(group_schedule_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'binary', 3, fun(schedule_id, _Req, _Default) -> {ok, <<"123">>} end}
        ]},
        {group_schedule_repo, [
            {'find_by_id', 2, fun(123, <<"schedule_id">>) ->
                #{<<"schedule_id">> => <<"sched_abc">>}
            end}
        ]},
        {group_schedule_logic, [
            {'get_schedule_detail', 1, fun(<<"sched_abc">>) ->
                {ok, #{schedule => #{<<"schedule_id">> => <<"sched_abc">>}}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => schedule_detail, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        Schedule = maps:get(schedule, Payload),
        ?assertEqual(<<"sched_abc">>, maps:get(<<"schedule_id">>, Schedule))
    end).

init_schedule_cancel_success_writes_audit_test_() ->
    ?WITH_MECKS(group_schedule_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"schedule_id">> => <<"sched_abc">>} end}
        ]},
        {group_schedule_repo, [
            {'find_by_schedule_id', 1, fun(<<"sched_abc">>) ->
                #{
                    <<"id">> => 77,
                    <<"group_id">> => 66,
                    <<"status">> => 1,
                    <<"schedule_id">> => <<"sched_abc">>
                }
            end},
            {'update_status', 2, fun(77, 4) ->
                {ok, 1}
            end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(schedule_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(schedule_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => schedule_cancel, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(schedule_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        ?assertEqual(9001, maps:get(uid, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"cancel_schedule">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(<<"sched_abc">>, maps:get(<<"target_id">>, AuditBody)),
        Extra = maps:get(<<"extra">>, AuditBody),
        ?assertEqual(<<"schedule">>, maps:get(<<"scope">>, Extra)),
        ?assertEqual(77, maps:get(<<"schedule_pk">>, Extra))
    end).

init_schedule_cancel_permission_denied_test_() ->
    ?WITH_MECKS(group_schedule_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_schedule_repo, [
            {'update_status', 2, fun(_Id, _Status) ->
                erlang:error(should_not_cancel_schedule_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => schedule_cancel, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_schedule_repo, update_status, 2))
    end).

init_schedule_restore_success_writes_audit_test_() ->
    ?WITH_MECKS(group_schedule_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"schedule_id">> => <<"sched_abc">>} end}
        ]},
        {group_schedule_repo, [
            {'find_by_schedule_id', 1, fun(<<"sched_abc">>) ->
                #{
                    <<"id">> => 77,
                    <<"group_id">> => 66,
                    <<"status">> => 4,
                    <<"schedule_id">> => <<"sched_abc">>
                }
            end},
            {'update_status', 2, fun(77, 1) ->
                {ok, 1}
            end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                put(schedule_restore_audit_data, Data),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        put(schedule_restore_audit_data, undefined),
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => schedule_restore, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq)),
        AuditData = get(schedule_restore_audit_data),
        ?assertEqual(902, maps:get(type, AuditData)),
        ?assertEqual(9001, maps:get(uid, AuditData)),
        AuditBody = jsone:decode(maps:get(body, AuditData)),
        ?assertEqual(<<"restore_schedule">>, maps:get(<<"action">>, AuditBody)),
        ?assertEqual(66, maps:get(<<"group_id">>, AuditBody)),
        ?assertEqual(<<"sched_abc">>, maps:get(<<"target_id">>, AuditBody)),
        Extra = maps:get(<<"extra">>, AuditBody),
        ?assertEqual(<<"schedule">>, maps:get(<<"scope">>, Extra)),
        ?assertEqual(77, maps:get(<<"schedule_pk">>, Extra)),
        ?assertEqual(4, maps:get(<<"previous_status">>, Extra)),
        ?assertEqual(1, maps:get(<<"target_status">>, Extra))
    end).

init_schedule_restore_permission_denied_test_() ->
    ?WITH_MECKS(group_schedule_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {group_schedule_repo, [
            {'update_status', 2, fun(_Id, _Status) ->
                erlang:error(should_not_restore_schedule_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => schedule_restore, adm_user_id => 9003}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(group_schedule_repo, update_status, 2))
    end).

init_governance_log_list_success_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9002, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9002, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end},
            {'int', 3, fun(Key, _Req, _Default) ->
                case Key of
                    uid -> {ok, 9002};
                    group_id -> {ok, 66}
                end
            end},
            {'binary', 3, fun(Key, _Req, _Default) ->
                case Key of
                    action -> {ok, <<"close_task">>};
                    target_id -> {ok, <<"99">>};
                    keyword -> {ok, <<"ops">>};
                    from_ts -> {ok, <<"2026-02-20T00:00:00+08:00">>};
                    to_ts -> {ok, <<>>}
                end
            end}
        ]},
        {elib_pg, [
            {'one', 2, fun(Sql, Params) ->
                ?assertNotEqual(nomatch, binary:match(Sql, <<"l.type = 902">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"adm_group_governance">>)),
                ?assert(length(Params) >= 5),
                {ok, #{<<"count">> => 1}}
            end},
            {'query', 2, fun(Sql, Params) ->
                ?assertNotEqual(nomatch, binary:match(Sql, <<"ORDER BY l.created_at DESC">>)),
                ?assert(length(Params) >= 7),
                {ok, [
                    #{
                        <<"uid">> => 9002,
                        <<"account">> => <<"ops_admin">>,
                        <<"nickname">> => <<"ops">>,
                        <<"body">> => <<"{\"action\":\"close_task\",\"operator_uid\":9002,\"group_id\":66,\"target_id\":99,\"occurred_at\":\"2026-02-24T08:00:00+08:00\",\"extra\":{\"scope\":\"task\"}}">>,
                        <<"created_at">> => <<"2026-02-24T08:00:00+08:00">>
                    }
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => governance_log_list, adm_user_id => 9002}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload)),
        ?assertEqual(1, maps:get(total_pages, Payload)),
        [Item] = maps:get(list, Payload),
        ?assertEqual(false, maps:is_key(items, Payload)),
        ?assertEqual(<<"close_task">>, maps:get(action, Item)),
        ?assertEqual(66, maps:get(group_id, Item)),
        ?assertEqual(99, maps:get(target_id, Item))
    end).

init_governance_log_list_permission_denied_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9010, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9010, <<"role_id">> => 0}
            end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) ->
                erlang:error(should_not_query_when_permission_denied)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => Code, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => governance_log_list, adm_user_id => 9010}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(0, meck:num_calls(elib_pg, one, 2))
    end).

init_governance_log_list_logs_view_role_allowed_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9003, <<"role_id">> => 3}
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 10} end},
            {'int', 3, fun(_Key, _Req, _Default) -> {ok, 0} end},
            {'binary', 3, fun(_Key, _Req, _Default) -> {ok, <<>>} end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) -> {ok, #{<<"count">> => 0}} end},
            {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => governance_log_list, adm_user_id => 9003}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(1, meck:num_calls(elib_pg, one, 2))
    end).

init_task_detail_supports_task_uid_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'binary', 3, fun(task_id, _Req, _Default) -> {ok, <<"task_uid_1">>} end}
        ]},
        {group_task_repo, [
            {'find_by_task_id', 1, fun(<<"task_uid_1">>) ->
                {ok, #{<<"id">> => 88}}
            end}
        ]},
        {group_task_logic, [
            {'detail', 1, fun(88) ->
                {ok, #{<<"id">> => 88, <<"task_id">> => <<"task_uid_1">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_detail, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(88, maps:get(<<"id">>, Payload))
    end).

init_task_delete_soft_delete_success_test_() ->
    ?WITH_MECKS(group_task_feature_enabled_mocks() ++ [
        {adm_user_logic, [
            {'find', 3, fun(9002, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9002, <<"role_id">> => 2}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"task_id">> => <<"99">>} end}
        ]},
        {group_task_repo, [
            {'find_by_id', 1, fun(99) ->
                {ok, #{<<"group_id">> => 66, <<"task_id">> => <<"task_uid_1">>}}
            end},
            {'soft_delete', 1, fun(99) -> {ok, 1} end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_group_handler:init(Req, #{action => task_delete, adm_user_id => 9002}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual("操作成功", maps:get(msg, RespReq))
    end).
