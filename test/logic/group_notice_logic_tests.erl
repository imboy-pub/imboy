-module(group_notice_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_notice_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群公告业务逻辑功能
%%% 覆盖：查询群公告、删除群公告、边界条件
%%%===================================================================

%% ===================================================================
%% group_notice_ds:insert/1 测试（通过 DS 层验证插入功能）
%% ===================================================================

insert_success_test_() ->
    ?WITH_MECK(
        group_notice_repo,
        [
            {'insert', 1, fun(_Data) -> {ok, 1} end}
        ],
        fun() ->
            Data = #{
                group_id => 1, user_id => 100, title => <<"公告标题"/utf8>>, body => <<"公告内容"/utf8>>
            },
            Result = group_notice_ds:insert(Data),
            ?assertMatch({ok, _}, Result)
        end
    ).

insert_with_empty_body_test_() ->
    ?WITH_MECK(
        group_notice_repo,
        [
            {'insert', 1, fun(_Data) -> {ok, 1} end}
        ],
        fun() ->
            Data = #{group_id => 1, user_id => 100, title => <<"标题"/utf8>>, body => <<>>},
            Result = group_notice_ds:insert(Data),
            ?assertMatch({ok, _}, Result)
        end
    ).

insert_with_long_content_test_() ->
    ?WITH_MECK(
        group_notice_repo,
        [
            {'insert', 1, fun(_Data) -> {ok, 1} end}
        ],
        fun() ->
            LongBody = binary:copy(<<"测试"/utf8>>, 100),
            Data = #{group_id => 1, user_id => 100, title => <<"标题"/utf8>>, body => LongBody},
            Result = group_notice_ds:insert(Data),
            ?assertMatch({ok, _}, Result)
        end
    ).

insert_with_extra_fields_test_() ->
    ?WITH_MECK(
        group_notice_repo,
        [
            {'insert', 1, fun(_Data) -> {ok, 1} end}
        ],
        fun() ->
            Data = #{
                group_id => 1,
                user_id => 100,
                title => <<"公告标题"/utf8>>,
                body => <<"公告内容"/utf8>>,
                priority => 1
            },
            Result = group_notice_ds:insert(Data),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% ===================================================================
%% group_notice_logic:delete/2 测试
%% ===================================================================

delete_success_test_() ->
    ?WITH_MECKS(
        [
            {group_notice_ds, [
                {'find_by_id', 1, fun(_Id) ->
                    {ok, #{<<"group_id">> => 1, <<"user_id">> => 100}}
                end},
                {'soft_delete', 1, fun(_Id) -> ok end}
            ]},
            {group_member_ds, [
                {'get_member_info', 3, fun(_Gid, _Uid, _Column) -> {ok, #{<<"role">> => 4}} end}
            ]}
        ],
        fun() ->
            CurrentUid = 100,
            NoticeId = 1,
            Result = group_notice_logic:delete(CurrentUid, NoticeId),
            ?assertEqual(ok, Result)
        end
    ).

delete_with_nonexistent_id_test_() ->
    ?WITH_MECK(
        group_notice_ds,
        [
            {'find_by_id', 1, fun(_Id) -> {error, not_found} end}
        ],
        fun() ->
            CurrentUid = 100,
            NoticeId = 999999,
            Result = group_notice_logic:delete(CurrentUid, NoticeId),
            ?assertMatch({error, _}, Result)
        end
    ).

delete_without_permission_test_() ->
    ?WITH_MECKS(
        [
            {group_notice_ds, [
                {'find_by_id', 1, fun(_Id) ->
                    {ok, #{<<"group_id">> => 1, <<"user_id">> => 100}}
                end}
            ]},
            {group_member_ds, [
                {'get_member_info', 3, fun(_Gid, _Uid, _Column) -> {ok, #{<<"role">> => 1}} end}
            ]}
        ],
        fun() ->
            % 普通成员无权限
            CurrentUid = 200,
            NoticeId = 1,
            Result = group_notice_logic:delete(CurrentUid, NoticeId),
            ?assertMatch({error, _}, Result)
        end
    ).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

insert_with_special_characters_test_() ->
    ?WITH_MECK(
        group_notice_repo,
        [
            {'insert', 1, fun(_Data) -> {ok, 1} end}
        ],
        fun() ->
            Data = #{
                group_id => 1,
                user_id => 100,
                title => <<"标题"/utf8>>,
                body => <<"公告@#$%^&*()内容"/utf8>>
            },
            Result = group_notice_ds:insert(Data),
            ?assertMatch({ok, _}, Result)
        end
    ).

insert_with_multiline_content_test_() ->
    ?WITH_MECK(
        group_notice_repo,
        [
            {'insert', 1, fun(_Data) -> {ok, 1} end}
        ],
        fun() ->
            Data = #{
                group_id => 1,
                user_id => 100,
                title => <<"标题"/utf8>>,
                body => <<"第一行\n第二行\n第三行"/utf8>>
            },
            Result = group_notice_ds:insert(Data),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% ===================================================================
%% group_notice_logic:publish_notice/3 测试
%% W1.1：验证 S2C group_notice_published 广播 Payload 契约，
%% 对齐客户端 lib/service/group_notice_s2c.dart 的解析。
%% ===================================================================

%% 完整广播：8 字段 Payload + 固定的 send 位置参数
publish_notice_broadcasts_full_payload_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [200, 300] end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(_Uid, _Col) -> #{<<"nickname">> => <<"Alice">>} end}
            ]},
            {elib_dt, [
                {'millisecond', 0, fun() -> 1700000000000 end}
            ]},
            {group_notice_ds, [
                {'find_by_id', 1, fun(_Id) ->
                    {ok, #{
                        <<"title">> => <<"通知标题"/utf8>>,
                        <<"body">> => <<"通知正文"/utf8>>,
                        <<"expired_at">> => 1699999999999
                    }}
                end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(FromId, ToUidLi, Action, MsgType, E2EE, Payload, Save) ->
                    put(captured_send, {FromId, ToUidLi, Action, MsgType, E2EE, Payload, Save}),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_notice_logic:publish_notice(100, 1, 555)),
            {FromId, ToUidLi, Action, MsgType, E2EE, Payload, Save} = get(captured_send),
            %% send 位置参数契约
            ?assertEqual(100, FromId),
            ?assertEqual([200, 300], ToUidLi),
            ?assertEqual(<<"group_notice_published">>, Action),
            ?assertEqual(<<>>, MsgType),
            ?assertEqual(null, E2EE),
            ?assertEqual(save, Save),
            %% Payload 8 字段契约
            ?assertEqual(1, maps:get(<<"gid">>, Payload)),
            ?assertEqual(555, maps:get(<<"notice_id">>, Payload)),
            ?assertEqual(100, maps:get(<<"publisher_id">>, Payload)),
            ?assertEqual(<<"Alice">>, maps:get(<<"publisher_nickname">>, Payload)),
            ?assertEqual(<<"通知标题"/utf8>>, maps:get(<<"title">>, Payload)),
            ?assertEqual(<<"通知正文"/utf8>>, maps:get(<<"body">>, Payload)),
            ?assertEqual(1699999999999, maps:get(<<"expired_at">>, Payload)),
            ?assertEqual(1700000000000, maps:get(<<"published_at">>, Payload))
        end
    ).

%% 公告行缺失（find_by_id 返回 error）→ title/body=<<>>，expired_at=0
publish_notice_missing_notice_defaults_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [200] end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(_Uid, _Col) -> #{<<"nickname">> => <<"Bob">>} end}
            ]},
            {elib_dt, [
                {'millisecond', 0, fun() -> 1700000000000 end}
            ]},
            {group_notice_ds, [
                {'find_by_id', 1, fun(_Id) -> {error, not_found} end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_F, _T, _A, _M, _E, Payload, _S) ->
                    put(captured_send, Payload),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_notice_logic:publish_notice(100, 1, 555)),
            Payload = get(captured_send),
            ?assertEqual(<<>>, maps:get(<<"title">>, Payload)),
            ?assertEqual(<<>>, maps:get(<<"body">>, Payload)),
            ?assertEqual(0, maps:get(<<"expired_at">>, Payload))
        end
    ).

%% 发布者昵称缺失 → publisher_nickname 兜底 <<>>
publish_notice_missing_nickname_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [200] end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(_Uid, _Col) -> #{} end}
            ]},
            {elib_dt, [
                {'millisecond', 0, fun() -> 1700000000000 end}
            ]},
            {group_notice_ds, [
                {'find_by_id', 1, fun(_Id) ->
                    {ok, #{<<"title">> => <<"T">>, <<"body">> => <<"B">>, <<"expired_at">> => 0}}
                end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_F, _T, _A, _M, _E, Payload, _S) ->
                    put(captured_send, Payload),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_notice_logic:publish_notice(100, 1, 555)),
            Payload = get(captured_send),
            ?assertEqual(<<>>, maps:get(<<"publisher_nickname">>, Payload))
        end
    ).
