-module(adm_moderation_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc adm_moderation_logic 的 EUnit 测试
%%% 覆盖：敏感词校验/去重、批量导入计数、复审动作映射
%%%===================================================================

%% ===================================================================
%% add_sensitive_word/3
%% ===================================================================

add_rejects_empty_word_test() ->
    ?assertEqual(
        {error, <<"关键词不能为空"/utf8>>},
        adm_moderation_logic:add_sensitive_word(<<"   ">>, <<"custom">>, <<"high">>)
    ).

add_normalizes_and_creates_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'sensitive_word_create', 3, fun(Word, Category, Severity) ->
                    ?assertEqual(<<"badword">>, Word),
                    %% 非法 severity 归一为 medium，空 category 归一为 custom
                    ?assertEqual(<<"custom">>, Category),
                    ?assertEqual(<<"medium">>, Severity),
                    {ok, created, 123}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = adm_moderation_logic:add_sensitive_word(
                <<" badword ">>, <<>>, <<"bogus">>
            ),
            ?assertEqual(123, maps:get(<<"id">>, Result)),
            ?assertEqual(<<"badword">>, maps:get(<<"word">>, Result))
        end
    ).

add_reports_duplicate_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'sensitive_word_create', 3, fun(_, _, _) -> {ok, skipped} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"该关键词已存在"/utf8>>},
                adm_moderation_logic:add_sensitive_word(<<"dup">>, <<"custom">>, <<"low">>)
            )
        end
    ).

%% ===================================================================
%% import_sensitive_words/1
%% ===================================================================

import_counts_imported_and_skipped_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'sensitive_word_create', 3, fun
                    (<<"new">>, _, _) -> {ok, created, 1};
                    (<<"dup">>, _, _) -> {ok, skipped};
                    (_, _, _) -> {error, boom}
                end}
            ]}
        ],
        fun() ->
            Words = [
                #{
                    <<"word">> => <<"new">>,
                    <<"category">> => <<"spam">>,
                    <<"severity">> => <<"low">>
                },
                #{<<"word">> => <<"dup">>},
                #{<<"word">> => <<"  ">>},
                #{<<"word">> => <<"err">>}
            ],
            {ok, R} = adm_moderation_logic:import_sensitive_words(Words),
            ?assertEqual(1, maps:get(<<"imported">>, R)),
            %% dup + 空词 + err = 3 skipped
            ?assertEqual(3, maps:get(<<"skipped">>, R))
        end
    ).

%% ===================================================================
%% moderate/4
%% ===================================================================

moderate_approve_maps_status_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'review_moderate', 4, fun(9, Status, _Reason, 7) ->
                    ?assertEqual(<<"approved">>, Status),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, adm_moderation_logic:moderate(9, <<"approve">>, undefined, 7))
        end
    ).

moderate_reject_maps_status_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'review_moderate', 4, fun(_, Status, Reason, _) ->
                    ?assertEqual(<<"rejected">>, Status),
                    ?assertEqual(<<"垃圾广告"/utf8>>, Reason),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok, adm_moderation_logic:moderate(9, <<"reject">>, <<"垃圾广告"/utf8>>, 7)
            )
        end
    ).

moderate_rejects_invalid_action_test() ->
    ?assertEqual(
        {error, <<"无效的审核操作"/utf8>>},
        adm_moderation_logic:moderate(9, <<"nuke">>, undefined, 7)
    ).

moderate_reports_no_pending_row_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'review_moderate', 4, fun(_, _, _, _) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"记录不存在或已审核"/utf8>>},
                adm_moderation_logic:moderate(9, <<"approve">>, undefined, 7)
            )
        end
    ).

%% ===================================================================
%% R-03：reject 联动撤下内容 + 队列 SLA 标记
%% ===================================================================

moderate_reject_channel_removes_content_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'review_moderate', 4, fun(9, <<"rejected">>, <<"违规确认"/utf8>>, 1001) ->
                    {ok, 1}
                end},
                {'review_find', 1, fun(9) ->
                    {ok, #{
                        <<"msg_type">> => <<"channel_message">>,
                        <<"msg_id">> => 501,
                        <<"to_id">> => 11
                    }}
                end}
            ]},
            {channel_message_ds, [
                {'delete', 1, fun(501) -> {ok, 1} end}
            ]},
            {channel_logic_notify, [
                {'notify_message_deleted', 2, fun(11, 501) -> ok end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_, _) -> erlang:error(should_not_hit_db) end}
            ]}
        ],
        fun() ->
            ok = adm_moderation_logic:moderate(9, <<"reject">>, <<"违规确认"/utf8>>, 1001),
            ?assertEqual(1, meck:num_calls(channel_message_ds, delete, 1))
        end
    ).

moderate_approve_keeps_content_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'review_moderate', 4, fun(9, <<"approved">>, undefined, 1001) -> {ok, 1} end},
                {'review_find', 1, fun(9) -> erlang:error(should_not_lookup_on_approve) end}
            ]},
            {channel_message_ds, [
                {'delete', 1, fun(_) -> erlang:error(should_not_remove_on_approve) end}
            ]}
        ],
        fun() ->
            ok = adm_moderation_logic:moderate(9, <<"approve">>, undefined, 1001),
            ?assertEqual(0, meck:num_calls(channel_message_ds, delete, 1))
        end
    ).

list_review_queue_marks_overdue_test_() ->
    NowMs = elib_dt:millisecond(),
    OldTs = elib_dt:to_rfc3339(NowMs - 25 * 3600 * 1000, millisecond),
    FreshTs = elib_dt:to_rfc3339(NowMs - 1 * 3600 * 1000, millisecond),
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'review_page', 3, fun(_Page, _Size, _Filters) ->
                    {ok, #{
                        list => [
                            #{
                                <<"id">> => 1,
                                <<"review_status">> => <<"pending">>,
                                <<"created_at">> => OldTs
                            },
                            #{
                                <<"id">> => 2,
                                <<"review_status">> => <<"pending">>,
                                <<"created_at">> => FreshTs
                            },
                            #{
                                <<"id">> => 3,
                                <<"review_status">> => <<"rejected">>,
                                <<"created_at">> => OldTs
                            }
                        ],
                        total => 3,
                        page => 1,
                        size => 20
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, Payload} = adm_moderation_logic:list_review_queue(1, 20, #{}),
            Rows = maps:get(list, Payload),
            Row1 = hd(Rows),
            Row2 = lists:nth(2, Rows),
            Row3 = lists:nth(3, Rows),
            %% pending 超 24h → overdue
            ?assertEqual(true, maps:get(<<"overdue">>, Row1)),
            ?assertEqual(false, maps:get(<<"overdue">>, Row2)),
            %% 已审行不标 overdue
            ?assertEqual(false, maps:get(<<"overdue">>, Row3))
        end
    ).

moderate_reject_moment_removes_post_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'review_moderate', 4, fun(9, <<"rejected">>, <<"违规确认"/utf8>>, 1001) ->
                    {ok, 1}
                end},
                {'review_find', 1, fun(9) ->
                    {ok, #{
                        <<"msg_type">> => <<"moment_post">>,
                        <<"msg_id">> => 601,
                        <<"to_id">> => 0
                    }}
                end}
            ]},
            {moment_ds, [
                {'delete_post_by_admin', 1, fun(601) -> ok end}
            ]}
        ],
        fun() ->
            ok = adm_moderation_logic:moderate(9, <<"reject">>, <<"违规确认"/utf8>>, 1001),
            ?assertEqual(1, meck:num_calls(moment_ds, delete_post_by_admin, 1))
        end
    ).

%% ===================================================================
%% R-03.1：reject 联动清空违规资料字段
%% ===================================================================

moderate_reject_profile_clears_field_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'review_moderate', 4, fun(10, <<"rejected">>, <<"违规确认"/utf8>>, 1001) ->
                    {ok, 1}
                end},
                {'review_find', 1, fun(10) ->
                    {ok, #{
                        <<"msg_type">> => <<"profile_field:sign">>,
                        <<"msg_id">> => 77,
                        <<"to_id">> => 0
                    }}
                end}
            ]},
            {user_ds, [
                {'update_field', 3, fun(77, <<"sign">>, <<>>) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ok = adm_moderation_logic:moderate(10, <<"reject">>, <<"违规确认"/utf8>>, 1001),
            ?assertEqual(1, meck:num_calls(user_ds, update_field, 3))
        end
    ).

moderate_reject_profile_clear_failure_still_ok_test_() ->
    ?WITH_MECKS(
        [
            {moderation_ds, [
                {'review_moderate', 4, fun(11, <<"rejected">>, <<"违规确认"/utf8>>, 1001) ->
                    {ok, 1}
                end},
                {'review_find', 1, fun(11) ->
                    {ok, #{
                        <<"msg_type">> => <<"profile_field:nickname">>,
                        <<"msg_id">> => 88,
                        <<"to_id">> => 0
                    }}
                end}
            ]},
            {user_ds, [
                {'update_field', 3, fun(_Uid, _Field, _Val) -> {error, db_down} end}
            ]}
        ],
        fun() ->
            %% 清空失败不影响审核判定落库（fail-open 撤下口径）
            ok = adm_moderation_logic:moderate(11, <<"reject">>, <<"违规确认"/utf8>>, 1001)
        end
    ).
