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
