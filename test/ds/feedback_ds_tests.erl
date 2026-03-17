-module(feedback_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% feedback_ds 模块的 EUnit 测试
%%%
%%% 目标：验证反馈服务功能
%%% 覆盖：反馈添加、删除、回复
%%%===================================================================

%% ===================================================================
%% add/10 测试
%% ===================================================================

add_creates_feedback_test_() ->
    ?WITH_MECKS([
        {elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"feedback_md5">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Table, _Field, _Where, _Opts, _Default) -> 0 end}
        ]},
        {feedback_repo, [
            {'add', 11, fun(Uid, Did, COS, COSV, AppVsn, Type, Rating, Contact, Body, Attach, Md5) ->
                ?assertEqual(1, Uid),
                ?assertEqual(<<"device-1">>, Did),
                ?assertEqual(<<"ios">>, COS),
                ?assertEqual(<<"17">>, COSV),
                ?assertEqual(<<"1.0.0">>, AppVsn),
                ?assertEqual(<<"bug">>, Type),
                ?assertEqual(<<"5">>, Rating),
                ?assertEqual(<<"test@example.com">>, Contact),
                ?assertEqual(<<"Test feedback content">>, Body),
                ?assertEqual(<<>>, Attach),
                ?assertEqual(<<"feedback_md5">>, Md5),
                ok
            end}
        ]}
    ], fun() ->
        Uid = 1,
        Result = feedback_ds:add(
            Uid,
            <<"device-1">>,
            <<"ios">>,
            <<"17">>,
            <<"1.0.0">>,
            <<"bug">>,
            <<"5">>,
            <<"test@example.com">>,
            <<"Test feedback content">>,
            <<>>
        ),
        ?assertEqual(ok, Result)
    end).

add_with_minimal_params_test_() ->
    ?WITH_MECKS([
        {elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"minimal_feedback_md5">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Table, _Field, _Where, _Opts, _Default) -> 0 end}
        ]},
        {feedback_repo, [
            {'add', 11, fun(_Uid, _Did, _COS, _COSV, _AppVsn, _Type, _Rating, _Contact, _Body, _Attach, _Md5) ->
                ok
            end}
        ]}
    ], fun() ->
        Uid = 1,
        Result = feedback_ds:add(
            Uid,
            <<"device-min">>,
            <<>>,
            <<>>,
            <<>>,
            <<>>,
            <<>>,
            <<>>,
            <<"Minimal feedback">>,
            <<>>
        ),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% remove/2 测试
%% ===================================================================

remove_deletes_feedback_test_() ->
    ?WITH_MECKS([
        {feedback_repo, [
            {tablename, 0, fun() -> <<"feedback">> end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
        ]},
        {elib_pg, [
            {'update', 4, fun(Table, Data, Where, Params) ->
                ?assertEqual(<<"feedback">>, Table),
                ?assertEqual(-1, maps:get(<<"status">>, Data)),
                ?assertEqual(<<"2024-01-01T00:00:00Z">>, maps:get(<<"updated_at">>, Data)),
                ?assertEqual(<<"user_id = $1 AND id = $2">>, Where),
                ?assertEqual([1, 1], Params),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Uid = 1,
        Id = 1,
        Result = feedback_ds:remove(Uid, Id),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% add_reply/1 测试
%% ===================================================================

add_reply_creates_reply_test_() ->
    ?WITH_MECKS([
        {feedback_reply_repo, [
            {tablename, 0, fun() -> <<"feedback_reply">> end}
        ]},
        {elib_pg_sql, [
            {'insert', 3, fun(Table, Data, OnConflict) ->
                ?assertEqual(<<"feedback_reply">>, Table),
                ?assertEqual(<<>>, OnConflict),
                ?assertEqual(1, maps:get(<<"feedback_id">>, Data)),
                ?assertEqual(<<"Test reply">>, maps:get(<<"body">>, Data)),
                {<<"INSERT INTO feedback_reply (...) VALUES (...)">>, [1]}
            end}
        ]},
        {feedback_repo, [
            {tablename, 0, fun() -> <<"feedback">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end},
            {'update', 4, fun(Table, Data, Where, Params) ->
                ?assertEqual(<<"feedback">>, Table),
                ?assertEqual(2, maps:get(<<"status">>, Data)),
                ?assertMatch({raw, <<"reply_count + 1">>}, maps:get(<<"reply_count">>, Data)),
                ?assertEqual(<<"id = $1">>, Where),
                ?assertEqual([1], Params),
                {ok, 1}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        FeedbackId = 1,
        Result = feedback_ds:add_reply(#{
            <<"feedback_id">> => FeedbackId,
            <<"body">> => <<"Test reply">>
        }),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% add/10 完整测试 - 测试去重逻辑
%% ===================================================================

add_with_duplicate_detection_test_() ->
    ?WITH_MECKS([
        {elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"unique_md5_hash">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Table, _Field, _Where, _Opts, _Default) -> 0 end}
        ]},
        {feedback_repo, [
            {'add', 11, fun(_Uid, _Did, _COS, _COSV, _AppVsn, _Type, _Rating, _ContactDetail, _Body, _Attach, _Md5) ->
                {ok, #{<<"id">> => 123}}
            end}
        ]}
    ], fun() ->
        Uid = 1,
        Did = <<"device123">>,
        COS = <<"android">>,
        COSV = <<"10">>,
        AppVsn = <<"1.0.0">>,
        Type = <<"bug">>,
        Rating = <<"5">>,
        ContactDetail = <<"test@example.com">>,
        Body = <<"Test feedback body">>,
        Attach = <<>>,

        Result = feedback_ds:add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach),
        ?assertEqual(ok, Result)
    end).

add_with_existing_duplicate_test_() ->
    ?WITH_MECKS([
        {elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"existing_md5_hash">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Table, _Field, _Where, _Opts, _Default) -> 1 end}
        ]}
    ], fun() ->
        Uid = 1,
        Did = <<"device123">>,
        COS = <<"android">>,
        COSV = <<"10">>,
        AppVsn = <<"1.0.0">>,
        Type = <<"bug">>,
        Rating = <<"5">>,
        ContactDetail = <<"test@example.com">>,
        Body = <<"Test feedback body">>,
        Attach = <<>>,

        % 当检测到重复时，应该返回 ok 而不插入
        Result = feedback_ds:add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach),
        ?assertEqual(ok, Result)
    end).

add_with_different_params_creates_different_md5_test_() ->
    ?WITH_MECKS([
        {elib_hasher, [
            {'md5', 1, fun(Input) ->
                case Input of
                    <<"1device1231.0.0bugTest feedback 1">> -> <<"md5_1">>;
                    <<"1device1231.0.0featureTest feedback 2">> -> <<"md5_2">>;
                    _ -> <<"md5_default">>
                end
            end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Table, _Field, _Where, _Opts, _Default) -> 0 end}
        ]},
        {feedback_repo, [
            {'add', 11, fun(_Uid, _Did, _COS, _COSV, _AppVsn, _Type, _Rating, _ContactDetail, _Body, _Attach, _Md5) ->
                {ok, #{<<"id">> => 123}}
            end}
        ]}
    ], fun() ->
        Uid = 1,
        Did = <<"device123">>,
        COS = <<"android">>,
        COSV = <<"10">>,
        AppVsn = <<"1.0.0">>,
        Rating = <<"5">>,
        ContactDetail = <<"test@example.com">>,
        Attach = <<>>,

        % 第一次添加
        Result1 = feedback_ds:add(Uid, Did, COS, COSV, AppVsn, <<"bug">>, Rating, ContactDetail, <<"Test feedback 1">>, Attach),
        ?assertEqual(ok, Result1),

        % 第二次添加不同类型和内容
        Result2 = feedback_ds:add(Uid, Did, COS, COSV, AppVsn, <<"feature">>, Rating, ContactDetail, <<"Test feedback 2">>, Attach),
        ?assertEqual(ok, Result2)
    end).

%% ===================================================================
%% remove/2 完整测试
%% ===================================================================

remove_updates_status_to_deleted_test_() ->
    ?WITH_MECKS([
        {feedback_repo, [
            {tablename, 0, fun() -> <<"feedback">> end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Table, _Data, _Where, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Uid = 1,
        FeedbackId = 123,
        Result = feedback_ds:remove(Uid, FeedbackId),
        ?assertEqual(ok, Result)
    end).

remove_with_nonexistent_feedback_test_() ->
    ?WITH_MECKS([
        {feedback_repo, [
            {tablename, 0, fun() -> <<"feedback">> end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Table, _Data, _Where, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        Uid = 1,
        FeedbackId = 999999,
        Result = feedback_ds:remove(Uid, FeedbackId),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% add_reply/1 完整测试
%% ===================================================================

add_reply_increments_reply_count_test_() ->
    ?WITH_MECKS([
        {feedback_reply_repo, [
            {tablename, 0, fun() -> <<"feedback_reply">> end}
        ]},
        {feedback_repo, [
            {tablename, 0, fun() -> <<"feedback">> end}
        ]},
        {elib_pg_sql, [
            {'insert', 3, fun(_Table, _Data, _OnConflict) ->
                {<<"INSERT INTO feedback_reply (...) VALUES (...)">>, []}
            end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end},
            {'update', 4, fun(_Table, Data, _Where, _Params) ->
                ?assertEqual(2, maps:get(<<"status">>, Data)),
                ?assertMatch({raw, <<"reply_count + 1">>}, maps:get(<<"reply_count">>, Data)),
                {ok, 1}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        Data = #{
            <<"feedback_id">> => 123,
            <<"feedback_reply_pid">> => 0,
            <<"replier_user_id">> => 456,
            <<"replier_name">> => <<"Admin"/utf8>>,
            <<"body">> => <<"回复内容"/utf8>>,
            <<"created_at">> => <<"2024-01-01T00:00:00Z">>
        },
        Result = feedback_ds:add_reply(Data),
        ?assertEqual(ok, Result)
    end).

add_reply_with_minimal_data_test_() ->
    ?WITH_MECKS([
        {feedback_reply_repo, [
            {tablename, 0, fun() -> <<"feedback_reply">> end}
        ]},
        {feedback_repo, [
            {tablename, 0, fun() -> <<"feedback">> end}
        ]},
        {elib_pg_sql, [
            {'insert', 3, fun(_Table, _Data, _OnConflict) ->
                {<<"INSERT">>, []}
            end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end},
            {'update', 4, fun(_Table, _Data, _Where, _Params) -> {ok, 1} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        Data = #{<<"feedback_id">> => 123, <<"body">> => <<"简单回复"/utf8>>},
        Result = feedback_ds:add_reply(Data),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

add_with_empty_body_test_() ->
    ?WITH_MECKS([
        {elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"empty_body_md5">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Table, _Field, _Where, _Opts, _Default) -> 0 end}
        ]},
        {feedback_repo, [
            {'add', 11, fun(_Uid, _Did, _COS, _COSV, _AppVsn, _Type, _Rating, _ContactDetail, _Body, _Attach, _Md5) ->
                {ok, #{<<"id">> => 123}}
            end}
        ]}
    ], fun() ->
        Result = feedback_ds:add(1, <<"device">>, <<"ios">>, <<"15">>, <<"1.0">>, <<"bug">>, <<"1">>, <<>>, <<>>, <<>>),
        ?assertEqual(ok, Result)
    end).

add_with_utf8_content_test_() ->
    ?WITH_MECKS([
        {elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"utf8_content_md5">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Table, _Field, _Where, _Opts, _Default) -> 0 end}
        ]},
        {feedback_repo, [
            {'add', 11, fun(_Uid, _Did, _COS, _COSV, _AppVsn, _Type, _Rating, _ContactDetail, Body, _Attach, _Md5) ->
                ?assertEqual(<<"这是中文反馈内容"/utf8>>, Body),
                {ok, #{<<"id">> => 123}}
            end}
        ]}
    ], fun() ->
        Result = feedback_ds:add(1, <<"device">>, <<"ios">>, <<"15">>, <<"1.0">>, <<"bug">>, <<"5">>, <<>>, <<"这是中文反馈内容"/utf8>>, <<>>),
        ?assertEqual(ok, Result)
    end).

add_with_special_characters_test_() ->
    ?WITH_MECKS([
        {elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"special_chars_md5">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Table, _Field, _Where, _Opts, _Default) -> 0 end}
        ]},
        {feedback_repo, [
            {'add', 11, fun(_Uid, _Did, _COS, _COSV, _AppVsn, _Type, _Rating, ContactDetail, _Body, _Attach, _Md5) ->
                ?assertEqual(<<"test+label@example.com">>, ContactDetail),
                {ok, #{<<"id">> => 123}}
            end}
        ]}
    ], fun() ->
        Result = feedback_ds:add(1, <<"device">>, <<"ios">>, <<"15">>, <<"1.0">>, <<"bug">>, <<"5">>, <<"test+label@example.com">>, <<"body">>, <<>>),
        ?assertEqual(ok, Result)
    end).
