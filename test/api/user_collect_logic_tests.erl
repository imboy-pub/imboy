-module(user_collect_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_collect_logic 模块的 EUnit 测试
%%%
%%% 目标：验证用户收藏业务逻辑功能
%%% 覆盖：添加收藏、删除收藏、查询收藏、修改收藏
%%%===================================================================

%% ===================================================================
%% 添加收藏测试
%% ===================================================================

add_text_collect_success_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'count_by_uid_kind_id', 2, fun(_Uid, _KindId) -> 0 end}
    ], fun() ->
        ?WITH_MECK(elib_pg, [
            {'with_tx', 1, fun(_TxFun) -> ok end}
        ], fun() ->
            Uid = 12345,
            Kind = <<"1">>,  % 文本收藏
            KindId = <<"msg123">>,
            Info = [{<<"payload">>, [{<<"content">>, <<"Hello World">>}]}],
            Source = <<"chat">>,
            Remark = <<"重要消息">>,
            
            Result = user_collect_logic:add(Uid, Kind, KindId, Info, Source, Remark),
            ?assertEqual({ok, <<"success">>}, Result)
        end)
    end).

add_image_collect_success_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'count_by_uid_kind_id', 2, fun(_Uid, _KindId) -> 0 end}
    ], fun() ->
        ?WITH_MECK(elib_uri, [
            {'get_params', 1, fun(_Uri) -> {#{path => "/uploads/img.jpg"}, []} end}
        ], fun() ->
            ?WITH_MECK(elib_pg, [
                {'with_tx', 1, fun(_TxFun) -> ok end}
            ], fun() ->
                Uid = 12345,
                Kind = <<"2">>,  % 图片收藏
                KindId = <<"img123">>,
                Info = [{<<"payload">>, [
                    {<<"uri">>, <<"http://example.com/img.jpg">>},
                    {<<"md5">>, <<"abc123">>},
                    {<<"size">>, 1024},
                    {<<"name">>, <<"image.jpg">>}
                ]}],
                Source = <<"chat">>,
                Remark = <<"图片收藏">>,
                
                Result = user_collect_logic:add(Uid, Kind, KindId, Info, Source, Remark),
                ?assertEqual({ok, <<"success">>}, Result)
            end)
        end)
    end).

add_collect_already_exists_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'count_by_uid_kind_id', 2, fun(_Uid, _KindId) -> 1 end}
    ], fun() ->
        Uid = 12345,
        Kind = <<"1">>,
        KindId = <<"msg123">>,
        Info = [{<<"payload">>, [{<<"content">>, <<"Hello World">>}]}],
        Source = <<"chat">>,
        Remark = <<"重要消息">>,
        
        Result = user_collect_logic:add(Uid, Kind, KindId, Info, Source, Remark),
        ?assertEqual({ok, <<"success">>}, Result)
    end).

add_collect_empty_source_test_() ->
    Uid = 12345,
    Kind = <<"1">>,
    KindId = <<"msg123">>,
    Info = [{<<"payload">>, [{<<"content">>, <<"Hello World">>}]}],
    Source = <<>>,  % 空来源
    Remark = <<"重要消息">>,
    
    Result = user_collect_logic:add(Uid, Kind, KindId, Info, Source, Remark),
    ?assertEqual({error, <<"source is empty">>}, Result).

add_collect_empty_kind_id_test_() ->
    Uid = 12345,
    Kind = <<"1">>,
    KindId = <<>>,  % 空资源ID
    Info = [{<<"payload">>, [{<<"content">>, <<"Hello World">>}]}],
    Source = <<"chat">>,
    Remark = <<"重要消息">>,
    
    Result = user_collect_logic:add(Uid, Kind, KindId, Info, Source, Remark),
    ?assertEqual({error, <<"kind_id is empty">>}, Result).

add_collect_unsupported_kind_test_() ->
    Uid = 12345,
    Kind = <<"99">>,  % 不支持的收藏类型
    KindId = <<"msg123">>,
    Info = [{<<"payload">>, [{<<"content">>, <<"Hello World">>}]}],
    Source = <<"chat">>,
    Remark = <<"重要消息">>,
    
    Result = user_collect_logic:add(Uid, Kind, KindId, Info, Source, Remark),
    ?assertEqual({error, <<"Unsupported collection kind">>}, Result).

%% ===================================================================
%% 删除收藏测试
%% ===================================================================

remove_collect_success_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'delete', 2, fun(_Uid, _KindId) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        KindId = <<"msg123">>,
        
        Result = user_collect_logic:remove(Uid, KindId),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% 修改收藏测试
%% ===================================================================

change_collect_remark_test_() ->
    ?WITH_MECK(userboy_dt, [
        {'now', 0, fun() -> 1640995200 end}  % 固定时间戳
    ], fun() ->
        ?WITH_MECK(user_collect_repo, [
            {'update', 3, fun(_Uid, _KindId, _Data) -> {ok, 1} end}
        ], fun() ->
            Uid = 12345,
            Action = <<"remark">>,
            KindId = <<"msg123">>,
            PostVals = [{<<"remark">>, <<"更新后的备注">>}],
            
            Result = user_collect_logic:change(Uid, Action, KindId, PostVals),
            ?assertEqual(ok, Result)
        end)
    end).

change_collect_transpond_callback_test_() ->
    ?WITH_MECK(elib_dt, [
        {'now', 0, fun() -> 1640995200 end}
    ], fun() ->
        ?WITH_MECK(user_collect_repo, [
            {'update', 3, fun(_Uid, _KindId, _Data) -> {ok, 1} end}
        ], fun() ->
            Uid = 12345,
            Action = <<"transpond_callback">>,
            KindId = <<"msg123">>,
            PostVals = [],

            Result = user_collect_logic:change(Uid, Action, KindId, PostVals),
            ?assertEqual(ok, Result)
        end)
    end).
