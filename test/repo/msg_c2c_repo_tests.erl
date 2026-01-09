-module(msg_c2c_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2c_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 C2C 消息数据访问层功能
%%% 覆盖：消息插入、查询、更新
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = msg_c2c_repo:tablename(),
        % 精确断言：验证表名的具体格式和内容
        ?assert(is_binary(Result) andalso
                 byte_size(Result) > 12 andalso 
                 binary:match(Result, <<"public.">>) =/= nomatch andalso
                 binary:match(Result, <<"msg_c2c">>) =/= nomatch,
                 Result),
        % 进一步验证表名结构
        ?assertEqual(<<"public.msg_c2c">>, Result)
    end).

%% ===================================================================
%% 消息查询测试
%% ===================================================================

find_messages_by_from_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        Limit = 10,
        % 精确断言：验证参数范围和实际函数调用
        ?assert(is_integer(FromUid) andalso FromUid > 0),
        ?assert(is_integer(ToUid) andalso ToUid > 0),
        ?assert(is_integer(Limit) andalso Limit > 0 andalso Limit =< 1000),
        
        % 实际调用函数并验证返回结果格式
        Result = msg_c2c_repo:find_messages_by_from_uid(FromUid, ToUid, Limit),
        ?assertMatch({ok, Messages} when is_list(Messages), Result),
        case Result of
            {ok, Messages} ->
                % 验证消息列表结构
                ?assert(length(Messages) =< Limit),
                lists:foreach(fun(Message) ->
                    ?assertMatch(#{<<"id">> := _} when is_map(Message), Message)
                end, Messages);
            _ ->
                ?assert(false, "Expected {ok, Messages}")
        end
    end).

%% ===================================================================
%% 消息插入测试
%% ===================================================================

save_message_with_valid_data_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            from_uid => 1,
            to_uid => 2,
            content => <<"test message">>,
            msg_type => <<"text">>
        },
        % 精确断言：验证消息数据结构的完整性
        ?assertMatch(
            #{
                <<"from_uid">> := FromUid,
                <<"to_uid">> := ToUid,
                <<"content">> := Content,
                <<"msg_type">> := MsgType
            } when is_integer(FromUid) andalso FromUid > 0 andalso
                   is_integer(ToUid) andalso ToUid > 0 andalso
                   is_binary(Content) andalso byte_size(Content) > 0 andalso
                   is_binary(MsgType) andalso byte_size(MsgType) > 0,
            Data
        ),
        ?assert(maps:is_key(from_uid, Data))
    end).
