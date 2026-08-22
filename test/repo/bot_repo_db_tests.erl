-module(bot_repo_db_tests).

%%%
% has_exchange/2 真库集成测试（TEST_WITH_DB，连本地 imboy_v1）
% 用独立 TSID 域的随机 uid 对做 insert→断言→清理，互不污染。
%%%

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

has_exchange_no_history_test_() ->
    ?TEST_WITH_DB(fun() ->
        {BotId, UserId} = random_uid_pair(),
        ?assertEqual(false, bot_repo:has_exchange(BotId, UserId))
    end).

has_exchange_user_to_bot_test_() ->
    ?TEST_WITH_DB(fun() ->
        {BotId, UserId} = random_uid_pair(),
        insert_c2c(UserId, BotId),
        try
            ?assertEqual(true, bot_repo:has_exchange(BotId, UserId))
        after
            cleanup_c2c(BotId, UserId)
        end
    end).

has_exchange_bot_to_user_test_() ->
    ?TEST_WITH_DB(fun() ->
        {BotId, UserId} = random_uid_pair(),
        insert_c2c(BotId, UserId),
        try
            ?assertEqual(true, bot_repo:has_exchange(BotId, UserId))
        after
            cleanup_c2c(BotId, UserId)
        end
    end).

%% ===================================================================
%% Internal
%% ===================================================================

%% 随机 uid 对：取当前毫秒 + 随机偏移，避开真实用户 ID 空间（测试即清理）
random_uid_pair() ->
    Base = erlang:system_time(millisecond) band 16#3FFFFFFFFF,
    {Base * 1000 + 1, Base * 1000 + 2}.

insert_c2c(FromId, ToId) ->
    MsgId = <<"test_hex_", (integer_to_binary(FromId))/binary>>,
    Sql =
        <<
            "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, payload,"
            " server_ts, created_at)"
            " VALUES ($1, $2, $3, $4, 'text', 'has_exchange_test', now(), now())"
        >>,
    {ok, _} = elib_pg:query(Sql, [FromId, FromId, ToId, MsgId]),
    ok.

cleanup_c2c(BotId, UserId) ->
    Sql = <<
        "DELETE FROM public.msg_c2c WHERE (from_id = $1 AND to_id = $2)"
        " OR (from_id = $2 AND to_id = $1)"
    >>,
    {ok, _} = elib_pg:query(Sql, [BotId, UserId]),
    ok.
