-module(friend_confirm_authorization_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% DB 不可用时按 TEST_WITH_DB 惯例返回 [] skip，不得计为通过。
confirm_recipient_pg_test_() ->
    {timeout, 60,
        {setup, fun eunit_runner:eunit_setup_with_db/0,
            fun
                ({ok, Conn}) -> eunit_runner:eunit_cleanup_db(Conn);
                (_) -> ok
            end, fun
                ({error, _}) ->
                    [];
                ({ok, Conn}) ->
                    ?WITH_MECKS(
                        [
                            {friend_ds, [
                                {pending_status, 2, fun(From, To) ->
                                    {ok, _, Rows} = epgsql:equery(
                                        Conn,
                                        <<"SELECT id FROM public.user_friend WHERE from_user_id=$1 AND to_user_id=$2 AND status=0">>,
                                        [From, To]
                                    ),
                                    case Rows of
                                        [] -> none;
                                        _ -> pending
                                    end
                                end},
                                {is_friend, 2, fun(_, _) -> false end},
                                {confirm_friend, 7, fun(_, From, To, _, _, _, _) ->
                                    {ok, 1} =
                                        case From of
                                            9007199254740991 ->
                                                epgsql:equery(
                                                    Conn,
                                                    <<"UPDATE public.user_friend SET status=1 WHERE from_user_id=$1 AND to_user_id=$2">>,
                                                    [From, To]
                                                );
                                            _ ->
                                                epgsql:equery(
                                                    Conn,
                                                    <<"INSERT INTO public.user_friend(id,from_user_id,to_user_id,status) VALUES($1,$2,$3,1)">>,
                                                    [9007199254740981, From, To]
                                                )
                                        end,
                                    ok
                                end},
                                {invalidate_cache, 2, fun(_, _) -> ok end}
                            ]},
                            {msg_s2c_ds, [{write_msg, 8, fun(_, _, _, _, _, _, _, _) -> ok end}]},
                            {message_ds, [
                                {assemble_msg, 8, fun(_, _, _, _, _, _, _, _) -> #{} end},
                                {send_next, 4, fun(_, _, _, _) -> ok end}
                            ]},
                            {elib_retry_config, [{intervals, 1, fun(_) -> [] end}]},
                            {imboy_cache, [{invalidate, 1, fun(_) -> ok end}]}
                        ],
                        fun() ->
                            From = 9007199254740991,
                            To = 9007199254740992,
                            {ok, [], []} = epgsql:squery(Conn, "BEGIN"),
                            try
                                {ok, 1} = epgsql:equery(
                                    Conn,
                                    <<"INSERT INTO public.user_friend(id,from_user_id,to_user_id,status) VALUES($1,$2,$3,0)">>,
                                    [9007199254740980, From, To]
                                ),
                                lists:foreach(
                                    fun(Actor) ->
                                        ?assertEqual(
                                            {error, <<"Parameter error">>, <<"to">>},
                                            friend_logic:confirm_friend(Actor, From, To, <<"{}">>)
                                        )
                                    end,
                                    [From, 9007199254740993]
                                ),
                                ?assertNot(meck:called(friend_ds, pending_status, '_')),
                                ?assertNot(meck:called(msg_s2c_ds, write_msg, '_')),
                                ?assertEqual([{From, To, 0}], rows(Conn, From, To)),
                                ?assertMatch(
                                    {ok, From, _, _},
                                    friend_logic:confirm_friend(
                                        To,
                                        <<"0", (integer_to_binary(From))/binary>>,
                                        <<"0", (integer_to_binary(To))/binary>>,
                                        <<"{}">>
                                    )
                                ),
                                ?assertEqual([{From, To, 1}, {To, From, 1}], rows(Conn, From, To)),
                                ?assertEqual(1, meck:num_calls(msg_s2c_ds, write_msg, '_')),
                                ?assert(
                                    meck:called(message_ds, assemble_msg, [
                                        <<"S2C">>,
                                        integer_to_binary(To),
                                        integer_to_binary(From),
                                        '_',
                                        '_',
                                        <<>>,
                                        <<"apply_friend_confirm">>,
                                        null
                                    ])
                                )
                            after
                                epgsql:squery(Conn, "ROLLBACK")
                            end
                        end
                    )
            end}}.

rows(Conn, From, To) ->
    {ok, _, Rows} = epgsql:equery(
        Conn,
        <<"SELECT from_user_id,to_user_id,status FROM public.user_friend WHERE (from_user_id=$1 AND to_user_id=$2) OR (from_user_id=$2 AND to_user_id=$1) ORDER BY from_user_id">>,
        [From, To]
    ),
    Rows.
