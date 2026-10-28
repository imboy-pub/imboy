-module(channel_comment_parent_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% DB 不可用时按 TEST_WITH_DB 惯例返回 [] skip，不得计为通过。
parent_id_pg_test_() ->
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
                            {channel_logic_common, [
                                {resolve_channel_id, 1, fun(_) -> 42 end},
                                {ensure_channel_content_access, 2, fun(_, _) -> ok end}
                            ]},
                            {channel_message_ds, [
                                {find_by_id, 1, fun(_) -> #{<<"channel_id">> => 42} end}
                            ]},
                            {user_ds, [{find_by_id, 2, fun(_, _) -> #{} end}]},
                            {channel_comment_ds, [
                                {add, 1, fun(Data) -> channel_comment_repo:add_tx(Conn, Data) end},
                                {find_by_id, 1, fun(Id) ->
                                    {ok, _, [{ParentId}]} = epgsql:equery(
                                        Conn,
                                        <<"SELECT parent_id FROM public.channel_comment WHERE id=$1">>,
                                        [Id]
                                    ),
                                    #{<<"parent_id">> => ParentId}
                                end}
                            ]}
                        ],
                        fun() ->
                            {ok, [], []} = epgsql:squery(Conn, "BEGIN"),
                            try
                                lists:foreach(
                                    fun({Input, Expected}) ->
                                        ?assertEqual(
                                            {ok, #{<<"parent_id">> => Expected}},
                                            channel_comment_logic:create(
                                                1001, <<"42">>, <<"99">>, <<"ok">>, Input
                                            )
                                        )
                                    end,
                                    [
                                        {9007199254740993, 9007199254740993},
                                        {<<"9007199254740993">>, 9007199254740993},
                                        {0, 0},
                                        {<<"0">>, 0}
                                    ]
                                )
                            after
                                epgsql:squery(Conn, "ROLLBACK")
                            end
                        end
                    )
            end}}.
