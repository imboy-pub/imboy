-module(message_result_pg_tests).
-include_lib("eunit/include/eunit.hrl").

message_result_pg_test_() ->
    isolated_pg_fixture:test(fun message_result_fixture/0).

message_result_fixture() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> {timeout, 30, fun scoped_result/0} end}.

setup() ->
    {ok, Manifest} = file:read_file(required("ATTACHMENT_REVIEW_MANIFEST")),
    #{<<"database">> := Db} = jsone:decode(Manifest),
    <<"attv5_chain_", _/binary>> = Db,
    {ok, [Config]} = file:consult(required("ATTACHMENT_REVIEW_CONFIG")),
    Env = proplists:get_value(imboy, Config),
    Pg = proplists:get_value(pg_conf, Env),
    {epgsql, connect, [Connect0]} = maps:get(start_mfa, Pg),
    Host = maps:get(host, Connect0),
    true = lists:member(Host, ["127.0.0.1", <<"127.0.0.1">>, "localhost", <<"localhost">>]),
    Connect = Connect0#{database => binary_to_list(Db)},
    application:set_env(imboy, sql_driver, pgsql),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    {ok, _} = pooler:new_pool(Pg#{name => pgsql, start_mfa => {epgsql, connect, [Connect]}}),
    {ok, [#{<<"db">> := Db}]} = elib_pg:query(<<"SELECT current_database() AS db">>, []),
    ok.

cleanup(_) -> pooler:rm_pool(pgsql).

required(Name) ->
    case os:getenv(Name) of
        false -> error({required_resource_missing, Name});
        "" -> error({required_resource_missing, Name});
        Value -> Value
    end.

scoped_result() ->
    %% A clean peer needs no TSID server; these IDs isolate this fixture's conversation.
    <<Seed:62, _:2>> = crypto:strong_rand_bytes(8),
    Id = Seed + 1,
    FromId = Seed + 2,
    ToId = Seed + 3,
    ConvKey = msg_archive_repo:conv_key(<<"c2c">>, FromId, ToId),
    OtherConvKey = msg_archive_repo:conv_key(<<"c2c">>, FromId, Seed + 4),
    MsgId = <<"result-", (elib_uuid:gen_v7())/binary>>,
    try
        {ok, 1} = personal_message_order_repo:execute_formal(FromId, ToId, MsgId,
            <<
                "INSERT INTO public.msg_store(id,chat_type,conv_key,conv_seq,msg_id,msg_type,"
                "from_id,to_id,payload) VALUES($1,'c2c',$2,$6,$3,'text',$4,$5,'fixture')"
            >>,
            [Id, ConvKey, MsgId, FromId, ToId]
        ),
        {ok, Seq} = personal_message_order_repo:accepted_sequence(FromId, ToId, MsgId),
        ?assert(is_integer(Seq) andalso Seq > 0),
        {ok, [#{<<"conv_seq">> := Seq} = Row]} =
            msg_archive_repo:message_result(ConvKey, MsgId, FromId, 0),
        ?assertEqual([<<"conv_seq">>], maps:keys(Row)),
        ?assertEqual({ok, [Row]}, msg_archive_repo:message_result(ConvKey, MsgId, FromId, Seq)),
        ?assertEqual({ok, []}, msg_archive_repo:message_result(ConvKey, MsgId, ToId, 0)),
        ?assertEqual({ok, []}, msg_archive_repo:message_result(OtherConvKey, MsgId, FromId, 0)),
        ?assertEqual(
            {ok, []}, msg_archive_repo:message_result(ConvKey, MsgId, FromId, Seq + 1)
        ),
        ?assertEqual(
            {ok, []},
            msg_archive_repo:message_result(ConvKey, <<"missing-fixture">>, FromId, 0)
        )
    after
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM public.msg_store WHERE conv_key=$1 AND msg_id=$2">>, [ConvKey, MsgId]
        ),
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM public.msg_store_seq WHERE conv_key=$1">>, [ConvKey]
        )
    end.
