-module(message_result_pg_tests).
-include_lib("eunit/include/eunit.hrl").

message_result_pg_test_() ->
    {setup,fun setup/0,fun cleanup/1,fun(_) -> {timeout,30,fun scoped_result/0} end}.

setup() ->
    {ok,Manifest} = file:read_file(required("ATTACHMENT_REVIEW_MANIFEST")),
    #{<<"database">> := Db} = jsone:decode(Manifest),
    <<"attv5_chain_",_/binary>> = Db,
    {ok,[Config]} = file:consult(required("ATTACHMENT_REVIEW_CONFIG")),
    Env = proplists:get_value(imboy,Config),
    Pg = proplists:get_value(pg_conf,Env),
    {epgsql,connect,[Connect0]} = maps:get(start_mfa,Pg),
    Host = maps:get(host,Connect0),
    true = lists:member(Host,["127.0.0.1",<<"127.0.0.1">>,"localhost",<<"localhost">>]),
    Connect = Connect0#{database => binary_to_list(Db)},
    application:set_env(imboy,sql_driver,pgsql),
    {ok,_} = application:ensure_all_started(epgsql),
    {ok,_} = application:ensure_all_started(pooler),
    {ok,_} = pooler:new_pool(Pg#{name => pgsql,start_mfa => {epgsql,connect,[Connect]}}),
    {ok,[#{<<"db">> := Db}]} = elib_pg:query(<<"SELECT current_database() AS db">>,[]),
    ok.

cleanup(_) -> pooler:rm_pool(pgsql).

required(Name) ->
    case os:getenv(Name) of
        false -> error({required_resource_missing,Name});
        "" -> error({required_resource_missing,Name});
        Value -> Value
    end.

scoped_result() ->
    Id = erlang:system_time(microsecond),
    MsgId = <<"result-fixture-",(integer_to_binary(Id))/binary>>,
    {ok,[]} = elib_pg:query(
        <<"INSERT INTO public.msg_store(id,chat_type,conv_key,conv_seq,msg_id,msg_type,"
          "from_id,to_id,payload) VALUES($1,'c2c','c2c:99191:99192',42,$2,'text',99191,99192,'fixture')">>,
        [Id,MsgId]),
    {ok,[#{<<"conv_seq">> := 42} = Row]} =
        msg_archive_repo:message_result(<<"c2c:99191:99192">>,MsgId,99191,0),
    ?assertEqual([<<"conv_seq">>],maps:keys(Row)),
    ?assertEqual({ok,[]},msg_archive_repo:message_result(<<"c2c:99191:99192">>,MsgId,99192,0)),
    ?assertEqual({ok,[]},msg_archive_repo:message_result(<<"c2c:99191:99193">>,MsgId,99191,0)),
    ?assertEqual({ok,[]},msg_archive_repo:message_result(<<"c2c:99191:99192">>,MsgId,99191,43)),
    ?assertEqual({ok,[]},msg_archive_repo:message_result(<<"c2c:99191:99192">>,<<"missing-fixture">>,99191,0)).
