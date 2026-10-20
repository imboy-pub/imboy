-module(websocket_delivery_revoke_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/0]).

%% Explicit isolated PG fixture: the standard marker helper creates a synthetic
%% database, runs current migrations, and drops it. No auth/repo/SQL mock.
%% Run explicitly in a fresh VM; never replace the full suite's pgsql pool.
run() ->
    false = lists:keymember(pooler, 1, application:which_applications()),
    false = lists:keymember(imboy, 1, application:which_applications()),
    eunit:test({timeout, 900,
        {setup, fun setup/0, fun cleanup/1, fun(S) -> ?_test(check(S)) end}}, [verbose]).
setup() ->
    Original = #{apps => [A || {A,_,_} <- application:which_applications()],
        pooler_env => application:get_env(pooler, pools)},
    try
        S0 = inttest_marker_db:provision(#{env_prefix => <<"WS_DELIVERY_REVOKE_INTTEST">>}),
        setup_rows(S0#{original_runtime => Original})
    catch Class:Reason:Stack ->
        restore_runtime(Original), erlang:raise(Class, Reason, Stack)
    end.

setup_rows(S) ->
    try
        C = maps:get(conn,S),
        sql(C, <<"INSERT INTO public.\"user\"(id,account,password,reg_ip,reg_cosv) VALUES (99002001,'ws-owner','synthetic-disabled','127.0.0.1','synthetic'),(99002002,'ws-member','synthetic-disabled','127.0.0.1','synthetic')">>),
        sql(C, <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid,title,type,join_limit,status,scope,e2ee_mode) VALUES (99002011,99002001,99002001,'delivery revoke fixture',2,3,1,'personal',0)">>),
        sql(C, <<"INSERT INTO public.channel(id,name,creator_uid,scope,status,discussion_group_id,access_type) VALUES (99002021,'delivery revoke fixture',99002001,'personal',1,99002011,0)">>),
        sql(C, <<"INSERT INTO public.channel_subscription(id,channel_id,user_id,status) VALUES (99002041,99002021,99002002,1)">>),
        sql(C, <<"INSERT INTO public.group_member(id,group_id,user_id,role,status) VALUES (99002051,99002011,99002002,1,1)">>),
        meck:new(imboy_policy,[no_link]), meck:expect(imboy_policy,current_profile,fun()->standard end),
        meck:new(imboy_feature,[no_link]), meck:expect(imboy_feature,enabled,fun(channel)->true end),
        application:set_env(pooler,pools,[]), {ok,_}=application:ensure_all_started(pooler),
        Opts=(maps:with([host,port,username,password],maps:get(server,S)))#{database=>binary_to_list(maps:get(db,S))},
        {ok,_}=pooler:new_pool(#{name=>pgsql,init_count=>1,max_count=>1,start_mfa=>{epgsql,connect,[Opts]}}),
        S
    catch Class:Reason:Stack -> cleanup(S), erlang:raise(Class,Reason,Stack) end.
check(S) ->
    C=maps:get(conn,S),
    Bin= <<"{\"type\":\"C2G\",\"to\":99002011,\"id\":\"pg-retry\",\"conv_seq\":42,\"sender_did\":\"sender\",\"payload\":{\"ciphertext\":\"opaque.same\"}}">>,
    State=#{current_uid=>99002002,framing=>v2}, Key={99002002,<<"did">>,<<"pg-retry">>},
    ?assert(micro_community_access_repo:delivery_allowed(99002011,99002002)),
    {reply,First,State,hibernate}=websocket_handler:websocket_info({reply,Bin},State),
    ?assertEqual({binary,imboy_codec:wrap_v2_frame(16#21,0,Bin)},First),
    Ref=make_ref(), ack_retry_cache:set(Key,Ref,10000),
    sql(C, <<"UPDATE public.channel_subscription SET status=0 WHERE id=99002041">>),
    ?assertNot(micro_community_access_repo:delivery_allowed(99002011,99002002)),
    ?assertEqual({ok,State,hibernate},websocket_handler:websocket_info({timeout,Ref,{[1],Key,Bin}},State)),
    ?assertEqual(undefined,ack_retry_cache:get(Key)),
    sql(C, <<"UPDATE public.channel_subscription SET status=1 WHERE id=99002041">>),
    sql(C, <<"UPDATE public.group_member SET status=0 WHERE id=99002051">>),
    ?assertEqual({ok,State,hibernate},websocket_handler:websocket_info({reply,Bin},State)),
    pooler:rm_pool(pgsql),
    ?assertEqual({ok,State,hibernate},websocket_handler:websocket_info({reply,Bin},State)).
sql(C,Sql) -> assert_sql(epgsql:squery(C,Sql)).
assert_sql({ok,_}) -> ok;
assert_sql({ok,_,_}) -> ok;
assert_sql(Results) when is_list(Results), Results =/= [] ->
    lists:foreach(fun assert_sql/1, Results);
assert_sql(Other) -> error({unexpected_sql_result,Other}).

cleanup(S) ->
    try
        try pooler:rm_pool(pgsql) catch _:_ -> ok end,
        [meck:unload(M)||M<-[imboy_policy,imboy_feature],lists:member(M,meck:mocked())],
        inttest_marker_db:release(S)
    after
        restore_runtime(maps:get(original_runtime,S))
    end.

restore_runtime(#{apps := OriginalApps, pooler_env := OriginalEnv}) ->
    NewApps = [A || {A,_,_} <- application:which_applications(),
        not lists:member(A,OriginalApps)],
    case lists:member(pooler,NewApps) of true -> application:stop(pooler); false -> ok end,
    [application:stop(A) || A <- NewApps, A =/= pooler],
    case OriginalEnv of
        undefined -> application:unset_env(pooler,pools);
        {ok,Value} -> application:set_env(pooler,pools,Value)
    end,
    ok.
