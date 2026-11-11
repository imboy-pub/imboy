-module(face_to_face_http_tests).
-export([setup/0, cleanup/1, run/1]).
-include_lib("eunit/include/eunit.hrl").

%% 真 JWT/Cowboy/PG；既有 fixture 仅替代外部存储，不启动共享应用。
face_to_face_http_test_() ->
    isolated_pg_fixture:test(fun() ->
        {setup, fun setup/0, fun cleanup/1, fun(S) -> {timeout, 90, fun() -> run(S) end} end}
    end).

%% 同一隔离 fixture 供真实 HTTP 和真机专项复用；只包含合成用户。
setup() ->
    S = intbe02_http_support:setup_all(),
    ok = intbe02_http_support:sql_exec(
        maps:get(conn, S),
        <<"UPDATE public.\"user\" SET nickname=CASE id WHEN 995011 THEN 'F2F Alice' ELSE 'F2F Bob' END WHERE id IN (995011,995012)">>
    ),
    S.

cleanup(S) ->
    intbe02_http_support:teardown_all(S),
    inttest_marker_db:release(S).

run(S) ->
    application:set_env(imboy, cluster_nodes, []),
    application:set_env(imboy, dsync_enabled, false),
    {ok, Cache} = imboy_cache_sync:start_link(),
    try
        _ = throttle:setup(three_second_once, 1, 3000),
        matched_members_and_friend_request(S),
        concurrent_match(S),
        invalid_and_expired(S),
        quota_and_rollback(S)
    after
        gen_server:stop(Cache)
    end.

matched_members_and_friend_request(S) ->
    First = request(S, 995011, <<"GET">>, match_path(<<"0123">>, <<"116.4000">>), #{}),
    Gid = maps:get(<<"gid">>, payload(First)),
    ?assertEqual([995011], member_ids(payload(First))),
    Second = request(S, 995012, <<"GET">>, match_path(<<"0123">>, <<"116.4001">>), #{}),
    ?assertEqual(Gid, maps:get(<<"gid">>, payload(Second))),
    ?assertEqual([995011, 995012], member_ids(payload(Second))),
    Save = #{<<"gid">> => Gid, <<"code">> => <<"0123">>},
    [
        begin
            Saved = payload(request(S, Uid, <<"POST">>, <<"/api/v1/group/face2face_save">>, Save)),
            ?assertEqual(Gid, maps:get(<<"id">>, maps:get(<<"group">>, Saved))),
            ?assertEqual([995011, 995012], member_ids(Saved))
        end
     || Uid <- [995012, 995011, 995011]
    ],
    #{<<"owner_uid">> := Owner, <<"member_count">> := Count} =
        intbe02_http_support:one(
            maps:get(conn, S),
            <<"SELECT owner_uid, member_count FROM public.\"group\" WHERE id=$1">>,
            [Gid]
        ),
    ?assertEqual(995011, Owner),
    ?assertEqual(2, Count),
    ?assert(
        maps:get(
            <<"code">>,
            request(
                S,
                995013,
                <<"POST">>,
                <<"/api/v1/group/face2face_save">>,
                Save
            )
        ) =/= 0
    ),
    %% 从面对面头像进入资料页后，复用现有申请/接受接口建立双向好友关系。
    Apply = #{
        <<"to">> => <<"995012">>,
        <<"created_at">> => erlang:system_time(millisecond),
        <<"payload">> => #{
            <<"from">> => #{<<"source">> => <<"group_member">>},
            <<"message">> => <<"fixture">>
        }
    },
    payload(request(S, 995011, <<"POST">>, <<"/api/v1/friend/add">>, Apply)),
    Accept = #{
        <<"from">> => <<"995011">>,
        <<"to">> => <<"995012">>,
        <<"payload">> => #{<<"from">> => #{<<"source">> => <<"group_member">>}, <<"to">> => #{}}
    },
    payload(request(S, 995012, <<"POST">>, <<"/api/v1/friend/confirm">>, Accept)),
    ?assertEqual(true, friend_ds:is_friend(995011, 995012)),
    ?assertEqual(true, friend_ds:is_friend(995012, 995011)),
    ok.

concurrent_match(_S) ->
    Parent = self(),
    [
        spawn(fun() ->
            Parent ! {matched, group_logic:face2face(Uid, <<"2345">>, <<"116.5">>, <<"39.9">>)}
        end)
     || Uid <- [995014, 995015]
    ],
    Results = [
        receive
            {matched, Result} -> Result
        after 10000 -> error(match_timeout)
        end
     || _ <- [1, 2]
    ],
    [{ok, Gid}, {ok, Gid}] = Results,
    ?assertEqual(2, length(group_ds:member_uids(Gid))),
    {ok, FarGid} = group_logic:face2face(995016, <<"2345">>, <<"117.5">>, <<"39.9">>),
    ?assertNotEqual(Gid, FarGid).

invalid_and_expired(S) ->
    Invalid = request(S, 995021, <<"GET">>, match_path(<<"12ab">>, <<"116.4">>), #{}),
    ?assert(maps:get(<<"code">>, Invalid) =/= 0),
    ?assertMatch({error, _}, group_logic:face2face(995021, <<"1234">>, <<"0.0">>, <<"0.0">>)),
    {ok, Gid} = group_logic:face2face(995021, <<"9876">>, <<"116.4">>, <<"39.9">>),
    ok = intbe02_http_support:sql_exec(
        maps:get(conn, S),
        <<"UPDATE public.group_random_code SET validity_at=now()-interval '1 second' WHERE group_id=$1">>,
        [Gid]
    ),
    Expired = request(
        S,
        995021,
        <<"POST">>,
        <<"/api/v1/group/face2face_save">>,
        #{<<"gid">> => Gid, <<"code">> => <<"9876">>}
    ),
    ?assert(maps:get(<<"code">>, Expired) =/= 0),
    ?assert(
        maps:get(
            <<"code">>,
            request(
                S,
                995021,
                <<"POST">>,
                <<"/api/v1/group/face2face_save">>,
                #{<<"gid">> => <<"bad">>, <<"code">> => <<"9876">>}
            )
        ) =/= 0
    ).

quota_and_rollback(S) ->
    Conn = maps:get(conn, S),
    ok = intbe02_http_support:sql_exec(
        Conn,
        <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid) SELECT 998000000+i,995017,995017 FROM generate_series(1,100) i">>
    ),
    ?assertEqual(
        {error, <<"每人最多创建100个群"/utf8>>},
        group_logic:face2face(995017, <<"4567">>, <<"116.4">>, <<"39.9">>)
    ),
    ok = intbe02_http_support:sql_exec(
        Conn,
        <<
            "CREATE FUNCTION f2f_fixture_reject() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN "
            "IF NEW.owner_uid=995002 THEN RAISE EXCEPTION 'fixture_storage_failure'; END IF; RETURN NEW; END $$"
        >>
    ),
    ok = intbe02_http_support:sql_exec(
        Conn,
        <<"CREATE TRIGGER f2f_fixture_reject BEFORE INSERT ON public.\"group\" FOR EACH ROW EXECUTE FUNCTION f2f_fixture_reject()">>
    ),
    ?assertEqual(
        {error, <<"面对面匹配失败，请重试"/utf8>>},
        group_logic:face2face(995002, <<"6789">>, <<"116.4">>, <<"39.9">>)
    ),
    #{<<"n">> := 0} = intbe02_http_support:one(
        Conn,
        <<"SELECT count(*) AS n FROM public.group_random_code WHERE code='6789'">>
    ),
    ok.

match_path(Code, Lng) ->
    <<"/api/v1/group/face2face?code=", Code/binary, "&longitude=", Lng/binary, "&latitude=39.9">>.

request(S, Uid, Method, Path, Body) ->
    Headers = intbe02_http_support:auth(token_ds:encrypt_token(Uid)),
    #{status := Status, body := Response} = intbe02_http_support:http(
        maps:get(port, S), Method, Path, Body, Headers
    ),
    ?assert(Status >= 200 andalso Status < 500),
    jsone:decode(Response).

payload(#{<<"code">> := 0, <<"payload">> := Payload}) -> Payload;
payload(Response) -> error({api_failed, Response}).

member_ids(Payload) ->
    lists:sort([maps:get(<<"user_id">>, Member) || Member <- maps:get(<<"member_list">>, Payload)]).
