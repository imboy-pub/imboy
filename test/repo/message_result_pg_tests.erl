-module(message_result_pg_tests).
-include_lib("eunit/include/eunit.hrl").

message_result_pg_test_() ->
    isolated_pg_fixture:test(fun message_result_fixture/0).

message_result_fixture() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        {timeout, 30, fun() ->
            scoped_result(),
            group_result()
        end}
    end}.

setup() ->
    {ok, Manifest} = file:read_file(required("ATTACHMENT_REVIEW_MANIFEST")),
    #{<<"database">> := Db} = Identity = jsone:decode(Manifest),
    {ok, [Config]} = file:consult(required("ATTACHMENT_REVIEW_CONFIG")),
    Env = proplists:get_value(imboy, Config),
    ok = attachment_w403_owned_fixture:assert_chain_fixture_database(
        Db, Identity, Env, required("ATTACHMENT_REVIEW_MANIFEST")
    ),
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

group_result() ->
    <<Seed:48, _/binary>> = crypto:strong_rand_bytes(8),
    Uid = Seed + 1,
    Gid = Seed + 2,
    MemberId = Seed + 3,
    MsgId = <<"result-g-", (integer_to_binary(Seed))/binary>>,
    Account = <<"result-", (integer_to_binary(Seed))/binary>>,
    ConvKey = msg_archive_repo:conv_key(<<"c2g">>, Uid, Gid),
    try
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.\"user\"(id,account,nickname,password,reg_ip,reg_cosv) VALUES($1,$2,'test','test','127.0.0.1','test')">>,
            [Uid, Account]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid,scope,status) VALUES($1,$2,$2,'personal',1)">>,
            [Gid, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.group_member(id,group_id,user_id,role,is_join,status) VALUES($1,$2,$3,1,true,1)">>,
            [MemberId, Gid, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.group_member_generation(group_id,user_id,start_seq) VALUES($1,$2,5)">>,
            [Gid, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.msg_c2g_recipient_snapshot(msg_id,from_id,to_gid,conv_seq,recipient_uids,created_at) VALUES($1,$2,$3,8,ARRAY[$2::bigint],now())">>,
            [MsgId, Uid, Gid]
        ),
        ?assertEqual(
            {ok, [#{<<"conv_seq">> => 8}]},
            msg_archive_repo:message_result_with_staging(<<"c2g">>, Uid, Gid, MsgId, ConvKey, 0)
        ),
        group_revoke_before_result(Uid, Gid, MsgId, ConvKey),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.group_member SET status=1 WHERE group_id=$1 AND user_id=$2">>, [
                Gid, Uid
            ]
        ),
        workspace_revoke_result(Uid, Gid, MsgId, ConvKey, Seed + 4),
        discussion_revoke_result(Uid, Gid, MsgId, ConvKey, Seed + 30),
        group_revoke_lock(Uid, Gid, MsgId, ConvKey),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.group_member SET status=1 WHERE group_id=$1 AND user_id=$2">>, [
                Gid, Uid
            ]
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.group_member_generation SET start_seq=9 WHERE group_id=$1 AND user_id=$2">>,
            [Gid, Uid]
        ),
        ?assertEqual(
            {ok, []},
            msg_archive_repo:message_result_with_staging(<<"c2g">>, Uid, Gid, MsgId, ConvKey, 0)
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.group_member SET status=0 WHERE group_id=$1 AND user_id=$2">>, [
                Gid, Uid
            ]
        ),
        ?assertEqual(
            {error, permission_denied},
            msg_archive_repo:message_result_with_staging(<<"c2g">>, Uid, Gid, MsgId, ConvKey, 0)
        )
    after
        elib_pg:execute(<<"DELETE FROM public.msg_c2g_recipient_snapshot WHERE msg_id=$1">>, [MsgId]),
        elib_pg:execute(<<"DELETE FROM public.group_member_generation WHERE group_id=$1">>, [Gid]),
        elib_pg:execute(<<"DELETE FROM public.group_member WHERE group_id=$1">>, [Gid]),
        elib_pg:execute(<<"DELETE FROM public.\"group\" WHERE id=$1">>, [Gid]),
        elib_pg:execute(<<"DELETE FROM public.\"user\" WHERE id=$1">>, [Uid])
    end.

discussion_revoke_result(Uid, Gid, MsgId, ConvKey, Base) ->
    Owner = Base + 1,
    Channel = Base + 2,
    Account = <<"mc-", (integer_to_binary(Owner))/binary>>,
    OrderNo = <<"order-", (integer_to_binary(Base))/binary>>,
    meck:new(imboy_policy, [passthrough, no_link]),
    meck:new(imboy_feature, [passthrough, no_link]),
    meck:expect(imboy_policy, current_profile, fun() -> community end),
    meck:expect(imboy_feature, enabled, fun
        (channel) -> true;
        (_) -> false
    end),
    try
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.\"user\"(id,account,nickname,password,reg_ip,reg_cosv) VALUES($1,$2,'test','test','127.0.0.1','test')">>,
            [Owner, Account]
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.\"group\" SET owner_uid=$2,creator_uid=$2,type=2,join_limit=3,e2ee_mode=0 WHERE id=$1">>,
            [Gid, Owner]
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.group_member SET role=3 WHERE group_id=$1 AND user_id=$2">>, [Gid, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.channel(id,name,creator_uid,scope,status,access_type,discussion_group_id) VALUES($1,'result-test',$2,'personal',1,1,$3)">>,
            [Channel, Owner, Gid]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.channel_subscription(id,channel_id,user_id,status) VALUES($1,$2,$3,1)">>,
            [Base + 3, Channel, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.channel_order(id,channel_id,user_id,order_no,amount,status,expires_at) VALUES($1,$2,$3,$4,1,1,now()+interval '1 day')">>,
            [Base + 4, Channel, Uid, OrderNo]
        ),
        ?assertMatch(
            {ok, [_]},
            msg_archive_repo:message_result_with_staging(<<"c2g">>, Uid, Gid, MsgId, ConvKey, 0)
        ),
        concurrent_send_and_result(Uid, Gid, MsgId, ConvKey, Base),
        entitlement_revoke_before_result(
            Uid,
            Gid,
            MsgId,
            ConvKey,
            <<"UPDATE public.channel_subscription SET status=0 WHERE channel_id=$1 AND user_id=$2">>,
            [Channel, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.channel_subscription SET status=1 WHERE channel_id=$1 AND user_id=$2">>,
            [Channel, Uid]
        ),
        entitlement_revoke_before_result(
            Uid,
            Gid,
            MsgId,
            ConvKey,
            <<"UPDATE public.channel_order SET status=2 WHERE channel_id=$1 AND user_id=$2">>,
            [Channel, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.channel_subscription SET status=0 WHERE channel_id=$1 AND user_id=$2">>,
            [Channel, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.channel_admin(id,channel_id,user_id,role) VALUES($1,$2,$3,2)">>, [
                Base + 5, Channel, Uid
            ]
        ),
        ?assertMatch(
            {ok, [_]},
            msg_archive_repo:message_result_with_staging(<<"c2g">>, Uid, Gid, MsgId, ConvKey, 0)
        ),
        entitlement_revoke_before_result(
            Uid,
            Gid,
            MsgId,
            ConvKey,
            <<"UPDATE public.channel_admin SET role=1 WHERE channel_id=$1 AND user_id=$2">>,
            [Channel, Uid]
        )
    after
        elib_pg:execute(<<"DELETE FROM public.channel_admin WHERE channel_id=$1">>, [Channel]),
        elib_pg:execute(<<"DELETE FROM public.channel_order WHERE channel_id=$1">>, [Channel]),
        elib_pg:execute(<<"DELETE FROM public.channel_subscription WHERE channel_id=$1">>, [Channel]),
        elib_pg:execute(<<"DELETE FROM public.channel WHERE id=$1">>, [Channel]),
        elib_pg:execute(
            <<"UPDATE public.\"group\" SET owner_uid=$2,creator_uid=$2 WHERE id=$1">>, [Gid, Uid]
        ),
        elib_pg:execute(<<"DELETE FROM public.\"user\" WHERE id=$1">>, [Owner]),
        meck:unload(imboy_feature),
        meck:unload(imboy_policy)
    end.

concurrent_send_and_result(Uid, Gid, MsgId, ConvKey, Base) ->
    ok = elib_tsid:init(#{dc_id => 0, node_id => 0}),
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO public.msg_store_seq(conv_key,seq) VALUES($1,8) ON CONFLICT(conv_key) DO UPDATE SET seq=8">>,
        [ConvKey]
    ),
    Parent = self(),
    Ids = [
        <<"c2g-result-send-", (integer_to_binary(Base))/binary, "-", (integer_to_binary(N))/binary>>
     || N <- lists:seq(1, 4)
    ],
    try
        Pids = [
            spawn_link(fun() ->
                receive
                    go -> ok
                end,
                Now = <<"2026-01-01T00:00:00Z">>,
                Result = msg_store_repo:stage(
                    <<"c2g">>,
                    Id,
                    <<"text">>,
                    <<>>,
                    null,
                    <<"{}">>,
                    Uid,
                    Gid,
                    Now,
                    Now,
                    <<>>,
                    1,
                    null
                ),
                Parent ! {send_result, Result}
            end)
         || Id <- Ids
        ],
        Readers = [
            spawn_link(fun() ->
                receive
                    go -> ok
                end,
                Parent !
                    {read_result,
                        msg_archive_repo:message_result_with_staging(
                            <<"c2g">>, Uid, Gid, MsgId, ConvKey, 0
                        )}
            end)
         || _ <- Ids
        ],
        lists:foreach(fun(Pid) -> Pid ! go end, Pids ++ Readers),
        lists:foreach(
            fun(_) ->
                receive
                    {send_result, {ok, _, _, _}} -> ok;
                    {send_result, Error} -> error({send_failed, Error})
                after 10000 -> error(send_timeout)
                end
            end,
            Ids
        ),
        lists:foreach(
            fun(_) ->
                receive
                    {read_result, {ok, [_]}} -> ok;
                    {read_result, Error} -> error({read_failed, Error})
                after 10000 -> error(read_timeout)
                end
            end,
            Ids
        )
    after
        lists:foreach(
            fun(Id) ->
                elib_pg:execute(
                    <<"DELETE FROM public.msg_store_staging WHERE type='c2g' AND msg_id=$1">>, [Id]
                ),
                elib_pg:execute(
                    <<"DELETE FROM public.msg_c2g_recipient_snapshot WHERE msg_id=$1">>, [Id]
                ),
                elib_pg:execute(<<"DELETE FROM public.msg_c2g_request_ledger WHERE msg_id=$1">>, [
                    Id
                ])
            end,
            Ids
        ),
        elib_pg:execute(<<"DELETE FROM public.msg_store_seq WHERE conv_key=$1">>, [ConvKey])
    end.

workspace_revoke_result(Uid, Gid, MsgId, ConvKey, Workspace) ->
    Owner = Workspace + 20,
    Account = <<"ws-", (integer_to_binary(Owner))/binary>>,
    try
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.\"user\"(id,account,nickname,password,reg_ip,reg_cosv) VALUES($1,$2,'test','test','127.0.0.1','test')">>,
            [Owner, Account]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.workspace(id,name,owner_id) VALUES($1,'result-test',$2)">>, [
                Workspace, Owner
            ]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.workspace_member(workspace_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
            [Workspace, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.\"group\" SET scope='workspace',workspace_id=$2 WHERE id=$1">>, [
                Gid, Workspace
            ]
        ),
        ?assertMatch(
            {ok, [_]},
            msg_archive_repo:message_result_with_staging(<<"c2g">>, Uid, Gid, MsgId, ConvKey, 0)
        ),
        entitlement_revoke_before_result(
            Uid,
            Gid,
            MsgId,
            ConvKey,
            <<"UPDATE public.workspace_member SET status='removed' WHERE workspace_id=$1 AND user_id=$2">>,
            [Workspace, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.workspace_member SET status='active' WHERE workspace_id=$1 AND user_id=$2">>,
            [Workspace, Uid]
        ),
        organization_revoke_result(Uid, Gid, MsgId, ConvKey, Workspace)
    after
        elib_pg:execute(
            <<"UPDATE public.\"group\" SET scope='personal',workspace_id=NULL WHERE id=$1">>, [Gid]
        ),
        elib_pg:execute(<<"DELETE FROM public.workspace_member WHERE workspace_id=$1">>, [Workspace]),
        elib_pg:execute(<<"DELETE FROM public.workspace WHERE id=$1">>, [Workspace]),
        elib_pg:execute(<<"DELETE FROM public.\"user\" WHERE id=$1">>, [Owner])
    end.

organization_revoke_result(Uid, Gid, MsgId, ConvKey, Workspace) ->
    Owner = Workspace + 10,
    Org = Workspace + 11,
    Account = <<"org-", (integer_to_binary(Owner))/binary>>,
    try
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.\"user\"(id,account,nickname,password,reg_ip,reg_cosv) VALUES($1,$2,'test','test','127.0.0.1','test')">>,
            [Owner, Account]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.organization(id,name,owner_id) VALUES($1,'result-test',$2)">>, [
                Org, Owner
            ]
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.organization_member(organization_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
            [Org, Uid]
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.workspace SET organization_id=$2 WHERE id=$1">>, [Workspace, Org]
        ),
        ?assertMatch(
            {ok, [_]},
            msg_archive_repo:message_result_with_staging(<<"c2g">>, Uid, Gid, MsgId, ConvKey, 0)
        ),
        entitlement_revoke_before_result(
            Uid,
            Gid,
            MsgId,
            ConvKey,
            <<"UPDATE public.organization_member SET status='removed' WHERE organization_id=$1 AND user_id=$2">>,
            [Org, Uid]
        )
    after
        elib_pg:execute(<<"UPDATE public.workspace SET organization_id=NULL WHERE id=$1">>, [
            Workspace
        ]),
        elib_pg:execute(<<"DELETE FROM public.organization WHERE id=$1">>, [Org]),
        elib_pg:execute(<<"DELETE FROM public.\"user\" WHERE id=$1">>, [Owner])
    end.

group_revoke_before_result(Uid, Gid, MsgId, ConvKey) ->
    entitlement_revoke_before_result(
        Uid,
        Gid,
        MsgId,
        ConvKey,
        <<"UPDATE public.group_member SET status=0 WHERE group_id=$1 AND user_id=$2">>,
        [Gid, Uid]
    ).

entitlement_revoke_before_result(Uid, Gid, MsgId, ConvKey, Sql, Params) ->
    Parent = self(),
    Holder = spawn_link(fun() ->
        Result = elib_pg:with_tx(fun(Conn) ->
            {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role=replica">>, []),
            {ok, _} = elib_pg:execute(Conn, Sql, Params),
            Parent ! revoked_uncommitted,
            receive
                release -> ok
            after 5000 -> error(lock_timeout)
            end
        end),
        Parent ! {revocation_committed, Result}
    end),
    receive
        revoked_uncommitted -> ok
    after 5000 -> error(revocation_not_started)
    end,
    spawn_link(fun() ->
        Parent ! result_started,
        Result = msg_archive_repo:message_result_with_staging(
            <<"c2g">>, Uid, Gid, MsgId, ConvKey, 0
        ),
        Parent ! {query_result, Result}
    end),
    receive
        result_started -> ok
    after 5000 -> error(result_not_started)
    end,
    receive
        {query_result, _} -> error(query_did_not_wait)
    after 200 -> ok
    end,
    Holder ! release,
    receive
        {revocation_committed, ok} -> ok;
        {revocation_committed, Failed} -> error({revocation_failed, Failed})
    after 5000 -> error(revocation_not_committed)
    end,
    receive
        {query_result, {error, _}} -> ok;
        {query_result, Other} -> error({unsafe_result, Other})
    after 5000 -> error(result_did_not_finish)
    end.

group_revoke_lock(Uid, Gid, MsgId, ConvKey) ->
    Parent = self(),
    Holder = spawn_link(fun() ->
        Result = elib_pg:with_tx(fun(Conn) ->
            {ok, [_]} = elib_pg:query(
                Conn,
                <<"SELECT id FROM public.group_member WHERE group_id=$1 AND user_id=$2 FOR SHARE">>,
                [Gid, Uid]
            ),
            Parent ! locked,
            receive
                release -> ok
            after 5000 -> error(lock_timeout)
            end
        end),
        Parent ! {released, Result}
    end),
    receive
        locked -> ok
    after 5000 -> error(lock_failed)
    end,
    spawn_link(fun() ->
        Parent ! update_started,
        Result = elib_pg:execute(
            <<"UPDATE public.group_member SET status=0 WHERE group_id=$1 AND user_id=$2">>, [
                Gid, Uid
            ]
        ),
        Parent ! {updated, Result}
    end),
    receive
        update_started -> ok
    after 5000 -> error(update_not_started)
    end,
    receive
        {updated, _} -> error(revoke_not_blocked)
    after 200 -> ok
    end,
    Holder ! release,
    receive
        {released, _} -> ok
    after 5000 -> error(lock_release_failed)
    end,
    receive
        {updated, {ok, _}} -> ok
    after 5000 -> error(revoke_not_completed)
    end,
    ?assertEqual(
        {error, permission_denied},
        msg_archive_repo:message_result_with_staging(<<"c2g">>, Uid, Gid, MsgId, ConvKey, 0)
    ).

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
        {ok, 1} = personal_message_order_repo:execute_formal(
            FromId,
            ToId,
            MsgId,
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
        ),
        StagedId = <<"staged-", (elib_uuid:gen_v7())/binary>>,
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.msg_store_staging(id,type,msg_id,from_id,to_id,conv_seq,payload) ",
                "VALUES($1,'c2c',$2,$3,$4,$5,'{}'::jsonb)">>,
            [Id + 1, StagedId, FromId, ToId, Seq + 1]
        ),
        try
            ?assertMatch(
                {ok, [#{<<"conv_seq">> := _}]},
                msg_archive_repo:message_result_with_staging(
                    <<"c2c">>, FromId, ToId, StagedId, ConvKey, 0
                )
            ),
            ?assertEqual(
                {ok, []},
                msg_archive_repo:message_result_with_staging(
                    <<"c2c">>, ToId, FromId, StagedId, ConvKey, 0
                )
            ),
            ?assertEqual(
                {ok, []},
                msg_archive_repo:message_result_with_staging(
                    <<"c2c">>, FromId, ToId + 1, StagedId, OtherConvKey, 0
                )
            ),
            ?assertEqual(
                {ok, []},
                msg_archive_repo:message_result_with_staging(
                    <<"c2c">>, FromId, ToId, StagedId, ConvKey, Seq + 2
                )
            )
        after
            {ok, _} = elib_pg:execute(
                <<"DELETE FROM public.msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [
                    StagedId
                ]
            )
        end
    after
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM public.msg_store WHERE conv_key=$1 AND msg_id=$2">>, [ConvKey, MsgId]
        ),
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM public.msg_store_seq WHERE conv_key=$1">>, [ConvKey]
        )
    end.
