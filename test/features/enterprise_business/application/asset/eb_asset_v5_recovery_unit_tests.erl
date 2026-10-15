-module(eb_asset_v5_recovery_unit_tests).
-include_lib("eunit/include/eunit.hrl").

%% 仅验证恢复编排与参数；真实事务/并发由专用 PG 验收负责。
recovery_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun status_hides_storage_and_uses_persisted_scope/0,
        fun wrong_principal_is_hidden/0,
        fun wrong_visitor_session_is_hidden/0,
        fun revoked_scope_denies_recovery/0,
        fun renew_projects_upload_and_committed_expiry/0,
        fun intent_never_renews/0,
        fun tombstone_precedes_insert/0,
        fun registration_reuses_original_identity/0,
        fun register_projects_real_session_view/0,
        fun widget_bridge_preserves_recovery_identity/0
    ]}.

setup() ->
    [meck:new(M, [passthrough, no_link]) || M <- [eb_pg_asset_meta,eb_asset_scope,eb_asset_upload_ref,elib_pg]],
    meck:new(cs_widget_support,[passthrough,no_link]),
    meck:new(v5_recovery_id,[non_strict,no_link]),
    meck:new(v5_recovery_clock, [non_strict,no_link]),
    meck:expect(v5_recovery_clock, now, fun() -> 100 end),
    meck:expect(eb_pg_asset_meta, v5_session, fun(1,2,3) -> {ok,row()} end),
    meck:expect(eb_asset_scope, authorize, fun(_,_,1,2,44,7) -> ok end),
    meck:expect(eb_asset_scope, authorize_contact, fun(_,1,2,44,7) -> ok end),
    ok.
cleanup(_) ->
    [meck:unload(M) || M <- [eb_pg_asset_meta,eb_asset_scope,eb_asset_upload_ref,elib_pg,v5_recovery_clock,cs_widget_support,v5_recovery_id]].
params() -> #{workspace_id => 2,asset_id => 3,actor_user_id => 7,
    auth => ignored,store => ignored,clock => v5_recovery_clock}.
row() -> #{<<"id">> => 3, <<"workspace_id">> => 2, <<"conversation_id">> => 44,
    <<"message_id">> => null, <<"file_name">> => <<"a.txt">>, <<"uploaded_by_user_id">> => 7,
    <<"object_hash">> => binary:copy(<<"a">>,64), <<"mime">> => <<"text/plain">>,
    <<"size_bytes">> => 4, <<"client_upload_id">> => <<"00000000-0000-4000-8000-000000000001">>,
    <<"session_deadline">> => 1000, <<"upload_expires_at">> => 500,
    <<"lifecycle_state">> => <<"live">>, <<"status">> => <<"pending_confirm">>,
    <<"object_key">> => <<"private-key">>,
    <<"upload_registration">> => #{<<"subject">> => #{<<"kind">> => <<"seat">>,
        <<"principal">> => <<"7">>, <<"org_id">> => <<"1">>}, <<"declaration">> => #{}}}.

status_hides_storage_and_uses_persisted_scope() ->
    {ok,V} = eb_asset_app:session_status(1,(params())#{conversation_id => 999}),
    ?assertNot(maps:is_key(object_key,V)),
    ?assertEqual(<<"registered">>,maps:get(session_state,V)),
    ?assert(meck:called(eb_asset_scope,authorize,[ignored,ignored,1,2,44,7])).
wrong_principal_is_hidden() ->
    ?assertEqual({error,not_found},eb_asset_app:session_status(1,(params())#{actor_user_id => 8})),
    ?assertEqual(0,meck:num_calls(eb_asset_scope,authorize,'_')).
wrong_visitor_session_is_hidden() ->
    R = row(), Reg = #{<<"subject">> => #{<<"kind">> => <<"visitor_session">>,
        <<"principal">> => <<"7">>,<<"org_id">> => <<"1">>,<<"session_id">> => 70}},
    meck:expect(eb_pg_asset_meta,v5_session,fun(_,_,_) -> {ok,R#{<<"upload_registration">> => Reg}} end),
    P = maps:remove(actor_user_id,params()),
    ?assertEqual({error,not_found},eb_asset_app:session_status(1,P#{actor_contact_id => 7,visitor_session_id => 71})),
    ?assertMatch({ok,_},eb_asset_app:session_status(1,P#{actor_contact_id => 7,visitor_session_id => 70})).
revoked_scope_denies_recovery() ->
    meck:expect(eb_asset_scope,authorize,fun(_,_,_,_,_,_) -> {error,{forbidden,not_assignee}} end),
    ?assertEqual({error,{forbidden,not_assignee}},eb_asset_app:session_status(1,params())).
renew_projects_upload_and_committed_expiry() ->
    meck:expect(eb_pg_asset_meta,renew_v5,fun(1,2,3,1000) -> {ok,#{committed_expires_at => 950}} end),
    meck:expect(eb_asset_upload_ref,mint,fun(_,Claims,_,_) ->
        ?assertEqual(950,maps:get(expires_at,Claims)), {ok,<<"opaque">>} end),
    {ok,V} = eb_asset_app:session_renew(1,(params())#{crypto => ignored}),
    ?assertMatch(#{upload := #{token := <<"opaque">>,expires_at := 950}},V),
    ?assertNot(maps:is_key(object_key,V)).
intent_never_renews() ->
    R = row(),meck:expect(eb_pg_asset_meta,v5_session,fun(_,_,_) -> {ok,R#{<<"lifecycle_state">> => <<"delete_intent">>}} end),
    meck:expect(eb_pg_asset_meta,renew_v5,fun(_,_,_,_) -> {error,not_renewable} end),
    ?assertEqual({error,session_not_renewable},eb_asset_app:session_renew(1,params())),
    ?assertEqual(0,meck:num_calls(eb_asset_upload_ref,mint,'_')).

tombstone_precedes_insert() ->
    meck:expect(elib_pg,with_tx,fun(F) -> F(test_conn) end),
    meck:expect(elib_pg,query,fun(test_conn,Sql,_) ->
        case binary:match(Sql,<<"enterprise_asset_delete_queue">>) of
            nomatch -> {ok,[]}; _ -> {ok,[#{<<"asset_id">> => 3}]}
        end end),
    ?assertEqual({error,not_found},eb_pg_asset_meta:register_session_v5(1,2,#{},<<"uuid">>,#{},{1000,500})),
    ?assertEqual(3,meck:num_calls(elib_pg,query,3)).
registration_reuses_original_identity() ->
    Registration = #{<<"subject">> => #{<<"principal">> => <<"7">>}},
    meck:expect(elib_pg,with_tx,fun(F) -> F(test_conn) end),
    meck:expect(elib_pg,query,fun(test_conn,Sql,_) ->
        case binary:match(Sql,<<"SELECT id,organization_id">>) of
            nomatch -> {ok,[]}; _ -> {ok,[#{<<"id">> => 3,<<"organization_id">> => 1,
                <<"workspace_id">> => 2,<<"upload_registration">> => Registration}]}
        end end),
    ?assertEqual({ok,3},eb_pg_asset_meta:register_session_v5(1,2,#{id => 999},<<"uuid">>,Registration,{1000,500})),
    ?assertEqual(4,meck:num_calls(elib_pg,query,3)).


register_projects_real_session_view() ->
    meck:expect(v5_recovery_id,new_id,fun(enterprise_asset) -> 999 end),
    meck:expect(eb_pg_asset_meta,register_session_v5,fun(1,2,_,_,_,_) -> {ok,3} end),
    meck:expect(eb_asset_upload_ref,mint,fun(_,Claims,_,_) ->
        ?assertEqual(3,maps:get(asset_id,Claims)),{ok,<<"opaque">>} end),
    R = row(),
    P = (params())#{conversation_id => 44,mime => <<"text/plain">>,size_bytes => 4,
        object_hash => maps:get(<<"object_hash">>,R), client_upload_id => maps:get(<<"client_upload_id">>,R),
        crypto => ignored,id => v5_recovery_id},
    {ok,V} = eb_asset_app:request_presign(1,P),
    ?assertMatch(#{asset_id := 3,session_state := <<"registered">>,upload_ref := <<"opaque">>,
        upload := #{method := <<"PUT">>,expires_at := 500}},V),
    ?assertNot(maps:is_key(object_key,V)).

widget_bridge_preserves_recovery_identity() ->
    meck:expect(cs_widget_support,verify_bootstrap_token,fun(1,_) -> {ok,#{contact_id => 7}} end),
    meck:expect(cs_widget_support,fetch_installation,fun(_,1,9) -> {ok,#{}} end),
    meck:expect(cs_widget_support,resolve_default_workspace,fun(1,_) -> {ok,2} end),
    meck:expect(cs_widget_support,session_fetch,fun(1,#{workspace_id := 2,session_id := 70}) ->
        {ok,#{id => 70,conversation_id => 44,contact_id => 7}} end),
    meck:expect(cs_widget_support,api_base,fun() -> <<"https://example.invalid">> end),
    R = row(), Reg = #{<<"subject">> => #{<<"kind">> => <<"visitor_session">>,
        <<"principal">> => <<"7">>,<<"org_id">> => <<"1">>,<<"session_id">> => 70}},
    meck:expect(eb_pg_asset_meta,v5_session_by_cid,fun(1,2,_) -> {ok,3} end),
    meck:expect(eb_pg_asset_meta,v5_session,fun(1,2,3) -> {ok,R#{<<"upload_registration">> => Reg}} end),
    {ok,V} = cs_widget_session_app:asset_presign(1,#{installation_id => 9,session_id => 70,
        operation => <<"status">>,client_upload_id => maps:get(<<"client_upload_id">>,R),eb_store => ignored}),
    ?assertMatch(#{asset_id := 3,session_state := <<"registered">>},V),
    ?assertNot(maps:is_key(upload_ref,V)).
