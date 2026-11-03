-module(eb_asset_v5_recovery_unit_tests).
-include_lib("eunit/include/eunit.hrl").

%% V3-C04 direct 会话协议：仅验证恢复编排与参数；真实事务/并发由专用 PG
%% 验收负责（eb_seat_send_protocol_tests / eb_asset_it_scenarios REAL_LOCAL）。
recovery_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun status_hides_storage_and_uses_persisted_scope/0,
        fun wrong_principal_is_hidden/0,
        fun wrong_visitor_session_is_hidden/0,
        fun revoked_scope_denies_recovery/0,
        fun renew_projects_upload_and_committed_expiry/0,
        fun confirmed_terminal_never_renews/0,
        fun tombstone_precedes_insert/0,
        fun registration_reuses_original_identity/0,
        fun presign_projects_direct_descriptor/0,
        fun nullable_file_name_is_absent_in_upload_claims/0,
        fun widget_bridge_preserves_recovery_identity/0
    ]}.

setup() ->
    [
        meck:new(M, [passthrough, no_link])
     || M <- [eb_pg_asset_meta, eb_asset_scope, eb_asset_upload_ref, elib_pg]
    ],
    meck:new(attach_direct_logic, [no_link]),
    meck:new(attachment_storage, [no_link]),
    meck:new(cs_widget_support, [passthrough, no_link]),
    meck:expect(eb_asset_scope, authorize, fun(_, _, 1, 2, 44, 7) -> ok end),
    meck:expect(eb_asset_scope, authorize_contact, fun(_, 1, 2, 44, 7) -> ok end),
    ok.

cleanup(_) ->
    [
        meck:unload(M)
     || M <- [
            eb_pg_asset_meta,
            eb_asset_scope,
            eb_asset_upload_ref,
            elib_pg,
            attach_direct_logic,
            attachment_storage,
            cs_widget_support
        ]
    ].

-define(KEY, <<"eb/1/2/20260101/aabbccdd.txt">>).
-define(SHA256, <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>).

%% direct 会话定位参数（workspace_id + object_key + actor 二选一）。
params() ->
    #{
        workspace_id => 2,
        object_key => ?KEY,
        actor_user_id => 7,
        auth => ignored,
        store => ignored
    }.

%% attach_pending 行（direct_session_row 的 SQL 投影形状）——
%% subject kind 与 actor_scope 原子同源（member/contact），声明含 EB 作用域。
direct_row() ->
    #{
        <<"object_key">> => ?KEY,
        <<"bucket">> => <<"imboy">>,
        <<"scope">> => <<"private">>,
        <<"creator_user_id">> => 7,
        <<"session_state">> => <<"GRANTED">>,
        <<"direct_state">> => <<"GRANTED">>,
        <<"upload_registration">> => #{
            <<"subject">> => #{<<"kind">> => <<"member">>, <<"principal">> => <<"7">>},
            <<"declaration">> => #{
                <<"org_id">> => <<"1">>,
                <<"workspace_id">> => <<"2">>,
                <<"conversation_id">> => <<"44">>,
                <<"message_id">> => null,
                <<"mime">> => <<"text/plain">>,
                <<"file_name">> => <<"a.txt">>
            }
        },
        <<"storage_driver">> => <<"garage">>,
        <<"storage_namespace">> => <<"ns">>,
        <<"storage_container">> => <<"imboy">>,
        <<"storage_locator_sha256">> => <<"loc">>,
        <<"session_deadline">> => null,
        <<"direct_expected">> => #{<<"sha256">> => ?SHA256},
        <<"storage_physical_key">> => null
    }.

renew_info() ->
    #{
        state => <<"GRANTED">>,
        attempt_id => 2,
        grant => #{
            method => <<"PUT">>,
            transport => <<"s3_put">>,
            url => <<"https://garage.invalid/put">>,
            required_headers => [{<<"x-amz-checksum-sha256">>, <<"sha">>}],
            expires_in => 600
        }
    }.

%% stub direct_session_row（elib_pg:query 全量回落 direct 行）。
expect_direct_session() ->
    expect_direct_session(direct_row()).
expect_direct_session(Row) ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> {ok, [Row]} end).

%% ===================================================================
%% status：只读投影——无写能力、无存储端点（storage 隐藏）
%% ===================================================================
status_hides_storage_and_uses_persisted_scope() ->
    expect_direct_session(),
    {ok, V} = eb_asset_app:session_status(1, params()),
    ?assertEqual(<<"registered">>, maps:get(session_state, V)),
    ?assertEqual(true, maps:get(can_resume, V)),
    ?assertNot(maps:is_key(upload, V)),
    ?assertNot(maps:is_key(attempt_id, V)),
    ?assertNot(maps:is_key(url, V)),
    ?assertNot(maps:is_key(grant, V)),
    ?assertEqual(0, meck:num_calls(attach_direct_logic, renew_grant, '_')).

%% 归属逐字比对（C04-02）：主体不符 → not_found（不泄露会话存在性）。
wrong_principal_is_hidden() ->
    expect_direct_session(),
    ?assertEqual(
        {error, not_found}, eb_asset_app:session_status(1, (params())#{actor_user_id => 8})
    ).

%% 访客主体同理：contact id 不符 → not_found；相符 → 只读回查。
wrong_visitor_session_is_hidden() ->
    Row = direct_row(),
    Reg = (maps:get(<<"upload_registration">>, Row))#{
        <<"subject">> => #{<<"kind">> => <<"contact">>, <<"principal">> => <<"7">>}
    },
    expect_direct_session(Row#{<<"upload_registration">> => Reg}),
    P = maps:remove(actor_user_id, params()),
    ?assertEqual({error, not_found}, eb_asset_app:session_status(1, P#{actor_contact_id => 8})),
    ?assertMatch({ok, _}, eb_asset_app:session_status(1, P#{actor_contact_id => 7})).

%% confirm 重新鉴权（presign 后撤权 fail-closed）：scope 拒绝即不推进状态。
revoked_scope_denies_recovery() ->
    expect_direct_session(),
    meck:expect(eb_asset_scope, authorize, fun(_, _, _, _, _, _) ->
        {error, {forbidden, not_assignee}}
    end),
    ?assertEqual({error, {forbidden, not_assignee}}, eb_asset_app:confirm_asset(1, params())),
    ?assertEqual(0, meck:num_calls(attach_direct_logic, finalize, '_')).

%% renew：GRANTED → 新 attempt + s3_put descriptor；committed deadline 投影。
renew_projects_upload_and_committed_expiry() ->
    DeadlineTuple = {{2026, 1, 2}, {0, 0, 0}},
    DeadlineMs = (calendar:datetime_to_gregorian_seconds(DeadlineTuple) - 62167219200) * 1000,
    Row = direct_row(),
    expect_direct_session(Row#{<<"session_deadline">> => DeadlineTuple}),
    meck:expect(attach_direct_logic, renew_grant, fun(Key, Sha) ->
        ?assertEqual(?KEY, Key),
        ?assertEqual(?SHA256, Sha),
        {ok, renew_info()}
    end),
    {ok, V} = eb_asset_app:session_renew(1, params()),
    ?assertMatch(
        #{
            attempt_id := 2,
            upload := #{method := <<"PUT">>, transport := <<"s3_put">>, expires_in := 600}
        },
        V
    ),
    ?assertEqual(DeadlineMs, maps:get(session_deadline, V)).

%% CONFIRMED 是幂等终态：不产生新 attempt/presign（intent never renews）。
confirmed_terminal_never_renews() ->
    Row = direct_row(),
    expect_direct_session(
        Row#{<<"direct_state">> => <<"CONFIRMED">>, <<"session_state">> => <<"CONFIRMED">>}
    ),
    ?assertMatch({ok, #{state_confirmed := true}}, eb_asset_app:session_renew(1, params())),
    ?assertEqual(0, meck:num_calls(attach_direct_logic, renew_grant, '_')).

%% ===================================================================
%% meta 层 register_session_v5（W2-02 存量恢复链，与 C04 并存）
%% ===================================================================
tombstone_precedes_insert() ->
    meck:expect(elib_pg, with_tx, fun(F) -> F(test_conn) end),
    meck:expect(elib_pg, query, fun(test_conn, Sql, _) ->
        case binary:match(Sql, <<"enterprise_asset_delete_queue">>) of
            nomatch -> {ok, []};
            _ -> {ok, [#{<<"asset_id">> => 3}]}
        end
    end),
    ?assertEqual(
        {error, not_found},
        eb_pg_asset_meta:register_session_v5(1, 2, #{}, <<"uuid">>, #{}, {1000, 500})
    ),
    ?assertEqual(3, meck:num_calls(elib_pg, query, 3)).

registration_reuses_original_identity() ->
    Registration = #{<<"subject">> => #{<<"principal">> => <<"7">>}},
    meck:expect(elib_pg, with_tx, fun(F) -> F(test_conn) end),
    meck:expect(elib_pg, query, fun(test_conn, Sql, _) ->
        case binary:match(Sql, <<"SELECT id,organization_id">>) of
            nomatch ->
                {ok, []};
            _ ->
                {ok, [
                    #{
                        <<"id">> => 3,
                        <<"organization_id">> => 1,
                        <<"workspace_id">> => 2,
                        <<"upload_registration">> => Registration
                    }
                ]}
        end
    end),
    ?assertEqual(
        {ok, 3},
        eb_pg_asset_meta:register_session_v5(
            1, 2, #{id => 999}, <<"uuid">>, Registration, {1000, 500}
        )
    ),
    ?assertEqual(4, meck:num_calls(elib_pg, query, 3)).

%% ===================================================================
%% presign：dv1 一站式 → s3_put descriptor（无 upload_ref / asset_id 预分配）
%% ===================================================================
presign_projects_direct_descriptor() ->
    meck:expect(attachment_storage, registration_location, fun(_, _, _) ->
        {ok, #{
            driver => <<"garage">>,
            namespace => <<"ns">>,
            container => <<"imboy">>,
            locator_digest => <<"loc">>
        }}
    end),
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> {ok, [#{<<"key">> => ?KEY}]} end),
    meck:expect(attach_direct_logic, declare, fun(_, _, _, _) -> ok end),
    meck:expect(attach_direct_logic, grant, fun(_, _) ->
        {ok, (renew_info())#{attempt_id => 1, object_key => ?KEY}}
    end),
    P = (params())#{
        conversation_id => 44,
        mime => <<"text/plain">>,
        size_bytes => 4,
        object_hash => ?SHA256,
        client_upload_id => <<"00000000-0000-4000-8000-000000000001">>,
        file_name => <<"a.txt">>
    },
    {ok, V} = eb_asset_app:request_presign(1, P),
    ?assertMatch(
        #{
            object_key := ?KEY,
            session_state := <<"GRANTED">>,
            attempt_id := 1,
            upload := #{method := <<"PUT">>, transport := <<"s3_put">>}
        },
        V
    ),
    %% S3-only 硬合同：同源正文凭证/预分配资产 id 不再出现。
    ?assertNot(maps:is_key(upload_ref, V)),
    ?assertNot(maps:is_key(asset_id, V)).

%% nullable file_name：声明缺失/空值不炸投影，值逐字透传。
nullable_file_name_is_absent_in_upload_claims() ->
    lists:foreach(
        fun(FileName) ->
            Row = direct_row(),
            Decl0 = maps:get(<<"declaration">>, maps:get(<<"upload_registration">>, Row)),
            Decl =
                case FileName of
                    undefined -> maps:remove(<<"file_name">>, Decl0);
                    Value -> Decl0#{<<"file_name">> => Value}
                end,
            Reg = (maps:get(<<"upload_registration">>, Row))#{<<"declaration">> => Decl},
            expect_direct_session(Row#{<<"upload_registration">> => Reg}),
            meck:expect(attach_direct_logic, renew_grant, fun(_, _) -> {ok, renew_info()} end),
            {ok, V} = eb_asset_app:session_renew(1, params()),
            ?assertEqual(FileName, maps:get(file_name, V, undefined))
        end,
        [null, undefined, <<"a.txt">>]
    ).

%% widget 桥（访客面）：令牌 contact 主体 + object_key 定位 → 同一 direct
%% 只读投影；S3-only 后无 upload_ref 出站。
widget_bridge_preserves_recovery_identity() ->
    meck:expect(cs_widget_support, verify_bootstrap_token, fun(1, _) ->
        {ok, #{contact_id => 7}}
    end),
    meck:expect(cs_widget_support, fetch_installation, fun(_, 1, 9) -> {ok, #{}} end),
    meck:expect(cs_widget_support, resolve_default_workspace, fun(1, _) -> {ok, 2} end),
    meck:expect(cs_widget_support, session_fetch, fun(1, #{workspace_id := 2, session_id := 70}) ->
        {ok, #{id => 70, conversation_id => 44, contact_id => 7}}
    end),
    Row = direct_row(),
    Reg = (maps:get(<<"upload_registration">>, Row))#{
        <<"subject">> => #{<<"kind">> => <<"contact">>, <<"principal">> => <<"7">>}
    },
    expect_direct_session(Row#{<<"upload_registration">> => Reg}),
    {ok, V} = cs_widget_session_app:asset_presign(1, #{
        installation_id => 9,
        session_id => 70,
        operation => <<"status">>,
        object_key => ?KEY,
        eb_store => ignored
    }),
    ?assertMatch(#{session_state := <<"registered">>, object_key := ?KEY}, V),
    ?assertNot(maps:is_key(upload, V)),
    ?assertNot(maps:is_key(upload_ref, V)).
