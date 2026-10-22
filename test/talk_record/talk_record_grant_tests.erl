%%% @doc talk_record T17 授权/分享/回放域逻辑级测试（真实 DB 55433）。
%%%
%%% 覆盖（T17 卡交付 5/7）：
%%%   * ACL 全量矩阵（owner/editor/viewer/陌生人 GET grants，§11.3）；
%%%   * PUT 全量替换语义（acl_revision 推进/ETag/目录边界/owner 行拒绝/
%%%     重复拒绝/移除未提交/撤销后重授权/与并发删除互斥/版本域纪律）；
%%%   * DELETE 撤销（立即生效/审计/非 owner 403/不存在 404）；
%%%   * share-card（严格/最小标题两态、不额外授权、审计、权限）；
%%%   * playback 票据（签发/校验/撤权后票据+Range+续签三路拒绝/续签重验/
%%%     媒体未就绪/位置越界/签名密钥缺失）；
%%%   * share-candidates（通用/record 级、跨企业与无权 ws 排除、翻页、
%%%     owner+已授权排除）。
-module(talk_record_grant_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    State = talk_record_t17_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            talk_record_t17_support:seed(State),
            State
        end,
        fun talk_record_t17_support:teardown_all/1
    ).

cleanup(State) ->
    talk_record_t17_support:teardown_all(State).

with_db(TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            ?_test(TestFun(State))
        end}}.

%%%===================================================================
%%% 夹具（编号段 4x；org 记录默认 ORG_A 无 ws）
%%%===================================================================

-spec fixture(map(), integer()) -> map().
fixture(State, N) ->
    fixture(State, N, #{}).

-spec fixture(map(), integer(), map()) -> map().
fixture(State, N, Opts) ->
    Conn = maps:get(conn, State),
    Ids = talk_record_t17_support:ids(),
    U1 = maps:get(u1, Ids),
    U2 = maps:get(u2, Ids),
    U3 = maps:get(u3, Ids),
    Owner = maps:get(owner, Opts, U1),
    {ok, Rid} =
        case maps:get(scope, Opts, personal) of
            personal ->
                talk_record_t15_support:create_record(
                    Owner, N, maps:get(title, Opts, <<"t17-记录"/utf8>>)
                );
            organization ->
                talk_record_t15_support:create_org_record(
                    Owner,
                    maps:get(org, Opts, maps:get(org_a, Ids)),
                    maps:get(ws, Opts, 0),
                    N
                )
        end,
    #{
        record => Rid,
        conn => Conn,
        u1 => U1,
        u2 => U2,
        u3 => U3,
        ids => Ids,
        owner => Owner
    }.

%% @doc 直插授权（不经 PUT——构造既有授权状态，用 T15 支撑）。
-spec seed_grant(map(), integer(), integer(), binary()) -> ok.
seed_grant(F, GranteeUid, GrantorUid, Role) ->
    talk_record_t15_support:add_grant(
        maps:get(conn, F),
        maps:get(record, F),
        GranteeUid,
        GrantorUid,
        Role
    ).

%% @private 按 grantee_user_id 取行（不依赖列表排序）。
-spec row_of([map()], integer()) -> map().
row_of(Rows, GranteeUid) ->
    hd([R || R <- Rows, maps:get(<<"grantee_user_id">>, R) =:= GranteeUid]).

%%%===================================================================
%%% GET /:id/grants：ACL 全量矩阵（§11.3）
%%%===================================================================

grants_list_acl_matrix_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 41),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        U3 = maps:get(u3, F),
        seed_grant(F, U2, U1, <<"viewer">>),
        seed_grant(F, U3, U1, <<"editor">>),

        %% owner：全量活跃名单 + allow_export/expires_at 字段 + acl ETag
        {ok, Owner} = talk_record_grant_logic:list(U1, Rid),
        ?assertEqual(2, length(maps:get(<<"list">>, Owner))),
        V1 = row_of(maps:get(<<"list">>, Owner), U2),
        E1 = row_of(maps:get(<<"list">>, Owner), U3),
        ?assertEqual(<<"viewer">>, maps:get(<<"role">>, V1)),
        ?assertEqual(false, maps:get(<<"allow_export">>, V1)),
        ?assertEqual(null, maps:get(<<"expires_at">>, V1)),
        ?assertEqual(null, maps:get(<<"revoked_at">>, V1)),
        ?assertEqual(<<"editor">>, maps:get(<<"role">>, E1)),
        ?assertEqual(1, maps:get(<<"acl_revision">>, Owner)),
        ?assertEqual(<<"acl:1">>, maps:get(<<"etag">>, Owner)),

        %% viewer/editor（非 owner）：只回自己的权限行
        {ok, Own2} = talk_record_grant_logic:list(U2, Rid),
        ?assertEqual(1, length(maps:get(<<"list">>, Own2))),
        [Only2] = maps:get(<<"list">>, Own2),
        ?assertEqual(U2, maps:get(<<"grantee_user_id">>, Only2)),
        {ok, Own3} = talk_record_grant_logic:list(U3, Rid),
        ?assertEqual(1, length(maps:get(<<"list">>, Own3))),

        %% 陌生人：统一 404
        ?assertEqual(
            {error, not_found},
            talk_record_grant_logic:list(maps:get(u7, maps:get(ids, F)), Rid)
        ),

        %% 无任何授权记录：非 owner 可见者不存在（404）；owner 空名单
        {ok, Empty} = talk_record_grant_logic:list(U1, maps:get(record, fixture(State, 42))),
        ?assertEqual([], maps:get(<<"list">>, Empty))
    end).

%%%===================================================================
%%% PUT /:id/grants：全量替换
%%%===================================================================

grants_put_happy_path_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 43),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        U3 = maps:get(u3, F),
        Body = #{
            <<"grants">> => [
                #{
                    <<"grantee_user_id">> => U2,
                    <<"role">> => <<"viewer">>,
                    <<"allow_export">> => true,
                    <<"expires_at">> => <<"2099-01-01T00:00:00Z">>
                },
                #{
                    <<"grantee_user_id">> => U3,
                    <<"role">> => <<"editor">>,
                    <<"allow_export">> => false
                }
            ]
        },
        {ok, Result} = talk_record_grant_logic:replace(U1, Rid, <<"acl:1">>, Body),
        %% acl_revision 推进 + 新 ETag + 全量名单
        ?assertEqual(2, maps:get(<<"acl_revision">>, Result)),
        ?assertEqual(<<"acl:2">>, maps:get(<<"etag">>, Result)),
        ?assertEqual(2, length(maps:get(<<"list">>, Result))),
        EditorRow = row_of(maps:get(<<"list">>, Result), U3),
        ViewerRow = row_of(maps:get(<<"list">>, Result), U2),
        ?assertEqual(<<"editor">>, maps:get(<<"role">>, EditorRow)),
        ?assertEqual(true, maps:get(<<"allow_export">>, ViewerRow)),
        ?assert(is_binary(maps:get(<<"expires_at">>, ViewerRow))),
        %% 目录 display_name 最小信息（§11.3）
        ?assertEqual(<<"viewer-b"/utf8>>, maps:get(<<"grantee_display_name">>, ViewerRow)),
        %% 版本域纪律：revision/content_revision 不动，只推 acl_revision
        Revs = talk_record_t17_support:revisions(Conn, Rid),
        ?assertEqual(
            #{<<"revision">> => 1, <<"content_revision">> => 1, <<"acl_revision">> => 2}, Revs
        ),
        %% 审计行（白名单 metadata，无正文）
        ?assertEqual(1, talk_record_t15_support:audit_count(Conn, Rid, <<"grants_replace">>)),
        AuditRow = talk_record_t15_support:audit_row(Conn, Rid, <<"grants_replace">>),
        ?assertEqual(U1, maps:get(<<"actor_id">>, AuditRow)),
        Meta = maps:get(<<"metadata_allowlist">>, AuditRow),
        ?assertEqual(2, maps:get(<<"entries">>, Meta))
    end).

grants_put_if_match_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 44),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        Body = #{<<"grants">> => [#{<<"grantee_user_id">> => U2, <<"role">> => <<"viewer">>}]},
        %% 缺 If-Match → 428 reason
        ?assertEqual(
            {error, precondition_required},
            talk_record_grant_logic:replace(U1, Rid, undefined, Body)
        ),
        %% 非法形态 → 422
        ?assertEqual(
            {error, invalid_if_match},
            talk_record_grant_logic:replace(U1, Rid, <<"acl:xyz">>, Body)
        ),
        %% 纯 revision 形态（T05/T07 兼容）可用
        ?assertMatch(
            {ok, _},
            talk_record_grant_logic:replace(U1, Rid, <<"\"1\"">>, Body)
        ),
        %% 旧 acl ETag → 412
        ?assertEqual(
            {error, precondition_failed},
            talk_record_grant_logic:replace(U1, Rid, <<"acl:1">>, Body)
        )
    end).

grants_put_validation_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 45),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        seed_grant(F, U2, U1, <<"editor">>),
        G = #{<<"grantee_user_id">> => U2, <<"role">> => <<"viewer">>},
        %% owner 行（owner 在主表）→ 422 grant_owner_row
        ?assertEqual(
            {error, grant_owner_row},
            talk_record_grant_logic:replace(
                U1,
                Rid,
                <<"acl:1">>,
                #{<<"grants">> => [G, #{<<"grantee_user_id">> => U1, <<"role">> => <<"viewer">>}]}
            )
        ),
        %% 清单重复 → 422 grant_duplicate
        ?assertEqual(
            {error, grant_duplicate},
            talk_record_grant_logic:replace(
                U1,
                Rid,
                <<"acl:1">>,
                #{<<"grants">> => [G, G]}
            )
        ),
        %% 非法角色 / 非法 ID / 非法到期
        ?assertEqual(
            {error, invalid_grant_role},
            talk_record_grant_logic:replace(
                U1,
                Rid,
                <<"acl:1">>,
                #{<<"grants">> => [#{<<"grantee_user_id">> => U2, <<"role">> => <<"admin">>}]}
            )
        ),
        ?assertEqual(
            {error, invalid_grantee_user_id},
            talk_record_grant_logic:replace(
                U1,
                Rid,
                <<"acl:1">>,
                #{<<"grants">> => [#{<<"grantee_user_id">> => 0, <<"role">> => <<"viewer">>}]}
            )
        ),
        ?assertEqual(
            {error, invalid_expires_at},
            talk_record_grant_logic:replace(
                U1,
                Rid,
                <<"acl:1">>,
                #{
                    <<"grants">> => [
                        #{
                            <<"grantee_user_id">> => U2,
                            <<"role">> => <<"viewer">>,
                            <<"expires_at">> => <<"not-a-date">>
                        }
                    ]
                }
            )
        ),
        %% 缺 grants 键 → 422
        ?assertEqual(
            {error, invalid_grants_body},
            talk_record_grant_logic:replace(U1, Rid, <<"acl:1">>, #{})
        )
    end).

grants_put_auth_gates_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 59),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        seed_grant(F, U2, U1, <<"editor">>),
        %% 非 owner（editor）→ 403；陌生人 → 404（不可见）
        ?assertEqual(
            {error, forbidden},
            talk_record_grant_logic:replace(U2, Rid, <<"acl:1">>, #{<<"grants">> => []})
        ),
        ?assertEqual(
            {error, not_found},
            talk_record_grant_logic:replace(
                maps:get(u7, maps:get(ids, F)),
                Rid,
                <<"acl:1">>,
                #{<<"grants">> => []}
            )
        )
    end).

grants_put_directory_test_() ->
    with_db(fun(State) ->
        Ids = maps:get(ids, fixture(State, 46)),
        U1 = maps:get(u1, Ids),
        U4 = maps:get(u4, Ids),
        %% org 记录 + 跨企业用户（ORG_B owner）→ 422 target_not_in_directory
        OrgF = fixture(State, 47, #{scope => organization}),
        OrgRid = maps:get(record, OrgF),
        ?assertEqual(
            {error, target_not_in_directory},
            talk_record_grant_logic:replace(
                U1,
                OrgRid,
                <<"acl:1">>,
                #{<<"grants">> => [#{<<"grantee_user_id">> => U4, <<"role">> => <<"viewer">>}]}
            )
        ),
        %% ws 绑定 org 记录 + 无 ws 授权成员（U2 非 WS_A1 成员）→ 422
        WsF = fixture(State, 48, #{scope => organization, ws => maps:get(ws_a1, Ids)}),
        WsRid = maps:get(record, WsF),
        ?assertEqual(
            {error, target_not_in_directory},
            talk_record_grant_logic:replace(
                U1,
                WsRid,
                <<"acl:1">>,
                #{
                    <<"grants">> => [
                        #{
                            <<"grantee_user_id">> => maps:get(u2, Ids),
                            <<"role">> => <<"viewer">>
                        }
                    ]
                }
            )
        ),
        %% 同 org 成员（U2 对无 ws 记录）→ 放行
        ?assertMatch(
            {ok, _},
            talk_record_grant_logic:replace(
                U1,
                OrgRid,
                <<"acl:1">>,
                #{
                    <<"grants">> => [
                        #{
                            <<"grantee_user_id">> => maps:get(u2, Ids),
                            <<"role">> => <<"viewer">>
                        }
                    ]
                }
            )
        )
    end).

grants_put_full_replacement_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 49),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        U3 = maps:get(u3, F),
        seed_grant(F, U2, U1, <<"viewer">>),
        seed_grant(F, U3, U1, <<"editor">>),
        %% 全量提交仅含 U2：U3 被撤销（未提交即移除——活跃 0、历史 revoked 行保留）
        {ok, R1} = talk_record_grant_logic:replace(
            U1,
            Rid,
            <<"acl:1">>,
            #{<<"grants">> => [#{<<"grantee_user_id">> => U2, <<"role">> => <<"editor">>}]}
        ),
        ?assertEqual(1, length(maps:get(<<"list">>, R1))),
        ?assertEqual(0, talk_record_t17_support:active_grant_count(Conn, Rid, U3)),
        RevokedRow = talk_record_t15_support:one(Conn, [
            "SELECT count(*) AS n FROM talk_record_grant WHERE record_id=",
            integer_to_binary(Rid),
            " AND grantee_user_id=",
            integer_to_binary(U3),
            " AND revoked_at IS NOT NULL"
        ]),
        ?assertEqual(1, maps:get(<<"n">>, RevokedRow)),
        %% U3 被 PUT 再次授权 → 新活跃行（撤销后可重新授权）
        {ok, R2} = talk_record_grant_logic:replace(
            U1,
            Rid,
            <<"acl:2">>,
            #{
                <<"grants">> => [
                    #{<<"grantee_user_id">> => U2, <<"role">> => <<"editor">>},
                    #{<<"grantee_user_id">> => U3, <<"role">> => <<"viewer">>}
                ]
            }
        ),
        ?assertEqual(2, length(maps:get(<<"list">>, R2))),
        %% 空清单全撤：owner 仍在主表（不能移除最后 owner 由主表保证）
        {ok, R3} = talk_record_grant_logic:replace(U1, Rid, <<"acl:3">>, #{<<"grants">> => []}),
        ?assertEqual([], maps:get(<<"list">>, R3)),
        ?assertEqual(0, talk_record_t17_support:grant_count(Conn, Rid))
    end).

grants_put_concurrent_delete_mutex_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 50),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        %% 并发删除（软删置位）后 PUT → 404（行锁后复核 deleted_at）
        talk_record_t17_support:sql(Conn, [
            "UPDATE talk_record SET deleted_at = now() WHERE id = ", integer_to_binary(Rid)
        ]),
        ?assertEqual(
            {error, not_found},
            talk_record_grant_logic:replace(U1, Rid, <<"acl:1">>, #{<<"grants">> => []})
        ),
        %% DELETE 同样被删除互斥拒绝
        ?assertEqual(
            {error, not_found},
            talk_record_grant_logic:revoke(U1, Rid, 1, <<"acl:1">>)
        )
    end).

%%%===================================================================
%%% DELETE /:id/grants/:grant_id
%%%===================================================================

grants_revoke_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 51),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        {ok, _} = talk_record_grant_logic:replace(
            U1,
            Rid,
            <<"acl:1">>,
            #{<<"grants">> => [#{<<"grantee_user_id">> => U2, <<"role">> => <<"viewer">>}]}
        ),
        {ok, Listed} = talk_record_grant_logic:list(U1, Rid),
        [GrantRow | _] = maps:get(<<"list">>, Listed),
        GrantId = maps:get(<<"id">>, GrantRow),
        %% 撤销成功：acl_revision 推进、名单即时清空、立即生效
        {ok, R} = talk_record_grant_logic:revoke(U1, Rid, GrantId, <<"acl:2">>),
        ?assertEqual([], maps:get(<<"list">>, R)),
        ?assertEqual(3, maps:get(<<"acl_revision">>, R)),
        ?assertEqual(0, talk_record_t17_support:active_grant_count(Conn, Rid, U2)),
        %% 被撤者立即失读（visible 404）
        ?assertEqual({error, not_found}, talk_record_grant_logic:list(U2, Rid)),
        %% 审计：target_id=grant_id + grantee/role 白名单 metadata
        AuditRow = talk_record_t15_support:audit_row(Conn, Rid, <<"grant_revoke">>),
        ?assertEqual(GrantId, maps:get(<<"target_id">>, AuditRow)),
        ?assertEqual(
            U2, maps:get(<<"grantee_user_id">>, maps:get(<<"metadata_allowlist">>, AuditRow))
        )
    end).

grants_revoke_errors_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 60),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        seed_grant(F, U2, U1, <<"viewer">>),
        {ok, Listed} = talk_record_grant_logic:list(U1, Rid),
        [GrantRow | _] = maps:get(<<"list">>, Listed),
        GrantId = maps:get(<<"id">>, GrantRow),
        %% 非 owner → 403；旧 If-Match → 412
        ?assertEqual(
            {error, forbidden},
            talk_record_grant_logic:revoke(U2, Rid, GrantId, <<"acl:1">>)
        ),
        ?assertEqual(
            {error, precondition_failed},
            talk_record_grant_logic:revoke(U1, Rid, GrantId, <<"acl:99">>)
        ),
        %% 撤销后重复撤（已 revoked）→ 404；外部不存在 id → 404
        {ok, _} = talk_record_grant_logic:revoke(U1, Rid, GrantId, <<"acl:1">>),
        ?assertEqual(
            {error, grant_not_found},
            talk_record_grant_logic:revoke(U1, Rid, GrantId, <<"acl:2">>)
        ),
        ?assertEqual(
            {error, grant_not_found},
            talk_record_grant_logic:revoke(U1, Rid, 9999999, <<"acl:2">>)
        )
    end).

%%%===================================================================
%%% share-card（严格/最小标题两态 + 不额外授权）
%%%===================================================================

share_card_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 52),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        %% 严格模式（默认）：只发「面聊记录」定位，不泄漏记录标题
        {ok, Card} = talk_record_card_logic:share_card(U1, Rid),
        ?assertEqual(<<"talk_record_card">>, maps:get(<<"card_type">>, Card)),
        ?assertEqual(Rid, maps:get(<<"record_id">>, Card)),
        ?assertEqual(<<"面聊记录"/utf8>>, maps:get(<<"title">>, Card)),
        ?assertEqual(
            <<"/talk-records/", (integer_to_binary(Rid))/binary>>,
            maps:get(<<"route">>, Card)
        ),
        %% 不额外授权：授权行数不变；审计留痕（strict 位，无标题）
        ?assertEqual(0, talk_record_t17_support:grant_count(Conn, Rid)),
        ?assertEqual(1, talk_record_t15_support:audit_count(Conn, Rid, <<"share_card_issue">>))
    end).

share_card_permissions_and_relaxed_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 61),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        %% 可读者（viewer）也可生成卡片；陌生人 404
        seed_grant(F, U2, U1, <<"viewer">>),
        ?assertMatch({ok, _}, talk_record_card_logic:share_card(U2, Rid)),
        ?assertEqual(
            {error, not_found},
            talk_record_card_logic:share_card(maps:get(u7, maps:get(ids, F)), Rid)
        ),
        %% 非严格模式：回记录标题（部署策略显式放开才生效）
        NonStrict = fun() ->
            {ok, C2} = talk_record_card_logic:share_card(U1, Rid),
            maps:get(<<"title">>, C2)
        end,
        ?assertEqual(
            <<"t17-记录"/utf8>>,
            talk_record_t17_support:with_env(talk_record_share_card_strict_title, false, NonStrict)
        )
    end).

%%%===================================================================
%%% playback 票据（签发/校验/撤权三路拒绝）
%%%===================================================================

playback_ticket_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 53),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        _Tid = talk_record_t15_support:seed_transcript(Conn, Rid, 1),
        talk_record_t17_support:sql(Conn, [
            "UPDATE talk_record SET duration_ms = 60000 WHERE id = ", integer_to_binary(Rid)
        ]),
        %% owner 开票：URL 受控（票据在 query，非长期直读地址）+ expiry
        {ok, T1} = talk_record_grant_playback:open(U1, Rid, undefined),
        Ticket = maps:get(<<"ticket">>, T1),
        ?assertEqual(
            <<"/api/v1/talk-records/", (integer_to_binary(Rid))/binary, "/playback/stream?ticket=",
                Ticket/binary>>,
            maps:get(<<"url">>, T1)
        ),
        Now = os:system_time(second),
        ?assert(maps:get(<<"expiry">>, T1) > Now),
        ?assert(maps:get(<<"expiry">>, T1) =< Now + 3600),
        ?assert(is_binary(maps:get(<<"expires_at">>, T1))),
        ?assertEqual(0, maps:get(<<"position_ms">>, T1)),
        %% 校验点（网关逐段/Range 重验）：验签 + ACL + acl_revision 通过
        {ok, Claims} = talk_record_grant_playback:verify_ticket(Ticket),
        ?assertEqual(Rid, maps:get(<<"rid">>, Claims)),
        ?assertEqual(U1, maps:get(<<"uid">>, Claims)),
        %% position_ms 可选且必须落在时长内
        {ok, T2} = talk_record_grant_playback:open(U1, Rid, 30000),
        ?assertEqual(30000, maps:get(<<"position_ms">>, T2)),
        ?assertEqual(
            {error, invalid_position_ms},
            talk_record_grant_playback:open(U1, Rid, 60001)
        )
    end).

playback_gates_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 62),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        _Tid = talk_record_t15_support:seed_transcript(Conn, Rid, 1),
        %% 媒体未就绪（local_only 新记录）→ 409 reason
        {ok, FreshRid} = talk_record_t15_support:create_record(U1, 63, <<"未就绪"/utf8>>),
        ?assertEqual(
            {error, playback_media_unavailable},
            talk_record_grant_playback:open(U1, FreshRid, undefined)
        ),
        %% 陌生人 / 无 grant → 404
        ?assertEqual(
            {error, not_found},
            talk_record_grant_playback:open(maps:get(u7, maps:get(ids, F)), Rid, undefined)
        )
    end).

playback_revoke_rejection_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 55),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        _Tid = talk_record_t15_support:seed_transcript(Conn, Rid, 1),
        %% U2 viewer 授权（acl 1→2）→ 开票（acl claim = 2）
        {ok, _} = talk_record_grant_logic:replace(
            U1,
            Rid,
            <<"acl:1">>,
            #{<<"grants">> => [#{<<"grantee_user_id">> => U2, <<"role">> => <<"viewer">>}]}
        ),
        {ok, T} = talk_record_grant_playback:open(U2, Rid, undefined),
        Ticket = maps:get(<<"ticket">>, T),
        ?assertMatch({ok, _}, talk_record_grant_playback:verify_ticket(Ticket)),
        %% 撤销（acl 2→3）：票据/Range 重验（verify_ticket）/续签三路拒绝
        {ok, #{<<"list">> := [GrantRow]}} = talk_record_grant_logic:list(U1, Rid),
        {ok, _} = talk_record_grant_logic:revoke(
            U1, Rid, maps:get(<<"id">>, GrantRow), <<"acl:2">>
        ),
        ?assertEqual({error, not_found}, talk_record_grant_playback:verify_ticket(Ticket)),
        ?assertEqual({error, not_found}, talk_record_grant_playback:open(U2, Rid, undefined)),
        %% 再授权（acl 3→4）：U2 恢复可见但旧票据 acl_revision 过期 → acl_stale
        {ok, _} = talk_record_grant_logic:replace(
            U1,
            Rid,
            <<"acl:3">>,
            #{<<"grants">> => [#{<<"grantee_user_id">> => U2, <<"role">> => <<"viewer">>}]}
        ),
        ?assertEqual({error, acl_stale}, talk_record_grant_playback:verify_ticket(Ticket)),
        %% 续签（同端点重开）：重验通过，携带新 acl_revision
        {ok, T2} = talk_record_grant_playback:open(U2, Rid, undefined),
        {ok, Claims2} = talk_record_grant_playback:verify_ticket(maps:get(<<"ticket">>, T2)),
        ?assertEqual(4, maps:get(<<"acl">>, Claims2)),
        %% 篡改票据 → 验签拒绝
        ?assertMatch(
            {error, _},
            talk_record_grant_playback:verify_ticket(<<"tampered.", Ticket/binary>>)
        )
    end).

playback_signing_key_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 56),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        _Tid = talk_record_t15_support:seed_transcript(Conn, Rid, 1),
        Open = fun() -> talk_record_grant_playback:open(U1, Rid, undefined) end,
        %% jwt_key 缺失/过短 → 拒绝签发（不回退弱密钥）
        ?assertEqual(
            {error, playback_signing_key_unavailable},
            talk_record_t17_support:with_env(jwt_key, <<"short">>, Open)
        )
    end).

%%%===================================================================
%%% share-candidates：通用 + record 级
%%%===================================================================

candidates_generic_org_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t17_support:ids(),
        U1 = maps:get(u1, Ids),
        U4 = maps:get(u4, Ids),
        U7 = maps:get(u7, Ids),
        OrgA = maps:get(org_a, Ids),
        OrgFilters = #{scope => <<"organization">>, organization_id => OrgA, workspace_id => 0},
        %% U1 查 ORG_A（limit=100 全量）：含 U2/U3 与翻页批；排除自己/
        %% 跨企业 U4/陌生人 U7（u.id DESC 使 730xxx 翻页批排前，member
        %% 断言须取全量页）
        {ok, Full} = talk_record_grant_candidates:generic(U1, OrgFilters, <<>>, undefined, 100),
        UsersFull = [maps:get(<<"user_id">>, C) || C <- maps:get(<<"list">>, Full)],
        ?assert(lists:member(maps:get(u2, Ids), UsersFull)),
        ?assert(lists:member(maps:get(u3, Ids), UsersFull)),
        ?assertNot(lists:member(U1, UsersFull)),
        ?assertNot(lists:member(U4, UsersFull)),
        ?assertNot(lists:member(U7, UsersFull)),
        %% 目录最小信息
        U2Row = hd([
            C
         || C <- maps:get(<<"list">>, Full),
            maps:get(<<"user_id">>, C) =:= maps:get(u2, Ids)
        ]),
        ?assertEqual(<<"viewer-b"/utf8>>, maps:get(<<"display_name">>, U2Row)),
        ?assertEqual(<<"ua2">>, maps:get(<<"account">>, U2Row)),
        %% limit=20 分页（27 候选 > 20）：cursor 绑定调用者与筛选、无重叠
        {ok, Page1} = talk_record_grant_candidates:generic(U1, OrgFilters, <<>>, undefined, 20),
        Users1 = [maps:get(<<"user_id">>, C) || C <- maps:get(<<"list">>, Page1)],
        ?assertEqual(20, length(Users1)),
        ?assert(is_binary(maps:get(<<"next_cursor">>, Page1))),
        {ok, Page2} = talk_record_grant_candidates:generic(
            U1,
            OrgFilters,
            <<>>,
            maps:get(<<"next_cursor">>, Page1),
            20
        ),
        Users2 = [maps:get(<<"user_id">>, C) || C <- maps:get(<<"list">>, Page2)],
        ?assert(Users2 =/= []),
        ?assert(lists:all(fun(U) -> not lists:member(U, Users1) end, Users2)),
        %% q 过滤（昵称前缀）
        {ok, QPage} = talk_record_grant_candidates:generic(
            U1,
            OrgFilters,
            <<"page-1">>,
            undefined,
            20
        ),
        QUsers = [maps:get(<<"user_id">>, C) || C <- maps:get(<<"list">>, QPage)],
        ?assert(lists:member(730001, QUsers)),
        ?assertNot(lists:member(730002, QUsers))
    end).

candidates_generic_access_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t17_support:ids(),
        U1 = maps:get(u1, Ids),
        U4 = maps:get(u4, Ids),
        OrgA = maps:get(org_a, Ids),
        OrgB = maps:get(org_b, Ids),
        WsB1 = maps:get(ws_b1, Ids),
        %% 跨企业：ORG_B owner 查 ORG_A → 403
        ?assertEqual(
            {error, forbidden},
            talk_record_grant_candidates:generic(
                U4,
                #{scope => <<"organization">>, organization_id => OrgA, workspace_id => 0},
                <<>>,
                undefined,
                20
            )
        ),
        %% ws 不属于该 org → 422
        ?assertEqual(
            {error, invalid_workspace_id},
            talk_record_grant_candidates:generic(
                U1,
                #{scope => <<"organization">>, organization_id => OrgA, workspace_id => WsB1},
                <<>>,
                undefined,
                20
            )
        ),
        %% organization 缺 org → 422；personal 带 org → 422
        ?assertEqual(
            {error, invalid_organization_id},
            talk_record_grant_candidates:generic(
                U1,
                #{scope => <<"organization">>, organization_id => 0, workspace_id => 0},
                <<>>,
                undefined,
                20
            )
        ),
        ?assertEqual(
            {error, personal_with_org},
            talk_record_grant_candidates:generic(
                U1,
                #{scope => <<"personal">>, organization_id => OrgB, workspace_id => 0},
                <<>>,
                undefined,
                20
            )
        ),
        %% 个人目录只包含有效本人和双向好友，不能枚举其他有效账号。
        {ok, PPage} = talk_record_grant_candidates:generic(
            maps:get(u7, Ids),
            #{scope => <<"personal">>, organization_id => 0, workspace_id => 0},
            <<>>,
            undefined,
            100
        ),
        PUsers = [maps:get(<<"user_id">>, C) || C <- maps:get(<<"list">>, PPage)],
        U7 = maps:get(u7, Ids),
        ?assert(lists:member(U7, PUsers)),
        ?assertNot(lists:member(U1, PUsers)),
        Conn = maps:get(conn, State),
        {ok, 2} = elib_pg:execute(
            Conn,
            <<
                "INSERT INTO user_friend(id,from_user_id,to_user_id,status) "
                "VALUES(9680771,$1,$2,1),(9680772,$2,$1,1)"
            >>,
            [U7, U1]
        ),
        {ok, FriendPage} = talk_record_grant_candidates:generic(
            U7,
            #{scope => <<"personal">>, organization_id => 0, workspace_id => 0},
            <<>>,
            undefined,
            100
        ),
        FriendUsers = [maps:get(<<"user_id">>, C) || C <- maps:get(<<"list">>, FriendPage)],
        ?assert(lists:member(U1, FriendUsers)),
        ?assertNot(lists:member(U4, FriendUsers))
    end).

candidates_ws_filter_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t17_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        OrgA = maps:get(org_a, Ids),
        WsA1 = maps:get(ws_a1, Ids),
        Conn = maps:get(conn, State),
        %% U2 非 WS_A1 成员：ws 候选不含 U2
        {ok, NoWs} = talk_record_grant_candidates:generic(
            U1,
            #{scope => <<"organization">>, organization_id => OrgA, workspace_id => WsA1},
            <<>>,
            undefined,
            20
        ),
        NoWsUsers = [maps:get(<<"user_id">>, C) || C <- maps:get(<<"list">>, NoWs)],
        ?assertNot(lists:member(U2, NoWsUsers)),
        %% U2 加入 WS_A1 后出现（候选侧 ws 资源授权谓词生效）
        talk_record_t17_support:sql(Conn, [
            "INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES (",
            integer_to_binary(WsA1),
            ", ",
            integer_to_binary(U2),
            ", 'member', 'active')"
        ]),
        {ok, WithWs} = talk_record_grant_candidates:generic(
            U1,
            #{scope => <<"organization">>, organization_id => OrgA, workspace_id => WsA1},
            <<>>,
            undefined,
            20
        ),
        WithWsUsers = [maps:get(<<"user_id">>, C) || C <- maps:get(<<"list">>, WithWs)],
        ?assert(lists:member(U2, WithWsUsers))
    end).

candidates_cursor_binding_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t17_support:ids(),
        U1 = maps:get(u1, Ids),
        U4 = maps:get(u4, Ids),
        OrgA = maps:get(org_a, Ids),
        OrgFilters = #{scope => <<"organization">>, organization_id => OrgA, workspace_id => 0},
        {ok, Page1} = talk_record_grant_candidates:generic(U1, OrgFilters, <<>>, undefined, 20),
        Cursor = maps:get(<<"next_cursor">>, Page1),
        %% cursor 绑定调用者：换人（ORG_B owner 对 ORG_A 本就 403）与换筛选
        %% 均拒绝沿用
        ?assertEqual(
            {error, forbidden},
            talk_record_grant_candidates:generic(U4, OrgFilters, <<>>, Cursor, 20)
        ),
        ?assertEqual(
            {error, cursor_invalid},
            talk_record_grant_candidates:generic(U1, OrgFilters, <<"page-1">>, Cursor, 20)
        )
    end).

candidates_record_level_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t17_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        U4 = maps:get(u4, Ids),
        %% org 记录（ORG_A）：owner 查候选（limit=100 全量）→ 排除 owner/
        %% 已授权/跨企业
        OrgF = fixture(State, 57, #{scope => organization}),
        OrgRid = maps:get(record, OrgF),
        Conn = maps:get(conn, OrgF),
        talk_record_t15_support:add_grant(Conn, OrgRid, U2, U1, <<"viewer">>),
        {ok, RC} = talk_record_grant_candidates:for_record(U1, OrgRid, <<>>, undefined, 100),
        RCUsers = [maps:get(<<"user_id">>, C) || C <- maps:get(<<"list">>, RC)],
        ?assert(lists:member(maps:get(u3, Ids), RCUsers)),
        ?assertNot(lists:member(U1, RCUsers)),
        ?assertNot(lists:member(U2, RCUsers)),
        ?assertNot(lists:member(U4, RCUsers)),
        %% 非 owner（被授权者）→ 403；陌生人 → 404
        ?assertEqual(
            {error, forbidden},
            talk_record_grant_candidates:for_record(U2, OrgRid, <<>>, undefined, 20)
        ),
        ?assertEqual(
            {error, not_found},
            talk_record_grant_candidates:for_record(maps:get(u7, Ids), OrgRid, <<>>, undefined, 20)
        ),
        %% personal 记录：owner 可查（最小实现全集，排除 owner/已授权）
        PF = fixture(State, 58),
        PRid = maps:get(record, PF),
        talk_record_t15_support:add_grant(maps:get(conn, PF), PRid, U2, U1, <<"viewer">>),
        {ok, PC} = talk_record_grant_candidates:for_record(U1, PRid, <<>>, undefined, 100),
        PCUsers = [maps:get(<<"user_id">>, C) || C <- maps:get(<<"list">>, PC)],
        ?assertNot(lists:member(U1, PCUsers)),
        ?assertNot(lists:member(U2, PCUsers)),
        ?assert(lists:member(maps:get(u3, Ids), PCUsers))
    end).
