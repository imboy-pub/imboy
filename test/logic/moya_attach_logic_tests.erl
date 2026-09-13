%% moya_attach_logic_tests
%% Step 10 MEDIA-01（授权 wiring 矩阵）/ MEDIA-02（清理不误删）/ 校验白名单。
%% submission_access 的完整 ACL 矩阵（跨 Org/跨 learner/仅群管理员/仅 Owner）
%% 已在 moya_acl_tests（Step 8）覆盖；本套件验证 attach 集成层 wiring
%% 与白名单/上限/时长/清理逻辑。
%%
%% P0-4（MN-MEDIA-02/03 后端）追加：
%%   * review_asset 读授权分流（draft 仅 reviewer / published 复用 submission_access）
%%   * save_draft assets 请求结构校验（0-1 video + 0-3 image + TSID string）
%%   * DTO：assets[] 严格 TSID string、video_attachment_id 只读派生、
%%     家长侧 published-only、AI 内部字段防御性剥离

-module(moya_attach_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(MB, 1024 * 1024).
-define(PARENT, 980002).
-define(TEACHER, 980001).
%% P0-4 独立 ID 段（995xxx，与真库集成测试夹具同段）
-define(RV_TEACHER, 995001).
-define(RV_PARENT, 995002).
-define(RV_SUBMISSION, 995701).
-define(RV_REVIEW_DRAFT, 995801).
-define(RV_REVIEW_PUB, 995802).

%%%===================================================================
%%% can_upload/1（教学身份守卫）
%%%===================================================================

can_upload_guardian_test_() ->
    ?WITH_MECKS([attach_mocks(guardian)], fun() ->
        ?assertEqual(ok, moya_attach_logic:can_upload(?PARENT))
    end).

can_upload_staff_only_test_() ->
    ?WITH_MECKS([attach_mocks(staff)], fun() ->
        ?assertEqual(ok, moya_attach_logic:can_upload(?TEACHER))
    end).

can_upload_no_identity_test_() ->
    ?WITH_MECKS([attach_mocks(none)], fun() ->
        ?assertEqual(false, moya_attach_logic:can_upload(980099))
    end).

can_upload_db_error_fail_closed_test_() ->
    ?WITH_MECKS([attach_mocks(db_error)], fun() ->
        ?assertEqual(false, moya_attach_logic:can_upload(?PARENT))
    end).

%%%===================================================================
%%% check_mime/1 + verify_upload/3（白名单/大小/时长）
%%%===================================================================

mime_whitelist_test_() ->
    ?WITH_MECKS([env_mocks()], fun() ->
        lists:foreach(
            fun(M) -> ?assertEqual(ok, moya_attach_logic:check_mime(M)) end,
            [
                <<"video/mp4">>,
                %% iPhone 原录 .mov
                <<"video/quicktime">>,
                <<"image/jpeg">>,
                <<"image/png">>,
                %% 部分安卓机型 chooseMedia 压缩产物（2026-09-12 扩充）
                <<"image/webp">>
            ]
        ),
        lists:foreach(
            fun(M) ->
                ?assertEqual({error, invalid_file_type}, moya_attach_logic:check_mime(M))
            end,
            [
                <<"video/x-msvideo">>,
                <<"image/gif">>,
                <<"application/octet-stream">>
            ]
        )
    end).

video_size_and_duration_test_() ->
    ?WITH_MECKS([env_mocks()], fun() ->
        %% video ≤100MB + duration ≤60s
        ?assertEqual(
            ok,
            moya_attach_logic:verify_upload(
                <<"video/mp4">>, 100 * ?MB, #{<<"duration">> => 60}
            )
        ),
        ?assertEqual(
            {error, file_too_large},
            moya_attach_logic:verify_upload(
                <<"video/mp4">>, 100 * ?MB + 1, #{<<"duration">> => 30}
            )
        ),
        ?assertEqual(
            {error, invalid_file_type},
            moya_attach_logic:verify_upload(
                <<"video/quicktime">>, 10 * ?MB, #{<<"duration">> => 61}
            )
        ),
        %% duration 未上报：首版放行（服务端抽帧复核留 Step 11）
        ?assertEqual(
            ok,
            moya_attach_logic:verify_upload(
                <<"video/mp4">>, 10 * ?MB, #{}
            )
        ),
        %% duration_seconds 别名同样受校验
        ?assertEqual(
            {error, invalid_file_type},
            moya_attach_logic:verify_upload(
                <<"video/mp4">>, 10 * ?MB, #{<<"duration_seconds">> => 90}
            )
        )
    end).

photo_size_test_() ->
    ?WITH_MECKS([env_mocks()], fun() ->
        ?assertEqual(ok, moya_attach_logic:verify_upload(<<"image/jpeg">>, 20 * ?MB, #{})),
        ?assertEqual(
            {error, file_too_large},
            moya_attach_logic:verify_upload(
                <<"image/png">>, 20 * ?MB + 1, #{}
            )
        )
    end).

%%%===================================================================
%%% MEDIA-01：authorize/2 授权 wiring（矩阵经 meck 注入 submission_access 结果）
%%%===================================================================

media01_guardian_view_ok_test_() ->
    ?WITH_MECKS(media01_mocks(<<"submitted">>, {ok, guardian, scope1()}), fun() ->
        ?assertEqual(true, moya_attach_logic:authorize(?PARENT, att_rec()))
    end).

media01_staff_view_ok_test_() ->
    ?WITH_MECKS(media01_mocks(<<"submitted">>, {ok, staff, scope1()}), fun() ->
        ?assertEqual(true, moya_attach_logic:authorize(?TEACHER, att_rec()))
    end).

%% 未授权家长（can_view_review=false）/ 同班其他成员 / 非任课老师 / 仅 Owner：
%% submission_access 全部 {error, _} → view_url 拒绝（Step 8 矩阵的 wiring 端）
media01_forbidden_test_() ->
    lists:map(
        fun(AccessErr) ->
            ?WITH_MECKS(media01_mocks(<<"submitted">>, {error, AccessErr}), fun() ->
                ?assertEqual(false, moya_attach_logic:authorize(980099, att_rec()))
            end)
        end,
        [forbidden, forbidden, owner_not_granted, not_found]
    ).

%% 未绑定 submission 的附件（上传后未提交）：任何身份都拿不到读 URL
media01_unbound_denied_test_() ->
    ?WITH_MECKS(media01_mocks(undefined, {ok, guardian, scope1()}), fun() ->
        ?assertEqual(false, moya_attach_logic:authorize(?PARENT, att_rec()))
    end).

%% 已撤回 submission：证据仅审计路径可及，日常 view_url 拒绝（T17）
media01_withdrawn_denied_test_() ->
    ?WITH_MECKS(media01_mocks(<<"withdrawn">>, {ok, guardian, scope1()}), fun() ->
        ?assertEqual(false, moya_attach_logic:authorize(?PARENT, att_rec()))
    end).

%%%===================================================================
%%% MEDIA-02：cleanup_unbound 不误删
%%%===================================================================

media02_cleanup_deletes_orphan_test_() ->
    ?WITH_MECKS(
        [
            {moya_submission_repo, [
                {'unbound_teaching_attachments', 1, fun(_) ->
                    {ok, [#{<<"id">> => 501, <<"path">> => <<"u99/orphan1">>}]}
                end}
            ]},
            {elib_oss, [
                {'delete_object', 1, fun(<<"u99/orphan1">>) -> ok end}
            ]},
            {attachment_ds, [
                {'soft_delete', 1, fun(501) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{cleaned => 1, errors => 0}},
                moya_attach_logic:cleanup_unbound(24)
            )
        end
    ).

%% 删对象失败：保留行（下轮重试），绝不先删行留孤儿对象
media02_cleanup_object_error_keeps_row_test_() ->
    ?WITH_MECKS(
        [
            {moya_submission_repo, [
                {'unbound_teaching_attachments', 1, fun(_) ->
                    {ok, [#{<<"id">> => 502, <<"path">> => <<"u99/orphan2">>}]}
                end}
            ]},
            {elib_oss, [
                {'delete_object', 1, fun(_) -> {error, s3_down} end}
            ]},
            {attachment_ds, [
                {'soft_delete', 1, fun(_) -> erlang:error(should_not_soft_delete) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{cleaned => 0, errors => 1}},
                moya_attach_logic:cleanup_unbound(24)
            )
        end
    ).

media02_cleanup_empty_test_() ->
    ?WITH_MECKS(
        [
            {moya_submission_repo, [
                {'unbound_teaching_attachments', 1, fun(_) -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{cleaned => 0, errors => 0}},
                moya_attach_logic:cleanup_unbound(24)
            )
        end
    ).

%%%===================================================================
%%% MEDIA-03：业务表与日志不得持久化 presigned URL（源码扫描断言）
%%%===================================================================

media03_no_presigned_url_persisted_test_() ->
    %% repo（唯一 DB 写入层）不得出现任何 presign 调用/签名串
    RepoFiles = [
        "src/repo/moya_submission_repo.erl",
        "src/repo/moya_review_repo.erl",
        "src/repo/moya_context_repo.erl"
    ],
    RepoAsserts = [
        ?_assert(begin
            {ok, Src} = file:read_file(F),
            binary:match(Src, <<"presign_put_for_key">>) =:= nomatch andalso
                binary:match(Src, <<"presign_get_for_key">>) =:= nomatch andalso
                binary:match(Src, <<"X-Amz-">>) =:= nomatch
        end)
     || F <- RepoFiles
    ],
    %% attach_logic 落库 map：path/url 均绑定 ObjectKey（URL 仅按请求签发，从不落库）
    {ok, AttachSrc} = file:read_file("src/logic/attach_logic.erl"),
    AttachAssert = ?_assert(begin
        binary:match(AttachSrc, <<"<<\"url\">> => ObjectKey">>) =/= nomatch andalso
            binary:match(AttachSrc, <<"<<\"path\">> => ObjectKey">>) =/= nomatch
    end),
    RepoAsserts ++ [AttachAssert].

%%%===================================================================
%%% MN-MEDIA-02：review_asset 读授权分流（view URL 签发）
%%% submission_asset 维度沿用 MEDIA-01 现有矩阵；review_asset 维度：
%%%   draft 仅创建该草稿的 reviewer；published 复用 submission_access
%%%   （本班 active staff / can_view_review=true active guardian）；
%%%   withdrawn / 未绑定 / discarded / 查询失败一律 fail closed
%%%===================================================================

%% submission_asset 未绑定 → 分流查 review_asset：draft + 本人 reviewer → 放行
review_asset_draft_by_reviewer_test_() ->
    ?WITH_MECKS(review_asset_mocks(draft_submitted, ?RV_TEACHER), fun() ->
        ?assertEqual(true, moya_attach_logic:authorize(?RV_TEACHER, att_rec()))
    end).

%% draft + 其他任何身份（含本班其他老师/家长）→ 拒绝
review_asset_draft_other_denied_test_() ->
    ?WITH_MECKS(review_asset_mocks(draft_submitted, ?RV_TEACHER), fun() ->
        ?assertEqual(false, moya_attach_logic:authorize(?RV_PARENT, att_rec()))
    end).

%% published + staff（submission_access ok）→ 放行
review_asset_published_staff_ok_test_() ->
    ?WITH_MECKS(review_asset_mocks(published_submitted, {ok, staff, scope105()}), fun() ->
        ?assertEqual(true, moya_attach_logic:authorize(?RV_TEACHER, att_rec()))
    end).

%% published + guardian can_view_review=true（submission_access ok）→ 放行
review_asset_published_guardian_ok_test_() ->
    ?WITH_MECKS(review_asset_mocks(published_submitted, {ok, guardian, scope105()}), fun() ->
        ?assertEqual(true, moya_attach_logic:authorize(?RV_PARENT, att_rec()))
    end).

%% published + submission_access 拒绝（跨班/跨机构/无 can_view_review/仅 Owner）→ fail closed
review_asset_published_access_denied_test_() ->
    lists:map(
        fun(AccessErr) ->
            ?WITH_MECKS(review_asset_mocks(published_submitted, {error, AccessErr}), fun() ->
                ?assertEqual(false, moya_attach_logic:authorize(980099, att_rec()))
            end)
        end,
        [forbidden, owner_not_granted, not_found, db_error]
    ).

%% submission 已撤回：即使 reviewer 本人 draft 也 fail closed（T17 撤回证据仅审计可及）
review_asset_withdrawn_fail_closed_test_() ->
    ?WITH_MECKS(review_asset_mocks(draft_withdrawn, {ok, staff, scope105()}), fun() ->
        ?assertEqual(false, moya_attach_logic:authorize(?RV_TEACHER, att_rec()))
    end).

%% review 已 discarded（审计终态）→ 拒绝
review_asset_discarded_denied_test_() ->
    ?WITH_MECKS(review_asset_mocks(discarded_submitted, {ok, staff, scope105()}), fun() ->
        ?assertEqual(false, moya_attach_logic:authorize(?RV_TEACHER, att_rec()))
    end).

%% 未绑定任何业务（submission_asset 与 review_asset 均无）→ 拒绝
review_asset_unbound_denied_test_() ->
    ?WITH_MECKS(review_asset_mocks(unbound, {ok, guardian, scope105()}), fun() ->
        ?assertEqual(false, moya_attach_logic:authorize(?RV_PARENT, att_rec()))
    end).

%% review 维度查询异常 → fail closed（不因错误放行）
review_asset_db_error_fail_closed_test_() ->
    ?WITH_MECKS(review_asset_mocks(db_error, {ok, guardian, scope105()}), fun() ->
        ?assertEqual(false, moya_attach_logic:authorize(?RV_PARENT, att_rec()))
    end).

%%%===================================================================
%%% MN-MEDIA-02：save_draft 请求 assets 结构校验（解析层，快失败）
%%%===================================================================

save_draft_two_videos_rejected_test_() ->
    ?_assertEqual(
        {error, assets_invalid},
        moya_review_logic:save_draft(?RV_TEACHER, ?RV_SUBMISSION, #{
            <<"comment">> => <<"ok">>,
            <<"assets">> => [
                #{<<"attachment_id">> => <<"995601">>, <<"kind">> => <<"feedback_video">>},
                #{<<"attachment_id">> => <<"995610">>, <<"kind">> => <<"feedback_video">>}
            ]
        })
    ).

save_draft_four_images_rejected_test_() ->
    ?_assertEqual(
        {error, assets_invalid},
        moya_review_logic:save_draft(?RV_TEACHER, ?RV_SUBMISSION, #{
            <<"comment">> => <<"ok">>,
            <<"assets">> => [
                #{<<"attachment_id">> => integer_to_binary(I), <<"kind">> => <<"feedback_image">>}
             || I <- [995602, 995603, 995604, 995605]
            ]
        })
    ).

save_draft_bad_asset_shape_rejected_test_() ->
    %% attachment_id 非 TSID string / kind 非法 / 重复 attachment_id / assets 非数组
    BadBodies = [
        #{
            <<"assets">> => [
                #{<<"attachment_id">> => <<"abc">>, <<"kind">> => <<"feedback_video">>}
            ]
        },
        #{<<"assets">> => [#{<<"attachment_id">> => 995601, <<"kind">> => <<"feedback_video">>}]},
        #{
            <<"assets">> => [
                #{<<"attachment_id">> => <<"995601">>, <<"kind">> => <<"practice_video">>}
            ]
        },
        #{
            <<"assets">> => [
                #{<<"attachment_id">> => <<"995602">>, <<"kind">> => <<"feedback_image">>},
                #{<<"attachment_id">> => <<"995602">>, <<"kind">> => <<"feedback_image">>}
            ]
        },
        #{<<"assets">> => <<"not-a-list">>},
        #{<<"assets">> => [#{<<"kind">> => <<"feedback_image">>}]},
        #{<<"assets">> => [#{<<"attachment_id">> => <<"995602">>}]}
    ],
    lists:map(
        fun(Body) ->
            ?WITH_MECKS([], fun() ->
                ?assertEqual(
                    {error, assets_invalid},
                    moya_review_logic:save_draft(?RV_TEACHER, ?RV_SUBMISSION, Body)
                )
            end)
        end,
        BadBodies
    ).

%%%===================================================================
%%% MN-MEDIA-03 后端：DTO（assets[] TSID string / video 派生 / 家长侧 published-only / AI 字段剥离）
%%%===================================================================

%% save_draft 响应：assets 全量回显（TSID string）+ video_attachment_id 从第一条
%% feedback_video 派生（只读兼容字段，不再是写入真源）
dto_save_draft_response_test_() ->
    ?WITH_MECKS(dto_mocks(draft_row(), review_asset_rows()), fun() ->
        {ok, Payload} = moya_review_logic:save_draft(?RV_TEACHER, ?RV_SUBMISSION, #{
            <<"comment">> => <<"结构不错"/utf8>>,
            <<"assets">> => [
                #{
                    <<"attachment_id">> => <<"995601">>,
                    <<"kind">> => <<"feedback_video">>,
                    <<"sort_order">> => 0
                },
                #{
                    <<"attachment_id">> => <<"995602">>,
                    <<"kind">> => <<"feedback_image">>,
                    <<"sort_order">> => 1
                }
            ]
        }),
        %% assets[]：attachment_id 一律 TSID string
        Assets = maps:get(<<"assets">>, Payload),
        ?assertEqual(2, length(Assets)),
        lists:foreach(
            fun(#{<<"attachment_id">> := AttId, <<"kind">> := Kind, <<"sort_order">> := Order}) ->
                ?assert(is_binary(AttId)),
                ?assertMatch(
                    {ok, _},
                    try
                        {ok, binary_to_integer(AttId)}
                    catch
                        _:_ -> error
                    end
                ),
                ?assert(lists:member(Kind, [<<"feedback_image">>, <<"feedback_video">>])),
                ?assert(is_integer(Order))
            end,
            Assets
        ),
        %% 兼容字段派生：第一条 feedback_video（995601），而非 DB 行旧列原值（995699）
        ?assertEqual(<<"995601">>, maps:get(<<"video_attachment_id">>, Payload))
    end).

%% save_draft 空 assets 合法：assets=[] + video_attachment_id=null
dto_save_draft_empty_assets_test_() ->
    ?WITH_MECKS(dto_mocks(draft_row(), []), fun() ->
        {ok, Payload} = moya_review_logic:save_draft(?RV_TEACHER, ?RV_SUBMISSION, #{
            <<"comment">> => <<"纯文字回评"/utf8>>,
            <<"assets">> => []
        }),
        ?assertEqual([], maps:get(<<"assets">>, Payload)),
        ?assertEqual(null, maps:get(<<"video_attachment_id">>, Payload))
    end).

%% 家长视角 submission_detail：只输出 published review 的 assets；
%% AI 草稿字段与任何内部字段在 DTO 组装处剥离（防御性：即使行数据带异常字段也不透出）
dto_parent_view_published_only_test_() ->
    ?WITH_MECKS(parent_view_mocks(), fun() ->
        {ok, Detail} = moya_review_logic:submission_detail(?RV_PARENT, ?RV_SUBMISSION),
        %% 家长 payload 无 ai_draft 键（D-10；防御性剥离——即使 ai_draft 行存在）
        ?assertEqual(false, maps:is_key(<<"ai_draft">>, Detail)),
        ?assertEqual(false, maps:is_key(<<"my_review_draft">>, Detail)),
        Pub = maps:get(<<"published_review">>, Detail),
        %% published review 的 assets：TSID string
        PubAssets = maps:get(<<"assets">>, Pub),
        ?assertEqual(2, length(PubAssets)),
        lists:foreach(
            fun(#{<<"attachment_id">> := AttId}) -> ?assert(is_binary(AttId)) end,
            PubAssets
        ),
        %% 兼容字段从 assets 第一条 video 派生
        ?assertEqual(<<"995601">>, maps:get(<<"video_attachment_id">>, Pub)),
        %% 老师署名（c8c086d3）：来自 user_repo nickname 解析（DEF-B 夹具）
        ?assertEqual(<<"李老师"/utf8>>, maps:get(<<"reviewer_display_name">>, Pub)),
        %% AI 内部字段不透出（model_profile/prompt_version/result_json/error_code）
        lists:foreach(
            fun(K) -> ?assertEqual(false, maps:is_key(K, Pub)) end,
            [
                <<"model_profile">>,
                <<"prompt_version">>,
                <<"rubric_version">>,
                <<"result_json">>,
                <<"error_code">>
            ]
        )
    end).

%%%===================================================================
%%% Helpers
%%%===================================================================

env_mocks() ->
    {config_ds, [
        {'env', 2, fun
            (teaching_video_max_mb, _) -> 100;
            (teaching_photo_max_mb, _) -> 20;
            (teaching_video_max_duration, _) -> 60;
            (_, Default) -> Default
        end}
    ]}.

attach_mocks(guardian) ->
    {moya_context_repo, ctx_mocks([g1], [])};
attach_mocks(staff) ->
    {moya_context_repo, ctx_mocks([], [s1])};
attach_mocks(none) ->
    {moya_context_repo, ctx_mocks([], [])};
attach_mocks(db_error) ->
    {moya_context_repo, [
        {'guardian_contexts', 1, fun(_) -> {error, db_down} end},
        {'staff_contexts', 1, fun(_) -> {error, db_down} end}
    ]}.

ctx_mocks(G, S) ->
    [
        {'guardian_contexts', 1, fun(_) -> {ok, G} end},
        {'staff_contexts', 1, fun(_) -> {ok, S} end}
    ].

%% media01 mocks：附件路径 → submission 绑定关系 + submission_access 注入结果
%% （P0-4 起 submission 未绑定时分流查 review_asset：固定注入未绑定）
media01_mocks(SubStatus, AccessResult) ->
    [
        {moya_submission_repo, [
            {'submission_for_asset_path', 1, fun(<<"u99/key1">>) ->
                case SubStatus of
                    undefined ->
                        {ok, undefined};
                    Status ->
                        {ok, #{
                            <<"submission_id">> => 6001,
                            <<"submission_status">> => Status
                        }}
                end
            end}
        ]},
        {moya_review_repo, [
            {'review_for_asset_path', 1, fun(_) -> {ok, undefined} end}
        ]},
        {moya_acl, [
            {'submission_access', 2, fun(_Uid, 6001) -> AccessResult end}
        ]}
    ].

att_rec() ->
    #{
        <<"path">> => <<"u99/key1">>,
        <<"scope">> => <<"teaching">>,
        <<"creator_user_id">> => ?PARENT
    }.

scope1() ->
    #{
        <<"submission_id">> => 6001,
        <<"learner_id">> => 3001,
        <<"group_id">> => 1001,
        <<"org_id">> => 100
    }.

%%%-------------------------------------------------------------------
%%% P0-4 helpers
%%%-------------------------------------------------------------------

scope105() ->
    #{
        <<"submission_id">> => ?RV_SUBMISSION,
        <<"learner_id">> => 995301,
        <<"group_id">> => 995201,
        <<"org_id">> => 995100
    }.

%% review_asset 分流 mocks：submission_asset 维度未绑定（undefined），
%% review 维度按 Case 注入；AccessResult 仅 published 分支使用
review_asset_mocks(Case, AccessResult) ->
    ReviewRow =
        case Case of
            draft_submitted ->
                #{
                    <<"review_id">> => ?RV_REVIEW_DRAFT,
                    <<"review_status">> => <<"draft">>,
                    <<"reviewer_uid">> => ?RV_TEACHER,
                    <<"submission_id">> => ?RV_SUBMISSION,
                    <<"submission_status">> => <<"submitted">>
                };
            draft_withdrawn ->
                #{
                    <<"review_id">> => ?RV_REVIEW_DRAFT,
                    <<"review_status">> => <<"draft">>,
                    <<"reviewer_uid">> => ?RV_TEACHER,
                    <<"submission_id">> => 995702,
                    <<"submission_status">> => <<"withdrawn">>
                };
            published_submitted ->
                #{
                    <<"review_id">> => ?RV_REVIEW_PUB,
                    <<"review_status">> => <<"published">>,
                    <<"reviewer_uid">> => ?RV_TEACHER,
                    <<"submission_id">> => ?RV_SUBMISSION,
                    <<"submission_status">> => <<"submitted">>
                };
            discarded_submitted ->
                #{
                    <<"review_id">> => ?RV_REVIEW_PUB,
                    <<"review_status">> => <<"discarded">>,
                    <<"reviewer_uid">> => ?RV_TEACHER,
                    <<"submission_id">> => ?RV_SUBMISSION,
                    <<"submission_status">> => <<"submitted">>
                };
            unbound ->
                undefined;
            db_error ->
                db_error
        end,
    [
        {moya_submission_repo, [
            {'submission_for_asset_path', 1, fun(_) -> {ok, undefined} end}
        ]},
        {moya_review_repo, [
            {'review_for_asset_path', 1, fun(_) ->
                case ReviewRow of
                    db_error -> {error, db_down};
                    Other -> {ok, Other}
                end
            end}
        ]},
        {moya_acl, [
            {'submission_access', 2, fun(_Uid, ?RV_SUBMISSION) -> AccessResult end}
        ]}
    ].

%% save_draft DTO 全链 mocks（不落 DB）：ACL 放行 + 事务直执 + repo 注入
dto_mocks(DraftRow, AssetRows) ->
    [
        {moya_acl, [
            {'submission_access', 2, fun(_, ?RV_SUBMISSION) ->
                {ok, staff, scope105()}
            end},
            {'resolve_staff', 3, fun(_, _, write) -> {ok, #{<<"role">> => <<"teacher">>}} end}
        ]},
        {elib_pg, [
            {'with_tx', 2, fun(Tx, _Opts) -> Tx(fake_conn) end}
        ]},
        {moya_submission_repo, [
            {'lock_submission_tx', 2, fun(_, ?RV_SUBMISSION) ->
                {ok, #{<<"id">> => ?RV_SUBMISSION, <<"status">> => <<"submitted">>}}
            end}
        ]},
        {moya_review_repo, [
            %% R22-PUBLISHED-DRAFT-01 后 save_draft 事务内在 upsert 前新增
            %% find_published_tx 守卫：正常草稿场景无已发布回评
            {'find_published_tx', 2, fun(_Conn, _Sid) -> {ok, undefined} end},
            {'upsert_draft_tx', 3, fun(_, _, _Fields) -> {ok, DraftRow} end},
            {'validate_assets_tx', 3, fun(_Conn, _Uid, Assets) -> {ok, Assets} end},
            {'replace_assets_tx', 4, fun(_Conn, _Rid, _Uid, _Assets) -> ok end},
            {'assets_tx', 2, fun(_Conn, _Rid) -> {ok, AssetRows} end}
        ]}
    ].

%% 家长视角 submission_detail 全链 mocks
parent_view_mocks() ->
    [
        {moya_acl, [
            {'submission_access', 2, fun(_, ?RV_SUBMISSION) ->
                {ok, guardian, scope105()}
            end}
        ]},
        {moya_context_repo, [
            {'submission_scope', 1, fun(?RV_SUBMISSION) -> {ok, scope105()} end}
        ]},
        {moya_submission_repo, [
            {'find', 1, fun(?RV_SUBMISSION) -> {ok, sub_row()} end},
            {'assets', 1, fun(?RV_SUBMISSION) -> {ok, []} end}
        ]},
        {moya_review_repo, [
            {'ai_draft', 1, fun(?RV_SUBMISSION) -> {ok, ai_row()} end},
            {'find_published', 1, fun(?RV_SUBMISSION) -> {ok, pub_row()} end},
            {'find_draft', 2, fun(_, _) -> {ok, undefined} end},
            {'assets', 1, fun(?RV_REVIEW_PUB) -> {ok, review_asset_rows()} end}
        ]},
        %% DEF-B（CM）：pub_row() 带 reviewer_uid，bundle 装配会经
        %% reviewer_display_name → user_repo:find_by_uid → elib_pg:one 解析
        %% 署名（c8c086d3 起）——夹具此前只 meck elib_pg:query/2，漏 one/2
        %% 与 user_repo，真连池 → {noproc, pgsql take_member}。补 user_repo
        %% 语义化 meck（nickname 命中路径）。
        {user_repo, [
            {'find_by_uid', 1, fun(?RV_TEACHER) ->
                {ok, #{<<"nickname">> => <<"李老师"/utf8>>}}
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_, _) -> {ok, []} end}
        ]}
    ].

%% DB 行旧列值为 995699：断言 DTO 输出从 assets 派生（995601）而非行旧列
draft_row() ->
    #{
        <<"id">> => ?RV_REVIEW_DRAFT,
        <<"submission_id">> => ?RV_SUBMISSION,
        <<"reviewer_uid">> => ?RV_TEACHER,
        <<"positive_point">> => <<>>,
        <<"focus_problem">> => <<>>,
        <<"practice_action">> => <<>>,
        <<"comment">> => <<"结构不错"/utf8>>,
        <<"video_attachment_id">> => 995699,
        <<"rework_required">> => false,
        <<"status">> => <<"draft">>,
        <<"published_at">> => null
    }.

pub_row() ->
    Base = draft_row(),
    Base#{
        <<"id">> => ?RV_REVIEW_PUB,
        <<"status">> => <<"published">>,
        <<"published_at">> => <<"2026-09-10T00:00:00Z">>
    }.

ai_row() ->
    #{
        <<"id">> => 995851,
        <<"submission_id">> => ?RV_SUBMISSION,
        <<"status">> => <<"succeeded">>,
        <<"model_profile">> => <<"internal-model">>,
        <<"prompt_version">> => <<"v9">>,
        <<"rubric_version">> => <<"r1">>,
        <<"result_json">> => #{<<"secret">> => true},
        <<"error_code">> => null,
        <<"created_at">> => null,
        <<"completed_at">> => null
    }.

sub_row() ->
    #{
        <<"id">> => ?RV_SUBMISSION,
        <<"assignment_id">> => 995501,
        <<"learner_id">> => 995301,
        <<"attempt_no">> => 1,
        <<"status">> => <<"submitted">>,
        <<"submitted_at">> => <<"2026-09-09T00:00:00Z">>,
        <<"withdrawn_at">> => null
    }.

review_asset_rows() ->
    [
        #{
            <<"review_id">> => ?RV_REVIEW_PUB,
            <<"attachment_id">> => 995601,
            <<"kind">> => <<"feedback_video">>,
            <<"sort_order">> => 0
        },
        #{
            <<"review_id">> => ?RV_REVIEW_PUB,
            <<"attachment_id">> => 995602,
            <<"kind">> => <<"feedback_image">>,
            <<"sort_order">> => 1
        }
    ].
