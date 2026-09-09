%% teaching_attach_logic_tests
%% Step 10 MEDIA-01（授权 wiring 矩阵）/ MEDIA-02（清理不误删）/ 校验白名单。
%% submission_access 的完整 ACL 矩阵（跨 Org/跨 learner/仅群管理员/仅 Owner）
%% 已在 teaching_acl_tests（Step 8）覆盖；本套件验证 attach 集成层 wiring
%% 与白名单/上限/时长/清理逻辑。

-module(teaching_attach_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(MB, 1024 * 1024).
-define(PARENT, 980002).
-define(TEACHER, 980001).

%%%===================================================================
%%% can_upload/1（教学身份守卫）
%%%===================================================================

can_upload_guardian_test_() ->
    ?WITH_MECKS([attach_mocks(guardian)], fun() ->
        ?assertEqual(ok, teaching_attach_logic:can_upload(?PARENT))
    end).

can_upload_staff_only_test_() ->
    ?WITH_MECKS([attach_mocks(staff)], fun() ->
        ?assertEqual(ok, teaching_attach_logic:can_upload(?TEACHER))
    end).

can_upload_no_identity_test_() ->
    ?WITH_MECKS([attach_mocks(none)], fun() ->
        ?assertEqual(false, teaching_attach_logic:can_upload(980099))
    end).

can_upload_db_error_fail_closed_test_() ->
    ?WITH_MECKS([attach_mocks(db_error)], fun() ->
        ?assertEqual(false, teaching_attach_logic:can_upload(?PARENT))
    end).

%%%===================================================================
%%% check_mime/1 + verify_upload/3（白名单/大小/时长）
%%%===================================================================

mime_whitelist_test_() ->
    ?WITH_MECKS([env_mocks()], fun() ->
        lists:foreach(
            fun(M) -> ?assertEqual(ok, teaching_attach_logic:check_mime(M)) end,
            [<<"video/mp4">>, <<"video/quicktime">>, <<"image/jpeg">>, <<"image/png">>]
        ),
        lists:foreach(
            fun(M) ->
                ?assertEqual({error, invalid_file_type}, teaching_attach_logic:check_mime(M))
            end,
            [
                <<"video/x-msvideo">>,
                <<"image/webp">>,
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
            teaching_attach_logic:verify_upload(
                <<"video/mp4">>, 100 * ?MB, #{<<"duration">> => 60}
            )
        ),
        ?assertEqual(
            {error, file_too_large},
            teaching_attach_logic:verify_upload(
                <<"video/mp4">>, 100 * ?MB + 1, #{<<"duration">> => 30}
            )
        ),
        ?assertEqual(
            {error, invalid_file_type},
            teaching_attach_logic:verify_upload(
                <<"video/quicktime">>, 10 * ?MB, #{<<"duration">> => 61}
            )
        ),
        %% duration 未上报：首版放行（服务端抽帧复核留 Step 11）
        ?assertEqual(
            ok,
            teaching_attach_logic:verify_upload(
                <<"video/mp4">>, 10 * ?MB, #{}
            )
        ),
        %% duration_seconds 别名同样受校验
        ?assertEqual(
            {error, invalid_file_type},
            teaching_attach_logic:verify_upload(
                <<"video/mp4">>, 10 * ?MB, #{<<"duration_seconds">> => 90}
            )
        )
    end).

photo_size_test_() ->
    ?WITH_MECKS([env_mocks()], fun() ->
        ?assertEqual(ok, teaching_attach_logic:verify_upload(<<"image/jpeg">>, 20 * ?MB, #{})),
        ?assertEqual(
            {error, file_too_large},
            teaching_attach_logic:verify_upload(
                <<"image/png">>, 20 * ?MB + 1, #{}
            )
        )
    end).

%%%===================================================================
%%% MEDIA-01：authorize/2 授权 wiring（矩阵经 meck 注入 submission_access 结果）
%%%===================================================================

media01_guardian_view_ok_test_() ->
    ?WITH_MECKS(media01_mocks(<<"submitted">>, {ok, guardian, scope1()}), fun() ->
        ?assertEqual(true, teaching_attach_logic:authorize(?PARENT, att_rec()))
    end).

media01_staff_view_ok_test_() ->
    ?WITH_MECKS(media01_mocks(<<"submitted">>, {ok, staff, scope1()}), fun() ->
        ?assertEqual(true, teaching_attach_logic:authorize(?TEACHER, att_rec()))
    end).

%% 未授权家长（can_view_review=false）/ 同班其他成员 / 非任课老师 / 仅 Owner：
%% submission_access 全部 {error, _} → view_url 拒绝（Step 8 矩阵的 wiring 端）
media01_forbidden_test_() ->
    lists:map(
        fun(AccessErr) ->
            ?WITH_MECKS(media01_mocks(<<"submitted">>, {error, AccessErr}), fun() ->
                ?assertEqual(false, teaching_attach_logic:authorize(980099, att_rec()))
            end)
        end,
        [forbidden, forbidden, owner_not_granted, not_found]
    ).

%% 未绑定 submission 的附件（上传后未提交）：任何身份都拿不到读 URL
media01_unbound_denied_test_() ->
    ?WITH_MECKS(media01_mocks(undefined, {ok, guardian, scope1()}), fun() ->
        ?assertEqual(false, teaching_attach_logic:authorize(?PARENT, att_rec()))
    end).

%% 已撤回 submission：证据仅审计路径可及，日常 view_url 拒绝（T17）
media01_withdrawn_denied_test_() ->
    ?WITH_MECKS(media01_mocks(<<"withdrawn">>, {ok, guardian, scope1()}), fun() ->
        ?assertEqual(false, teaching_attach_logic:authorize(?PARENT, att_rec()))
    end).

%%%===================================================================
%%% MEDIA-02：cleanup_unbound 不误删
%%%===================================================================

media02_cleanup_deletes_orphan_test_() ->
    ?WITH_MECKS(
        [
            {teaching_submission_repo, [
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
                teaching_attach_logic:cleanup_unbound(24)
            )
        end
    ).

%% 删对象失败：保留行（下轮重试），绝不先删行留孤儿对象
media02_cleanup_object_error_keeps_row_test_() ->
    ?WITH_MECKS(
        [
            {teaching_submission_repo, [
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
                teaching_attach_logic:cleanup_unbound(24)
            )
        end
    ).

media02_cleanup_empty_test_() ->
    ?WITH_MECKS(
        [
            {teaching_submission_repo, [
                {'unbound_teaching_attachments', 1, fun(_) -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{cleaned => 0, errors => 0}},
                teaching_attach_logic:cleanup_unbound(24)
            )
        end
    ).

%%%===================================================================
%%% MEDIA-03：业务表与日志不得持久化 presigned URL（源码扫描断言）
%%%===================================================================

media03_no_presigned_url_persisted_test_() ->
    %% repo（唯一 DB 写入层）不得出现任何 presign 调用/签名串
    RepoFiles = [
        "src/repo/teaching_submission_repo.erl",
        "src/repo/teaching_review_repo.erl",
        "src/repo/teaching_context_repo.erl"
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
    {teaching_context_repo, ctx_mocks([g1], [])};
attach_mocks(staff) ->
    {teaching_context_repo, ctx_mocks([], [s1])};
attach_mocks(none) ->
    {teaching_context_repo, ctx_mocks([], [])};
attach_mocks(db_error) ->
    {teaching_context_repo, [
        {'guardian_contexts', 1, fun(_) -> {error, db_down} end},
        {'staff_contexts', 1, fun(_) -> {error, db_down} end}
    ]}.

ctx_mocks(G, S) ->
    [
        {'guardian_contexts', 1, fun(_) -> {ok, G} end},
        {'staff_contexts', 1, fun(_) -> {ok, S} end}
    ].

%% media01 mocks：附件路径 → submission 绑定关系 + submission_access 注入结果
media01_mocks(SubStatus, AccessResult) ->
    [
        {teaching_submission_repo, [
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
        {teaching_acl, [
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
