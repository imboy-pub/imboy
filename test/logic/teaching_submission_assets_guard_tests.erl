%% teaching_submission_assets_guard_tests
%% W2-A2-HARDEN（CM-加固）：家长提交附件守卫对齐 review 侧口径 +
%% 请求形状去重（attachment_id / {kind, sort_order}）。
%%
%% 背景（A2 Wave 1 审计疑点 1）：teaching_submission_repo:validate_assets/2
%% 此前只校验「存在 + creator」，未强制 scope='teaching' / status>=0 /
%% MIME↔kind——私聊附件（scope=private）或软删附件可绑入教学提交并经
%% submission DTO + view_url 变成本班 staff 可读。review 侧
%% validate_assets_tx（teaching_review_repo）三种校验俱全，此处补齐对称。

-module(teaching_submission_assets_guard_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 985001).
-define(ATT_VIDEO, 985101).
-define(ATT_PHOTO_A, 985102).
-define(ATT_PHOTO_B, 985103).

%%%===================================================================
%%% validate_assets/2：scope / status / MIME↔kind（repo 层，meck elib_pg）
%%%===================================================================

att_row(Id, Mime, Status, Scope) ->
    #{
        <<"id">> => Id,
        <<"mime_type">> => Mime,
        <<"status">> => Status,
        <<"creator_user_id">> => ?UID,
        <<"scope">> => Scope
    }.

meck_query(Rows) ->
    {elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, Rows} end}
    ]}.

%% 合法：teaching scope + active + video↔practice_video / image↔final_photo
validate_assets_accepts_valid_test_() ->
    ?WITH_MECKS(
        [
            meck_query([
                att_row(?ATT_VIDEO, <<"video/mp4">>, 0, <<"teaching">>),
                att_row(?ATT_PHOTO_A, <<"image/jpeg">>, 0, <<"teaching">>)
            ])
        ],
        fun() ->
            ?assertMatch(
                {ok, [_ | _]},
                teaching_submission_repo:validate_assets(?UID, [
                    {?ATT_VIDEO, <<"practice_video">>, 0},
                    {?ATT_PHOTO_A, <<"final_photo">>, 1}
                ])
            )
        end
    ).

%% scope=private（如私聊附件）拒绝——教学提交不得跨界引用非教学对象
validate_assets_rejects_private_scope_test_() ->
    ?WITH_MECKS(
        [meck_query([att_row(?ATT_VIDEO, <<"video/mp4">>, 0, <<"private">>)])],
        fun() ->
            ?assertEqual(
                {error, assets_invalid},
                teaching_submission_repo:validate_assets(?UID, [
                    {?ATT_VIDEO, <<"practice_video">>, 0}
                ])
            )
        end
    ).

%% 软删附件（status=-1）拒绝——与 view_url 授权的 att.status>=0 同口径
validate_assets_rejects_soft_deleted_test_() ->
    ?WITH_MECKS(
        [meck_query([att_row(?ATT_VIDEO, <<"video/mp4">>, -1, <<"teaching">>)])],
        fun() ->
            ?assertEqual(
                {error, assets_invalid},
                teaching_submission_repo:validate_assets(?UID, [
                    {?ATT_VIDEO, <<"practice_video">>, 0}
                ])
            )
        end
    ).

%% MIME↔kind 必须匹配：图片附件冒充 practice_video 拒绝
validate_assets_rejects_kind_mismatch_test_() ->
    ?WITH_MECKS(
        [meck_query([att_row(?ATT_PHOTO_A, <<"image/jpeg">>, 0, <<"teaching">>)])],
        fun() ->
            ?assertEqual(
                {error, assets_invalid},
                teaching_submission_repo:validate_assets(?UID, [
                    {?ATT_PHOTO_A, <<"practice_video">>, 0}
                ])
            )
        end
    ).

%% 附件缺失仍拒绝（既有语义保留；错误码族不变 → assets_invalid/5441）
validate_assets_rejects_missing_test_() ->
    ?WITH_MECKS(
        [meck_query([att_row(?ATT_PHOTO_A, <<"image/jpeg">>, 0, <<"teaching">>)])],
        fun() ->
            ?assertEqual(
                {error, not_found},
                teaching_submission_repo:validate_assets(?UID, [
                    {?ATT_PHOTO_A, <<"final_photo">>, 0},
                    {?ATT_PHOTO_B, <<"final_photo">>, 1}
                ])
            )
        end
    ).

%%%===================================================================
%%% normalize_assets/1：请求形状去重（assignment logic 纯函数）
%%%===================================================================

valid_body() ->
    #{
        <<"assets">> => [
            #{
                <<"attachment_id">> => integer_to_binary(?ATT_VIDEO),
                <<"kind">> => <<"practice_video">>
            },
            #{
                <<"attachment_id">> => integer_to_binary(?ATT_PHOTO_A),
                <<"kind">> => <<"final_photo">>,
                <<"sort_order">> => 0
            },
            #{
                <<"attachment_id">> => integer_to_binary(?ATT_PHOTO_B),
                <<"kind">> => <<"final_photo">>,
                <<"sort_order">> => 1
            }
        ]
    }.

normalize_accepts_unique_test_() ->
    {ok, Assets} = teaching_assignment_logic:normalize_assets(
        maps:get(<<"assets">>, valid_body())
    ),
    [?_assertEqual(3, length(Assets))].

%% 同一 attachment 重复出现：拒绝（review 侧 assets_shape_ok 同款）
normalize_rejects_duplicate_attachment_test_() ->
    Assets = [
        #{<<"attachment_id">> => integer_to_binary(?ATT_PHOTO_A), <<"kind">> => <<"final_photo">>},
        #{
            <<"attachment_id">> => integer_to_binary(?ATT_PHOTO_A),
            <<"kind">> => <<"final_photo">>,
            <<"sort_order">> => 2
        },
        #{<<"attachment_id">> => integer_to_binary(?ATT_VIDEO), <<"kind">> => <<"practice_video">>}
    ],
    [?_assertEqual({error, invalid}, teaching_assignment_logic:normalize_assets(Assets))].

%% 同 kind + 同 sort_order 重复：拒绝（对齐 review_asset 触发器的
%% kind/order 唯一约束语义——此前两张 final_photo 均缺省 order=0 可通过）
normalize_rejects_duplicate_kind_order_test_() ->
    Assets = [
        #{<<"attachment_id">> => integer_to_binary(?ATT_PHOTO_A), <<"kind">> => <<"final_photo">>},
        #{<<"attachment_id">> => integer_to_binary(?ATT_PHOTO_B), <<"kind">> => <<"final_photo">>},
        #{<<"attachment_id">> => integer_to_binary(?ATT_VIDEO), <<"kind">> => <<"practice_video">>}
    ],
    [?_assertEqual({error, invalid}, teaching_assignment_logic:normalize_assets(Assets))].
