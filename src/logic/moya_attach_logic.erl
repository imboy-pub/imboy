-module(moya_attach_logic).
-moduledoc "墨芽教学附件授权/校验/生命周期（Step 10）。".
%%%
% 墨芽教学附件授权/校验/生命周期（Step 10）
% Teaching attachment scope: upload guard, MIME/size/duration whitelist,
% view_url read ACL, orphan cleanup
%
% scope 定名 teaching（STEP-17-PREP gap#2；moya 侧 TEACHING_SCOPE 单点常量）。
% 集成点（attach_logic.erl 四处钩子，均最小侵入）：
%   ① can_upload/3 加 <<"teaching">> 子句 → can_upload/1（任一有效教学身份）
%   ② authorize/3 加 <<"teaching">> 子句 → authorize/2（绑定 submission 后
%      走 moya_acl:submission_access；未绑定/已撤回一律拒绝）
%   ③ verify_and_save 教学白名单/大小/时长复核（HEAD 真实值 + 客户端时长上报）
%   ④ confirm 响应补 attachment_id（字符串，TSID 契约；保留 object_key 兼容）
%
% MEDIA-03：本模块与 attach 均不持久化 presigned URL（attachment.url 存 ObjectKey，
% 见 attach_logic do_save_1；集成测试断言）。
%%%

-export([
    can_upload/1,
    check_mime/1,
    verify_upload/3,
    authorize/2,
    list_unbound/1,
    cleanup_unbound/1,
    run_unbound_cleanup/0
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 教学媒体白名单（首版；iPhone 原录 .mov=video/quicktime 必须放行）
-define(TEACHING_VIDEO_MIMES, [<<"video/mp4">>, <<"video/quicktime">>]).
-define(TEACHING_VIDEO_EXTS, [<<"mp4">>, <<"mov">>]).
%% webp：2026-09-12 moya 报障收口——部分安卓机型 chooseMedia 压缩产物为 image/webp，
%% presign 阶段即拒会导致提交作业直接失败（elib_oss 全局层早已放行 webp，此层收紧过度）
-define(TEACHING_PHOTO_MIMES, [<<"image/jpeg">>, <<"image/png">>, <<"image/webp">>]).
-define(TEACHING_PHOTO_EXTS, [<<"jpg">>, <<"jpeg">>, <<"png">>, <<"webp">>]).
%% 60s 视频 ≈ 100MB 上限；照片 20MB；时长 ≤60s（客户端上报，服务端抽帧复核留 Step 11）
-define(DEFAULT_VIDEO_MAX_MB, 100).
-define(DEFAULT_PHOTO_MAX_MB, 20).
-define(DEFAULT_VIDEO_MAX_DURATION, 60).
%% 未绑定 submission 的教学附件回收下限（小时），防误删刚 confirm 的附件
-define(MIN_UNBOUND_AGE_HOURS, 2).
-define(DEFAULT_UNBOUND_AGE_HOURS, 24).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 教学上传权（presign/confirm 共用，deny-by-default）：
%% 上传人至少持有一个有效教学身份（active guardian 或 active staff）。
%% 强归属在 submission 创建时由 validate_assets（creator_user_id == 提交人）把关。
-spec can_upload(integer()) -> ok | false.
can_upload(Uid) ->
    G =
        try
            moya_context_repo:guardian_contexts(Uid)
        catch
            _:_ -> error
        end,
    S =
        try
            moya_context_repo:staff_contexts(Uid)
        catch
            _:_ -> error
        end,
    case {G, S} of
        {{ok, []}, {ok, []}} ->
            false;
        {{ok, _}, _} ->
            ok;
        {_, {ok, _}} ->
            ok;
        _ ->
            %% 查询异常：fail-closed
            false
    end.

%% @doc presign 阶段的客户端声明 MIME 预检（confirm 阶段以 HEAD 真实值复核为准）
-spec check_mime(binary()) -> ok | {error, invalid_file_type}.
check_mime(MimeType) ->
    case classify_mime(MimeType) of
        undefined -> {error, invalid_file_type};
        _ -> ok
    end.

%% @doc confirm 阶段复核（HEAD 真实 size/mime + 客户端 duration 上报 + 扩展名）：
%%   video → mp4/mov ≤100MB、duration ≤60s（上报存在才校验）
%%   photo → jpeg/png ≤20MB
%% SizeType 由 attach_logic:verify_and_save 在通用校验后调用；失败时调用方删对象。
%% 错误复用既有 handler 映射：file_too_large（超大小）/ invalid_file_type（类型/时长不合规）。
-spec verify_upload(binary(), non_neg_integer(), map()) ->
    ok | {error, file_too_large | invalid_file_type}.
verify_upload(MimeType, Size, Meta) ->
    case classify_mime(MimeType) of
        video ->
            check_video(Size, Meta);
        photo ->
            check_photo(Size);
        undefined ->
            {error, invalid_file_type}
    end.

%% @doc 教学附件读授权（view_url 每次签发前调用）：
%%   1. 附件必须已绑定 submission（submission_asset）；未绑定 → 拒绝
%%      （上传后未提交的素材不经任何 URL 外泄）
%%   2. submission 已撤回 → 拒绝（T17：撤回证据仅审计路径可及，不在日常 API 面）
%%   3. moya_acl:submission_access（guardian 需 can_view_review / 本班 staff；
%%      Owner 不因身份获得——MEDIA-01 矩阵）
%%   P0-4（MN-MEDIA-02）分流：submission_asset 未绑定时再查 review_asset 绑定——
%%   * draft：仅创建该草稿的 reviewer 本人可读（其他老师/家长/Owner 全拒绝）
%%   * published：复用 submission_access（本班 active staff / 对该 learner
%%     can_view_review=true 且 active 的 guardian；跨班/跨机构 fail closed）
%%   * withdrawn submission / discarded review / 未绑定 / 任一查询失败 → fail closed
-spec authorize(integer(), map()) -> boolean().
authorize(Uid, #{<<"path">> := Path}) when is_binary(Path), Path =/= <<>> ->
    case moya_submission_repo:submission_for_asset_path(Path) of
        {ok, #{<<"submission_id">> := Sid, <<"submission_status">> := <<"submitted">>}} ->
            case moya_acl:submission_access(Uid, Sid) of
                {ok, _, _} -> true;
                _ -> false
            end;
        {ok, undefined} ->
            authorize_review_asset(Uid, Path);
        {ok, _} ->
            %% 已绑 submission 但 withdrawn：撤回证据仅审计路径可及（T17）
            false;
        {error, _} ->
            %% submission 维度查询异常：fail closed
            false
    end;
authorize(_Uid, _Rec) ->
    false.

%% review_asset 维度读授权（P0-4）：
%% draft 仅创建该草稿的 reviewer；published 复用 submission_access；
%% withdrawn / discarded / 未绑定 / 查询失败一律 false
-spec authorize_review_asset(integer(), binary()) -> boolean().
authorize_review_asset(Uid, Path) ->
    case moya_review_repo:review_for_asset_path(Path) of
        {ok, #{
            <<"review_status">> := <<"draft">>,
            <<"reviewer_uid">> := Uid,
            <<"submission_status">> := <<"submitted">>
        }} ->
            true;
        {ok, #{
            <<"review_status">> := <<"published">>,
            <<"submission_id">> := Sid,
            <<"submission_status">> := <<"submitted">>
        }} ->
            case moya_acl:submission_access(Uid, Sid) of
                {ok, _, _} -> true;
                _ -> false
            end;
        _ ->
            %% withdrawn submission / discarded / 未绑定 / 查询失败：fail closed
            false
    end.

%% @doc 列出超龄未绑定的教学附件（double NOT EXISTS：submission_asset ∪
%% review_asset 任一引用即豁免——含草稿引用与撤回 submission 的证据附件，
%% MEDIA-02 不误删；SQL 见 moya_submission_repo:unbound_run）
-spec list_unbound(integer()) -> {ok, [map()]} | {error, term()}.
list_unbound(AgeHours) ->
    moya_submission_repo:unbound_teaching_attachments(max(?MIN_UNBOUND_AGE_HOURS, AgeHours)).

%% @doc 清理未绑定教学孤儿附件：先删对象再软删行（status=-1）。
%% 供 ecron 定时接线（首版交付函数+测试，定时接线遗留，见 STEP-10/notes.md）。
-spec cleanup_unbound(integer()) -> {ok, #{cleaned => integer(), errors => integer()}}.
cleanup_unbound(AgeHours) ->
    case list_unbound(AgeHours) of
        {ok, []} ->
            {ok, #{cleaned => 0, errors => 0}};
        {ok, Rows} ->
            {Cleaned, Errors} = lists:foldl(
                fun(Row, {C, E}) ->
                    Key = maps:get(<<"path">>, Row, <<>>),
                    Id = maps:get(<<"id">>, Row, 0),
                    case elib_oss:delete_object(Key) of
                        ok ->
                            _ =
                                (try
                                    attachment_ds:soft_delete(Id)
                                catch
                                    _:_ -> ok
                                end),
                            {C + 1, E};
                        _ ->
                            %% 删对象失败：保留行（下轮重试），绝不先删行留对象
                            {C, E + 1}
                    end
                end,
                {0, 0},
                Rows
            ),
            ?INFO_LOG([
                "moya_attach_logic cleanup_unbound done",
                {age_hours, AgeHours},
                {cleaned, Cleaned},
                {errors, Errors}
            ]),
            {ok, #{cleaned => Cleaned, errors => Errors}};
        {error, Reason} ->
            ?ERROR_LOG(["moya_attach_logic cleanup_unbound list failed: ", Reason]),
            {ok, #{cleaned => 0, errors => 0}}
    end.

%% @doc ecron 入口（Step 10 遗留接线，Step 11 批次收口）：
%% 阈值走 config teaching_unbound_cleanup_age_hours（默认 24，下限 2 在
%% cleanup_unbound 内强制）
-spec run_unbound_cleanup() -> ok.
run_unbound_cleanup() ->
    AgeHours = config_ds:env(teaching_unbound_cleanup_age_hours, 24),
    _ = cleanup_unbound(AgeHours),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec classify_mime(binary()) -> video | photo | undefined.
classify_mime(<<"video/", _/binary>> = M) ->
    case lists:member(M, ?TEACHING_VIDEO_MIMES) of
        true -> video;
        false -> undefined
    end;
classify_mime(<<"image/", _/binary>> = M) ->
    case lists:member(M, ?TEACHING_PHOTO_MIMES) of
        true -> photo;
        false -> undefined
    end;
classify_mime(_) ->
    undefined.

-spec check_video(non_neg_integer(), map()) ->
    ok | {error, file_too_large | invalid_file_type}.
check_video(Size, Meta) ->
    MaxBytes = config_ds:env(teaching_video_max_mb, ?DEFAULT_VIDEO_MAX_MB) * 1024 * 1024,
    case Size =< MaxBytes of
        false ->
            {error, file_too_large};
        true ->
            check_duration(Meta)
    end.

-spec check_duration(map()) -> ok | {error, invalid_file_type}.
check_duration(Meta) ->
    MaxDur = config_ds:env(teaching_video_max_duration, ?DEFAULT_VIDEO_MAX_DURATION),
    case reported_duration(Meta) of
        undefined ->
            %% 客户端未上报：放行，服务端抽帧复核在 Step 11（任务书边界）
            ok;
        Dur when is_number(Dur), Dur >= 0, Dur =< MaxDur ->
            ok;
        _ ->
            {error, invalid_file_type}
    end.

%% 客户端上报字段：duration（秒，number）或 duration_seconds
-spec reported_duration(map()) -> number() | undefined.
reported_duration(Meta) ->
    case maps:get(<<"duration">>, Meta, undefined) of
        D when is_number(D) -> D;
        _ ->
            case maps:get(<<"duration_seconds">>, Meta, undefined) of
                D2 when is_number(D2) -> D2;
                _ -> undefined
            end
    end.

-spec check_photo(non_neg_integer()) -> ok | {error, file_too_large}.
check_photo(Size) ->
    MaxBytes = config_ds:env(teaching_photo_max_mb, ?DEFAULT_PHOTO_MAX_MB) * 1024 * 1024,
    case Size =< MaxBytes of
        true -> ok;
        false -> {error, file_too_large}
    end.
