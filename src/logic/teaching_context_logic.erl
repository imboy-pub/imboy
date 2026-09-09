-module(teaching_context_logic).
%%%
% 墨芽教学上下文业务逻辑（contexts / context switch）
% Teaching identity contexts logic
%
% 语义（STEP-04 冻结契约 / ACL-02）：
%   - contexts/1：从 class_staff + guardian_learner + organization.owner_id
%     解析 JWT uid 的全部可用教学身份（服务端全量解析，客户端不传过滤器）
%   - switch/2：仅校验"所选上下文确实属于该 uid"并回显快照；
%     **不签发任何凭证**——业务 API 每次独立鉴权（D-04：不引入第二套鉴权）
%   - TSID JSON 表达硬约束：所有 64-bit ID 在 payload 中一律 binary 字符串
%%%

-export([contexts/1, switch/2]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 当前用户全部教学身份上下文（可能为空列表）
-spec contexts(integer()) -> {ok, map()} | {error, db_error}.
contexts(Uid) ->
    case
        {
            teaching_context_repo:guardian_contexts(Uid),
            teaching_context_repo:staff_contexts(Uid),
            teaching_context_repo:owner_contexts(Uid)
        }
    of
        {{ok, Guardians}, {ok, Staffs}, {ok, Owners}} ->
            GuardianCtxs = [guardian_context(R) || R <- Guardians],
            StaffCtxs = [staff_context(R) || R <- Staffs],
            OwnerCtxs = [owner_context(R) || R <- Owners],
            {ok, #{contexts => GuardianCtxs ++ StaffCtxs ++ OwnerCtxs}};
        _Error ->
            ?LOG_ERROR("teaching_context_logic contexts db error uid=~p", [Uid]),
            {error, db_error}
    end.

%% @doc 显式切换教学上下文：校验归属 → 回显快照（无凭证、无服务端会话态）
%% 出参 {ok, ContextMap} | {error, Reason}
%% Reason：invalid_type | missing_learner_id | missing_group_id | missing_org_id
%%         | context_mismatch（不属于当前用户，5420）| inactive（5421）| db_error
-spec switch(integer(), map()) -> {ok, map()} | {error, atom()}.
switch(Uid, #{<<"context_type">> := <<"guardian">>} = Params) ->
    switch_guardian(Uid, Params);
switch(Uid, #{<<"context_type">> := <<"teacher">>} = Params) ->
    switch_teacher(Uid, Params);
switch(Uid, #{<<"context_type">> := <<"org_owner">>} = Params) ->
    switch_owner(Uid, Params);
switch(_Uid, #{<<"context_type">> := _}) ->
    {error, invalid_type};
switch(_Uid, _) ->
    {error, invalid_type}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% ---- switch 分支 ----

-spec switch_guardian(integer(), map()) -> {ok, map()} | {error, atom()}.
switch_guardian(Uid, Params) ->
    case tsid_param(Params, <<"learner_id">>) of
        {error, missing} ->
            {error, missing_learner_id};
        {ok, LearnerId} ->
            case teaching_acl:resolve_guardian(Uid, LearnerId) of
                {ok, _Guardian} ->
                    %% 从上下文列表中回显该 learner 的快照（未入班时班级字段缺省）
                    case find_guardian_context(Uid, LearnerId) of
                        {ok, Ctx} ->
                            claims_mismatch(Params, Ctx);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, inactive} ->
                    {error, inactive};
                _ ->
                    %% 无监护关系 / can_submit 细分不适用 switch（查看权限即可切换）
                    {error, context_mismatch}
            end
    end.

-spec switch_teacher(integer(), map()) -> {ok, map()} | {error, atom()}.
switch_teacher(Uid, Params) ->
    case tsid_param(Params, <<"group_id">>) of
        {error, missing} ->
            {error, missing_group_id};
        {ok, GroupId} ->
            case teaching_acl:resolve_staff(Uid, GroupId) of
                {ok, _Staff} ->
                    case find_staff_context(Uid, GroupId) of
                        {ok, Ctx} ->
                            claims_mismatch(Params, Ctx);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, inactive} ->
                    {error, inactive};
                _ ->
                    {error, context_mismatch}
            end
    end.

-spec switch_owner(integer(), map()) -> {ok, map()} | {error, atom()}.
switch_owner(Uid, Params) ->
    case tsid_param(Params, <<"organization_id">>) of
        {error, missing} ->
            {error, missing_org_id};
        {ok, OrgId} ->
            case teaching_acl:resolve_org_owner(Uid, OrgId) of
                ok ->
                    case find_owner_context(Uid, OrgId) of
                        {ok, Ctx} ->
                            claims_mismatch(Params, Ctx);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {error, context_mismatch}
            end
    end.

%% 客户端自报 org/workspace/group 与服务端解析不一致 → 拒绝（T12：不信任声明）。
%% 只校验"上下文标识"字段（org/workspace/group）；快照内其余字段以服务端为准。
-spec claims_mismatch(map(), map()) -> {ok, map()} | {error, context_mismatch}.
claims_mismatch(Params, Ctx) ->
    Keys = [<<"organization_id">>, <<"workspace_id">>, <<"group_id">>],
    AllMatch =
        lists:all(
            fun(K) ->
                case maps:find(K, Params) of
                    {ok, Claimed} when is_binary(Claimed), Claimed =/= <<>> ->
                        case tsid_to_int(Claimed) of
                            {ok, ClaimedInt} ->
                                Actual = tsid_to_int(maps:get(K, Ctx, <<"">>)),
                                Actual =:= {ok, ClaimedInt};
                            _ ->
                                false
                        end;
                    _ ->
                        true
                end
            end,
            Keys
        ),
    case AllMatch of
        true -> {ok, Ctx};
        false -> {error, context_mismatch}
    end.

%% ---- 上下文快照查找 ----

-spec find_guardian_context(integer(), integer()) -> {ok, map()} | {error, atom()}.
find_guardian_context(Uid, LearnerId) ->
    {ok, #{contexts := Ctxs}} = contexts(Uid),
    case
        [
            C
         || C <- Ctxs,
            maps:get(<<"context_type">>, C) =:= <<"guardian">>,
            maps:get(<<"learner_id">>, C, <<>>) =:= tsid(LearnerId)
        ]
    of
        [Ctx | _] -> {ok, Ctx};
        [] -> {error, context_mismatch}
    end.

-spec find_staff_context(integer(), integer()) -> {ok, map()} | {error, atom()}.
find_staff_context(Uid, GroupId) ->
    {ok, #{contexts := Ctxs}} = contexts(Uid),
    case
        [
            C
         || C <- Ctxs,
            maps:get(<<"context_type">>, C) =:= <<"teacher">>,
            maps:get(<<"group_id">>, C, <<>>) =:= tsid(GroupId)
        ]
    of
        [Ctx | _] -> {ok, Ctx};
        [] -> {error, context_mismatch}
    end.

-spec find_owner_context(integer(), integer()) -> {ok, map()} | {error, atom()}.
find_owner_context(Uid, OrgId) ->
    {ok, #{contexts := Ctxs}} = contexts(Uid),
    case
        [
            C
         || C <- Ctxs,
            maps:get(<<"context_type">>, C) =:= <<"org_owner">>,
            maps:get(<<"organization_id">>, C, <<>>) =:= tsid(OrgId)
        ]
    of
        [Ctx | _] -> {ok, Ctx};
        [] -> {error, context_mismatch}
    end.

%% ---- 行 → 契约快照（TSID 一律字符串） ----

-spec guardian_context(map()) -> map().
guardian_context(R) ->
    #{
        <<"context_type">> => <<"guardian">>,
        <<"organization_id">> => tsid(maps:get(<<"organization_id">>, R, null)),
        <<"organization_name">> => nullabled(
            maps:get(<<"org_name">>, R, null), fun elib_cnv:safe_to_binary/1
        ),
        <<"workspace_id">> => tsid(maps:get(<<"workspace_id">>, R, null)),
        <<"workspace_name">> => nullabled(
            maps:get(<<"workspace_name">>, R, null), fun elib_cnv:safe_to_binary/1
        ),
        <<"group_id">> => tsid(maps:get(<<"group_id">>, R, null)),
        <<"group_name">> => nullabled(
            maps:get(<<"group_title">>, R, null), fun elib_cnv:safe_to_binary/1
        ),
        <<"learner_id">> => tsid(maps:get(<<"learner_id">>, R)),
        <<"learner_display_name">> => elib_cnv:safe_to_binary(
            maps:get(<<"display_name">>, R, <<>>)
        ),
        <<"can_submit">> => maps:get(<<"can_submit">>, R, false) =:= true,
        <<"can_view_review">> => maps:get(<<"can_view_review">>, R, false) =:= true
    }.

-spec staff_context(map()) -> map().
staff_context(R) ->
    #{
        <<"context_type">> => <<"teacher">>,
        <<"organization_id">> => tsid(maps:get(<<"org_id">>, R)),
        <<"organization_name">> => elib_cnv:safe_to_binary(maps:get(<<"org_name">>, R, <<>>)),
        <<"workspace_id">> => tsid(maps:get(<<"workspace_id">>, R)),
        <<"workspace_name">> => elib_cnv:safe_to_binary(maps:get(<<"workspace_name">>, R, <<>>)),
        <<"group_id">> => tsid(maps:get(<<"group_id">>, R)),
        <<"group_name">> => elib_cnv:safe_to_binary(maps:get(<<"group_title">>, R, <<>>)),
        <<"role">> => elib_cnv:safe_to_binary(maps:get(<<"role">>, R, <<>>))
    }.

-spec owner_context(map()) -> map().
owner_context(R) ->
    #{
        <<"context_type">> => <<"org_owner">>,
        <<"organization_id">> => tsid(maps:get(<<"org_id">>, R)),
        <<"organization_name">> => elib_cnv:safe_to_binary(maps:get(<<"org_name">>, R, <<>>))
    }.

%% ---- 小工具 ----

%% TSID integer → JSON 字符串（防 JS 2^53 精度丢失，API-01）
-spec tsid(integer() | null | undefined) -> binary().
tsid(Id) when is_integer(Id) ->
    integer_to_binary(Id);
tsid(_) ->
    <<"">>.

-spec nullabled(null | undefined | term(), fun((term()) -> binary())) -> binary().
nullabled(V, _F) when V =:= null; V =:= undefined ->
    <<"">>;
nullabled(V, F) ->
    F(V).

-spec tsid_param(map(), binary()) -> {ok, integer()} | {error, missing}.
tsid_param(Params, Key) ->
    case maps:find(Key, Params) of
        {ok, V} ->
            case tsid_to_int(elib_cnv:safe_to_binary(V)) of
                {ok, Int} -> {ok, Int};
                _ -> {error, missing}
            end;
        _ ->
            {error, missing}
    end.

-spec tsid_to_int(binary()) -> {ok, integer()} | {error, badarg}.
tsid_to_int(Bin) when is_binary(Bin) ->
    try binary_to_integer(Bin) of
        Int when is_integer(Int), Int > 0 -> {ok, Int};
        _ -> {error, badarg}
    catch
        _:_ -> {error, badarg}
    end;
tsid_to_int(_) ->
    {error, badarg}.
