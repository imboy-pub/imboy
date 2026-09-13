-module(moya_learner_bind_handler).
%%%
% 墨芽学员账号绑定 HTTP 适配层（Step 16：管理侧最小动作，计划 §6.4）
% POST /api/v1/moya/learners/:id/bind     绑定学员档案与 IMBoy 账号
% POST /api/v1/moya/learners/:id/unbind   解绑（仅清 user_id，零删除）
%
% 守卫在 logic/repo 层（Org owner 或班 class manager，deny-by-default）。
% 错误映射（本 handler 内联；错误码 5427-5429 见 error_code.hrl，不复用 5300-5399）：
%   not_authorized       → HTTP 403 + 5429（管理动作守卫，域码信息量大于通用 403）
%   learner_not_found    → HTTP 404 + 404（资源不存在，通用码）
%   learner_inactive     → HTTP 404 + 404（学员档案停用=目标资源当前不可用，与
%                          learner_not_found 同族；不新增域码）
%   duplicate_bind_in_org→ HTTP 409 + 5427（状态冲突 + 域码精确提示）
%   invalid_target_user  → HTTP 422 + 5428（域业务错误：目标账号不存在/不可绑定，
%                          与通用参数格式错 422 区分——handoff #3 定夺，理由：
%                          5428 已预定义且客户端需精确提示"换目标账号"而非"改参数"）
%   not_bound            → HTTP 409 + 409（解绑时无绑定，通用冲突码）
%   db_error             → HTTP 500 + 1（通用错误）
%%%

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(bind, Req, State) -> bind(Req, State);
handle_action(unbind, Req, State) -> unbind(Req, State);
handle_action(false, Req, _State) -> Req.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec bind(cowboy_req:req(), map()) -> cowboy_req:req().
bind(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_learner_id(Req0) of
        {ok, LearnerId} ->
            case target_uid(elib_param:post(Req0)) of
                {ok, TargetUid} ->
                    case moya_learner_bind_logic:bind_learner(Uid, LearnerId, TargetUid) of
                        {ok, Row} ->
                            elib_response:success_rfc3339(Req0, tsid_strings(Row), <<"绑定成功"/utf8>>);
                        {error, Reason} ->
                            error_response(Req0, Reason)
                    end;
                error ->
                    elib_response:error_with_status(
                        Req0,
                        422,
                        <<"缺少目标账号 user_id"/utf8>>,
                        ?ERR_MISSING_PARAM
                    )
            end;
        error ->
            elib_response:error_with_status(
                Req0,
                422,
                <<"学员ID必填"/utf8>>,
                ?ERR_MISSING_PARAM
            )
    end.

-spec unbind(cowboy_req:req(), map()) -> cowboy_req:req().
unbind(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_learner_id(Req0) of
        {ok, LearnerId} ->
            case moya_learner_bind_logic:unbind_learner(Uid, LearnerId) of
                {ok, Row} ->
                    elib_response:success_rfc3339(Req0, tsid_strings(Row), <<"解绑成功"/utf8>>);
                {error, Reason} ->
                    error_response(Req0, Reason)
            end;
        error ->
            elib_response:error_with_status(
                Req0,
                422,
                <<"学员ID必填"/utf8>>,
                ?ERR_MISSING_PARAM
            )
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec path_learner_id(cowboy_req:req()) -> {ok, integer()} | error.
path_learner_id(Req) ->
    case cowboy_req:binding(id, Req) of
        undefined ->
            error;
        Bin when is_binary(Bin) ->
            try binary_to_integer(Bin) of
                Int when Int > 0 -> {ok, Int};
                _ -> error
            catch
                _:_ -> error
            end
    end.

%% 目标账号：body.user_id（兼容整数与数字字符串两种 JSON 形态）
-spec target_uid(map() | list()) -> {ok, integer()} | error.
target_uid(Body) when is_map(Body) ->
    case maps:get(<<"user_id">>, Body, undefined) of
        N when is_integer(N), N > 0 -> {ok, N};
        B when is_binary(B) ->
            try binary_to_integer(B) of
                N when N > 0 -> {ok, N};
                _ -> error
            catch
                _:_ -> error
            end;
        _ ->
            error
    end;
target_uid(_) ->
    error.

%% 契约硬规则1（STEP-04）：64-bit ID 字段 JSON 一律 string（防 JS 精度丢失）。
%% repo 行回显经此归一：id / organization_id / user_id / account_bound_by。
%% user_id=null（未绑定）保持 null。
-spec tsid_strings(map()) -> map().
tsid_strings(Row) when is_map(Row) ->
    lists:foldl(
        fun(K, Acc) ->
            case maps:get(K, Acc, undefined) of
                N when is_integer(N) ->
                    Acc#{K => integer_to_binary(N)};
                _ ->
                    Acc
            end
        end,
        Row,
        [<<"id">>, <<"organization_id">>, <<"user_id">>, <<"account_bound_by">>]
    ).

-spec error_response(cowboy_req:req(), atom()) -> cowboy_req:req().
error_response(Req, not_authorized) ->
    elib_response:error_with_status(
        Req,
        403,
        <<"无学员绑定操作权限"/utf8>>,
        ?ERR_TEACHING_BIND_NOT_AUTHORIZED
    );
error_response(Req, learner_not_found) ->
    elib_response:error_with_status(Req, 404, <<"学员不存在"/utf8>>, ?ERR_NOT_FOUND);
error_response(Req, learner_inactive) ->
    elib_response:error_with_status(Req, 404, <<"学员档案已停用"/utf8>>, ?ERR_NOT_FOUND);
error_response(Req, duplicate_bind_in_org) ->
    elib_response:error_with_status(
        Req,
        409,
        <<"该账号在同机构已绑定其他学员"/utf8>>,
        ?ERR_TEACHING_BIND_DUPLICATE_IN_ORG
    );
error_response(Req, invalid_target_user) ->
    elib_response:error_with_status(
        Req,
        422,
        <<"目标账号不存在或不可用"/utf8>>,
        ?ERR_TEACHING_BIND_INVALID_TARGET
    );
error_response(Req, not_bound) ->
    elib_response:error_with_status(Req, 409, <<"该学员当前未绑定账号"/utf8>>, ?ERR_CONFLICT);
error_response(Req, _Other) ->
    elib_response:error_with_status(Req, 500, <<"操作失败，请重试"/utf8>>, ?ERR_ERROR).
