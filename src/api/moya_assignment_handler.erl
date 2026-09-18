-module(moya_assignment_handler).
%%%
% 墨芽家长侧 HTTP 适配层（Step 9）
% GET  /api/v1/moya/assignments                家长作业列表
% GET  /api/v1/moya/assignments/:id            作业详情
% POST /api/v1/moya/assignments/:id/submissions 幂等创建提交
% GET  /api/v1/moya/submissions/:id            提交详情（视角感知）
% POST /api/v1/moya/submissions/:id/withdraw    撤回
% GET  /api/v1/moya/learners/:id/history        学员历史
% GET  /api/v1/moya/learners/:id/history/unread-count  未读点评数（家长角标）
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
handle_action(list, Req, State) -> list_assignments(Req, State);
handle_action(detail, Req, State) -> assignment_detail(Req, State);
handle_action(create_submission, Req, State) -> create_submission(Req, State);
handle_action(submission_detail, Req, State) -> submission_detail(Req, State);
handle_action(withdraw, Req, State) -> withdraw(Req, State);
handle_action(history, Req, State) -> history(Req, State);
handle_action(history_unread_count, Req, State) -> history_unread_count(Req, State);
handle_action(false, Req, _State) -> Req.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec list_assignments(cowboy_req:req(), map()) -> cowboy_req:req().
list_assignments(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    case {elib_tsid:from_binary(proplists:get_value(<<"learner_id">>, Qs)), status_param(Qs)} of
        {{ok, LearnerId}, {ok, Status}} ->
            Page = elib_param:page_qs(Qs, 20),
            case moya_assignment_logic:list(Uid, LearnerId, Page, Status) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload);
                {error, Reason} ->
                    moya_error:to_response(Req0, Reason)
            end;
        {_, {error, _}} ->
            %% CM-F4：非法 status 一律 422 拒绝（deny-by-default，不下发查询）
            elib_response:error(Req0, <<"状态过滤参数非法"/utf8>>, ?ERR_PARAM_INVALID);
        _ ->
            elib_response:error(Req0, <<"缺少学员ID"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec assignment_detail(cowboy_req:req(), map()) -> cowboy_req:req().
assignment_detail(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, AssignmentId} ->
            case moya_assignment_logic:detail(Uid, AssignmentId) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload);
                {error, Reason} ->
                    moya_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec create_submission(cowboy_req:req(), map()) -> cowboy_req:req().
create_submission(Req0, State) ->
    Uid = maps:get(current_uid, State),
    IdemKey = cowboy_req:header(<<"idempotency-key">>, Req0, <<>>),
    case {path_id(Req0), IdemKey} of
        {{ok, AssignmentId}, Key} when is_binary(Key), byte_size(Key) >= 8, byte_size(Key) =< 128 ->
            Body = elib_param:post(Req0),
            case moya_assignment_logic:create_submission(Uid, AssignmentId, Key, Body) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload, <<"提交成功"/utf8>>);
                {error, Reason} ->
                    moya_error:to_response(Req0, Reason)
            end;
        {{ok, _}, _} ->
            elib_response:error(Req0, <<"缺少幂等键"/utf8>>, ?ERR_IDEMPOTENCY_KEY_REQUIRED);
        _ ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec submission_detail(cowboy_req:req(), map()) -> cowboy_req:req().
submission_detail(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, SubmissionId} ->
            case moya_review_logic:submission_detail(Uid, SubmissionId) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload);
                {error, Reason} ->
                    moya_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"提交ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec withdraw(cowboy_req:req(), map()) -> cowboy_req:req().
withdraw(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, SubmissionId} ->
            case moya_review_logic:withdraw(Uid, SubmissionId) of
                {ok, withdrawn} ->
                    %% CM-F1：契约必填 submission_id（TSID string；MN-WITHDRAW-01
                    %% 冻结契约 WithdrawnSubmission——moya tsidOrThrow 缺字段必抛错）
                    elib_response:success_rfc3339(
                        Req0,
                        #{
                            <<"submission_id">> => integer_to_binary(SubmissionId),
                            <<"status">> => <<"withdrawn">>
                        },
                        <<"已撤回"/utf8>>
                    );
                {error, Reason} ->
                    moya_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"提交ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec history(cowboy_req:req(), map()) -> cowboy_req:req().
history(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, LearnerId} ->
            Page = elib_param:page_qs(cowboy_req:parse_qs(Req0), 20),
            case moya_review_logic:history(Uid, LearnerId, Page) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload);
                {error, Reason} ->
                    moya_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"学员ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec history_unread_count(cowboy_req:req(), map()) -> cowboy_req:req().
history_unread_count(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, LearnerId} ->
            case since_param(cowboy_req:parse_qs(Req0)) of
                {ok, Since} ->
                    case moya_review_logic:history_unread_count(Uid, LearnerId, Since) of
                        {ok, Payload} ->
                            elib_response:success_rfc3339(Req0, Payload);
                        {error, Reason} ->
                            moya_error:to_response(Req0, Reason)
                    end;
                error ->
                    %% A1-D08：非法 since 一律 422 拒绝（deny-by-default，不下发
                    %% logic——脏串交给 PG ::timestamptz 会被 epgsql rfc3339 codec
                    %% 退化 epoch → 恒真 → 计数膨胀；口径照抄 review-queue 时间
                    %% 参数 deny 先例）
                    elib_response:error(Req0, <<"未读计数时间参数非法"/utf8>>, ?ERR_PARAM_INVALID)
            end;
        _ ->
            elib_response:error(Req0, <<"学员ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec path_id(cowboy_req:req()) -> {ok, integer()} | error.
path_id(Req) ->
    case cowboy_req:binding(id, Req) of
        undefined -> error;
        Bin when is_binary(Bin) -> elib_tsid:from_binary(Bin)
    end.

%% since：上次看到的 published_at（RFC3339，客户端原样回传）；缺省计全部。
%% A1-D08：加格式白名单（正则口径照抄 review-queue 的 ?TIME_PARAM_RE）：
%% `YYYY-MM-DD` 或带时间 RFC3339（可选小数秒/时区）放行；非法串 → error
%% （handler 422，绝不把脏串交给 PG 的 ::timestamptz）。
-define(TIME_PARAM_RE,
    <<"^\\d{4}-\\d{2}-\\d{2}(T\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?(Z|[+-]\\d{2}:\\d{2})?)?$">>
).

-spec since_param(list()) -> {ok, binary() | undefined} | error.
since_param(Qs) ->
    case proplists:get_value(<<"since">>, Qs) of
        V when is_binary(V), byte_size(V) > 0 ->
            case re:run(V, ?TIME_PARAM_RE, [{capture, none}]) of
                match -> {ok, V};
                nomatch -> error
            end;
        _ ->
            {ok, undefined}
    end.

%% CM-F4：status 过滤四态白名单（与 moya parent-api.ts AssignmentStatus 对齐）。
%% 缺省 → undefined（不过滤）；非法值 → error（handler 422 拒绝）。
-define(ASSIGNMENT_STATUS_FILTERS, [
    <<"pending">>, <<"submitted">>, <<"reviewing">>, <<"reviewed">>
]).

-spec status_param(list()) -> {ok, binary() | undefined} | error.
status_param(Qs) ->
    case proplists:get_value(<<"status">>, Qs, undefined) of
        undefined ->
            {ok, undefined};
        <<>> ->
            {ok, undefined};
        Status when is_binary(Status) ->
            case lists:member(Status, ?ASSIGNMENT_STATUS_FILTERS) of
                true -> {ok, Status};
                false -> error
            end;
        _ ->
            error
    end.
