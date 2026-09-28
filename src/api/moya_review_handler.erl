-module(moya_review_handler).
%%%
% 墨芽老师侧 HTTP 适配层（Step 9）
% GET  /api/v1/moya/review-queue                       待评队列
% GET  /api/v1/moya/submissions/:id/review-workbench   工作台
% POST /api/v1/moya/submissions/:id/ai-draft           手动触发 AI 整理
% PUT  /api/v1/moya/submissions/:id/review-draft       保存草稿
% POST /api/v1/moya/submissions/:id/reviews/publish    发布回评
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
handle_action(queue, Req, State) -> queue(Req, State);
handle_action(workbench, Req, State) -> workbench(Req, State);
handle_action(ai_draft, Req, State) -> ai_draft(Req, State);
handle_action(save_draft, Req, State) -> save_draft(Req, State);
handle_action(publish, Req, State) -> publish(Req, State);
handle_action(false, Req, _State) -> Req.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec queue(cowboy_req:req(), map()) -> cowboy_req:req().
queue(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    case ai_status_param(Qs) of
        {ok, AiStatus} ->
            case time_params(Qs) of
                {ok, From, To} ->
                    Filters = #{
                        <<"group_id">> => proplists:get_value(<<"group_id">>, Qs, undefined),
                        %% ai_status 白名单化（deny-by-default）：缺省 undefined
                        %% 不过滤；键名保持 binary（handler 输出线格式键，与
                        %% group_id/assignment_id 一致），atom 归一是 logic 层的事。
                        <<"ai_status">> => AiStatus,
                        %% CM-F3：assignment_id 过滤此前被 handler 丢弃（logic
                        %% queue_with_groups 早已解析该键）——一行接线补透传
                        <<"assignment_id">> => proplists:get_value(
                            <<"assignment_id">>, Qs, undefined
                        ),
                        %% 老师「待点评」页快速检索：按作业 / 按提交人 / 按提交时间范围
                        <<"task_id">> => proplists:get_value(<<"task_id">>, Qs, undefined),
                        <<"learner_id">> => proplists:get_value(<<"learner_id">>, Qs, undefined),
                        <<"submitted_from">> => From,
                        <<"submitted_to">> => To
                    },
                    {Page, Size} = elib_param:page_qs(Qs, 20),
                    case moya_review_logic:queue(Uid, Filters, {Page, Size}) of
                        {ok, Payload} ->
                            elib_response:success_rfc3339(Req0, Payload);
                        {error, Reason} ->
                            moya_error:to_response(Req0, Reason)
                    end;
                error ->
                    %% 非法时间边界一律 422 拒绝（deny-by-default，不静默忽略）
                    elib_response:error(Req0, <<"提交时间范围参数非法"/utf8>>, ?ERR_PARAM_INVALID)
            end;
        error ->
            %% 非法 ai_status 一律 422 拒绝（deny-by-default，绝不下发 logic）
            elib_response:error(Req0, <<"AI状态过滤参数非法"/utf8>>, ?ERR_PARAM_INVALID)
    end.

-spec workbench(cowboy_req:req(), map()) -> cowboy_req:req().
workbench(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, SubmissionId} ->
            case moya_review_logic:workbench(Uid, SubmissionId) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload);
                {error, Reason} ->
                    moya_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"提交ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

%% @doc 手动触发 AI 整理（老师主动请求；入队后异步执行，客户端轮询工作台看进度）
-spec ai_draft(cowboy_req:req(), map()) -> cowboy_req:req().
ai_draft(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, SubmissionId} ->
            case moya_review_logic:request_ai_draft(Uid, SubmissionId) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload, <<"已开始整理"/utf8>>);
                {error, Reason} ->
                    moya_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"提交ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec save_draft(cowboy_req:req(), map()) -> cowboy_req:req().
save_draft(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, SubmissionId} ->
            Body = elib_param:post(Req0),
            case moya_review_logic:save_draft(Uid, SubmissionId, Body) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload, <<"草稿已保存"/utf8>>);
                {error, Reason} ->
                    moya_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"提交ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec publish(cowboy_req:req(), map()) -> cowboy_req:req().
publish(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, SubmissionId} ->
            Body = elib_param:post(Req0),
            case moya_review_logic:publish(Uid, SubmissionId, Body) of
                {ok, Review, Already} ->
                    %% 订阅消息下发为异步附加能力（spawn 在 logic 内），
                    %% 失败不影响发布响应；Already=true 为幂等重放，不重复触发通知
                    case Already of
                        false -> moya_subscribe_logic:notify_review_published_async(SubmissionId);
                        true -> ok
                    end,
                    elib_response:success_rfc3339(
                        Req0,
                        #{
                            <<"review">> => Review,
                            <<"already_published">> => Already
                        },
                        <<"发布成功"/utf8>>
                    );
                {error, Reason} ->
                    moya_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"提交ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% ai_status 过滤五态白名单（与 repo queue/4 LATERAL join 的
%% ('queued','running','succeeded','failed') 字面量集合一致，
%% 另补 none 表示「无有效 AI 草稿」）。
%% 缺省 → undefined（不过滤）；非法值 → error（handler 422 拒绝）。
-define(AI_STATUS_FILTERS, [
    <<"none">>, <<"queued">>, <<"running">>, <<"succeeded">>, <<"failed">>
]).

-spec ai_status_param(list()) -> {ok, binary() | undefined} | error.
ai_status_param(Qs) ->
    case proplists:get_value(<<"ai_status">>, Qs, undefined) of
        undefined ->
            {ok, undefined};
        <<>> ->
            {ok, undefined};
        Status when is_binary(Status) ->
            case lists:member(Status, ?AI_STATUS_FILTERS) of
                true -> {ok, Status};
                false -> error
            end;
        _ ->
            error
    end.

%% 提交时间范围白名单（与 logic ?TIME_PARAM_RE 同口径）：`YYYY-MM-DD` 或
%% 带时间的 RFC3339（可选小数秒/时区）。缺省/空 → undefined（该边界不生效）。
%% 非白名单 → error（handler 422 拒绝，绝不把脏串交给 PG 的 ::timestamptz）。
-define(TIME_PARAM_RE,
    <<"^\\d{4}-\\d{2}-\\d{2}(T\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?(Z|[+-]\\d{2}:\\d{2})?)?$">>
).

-spec time_params(list()) -> {ok, binary() | undefined, binary() | undefined} | error.
time_params(Qs) ->
    case
        {
            time_param(proplists:get_value(<<"submitted_from">>, Qs, undefined)),
            time_param(proplists:get_value(<<"submitted_to">>, Qs, undefined))
        }
    of
        {{ok, From}, {ok, To}} -> {ok, From, To};
        _ -> error
    end.

-spec time_param(term()) -> {ok, binary() | undefined} | error.
time_param(undefined) ->
    {ok, undefined};
time_param(<<>>) ->
    {ok, undefined};
time_param(Bin) when is_binary(Bin) ->
    case re:run(Bin, ?TIME_PARAM_RE, [{capture, none}]) of
        match -> {ok, Bin};
        nomatch -> error
    end;
time_param(_) ->
    error.

-spec path_id(cowboy_req:req()) -> {ok, integer()} | error.
path_id(Req) ->
    case cowboy_req:binding(id, Req) of
        undefined -> error;
        Bin when is_binary(Bin) -> elib_tsid:from_binary(Bin)
    end.
