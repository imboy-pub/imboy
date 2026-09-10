-module(teaching_assignment_handler).
%%%
% 墨芽家长侧 HTTP 适配层（Step 9）
% GET  /api/v1/teaching/assignments                家长作业列表
% GET  /api/v1/teaching/assignments/:id            作业详情
% POST /api/v1/teaching/assignments/:id/submissions 幂等创建提交
% GET  /api/v1/teaching/submissions/:id            提交详情（视角感知）
% POST /api/v1/teaching/submissions/:id/withdraw    撤回
% GET  /api/v1/teaching/learners/:id/history        学员历史
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
handle_action(false, Req, _State) -> Req.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec list_assignments(cowboy_req:req(), map()) -> cowboy_req:req().
list_assignments(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    case tsid(proplists:get_value(<<"learner_id">>, Qs)) of
        {ok, LearnerId} ->
            Page = page_param(Qs),
            case teaching_assignment_logic:list(Uid, LearnerId, Page) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Reason} ->
                    teaching_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"缺少学员ID"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec assignment_detail(cowboy_req:req(), map()) -> cowboy_req:req().
assignment_detail(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, AssignmentId} ->
            case teaching_assignment_logic:detail(Uid, AssignmentId) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Reason} ->
                    teaching_error:to_response(Req0, Reason)
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
            case teaching_assignment_logic:create_submission(Uid, AssignmentId, Key, Body) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload, <<"提交成功"/utf8>>);
                {error, Reason} ->
                    teaching_error:to_response(Req0, Reason)
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
            case teaching_review_logic:submission_detail(Uid, SubmissionId) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Reason} ->
                    teaching_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"提交ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec withdraw(cowboy_req:req(), map()) -> cowboy_req:req().
withdraw(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, SubmissionId} ->
            case teaching_review_logic:withdraw(Uid, SubmissionId) of
                {ok, withdrawn} ->
                    elib_response:success(
                        Req0,
                        #{<<"status">> => <<"withdrawn">>},
                        <<"已撤回"/utf8>>
                    );
                {error, Reason} ->
                    teaching_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"提交ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

-spec history(cowboy_req:req(), map()) -> cowboy_req:req().
history(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, LearnerId} ->
            Page = page_param(cowboy_req:parse_qs(Req0)),
            case teaching_review_logic:history(Uid, LearnerId, Page) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Reason} ->
                    teaching_error:to_response(Req0, Reason)
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
        Bin when is_binary(Bin) -> tsid(Bin)
    end.

-spec tsid(binary()) -> {ok, integer()} | error.
tsid(Bin) when is_binary(Bin) ->
    try binary_to_integer(Bin) of
        Int when Int > 0 -> {ok, Int};
        _ -> error
    catch
        _:_ -> error
    end;
tsid(_) ->
    error.

-spec page_param(list()) -> {integer(), integer()}.
page_param(Qs) ->
    Page = int_param(Qs, <<"page">>, 1),
    Size = int_param(Qs, <<"size">>, 20),
    {max(1, Page), min(100, max(1, Size))}.

-spec int_param(list(), binary(), integer()) -> integer().
int_param(Qs, Key, Default) ->
    try binary_to_integer(proplists:get_value(Key, Qs, <<>>)) of
        Int when is_integer(Int) -> Int
    catch
        _:_ -> Default
    end.
