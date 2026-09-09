-module(teaching_review_handler).
%%%
% 墨芽老师侧 HTTP 适配层（Step 9）
% GET /api/v1/teaching/review-queue                       待评队列
% GET /api/v1/teaching/submissions/:id/review-workbench   工作台
% PUT /api/v1/teaching/submissions/:id/review-draft       保存草稿
% POST /api/v1/teaching/submissions/:id/reviews/publish   发布回评
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
    Filters = #{
        <<"group_id">> => proplists:get_value(<<"group_id">>, Qs, undefined),
        <<"ai_status">> => proplists:get_value(<<"ai_status">>, Qs, undefined)
    },
    {Page, Size} = page_param(Qs),
    case teaching_review_logic:queue(Uid, Filters, {Page, Size}) of
        {ok, Payload} ->
            elib_response:success(Req0, Payload);
        {error, Reason} ->
            teaching_error:to_response(Req0, Reason)
    end.

-spec workbench(cowboy_req:req(), map()) -> cowboy_req:req().
workbench(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_id(Req0) of
        {ok, SubmissionId} ->
            case teaching_review_logic:workbench(Uid, SubmissionId) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Reason} ->
                    teaching_error:to_response(Req0, Reason)
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
            case teaching_review_logic:save_draft(Uid, SubmissionId, Body) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload, <<"草稿已保存"/utf8>>);
                {error, Reason} ->
                    teaching_error:to_response(Req0, Reason)
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
            case teaching_review_logic:publish(Uid, SubmissionId, Body) of
                {ok, Review, Already} ->
                    elib_response:success(
                        Req0,
                        #{
                            <<"review">> => Review,
                            <<"already_published">> => Already
                        },
                        <<"发布成功"/utf8>>
                    );
                {error, Reason} ->
                    teaching_error:to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"提交ID必填"/utf8>>, ?ERR_MISSING_PARAM)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec path_id(cowboy_req:req()) -> {ok, integer()} | error.
path_id(Req) ->
    case cowboy_req:binding(id, Req) of
        undefined ->
            error;
        Bin ->
            try binary_to_integer(Bin) of
                Int when Int > 0 -> {ok, Int};
                _ -> error
            catch
                _:_ -> error
            end
    end.

-spec page_param(list()) -> {integer(), integer()}.
page_param(Qs) ->
    Page = int_param(Qs, <<"page">>, 1),
    Size = int_param(Qs, <<"size">>, 20),
    {max(1, Page), min(100, max(1, Size))}.

-spec int_param(list(), binary(), integer()) -> integer().
int_param(Qs, Key, Default) ->
    try binary_to_integer(proplists:get_value(Key, Qs, <<>>)) of
        Int when is_integer(Int) -> Int;
        _ -> Default
    catch
        _:_ -> Default
    end.
