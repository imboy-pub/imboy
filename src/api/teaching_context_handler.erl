-module(teaching_context_handler).
%%%
% 墨芽教学上下文 HTTP 适配层（contexts / context switch）
% Thin HTTP adapter for teaching contexts
%
% GET  /api/v1/teaching/contexts        用户可用身份上下文
% POST /api/v1/teaching/context/switch  显式切换（无凭证语义，仅校验+回显）
%
% 错误码映射（STEP-04 error-codes.md 5420 段）。
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
handle_action(contexts, Req, State) ->
    contexts(Req, State);
handle_action(switch, Req, State) ->
    switch(Req, State);
handle_action(false, Req, _State) ->
    Req.

%% @doc 教学身份上下文列表
-spec contexts(cowboy_req:req(), map()) -> cowboy_req:req().
contexts(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Schema =
        case proplists:get_value(<<"schema_version">>, Qs, <<"1">>) of
            <<"2">> -> organization;
            _ -> legacy
        end,
    case teaching_context_logic:contexts(CurrentUid, Schema) of
        {ok, Payload} ->
            elib_response:success_rfc3339(Req0, Payload);
        {error, db_error} ->
            elib_response:error(Req0, <<"上下文解析失败"/utf8>>, ?ERR_ERROR)
    end.

%% @doc 切换教学上下文（仅校验归属并回显快照，不签发凭证）
-spec switch(cowboy_req:req(), map()) -> cowboy_req:req().
switch(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    case teaching_context_logic:switch(CurrentUid, PostVals) of
        {ok, Ctx} ->
            elib_response:success_rfc3339(Req0, Ctx, <<"切换成功"/utf8>>);
        {error, Reason} ->
            switch_error(Req0, Reason)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec switch_error(cowboy_req:req(), atom()) -> cowboy_req:req().
switch_error(Req, invalid_type) ->
    elib_response:error(Req, <<"身份类型参数非法"/utf8>>, ?ERR_PARAM_INVALID);
switch_error(Req, missing_learner_id) ->
    elib_response:error(Req, <<"缺少学员ID"/utf8>>, ?ERR_MISSING_PARAM);
switch_error(Req, missing_group_id) ->
    elib_response:error(Req, <<"缺少班级ID"/utf8>>, ?ERR_MISSING_PARAM);
switch_error(Req, missing_org_id) ->
    elib_response:error(Req, <<"缺少机构ID"/utf8>>, ?ERR_MISSING_PARAM);
switch_error(Req, inactive) ->
    elib_response:error(Req, <<"所选身份已失效"/utf8>>, ?ERR_TEACHING_CONTEXT_INACTIVE);
switch_error(Req, db_error) ->
    elib_response:error(Req, <<"切换失败，请重试"/utf8>>, ?ERR_ERROR);
switch_error(Req, _NotOwnedOrMismatch) ->
    %% context_mismatch / 关系不存在：统一为"不属于当前用户"，不区分细节（T14）
    elib_response:error(Req, <<"所选身份不属于当前用户"/utf8>>, ?ERR_TEACHING_CONTEXT_INVALID).
