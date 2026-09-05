-module(adm_report_action_handler).

-behavior(cowboy_rest).

-export([init/2]).

-include("error_code.hrl").

%% R-02：处置动作端点（case = report_ticket 行）。权限门复用
%% reports:handle（与举报处理同权限面，A-02 角色基线落地后再细分
%% moderator 专属权限点）。

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            execute -> execute_action(Req0, State);
            reverse -> reverse_action(Req0, State);
            list -> list_action(Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

-spec execute_action(cowboy_req:req(), map()) -> cowboy_req:req().
execute_action(Req0, State) ->
    case adm_acl:ensure_any_permission(State, [<<"reports:handle">>], Req0) of
        ok ->
            AdmUid = maps:get(adm_user_id, State, 0),
            PostVals = elib_param:post(Req0),
            CaseId = ec_cnv:to_integer(maps:get(<<"case_id">>, PostVals, 0)),
            Action = ec_cnv:safe_to_binary(maps:get(<<"action">>, PostVals, <<>>)),
            TargetUid = ec_cnv:to_integer(maps:get(<<"target_uid">>, PostVals, 0)),
            Opts = #{
                reason => ec_cnv:safe_to_binary(maps:get(<<"reason">>, PostVals, <<>>)),
                gid => ec_cnv:to_integer(maps:get(<<"gid">>, PostVals, 0)),
                duration_minutes => ec_cnv:to_integer(
                    maps:get(<<"duration_minutes">>, PostVals, 0)
                ),
                target_type => ec_cnv:safe_to_binary(
                    maps:get(<<"target_type">>, PostVals, <<>>)
                ),
                target_id => ec_cnv:to_integer(maps:get(<<"target_id">>, PostVals, 0))
            },
            case moderation_action_logic:execute(AdmUid, CaseId, Action, TargetUid, Opts) of
                {ok, Row} ->
                    elib_response:success(Req0, Row);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end.

-spec reverse_action(cowboy_req:req(), map()) -> cowboy_req:req().
reverse_action(Req0, State) ->
    case adm_acl:ensure_any_permission(State, [<<"reports:handle">>], Req0) of
        ok ->
            AdmUid = maps:get(adm_user_id, State, 0),
            PostVals = elib_param:post(Req0),
            ActionId = ec_cnv:to_integer(maps:get(<<"action_id">>, PostVals, 0)),
            Reason = ec_cnv:safe_to_binary(maps:get(<<"reason">>, PostVals, <<>>)),
            case moderation_action_logic:reverse(AdmUid, ActionId, Reason) of
                {ok, Row} ->
                    elib_response:success(Req0, Row);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end.

-spec list_action(cowboy_req:req(), map()) -> cowboy_req:req().
list_action(Req0, State) ->
    case adm_acl:ensure_any_permission(State, [<<"reports:handle">>], Req0) of
        ok ->
            Qs = cowboy_req:parse_qs(Req0),
            CaseId = ec_cnv:to_integer(proplists:get_value(<<"case_id">>, Qs, 0)),
            case moderation_action_logic:list_by_case(CaseId) of
                {ok, Rows} ->
                    elib_response:success(Req0, Rows);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end.
