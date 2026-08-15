-module(adm_group_vote_handler).
%%% adm_group_vote_handler

-behavior(cowboy_rest).

-export([init/2]).

-import(adm_group_helper, [
    audit_group_governance/5,
    build_governance_log_where_sql/1,
    calc_total_pages/2,
    extract_governance_log_filters/1,
    list_group_files_with_total/5,
    list_tasks_with_total/5,
    list_user_categories_with_total/4,
    normalize_album_pk/1,
    normalize_assignment_pk/1,
    normalize_category_pk/1,
    normalize_file_pk/1,
    normalize_governance_log_row/1,
    normalize_notice_pk/1,
    normalize_notice_row/1,
    normalize_page_payload/3,
    normalize_positive_int/1,
    normalize_restore_task_pk/1,
    normalize_schedule_id/1,
    normalize_task_pk/1,
    normalize_user_pk/1,
    parse_gid_param/1,
    resolve_category_uid/2,
    resolve_task_audit_meta/1,
    resolve_vote_group_id/1,
    task_group_id_by_uid/1,
    task_is_deleted/1
]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case imboy_plugin_registry:required_feature(admin, adm_group_sub_handler, Action) of
            undefined ->
                dispatch(Action, Method, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        dispatch(Action, Method, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%%

-spec dispatch(atom(), binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(vote_list, Method, Req0, State) -> vote_list(Method, Req0, State);
dispatch(vote_detail, Method, Req0, State) -> vote_detail(Method, Req0, State);
dispatch(vote_close, Method, Req0, State) -> vote_close(Method, Req0, State);
dispatch(_, _Method, Req0, _State) -> Req0.

vote_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:vote:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {Page, Size} = elib_param:page(Req0),
            case Gid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_vote_logic:list_votes(Gid, Page, Size) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, Reason} ->
                            ?ERROR_LOG(["adm vote list error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 群投票详情
-spec vote_detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
vote_detail(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:vote:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, VoteId0} = elib_param:binary(vote_id, Req0, <<>>),
            VoteId = ec_cnv:to_binary(VoteId0),
            case VoteId of
                <<>> ->
                    elib_response:error(Req0, "参数错误");
                _ ->
                    case group_vote_logic:get_vote_detail(VoteId) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, vote_not_found} ->
                            elib_response:error(Req0, "投票不存在");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm vote detail error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 结束群投票
-spec vote_close(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
vote_close(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:vote:close">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            VoteId = maps:get(<<"vote_id">>, PostVals, <<>>),
            case VoteId of
                <<>> ->
                    elib_response:error(Req0, "参数错误");
                _ ->
                    %% 平台管理员治理操作：权限已由 adm_acl(groups:vote:close) 校验，
                    %% 不能复用客户端 group_vote_logic:close_vote/2（会再校验
                    %% 「creator 或群管理员」，平台管理员非群成员恒被拒）。
                    %% 与 adm_group_task_handler:task_close 同模式，直接改状态。
                    case group_vote_ds:find_by_vote_id(VoteId) of
                        {error, not_found} ->
                            elib_response:error(Req0, "投票不存在");
                        {ok, Vote} ->
                            case maps:get(<<"status">>, Vote, 0) of
                                2 ->
                                    elib_response:error(Req0, "投票已结束");
                                1 ->
                                    case group_vote_ds:update_vote_status(VoteId, 2) of
                                        {ok, _Count} ->
                                            GroupId = resolve_vote_group_id(VoteId),
                                            _ = audit_group_governance(
                                                maps:get(adm_user_id, State, 0),
                                                GroupId,
                                                <<"close_vote">>,
                                                VoteId,
                                                #{<<"scope">> => <<"vote">>}
                                            ),
                                            elib_response:success(Req0, #{}, "操作成功");
                                        {error, Reason} ->
                                            ?ERROR_LOG(["adm vote close error: ", Reason]),
                                            elib_response:error(Req0, "操作失败")
                                    end;
                                _ ->
                                    ?ERROR_LOG(["adm vote close error: invalid_vote_status"]),
                                    elib_response:error(Req0, "操作失败")
                            end
                    end
            end
    end.
