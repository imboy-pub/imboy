-module(adm_feedback_handler).

%%%
% adm_feedback 控制器模块
% adm_feedback controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化反馈管理处理器
%% 根据请求中的 action 参数分发到不同的处理函数
%% @param Req0 Cowboy 请求对象
%% @param State0 状态映射，包含 action 等信息
%% @return {ok, Req, State} 更新后的请求和状态
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            index ->
                {ok, Ajax} = elib_param:int(ajax, Req0, -2),
                % elib_log:info(["AjaxAjaxAjaxAjaxAjax: ", Ajax, ";  Ajax"]),
                index(Method, Ajax, Req0, State);
            reply ->
                reply(Method, Req0, State);
            workflow_config ->
                workflow_config(Method, Req0, State);
            delete ->
                delete(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 处理反馈列表页面
-spec index(binary(), integer(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, _Ajax, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    % Where2 = <<"status > 0 AND ", Where/binary>>,
    Where = #{status => {op, <<">">>, -2}},
    Column =
        <<
            "id as feedback_id, user_id, device_id, client_operating_system, "
            "client_operating_system_vsn, type, rating, contact_detail, "
            "body, attach, reply_count, status, updated_at, created_at, "
            "app_vsn"
        >>,
    {ok, P} = feedback_ds:page(Column, Where, <<"id desc">>, Page, Size),
    P2 = normalize_feedback_payload(P),
    elib_response:success(Req0, P2);
index(_, _Ajax, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 处理反馈回复
%% 管理员回复用户反馈，记录回复内容和回复者信息
%% @param Method HTTP 方法
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射，包含管理员 ID 等信息
%% @return cowboy_req:req() 更新后的请求对象
-spec reply(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
reply(<<"POST">>, Req0, State) ->
    AdmUserId = maps:get(adm_user_id, State),
    Key = {adm_user_sample, AdmUserId},
    U = adm_user_logic:find(AdmUserId, <<"id,nickname">>, Key),
    Nickname = maps:get(<<"nickname">>, U, <<"">>),
    % replier_user_id
    PostVals = elib_param:post(Req0),
    % FeedbackId = proplists:get_value(<<"feedback_id">>, PostVals),
    % FeedbackId = elib_param:int(<<"FeedbackId">>, Req0, 0),
    {ok, FeedbackId} =
        case
            string:to_integer(
                ec_cnv:to_list(
                    maps:get(<<"feedback_id">>, PostVals, <<"0">>)
                )
            )
        of
            {error, _} ->
                {ok, 0};
            {Val2, _} ->
                {ok, Val2}
        end,
    % ?DEBUG_LOG(["FeedbackId", FeedbackId, PostVals]),
    if
        is_integer(FeedbackId), FeedbackId > 0 ->
            ReplyPid = ec_cnv:to_integer(maps:get(<<"feedback_reply_pid">>, PostVals, 0)),
            Body = maps:get(<<"body">>, PostVals, ""),
            ok = feedback_ds:add_reply(#{
                <<"feedback_id">> => FeedbackId,
                <<"feedback_reply_pid">> => ReplyPid,
                <<"replier_user_id">> => AdmUserId,
                <<"replier_name">> => Nickname,
                <<"body">> => Body,
                <<"created_at">> => elib_dt:now()
            }),
            elib_response:success(Req0, #{}, "success.");
        true ->
            elib_response:error(Req0)
    end.

%% @doc 软删除反馈（status => -1）
-spec delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"feedback:delete">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            FeedbackIdRaw = maps:get(<<"feedback_id">>, PostVals, 0),
            {ok, FeedbackId} =
                case string:to_integer(ec_cnv:to_list(FeedbackIdRaw)) of
                    {error, _} -> {ok, 0};
                    {Val, _} -> {ok, Val}
                end,
            case FeedbackId > 0 of
                true ->
                    case
                        elib_pg:update(
                            feedback_repo:tablename(),
                            #{
                                <<"status">> => -1,
                                <<"updated_at">> => elib_dt:now()
                            },
                            <<"id = $1">>,
                            [FeedbackId]
                        )
                    of
                        {ok, _} ->
                            elib_response:success(Req0, #{});
                        {error, Reason} ->
                            elib_response:error(
                                Req0, ec_cnv:to_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end;
                false ->
                    elib_response:error(Req0, <<"feedback_id 无效"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
delete(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 读取/保存反馈流程配置
-spec workflow_config(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
workflow_config(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"feedback:workflow:read">>, Req0) of
        ok ->
            elib_response:success(Req0, read_workflow_config());
        {error, Req1} ->
            Req1
    end;
workflow_config(<<"PUT">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"feedback:workflow:write">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            Saved = save_workflow_config(PostVals),
            elib_response:success(Req0, Saved);
        {error, Req1} ->
            Req1
    end;
workflow_config(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"feedback:workflow:write">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            Saved = save_workflow_config(PostVals),
            elib_response:success(Req0, Saved);
        {error, Req1} ->
            Req1
    end;
workflow_config(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% 数据规范化函数（ID编码）
%% ===================================================================

%% @doc 规范化反馈分页数据（编码ID字段）
-spec normalize_feedback_payload(map()) -> map().
normalize_feedback_payload(Payload) ->
    List = maps:get(list, Payload, []),
    List2 = [normalize_feedback(Item) || Item <- List],
    maps:remove(items, Payload#{list => List2}).

%% @doc 规范化单条反馈数据（编码ID字段）
%% Admin API 将 TSID 整数转为字符串，避免 JS 精度丢失。
-spec normalize_feedback(map()) -> map().
normalize_feedback(Feedback) ->
    %% `user_id` not `uid` — feedback table column is user_id
    elib_id:tsid_keys_to_bin(Feedback, [<<"id">>, <<"user_id">>]).

%% ===================================================================
%% Feedback Workflow Config
%% ===================================================================

-spec workflow_config_key() -> tuple().
workflow_config_key() ->
    {?MODULE, workflow_config}.

-spec default_workflow_config() -> map().
default_workflow_config() ->
    #{
        <<"reply_templates">> => [
            <<"感谢反馈，我们已收到并会尽快处理。"/utf8>>,
            <<"问题已记录到修复队列，预计将在后续版本优化。"/utf8>>,
            <<"请补充相关截图和复现步骤，便于我们进一步排查。"/utf8>>,
            <<"该反馈已转交对应业务负责人处理，请留意后续通知。"/utf8>>
        ],
        <<"sla_hours">> => 24
    }.

-spec read_workflow_config() -> map().
read_workflow_config() ->
    case persistent_term:get(workflow_config_key(), undefined) of
        Cfg when is_map(Cfg) ->
            Cfg;
        _ ->
            default_workflow_config()
    end.

-spec save_workflow_config(map()) -> map().
save_workflow_config(PostVals) ->
    Fallback = read_workflow_config(),
    FallbackTemplates = maps:get(<<"reply_templates">>, Fallback, []),
    FallbackSla = maps:get(<<"sla_hours">>, Fallback, 24),
    RawTemplates = maps:get(<<"reply_templates">>, PostVals, FallbackTemplates),
    RawSla = maps:get(<<"sla_hours">>, PostVals, FallbackSla),
    Cfg = #{
        <<"reply_templates">> => normalize_reply_templates(RawTemplates, FallbackTemplates),
        <<"sla_hours">> => normalize_sla_hours(RawSla, FallbackSla)
    },
    persistent_term:put(workflow_config_key(), Cfg),
    Cfg.

-spec normalize_reply_templates(term(), list()) -> list().
normalize_reply_templates(RawTemplates, Fallback) when is_list(RawTemplates) ->
    Norm0 = [normalize_template_binary(Item) || Item <- RawTemplates],
    Norm1 = [Item || Item <- Norm0, Item =/= <<>>],
    Norm2 = dedupe_templates(Norm1, #{}, []),
    case Norm2 of
        [] ->
            Fallback;
        _ ->
            lists:sublist(Norm2, 20)
    end;
normalize_reply_templates(_, Fallback) ->
    Fallback.

-spec normalize_template_binary(term()) -> binary().
normalize_template_binary(Value) when is_binary(Value); is_list(Value) ->
    try
        unicode:characters_to_binary(string:trim(unicode:characters_to_list(Value)))
    catch
        _:_ -> <<>>
    end;
normalize_template_binary(Value) when is_map(Value) ->
    normalize_template_binary(
        maps:get(
            <<"text">>,
            Value,
            maps:get(
                <<"label">>,
                Value,
                maps:get(<<"content">>, Value, <<>>)
            )
        )
    );
normalize_template_binary(_) ->
    <<>>.

-spec dedupe_templates(list(), map(), list()) -> list().
dedupe_templates([], _Seen, Acc) ->
    lists:reverse(Acc);
dedupe_templates([Item | Rest], Seen, Acc) ->
    case maps:is_key(Item, Seen) of
        true ->
            dedupe_templates(Rest, Seen, Acc);
        false ->
            dedupe_templates(Rest, Seen#{Item => true}, [Item | Acc])
    end.

-spec normalize_sla_hours(term(), integer()) -> integer().
normalize_sla_hours(Value, Default) ->
    Base =
        case safe_to_integer(Value) of
            {ok, Int} ->
                Int;
            error ->
                Default
        end,
    if
        Base < 1 ->
            1;
        Base > 720 ->
            720;
        true ->
            Base
    end.

-spec safe_to_integer(term()) -> {ok, integer()} | error.
safe_to_integer(Value) when is_integer(Value) ->
    {ok, Value};
safe_to_integer(Value) when is_binary(Value) ->
    try
        {ok, binary_to_integer(Value)}
    catch
        _:_ -> error
    end;
safe_to_integer(Value) when is_list(Value) ->
    try
        {ok, list_to_integer(Value)}
    catch
        _:_ -> error
    end;
safe_to_integer(_) ->
    error.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
