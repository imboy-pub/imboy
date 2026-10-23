-module(adm_cs_ai_handler).

%%%
% adm_cs_ai 控制器（v4 P0-10 的后端面，G.4 冻结路径族 /api/adm/cs/ai/*）
%
%   * 鉴权镜像 adm_organization_handler：每 action 显式 adm_acl:ensure_permission
%     （fail-closed：无权限/无 adm_user_id 恒 403）；读 = customer_service:read，
%     写 = customer_service:write。
%   * handler 只做平台鉴权、参数转换（TSID string→int）与稳定错误分类；
%     业务一律委派 customer_service_facade 的 ai_*（P0-8 接线；矩阵裁决
%     真源 02-C.4 在 cs_ai_app:authorize/3，本面 principal=平台管理员）。
%   * TSID 传输规则：JSON 里 64-bit ID 一律 string 下发/接收（防 JS 精度丢失）。
%%%

-behaviour(cowboy_rest).
-compile([nowarn_deprecated_catch]).

-export([init/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").
-include("generated/imboy_product_features.hrl").

-define(ACL_READ, <<"customer_service:read">>).
-define(ACL_WRITE, <<"customer_service:write">>).

%% ===================================================================
%% cowboy_rest
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = dispatch(Action, Method, Req0, State),
    {ok, Req1, State}.

%% 特性裁剪门（auth_middleware_api_v1 同款）：customer_service 未选中档时
%% 本 handler 的全部 action 返回 404——adm 模块恒编译，不得在未选中档下
%% 遥指 feature 模块（prune 门 hard/裸调用永不豁免）。
-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE).
dispatch(config_view, Method, Req0, State) ->
    config_view_action(Method, Req0, State);
dispatch(config_set, Method, Req0, State) ->
    write_action(Method, Req0, State, fun config_set_params/1, config_view);
dispatch(faq_list, Method, Req0, State) ->
    faq_list_action(Method, Req0, State);
dispatch(faq_upsert, Method, Req0, State) ->
    write_action(Method, Req0, State, fun faq_upsert_params/1, none);
dispatch(faq_delete, Method, Req0, State) ->
    write_action(Method, Req0, State, fun faq_delete_params/1, none);
dispatch(sessions, Method, Req0, State) ->
    sessions_action(Method, Req0, State);
dispatch(session_turns, Method, Req0, State) ->
    session_turns_action(Method, Req0, State);
dispatch(stats, Method, Req0, State) ->
    stats_action(Method, Req0, State);
dispatch(preview, Method, Req0, State) ->
    write_action(Method, Req0, State, fun preview_params/1, none);
dispatch(_, _Method, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% 读 actions
%% ===================================================================

config_view_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case org_id_from(Req0, undefined) of
                {ok, OrgId} ->
                    case customer_service_facade:ai_config_view(OrgId, #{}) of
                        {ok, Config} ->
                            elib_response:success(Req0, normalize(Config));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code);
                        {error, Reason} ->
                            elib_response:error(Req0, to_binary(Reason), ?ERR_BAD_REQUEST)
                    end;
                error ->
                    elib_response:error(Req0, <<"org_id 必须是正整数"/utf8>>, ?ERR_BAD_REQUEST)
            end
    end;
config_view_action(_, Req0, _State) ->
    method_not_allowed(Req0).

faq_list_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case org_id_from(Req0, undefined) of
                {ok, OrgId} ->
                    {ok, Intent} = elib_param:binary(intent, Req0, <<>>),
                    {Page, Size} = elib_param:page(Req0),
                    Params = #{
                        intent => Intent,
                        limit => Size,
                        offset => (Page - 1) * Size
                    },
                    case customer_service_facade:ai_faq_list(OrgId, Params) of
                        {ok, Rows} ->
                            elib_response:success(Req0, [normalize(R) || R <- Rows]);
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code);
                        {error, Reason} ->
                            elib_response:error(Req0, to_binary(Reason), ?ERR_BAD_REQUEST)
                    end;
                error ->
                    elib_response:error(Req0, <<"org_id 必须是正整数"/utf8>>, ?ERR_BAD_REQUEST)
            end
    end;
faq_list_action(_, Req0, _State) ->
    method_not_allowed(Req0).

sessions_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case org_id_from(Req0, undefined) of
                {ok, OrgId} ->
                    {Page, Size} = elib_param:page(Req0),
                    Params0 = #{limit => Size, offset => (Page - 1) * Size},
                    Params = maybe_ws(Req0, Params0),
                    case customer_service_facade:ai_sessions(OrgId, Params) of
                        {ok, #{sessions := Sessions} = Page0} ->
                            elib_response:success(
                                Req0,
                                normalize(Page0#{sessions := [normalize(S) || S <- Sessions]})
                            );
                        {ok, Other} ->
                            elib_response:success(Req0, normalize(Other));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code);
                        {error, Reason} ->
                            elib_response:error(Req0, to_binary(Reason), ?ERR_BAD_REQUEST)
                    end;
                error ->
                    elib_response:error(Req0, <<"org_id 必须是正整数"/utf8>>, ?ERR_BAD_REQUEST)
            end
    end;
sessions_action(_, Req0, _State) ->
    method_not_allowed(Req0).

session_turns_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case org_id_from(Req0, undefined) of
                {ok, OrgId} ->
                    case elib_param:int(session_id, Req0, 0) of
                        {ok, S} when S > 0 ->
                            {ok, Limit} = elib_param:int(limit, Req0, 200),
                            Params0 = #{session_id => S, limit => Limit},
                            Params = maybe_ws(Req0, Params0),
                            case customer_service_facade:ai_session_turns(OrgId, Params) of
                                {ok, Rows} ->
                                    elib_response:success(Req0, [normalize(R) || R <- Rows]);
                                {error, {Code, Msg}} ->
                                    elib_response:error(Req0, Msg, Code);
                                {error, Reason} ->
                                    elib_response:error(Req0, to_binary(Reason), ?ERR_BAD_REQUEST)
                            end;
                        _ ->
                            elib_response:error(
                                Req0, <<"session_id 必须是正整数"/utf8>>, ?ERR_BAD_REQUEST
                            )
                    end;
                error ->
                    elib_response:error(Req0, <<"org_id 必须是正整数"/utf8>>, ?ERR_BAD_REQUEST)
            end
    end;
session_turns_action(_, Req0, _State) ->
    method_not_allowed(Req0).

stats_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case org_id_from(Req0, undefined) of
                {ok, OrgId} ->
                    {ok, Date} = elib_param:binary(date, Req0, <<>>),
                    Params0 = #{},
                    Params1 =
                        case Date of
                            <<>> -> Params0;
                            _ -> Params0#{date => Date}
                        end,
                    Params = maybe_ws(Req0, Params1),
                    case customer_service_facade:ai_stats(OrgId, Params) of
                        {ok, Stats} ->
                            elib_response:success(Req0, normalize(Stats));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code);
                        {error, Reason} ->
                            elib_response:error(Req0, to_binary(Reason), ?ERR_BAD_REQUEST)
                    end;
                error ->
                    elib_response:error(Req0, <<"org_id 必须是正整数"/utf8>>, ?ERR_BAD_REQUEST)
            end
    end;
stats_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% 写 actions（POST；统一 write ACL + body → 委派）
%% ===================================================================

write_action(<<"POST">>, Req0, State, ParamsFun, ReViewAction) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case read_body_map(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, Data} ->
                    case org_id_from(Req0, Data) of
                        {ok, OrgId} ->
                            write_delegate(
                                Req0, OrgId, ParamsFun(Data), ReViewAction
                            );
                        error ->
                            elib_response:error(
                                Req0, <<"org_id 必须是正整数"/utf8>>, ?ERR_BAD_REQUEST
                            )
                    end
            end
    end;
write_action(_, Req0, _State, _ParamsFun, _ReViewAction) ->
    method_not_allowed(Req0).

write_delegate(Req0, OrgId, {FacadeFun, AppParams}, config_view) ->
    case customer_service_facade:FacadeFun(OrgId, AppParams) of
        {ok, _} = Ok ->
            {ok, _Config} = Ok,
            %% 写后回读最新配置（管理台「保存即见」语义）
            case customer_service_facade:ai_config_view(OrgId, #{}) of
                {ok, Config} -> elib_response:success(Req0, normalize(Config));
                _ -> elib_response:success(Req0, #{})
            end;
        {error, {Code, Msg}} ->
            elib_response:error(Req0, Msg, Code);
        {error, Reason} ->
            elib_response:error(Req0, to_binary(Reason), ?ERR_BAD_REQUEST)
    end;
write_delegate(Req0, OrgId, {FacadeFun, AppParams}, none) ->
    case customer_service_facade:FacadeFun(OrgId, AppParams) of
        {ok, Result} when is_map(Result) ->
            elib_response:success(Req0, normalize(Result));
        {ok, Result} when is_list(Result) ->
            elib_response:success(Req0, [normalize(I) || I <- Result]);
        ok ->
            elib_response:success(Req0, #{});
        {error, {Code, Msg}} ->
            elib_response:error(Req0, Msg, Code);
        {error, Reason} ->
            elib_response:error(Req0, to_binary(Reason), ?ERR_BAD_REQUEST)
    end.

%% ===================================================================
%% 参数装配（binary JSON 键 → application atom 键；只白名单放行）
%% ===================================================================

config_set_params(Data) ->
    Keys = [
        name,
        provider,
        model,
        system_prompt,
        greeting,
        auto_reply_mode,
        faq_require_hit,
        max_unresolved_turns,
        max_inflight,
        job_timeout_ms,
        max_job_age_ms,
        seat_identity_id,
        id,
        config_version
    ],
    {ai_config_set, atom_params(Keys, Data)}.

faq_upsert_params(Data) ->
    Keys0 = [
        id, question, answer, intent, category, status, keywords
    ],
    P = atom_params(Keys0, Data),
    {ai_faq_upsert, maybe_keywords(P)}.

faq_delete_params(Data) ->
    {ai_faq_delete, atom_params([id], Data)}.

preview_params(Data) ->
    Params = atom_params(
        [text, intent, confidence, resolved, faq_score, unresolved_turns], Data
    ),
    %% JSON 内层键也是 binary；domain 的试算配置使用 atom 白名单。
    Config = maps:get(<<"config">>, Data, #{}),
    case is_map(Config) of
        true ->
            {ai_preview, Params#{
                config => atom_params(
                    [threshold, max_unresolved_turns, faq_min_score], Config
                )
            }};
        false ->
            {ai_preview, Params}
    end.

atom_params(Keys, Data) ->
    maps:from_list([
        {K, V}
     || K <- Keys,
        V <- [maps:get(atom_to_binary(K), Data, undefined)],
        V =/= undefined
    ]).

maybe_keywords(P) ->
    case maps:get(keywords, P, undefined) of
        L when is_list(L) -> P#{keywords => [to_binary(I) || I <- L]};
        B when is_binary(B) ->
            P#{keywords => [K || K <- binary:split(B, <<",">>, [global, trim_all]), K =/= <<>>]};
        _ ->
            P
    end.

maybe_ws(Req0, Params) ->
    case elib_param:int(workspace_id, Req0, 0) of
        {ok, Ws} when Ws > 0 -> Params#{workspace_id => Ws};
        _ -> Params
    end.

%% org_id：GET 取 qs，POST 允许 body 覆盖（TSID string → int）。
org_id_from(Req0, Data) ->
    Raw =
        case Data of
            #{<<"org_id">> := V} ->
                V;
            _ ->
                %% Cowboy 2 无 qs/2（lefthook 门注释的 qs/2 事故同款）：
                %% 用 parse_qs + proplists 取值。
                Qs = cowboy_req:parse_qs(Req0),
                proplists:get_value(<<"org_id">>, Qs, <<>>)
        end,
    parse_tsid(Raw).

parse_tsid(Value) when is_integer(Value), Value > 0 -> {ok, Value};
parse_tsid(Value) when is_binary(Value) ->
    case
        try binary_to_integer(string:trim(Value)) of
            Id when is_integer(Id), Id > 0 -> {ok, Id};
            _ -> error
        catch
            _:_ -> error
        end
    of
        Result -> Result
    end;
parse_tsid(_) ->
    error.

read_body_map(Req0) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req0),
    case byte_size(Body) of
        0 ->
            {ok, #{}};
        _ ->
            try jsone:decode(Body, [{object_format, map}]) of
                Data when is_map(Data) ->
                    {ok, Data};
                _ ->
                    {error, <<"请求体必须是 JSON 对象"/utf8>>}
            catch
                _:_ ->
                    {error, <<"请求体必须是合法 JSON"/utf8>>}
            end
    end.

%% TSID 传输规则：id 类字段 int → string（防 JS 精度丢失）。
-define(ID_KEYS, [
    id,
    organization_id,
    workspace_id,
    session_id,
    conversation_id,
    contact_id,
    job_id,
    faq_id,
    seat_identity_id,
    business_identity_id,
    contact_identity_id,
    owner_user_id,
    message_id
]).

normalize(Map) when is_map(Map) ->
    maps:fold(
        fun(K, V, Acc) -> Acc#{K => normalize_value(K, V)} end,
        #{},
        Map
    );
normalize(Other) ->
    Other.

normalize_value(K, V) when is_integer(V) ->
    case is_id_key(K) of
        true -> integer_to_binary(V);
        false -> V
    end;
normalize_value(K, V) when is_list(V), K =:= faq_ids; is_list(V), K =:= <<"faq_ids">> ->
    [normalize_value(faq_id, I) || I <- V];
normalize_value(_K, V) when is_list(V) ->
    [normalize(I) || I <- V];
normalize_value(_K, V) when is_map(V) ->
    normalize(V);
normalize_value(_K, V) ->
    V.

is_id_key(K) when is_atom(K) ->
    lists:member(K, ?ID_KEYS);
is_id_key(K) when is_binary(K) ->
    lists:any(fun(IdKey) -> atom_to_binary(IdKey) =:= K end, ?ID_KEYS);
is_id_key(_) ->
    false.

to_binary(V) when is_binary(V) -> V;
to_binary(V) -> iolist_to_binary(io_lib:format("~0p", [V])).

method_not_allowed(Req0) ->
    elib_response:error(Req0, <<"method not allowed"/utf8>>, ?ERR_METHOD_NOT_ALLOWED).

-else.
dispatch(_Action, _Method, Req0, _State) ->
    elib_response:error(Req0, <<"not found"/utf8>>, ?ERR_NOT_FOUND).
-endif.
