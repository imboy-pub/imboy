-module(adm_owner_activation_handler).
-compile([nowarn_deprecated_catch]).

%%%
% adm_owner_activation 控制器（GZAPP-06 / 待激活 Owner 全链路）
% Platform Admin Owner 激活治理 API——全部挂在 /api/adm/organizations/:id 下：
%   GET  /owner-activation          状态卡（organizations:read）
%   POST /owner-activation/resend   重发激活短信（token 轮换，幂等重入）
%   POST /owner-activation/reactivate 30 天 TTL 重新激活（刷新 TTL 回 pending）
%   POST /owner-activation/consume  激活 token 单次消费（合同级；落地页后续卡）
%   POST /owner-transfer-by-phone   按手机号换 Owner（D13 恰一不变量）
%
% 硬边界（与 adm_organization_handler 同口径）：
%   * 鉴权 adm_acl:ensure_permission fail-closed；
%   * handler 只做平台鉴权、参数转换（TSID string→int）、审计
%     （adm_operation_log_ds，手机号一律脱敏前3后4）与稳定错误分类；
%     业务一律经 organization_owner_activation_logic；
%   * 手机号 PII：审计 Detail / 错误消息 / 日志绝不携带 mobile 原文。
%
% TSID 传输规则：JSON 里 64-bit ID 一律 string 下发/接收（防 JS 精度丢失）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

-define(ACL_READ, <<"organizations:read">>).
-define(ACL_WRITE, <<"organizations:write">>).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case imboy_plugin_registry:required_feature(admin, adm_owner_activation_handler, Action) of
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

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec dispatch(atom(), binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(owner_activation_show, Method, Req0, State) ->
    show_action(Method, Req0, State);
dispatch(owner_activation_resend, Method, Req0, State) ->
    resend_action(Method, Req0, State);
dispatch(owner_activation_reactivate, Method, Req0, State) ->
    reactivate_action(Method, Req0, State);
dispatch(owner_activation_consume, Method, Req0, State) ->
    consume_action(Method, Req0, State);
dispatch(owner_transfer_by_phone, Method, Req0, State) ->
    transfer_action(Method, Req0, State);
dispatch(_, _Method, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 读：待激活 Owner 状态卡
%% ------------------------------------------------------------------

-spec show_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
show_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    case organization_owner_activation_logic:admin_status(OrgId) of
                        {ok, Status} ->
                            elib_response:success(Req0, normalize_status(Status));
                        {error, {Code, Msg2}} ->
                            elib_response:error(Req0, Msg2, Code)
                    end
            end
    end;
show_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 写：重发（token 轮换 + resend_count++；短信失败不回滚）
%% ------------------------------------------------------------------

-spec resend_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
resend_action(<<"POST">>, Req0, State) ->
    write_action(Req0, State, resend, fun organization_owner_activation_logic:admin_resend/2);
resend_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 写：重新激活（30 天 TTL 刷新回 pending + token 轮换）
%% ------------------------------------------------------------------

-spec reactivate_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
reactivate_action(<<"POST">>, Req0, State) ->
    write_action(
        Req0, State, reactivate, fun organization_owner_activation_logic:admin_reactivate/2
    );
reactivate_action(_, Req0, _State) ->
    method_not_allowed(Req0).

write_action(Req0, State, Action, Fun) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    case catch Fun(AdmUserId, OrgId) of
                        {ok, Result} ->
                            audit(AdmUserId, OrgId, Action, #{}, Req0),
                            elib_response:success(
                                Req0, normalize_rotate(Result), success_msg(Action)
                            );
                        {error, {Code, Msg2}} ->
                            elib_response:error(Req0, Msg2, Code);
                        {'EXIT', {Reason, Stack}} ->
                            ?ERROR_LOG([
                                owner_activation_write_crash, Action, OrgId, Reason, Stack
                            ]),
                            elib_response:error(Req0, <<"操作失败，请稍后重试"/utf8>>, 500);
                        {'EXIT', Reason} ->
                            ?ERROR_LOG([owner_activation_write_crash, Action, OrgId, Reason]),
                            elib_response:error(Req0, <<"操作失败，请稍后重试"/utf8>>, 500)
                    end
            end
    end.

success_msg(resend) ->
    <<"已重发激活短信（新链接生效，旧链接失效）"/utf8>>;
success_msg(reactivate) ->
    <<"已重新激活邀请（TTL 重置 30 天）"/utf8>>.

%% ------------------------------------------------------------------
%% 写：激活 token 单次消费（合同级端点；落地页/短信链接载体后续卡）
%% ------------------------------------------------------------------

-spec consume_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
consume_action(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    case read_body_map(Req0) of
                        {error, Msg2} ->
                            elib_response:error(Req0, Msg2, ?ERR_BAD_REQUEST);
                        {ok, Data} ->
                            consume_write(Req0, AdmUserId, OrgId, Data)
                    end
            end
    end;
consume_action(_, Req0, _State) ->
    method_not_allowed(Req0).

consume_write(Req0, AdmUserId, OrgId, Data) ->
    Token = maps:get(<<"token">>, Data, undefined),
    case is_binary(Token) andalso byte_size(Token) > 0 of
        false ->
            elib_response:error(Req0, <<"token 必填"/utf8>>, ?ERR_BAD_REQUEST);
        true ->
            case organization_owner_activation_logic:activate_by_token(Token) of
                {ok, Result} ->
                    ConsumedOrgId = maps:get(<<"organization_id">>, Result, OrgId),
                    audit(
                        AdmUserId,
                        ConsumedOrgId,
                        consume,
                        #{<<"invite_id">> => maps:get(<<"invite_id">>, Result)},
                        Req0
                    ),
                    elib_response:success(
                        Req0,
                        normalize_consume(Result),
                        <<"Owner 已激活"/utf8>>
                    );
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% ------------------------------------------------------------------
%% 写：按手机号换 Owner（D13：恰一 active Owner；事务失败整体回滚）
%% ------------------------------------------------------------------

-spec transfer_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
transfer_action(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    case read_body_map(Req0) of
                        {error, Msg2} ->
                            elib_response:error(Req0, Msg2, ?ERR_BAD_REQUEST);
                        {ok, Data} ->
                            transfer_write(Req0, AdmUserId, OrgId, Data)
                    end
            end
    end;
transfer_action(_, Req0, _State) ->
    method_not_allowed(Req0).

transfer_write(Req0, AdmUserId, OrgId, Data) ->
    Mobile = maps:get(<<"owner_mobile">>, Data, maps:get(<<"mobile">>, Data, undefined)),
    case is_binary(Mobile) andalso byte_size(Mobile) > 0 of
        false ->
            elib_response:error(
                Req0, <<"owner_mobile 必填（5-20 位数字）"/utf8>>, ?ERR_BAD_REQUEST
            );
        true ->
            case
                organization_owner_activation_logic:admin_transfer_by_phone(
                    AdmUserId, OrgId, Mobile, elib_req:peer_ip(Req0)
                )
            of
                {ok, Result} ->
                    audit(
                        AdmUserId,
                        OrgId,
                        transfer_by_phone,
                        #{
                            <<"new_owner_user_id">> => maps:get(<<"owner_user_id">>, Result),
                            <<"mode">> => maps:get(<<"mode">>, Result),
                            %% 审计只落脱敏形态（D11 PII）
                            <<"owner_mobile_masked">> =>
                                case maps:get(<<"invite">>, Result) of
                                    Invite when is_map(Invite) ->
                                        maps:get(<<"mobile_masked">>, Invite, <<"****">>);
                                    _ ->
                                        imboy_mobile:mask(Mobile)
                                end
                        },
                        Req0
                    ),
                    elib_response:success(
                        Req0, normalize_transfer(Result), <<"Owner 已更换"/utf8>>
                    );
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% ===================================================================
%% 参数解析（TSID string→int）
%% ===================================================================

-spec parse_org_id(cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_org_id(Req0) ->
    case cowboy_req:binding(organization_id, Req0, <<>>) of
        Bin when is_binary(Bin), byte_size(Bin) > 0 ->
            case catch binary_to_integer(string:trim(Bin)) of
                Id when is_integer(Id), Id > 0 ->
                    {ok, Id};
                _ ->
                    {error, <<"Organization ID 格式错误"/utf8>>}
            end;
        _ ->
            {error, <<"Organization ID 不能为空"/utf8>>}
    end.

%% @doc 读 JSON body（空 body = #{}）；非 JSON body → 400。
-spec read_body_map(cowboy_req:req()) -> {ok, map()} | {error, binary()}.
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

-spec method_not_allowed(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed(Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% 出站归一化（TSID 一律 string 下发，防 JS 精度丢失；mobile 明文永不出站）
%% ===================================================================

-define(TOP_ID_KEYS, [
    <<"organization_id">>,
    <<"owner_user_id">>,
    <<"previous_owner_id">>,
    <<"invite_id">>
]).

-define(INVITE_ID_KEYS, [
    <<"invite_id">>,
    <<"organization_id">>,
    <<"owner_user_id">>
]).

normalize_status(Status) ->
    Top = elib_id:tsid_keys_to_bin(Status, [<<"organization_id">>, <<"owner_user_id">>]),
    case maps:get(<<"invite">>, Top, null) of
        Invite when is_map(Invite) ->
            Top#{<<"invite">> => elib_id:tsid_keys_to_bin(Invite, ?INVITE_ID_KEYS)};
        _ ->
            Top
    end.

normalize_rotate(#{<<"invite">> := Invite} = Result) ->
    Top = elib_id:tsid_keys_to_bin(Result, []),
    Top#{<<"invite">> => elib_id:tsid_keys_to_bin(Invite, ?INVITE_ID_KEYS)};
normalize_rotate(Result) ->
    Result.

normalize_consume(Result) ->
    elib_id:tsid_keys_to_bin(Result, [
        <<"invite_id">>, <<"organization_id">>, <<"owner_user_id">>
    ]).

normalize_transfer(Result) ->
    Top = elib_id:tsid_keys_to_bin(Result, ?TOP_ID_KEYS),
    case maps:get(<<"invite">>, Top, null) of
        Invite when is_map(Invite) ->
            Top#{<<"invite">> => elib_id:tsid_keys_to_bin(Invite, ?INVITE_ID_KEYS)};
        _ ->
            Top
    end.

%% ===================================================================
%% 审计（审计失败不阻断已完成的业务操作；手机号只落脱敏形态）
%% ===================================================================

-spec audit(
    integer(), integer(), resend | reactivate | consume | transfer_by_phone, map(), cowboy_req:req()
) ->
    ok.
audit(AdmUserId, OrgId, Action, Extra, Req0) ->
    ActionBin =
        case Action of
            resend -> <<"owner_activation_resend">>;
            reactivate -> <<"owner_activation_reactivate">>;
            consume -> <<"owner_activation_consume">>;
            transfer_by_phone -> <<"owner_transfer_by_phone">>
        end,
    Detail = maps:merge(
        #{
            <<"organization_id">> => OrgId,
            <<"action">> => ActionBin
        },
        Extra
    ),
    try
        _ = adm_operation_log_ds:insert(
            AdmUserId,
            <<"organization_", ActionBin/binary>>,
            OrgId,
            <<"organization">>,
            Detail,
            elib_req:peer_ip(Req0)
        ),
        ok
    catch
        Class:Reason:Stacktrace ->
            ?DEBUG_LOG("owner activation audit failed: ~p", [{Class, Reason, Stacktrace}]),
            ok
    end.
