-module(moya_subscribe_handler).
%%%
% 墨芽订阅消息授权上报 HTTP 适配层
% POST /api/v1/moya/subscribe/report   上报用户在微信小程序
%                                     requestSubscribeMessage 中 accept 的模板
%
% 客户端只上报「accept」的模板 ID；未选/拒绝的模板不上报（服务端按全量
% 授权-消费模型处理，见 moya_subscribe_logic）。
% 错误映射（错误码 5488 见 error_code.hrl 墨芽教学域 5400-5519）：
%   template_ids 缺失/非数组/元素非 binary/元素空串或超 64 字节
%                        → HTTP 422 + 5488（域码：模板列表非法；空数组放行 → count:0）
%   invalid_templates     → HTTP 422 + 5488（logic 二次校验不通过，同域码）
%   其他（db 类）         → HTTP 500 + 1（通用错误）
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
handle_action(report, Req, State) -> report(Req, State);
handle_action(false, Req, _State) -> Req.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec report(cowboy_req:req(), map()) -> cowboy_req:req().
report(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case template_ids(elib_param:post(Req0)) of
        {ok, TemplateIds} ->
            case moya_subscribe_logic:report(Uid, TemplateIds) of
                {ok, Count} ->
                    elib_response:success_rfc3339(
                        Req0,
                        #{<<"count">> => Count},
                        <<"已记录订阅授权"/utf8>>
                    );
                {error, invalid_templates} ->
                    templates_invalid(Req0);
                {error, _Other} ->
                    elib_response:error_with_status(
                        Req0, 500, <<"操作失败，请重试"/utf8>>, ?ERR_ERROR
                    )
            end;
        error ->
            templates_invalid(Req0)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% template_ids 契约：数组，元素全为非空 binary 且单个 ≤64 字节
%% （微信模板 ID 形如 "AbCdEf...-...-..."，64 字节余量充足）。
%% 越界形态（缺失/非数组/非字符串/空串/超长）一律 422 + 5488，
%% 绝不把脏数据下发 logic（deny-by-default）。
%% **空数组放行**（→ logic 返回 {ok, 0}）：宽容语义与 logic 白名单过滤一致
%% ——服务端未配置模板时任何上报都落 count:0，空列表不该比"全不在白名单"
%% 更严苛；客户端侧本就不会发空列表（accepted 为空前置 return）。
-spec template_ids(map() | list()) -> {ok, [binary()]} | error.
template_ids(Body) when is_map(Body) ->
    case maps:get(<<"template_ids">>, Body, undefined) of
        Ids when is_list(Ids) ->
            case lists:all(fun valid_template_id/1, Ids) of
                true -> {ok, Ids};
                false -> error
            end;
        _ ->
            error
    end;
template_ids(_) ->
    error.

-spec valid_template_id(term()) -> boolean().
valid_template_id(Bin) when is_binary(Bin), Bin =/= <<>> ->
    byte_size(Bin) =< 64;
valid_template_id(_) ->
    false.

-spec templates_invalid(cowboy_req:req()) -> cowboy_req:req().
templates_invalid(Req) ->
    elib_response:error_with_status(
        Req,
        422,
        <<"订阅消息模板列表非法"/utf8>>,
        ?ERR_SUBSCRIBE_TEMPLATES_INVALID
    ).
