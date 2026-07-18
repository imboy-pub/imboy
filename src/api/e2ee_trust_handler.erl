-module(e2ee_trust_handler).
%%%
%% e2ee_trust_handler — 设备信任决策事件 HTTP 适配层（ADR 06 §8）。
%%
%% thin adapter：capability gate (e2ee_enabled) + 参数解析 + 调 logic。
%% 服务端零算法（ADR 06 §8.1）：只记录带签事件 + 广播，不判 verification。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").
-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            record ->
                record(Req0, State);
            _ ->
                elib_response:error(Req0, <<"not_found">>, 404)
        end,
    {ok, Req1, State}.

%% ===================================================================
%% POST /api/v1/e2ee/trust/record — 记录一条信任决策事件
%% Body: {actor_device_id, target_uid, target_device_id, target_ed25519,
%%        from_state, to_state, method, ts, actor_signature}
%% ===================================================================

-spec record(cowboy_req:req(), map()) -> cowboy_req:req().
record(Req0, State) ->
    case imboy_policy:e2ee_enabled() of
        false ->
            elib_response:error(
                Req0, imboy_error:error_msg(?ERR_FEATURE_DISABLED), ?ERR_FEATURE_DISABLED
            );
        true ->
            do_record(Req0, State)
    end.

-spec do_record(cowboy_req:req(), map()) -> cowboy_req:req().
do_record(Req0, State) ->
    ActorUid = auth_ds:current_uid(State),
    P = elib_param:post(Req0),
    TargetUid = elib_cnv:safe_to_integer(maps:get(<<"target_uid">>, P, <<"">>)),
    case is_integer(TargetUid) andalso TargetUid > 0 of
        false ->
            elib_response:error(Req0, <<"bad_request">>, 400);
        true ->
            Result = e2ee_trust_logic:record_trust_event(
                ActorUid,
                maps:get(<<"actor_device_id">>, P, <<>>),
                TargetUid,
                maps:get(<<"target_device_id">>, P, <<>>),
                maps:get(<<"target_ed25519">>, P, <<>>),
                maps:get(<<"from_state">>, P, <<>>),
                maps:get(<<"to_state">>, P, <<>>),
                maps:get(<<"method">>, P, <<>>),
                to_bin(maps:get(<<"ts">>, P, <<>>)),
                maps:get(<<"actor_signature">>, P, <<>>)
            ),
            case Result of
                ok ->
                    elib_response:success(Req0, #{<<"success">> => true});
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end
    end.

%% ts 可能以数字或字符串传入，统一为 binary（纳入签名负载须与客户端一致）
-spec to_bin(term()) -> binary().
to_bin(B) when is_binary(B) -> B;
to_bin(I) when is_integer(I) -> integer_to_binary(I);
to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(_) -> <<>>.
