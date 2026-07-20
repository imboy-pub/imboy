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
%% POST /api/v1/e2ee/trust/record — 记录一条信任决策事件（ADR 16 §3.3.1）
%% Body: {actor_device_id, target_uid, target_device_id, target_ed25519,
%%        from_state, to_state, method, event_id, issued_at, expires_at,
%%        actor_device_generation, target_identity_version, actor_signature}
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
    %% 数值字段透传给 logic 统一规范化（binary/integer 皆可），logic 做完整校验
    Fields = #{
        <<"actor_device_id">> => maps:get(<<"actor_device_id">>, P, <<>>),
        <<"target_uid">> => maps:get(<<"target_uid">>, P, <<>>),
        <<"target_device_id">> => maps:get(<<"target_device_id">>, P, <<>>),
        <<"target_ed25519">> => maps:get(<<"target_ed25519">>, P, <<>>),
        <<"from_state">> => maps:get(<<"from_state">>, P, <<>>),
        <<"to_state">> => maps:get(<<"to_state">>, P, <<>>),
        <<"method">> => maps:get(<<"method">>, P, <<>>),
        <<"event_id">> => maps:get(<<"event_id">>, P, <<>>),
        <<"issued_at">> => maps:get(<<"issued_at">>, P, <<>>),
        <<"expires_at">> => maps:get(<<"expires_at">>, P, <<>>),
        <<"actor_device_generation">> => maps:get(<<"actor_device_generation">>, P, <<>>),
        <<"target_identity_version">> => maps:get(<<"target_identity_version">>, P, <<>>),
        <<"actor_signature">> => maps:get(<<"actor_signature">>, P, <<>>)
    },
    case e2ee_trust_logic:record_trust_event(ActorUid, Fields) of
        ok ->
            elib_response:success(Req0, #{<<"success">> => true});
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
    end.
