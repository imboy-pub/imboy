-module(adm_bot_delivery_handler).

%%%
% WH-01：Bot 出站交付死信重放（管理员，单次重放）。
% POST /api/adm/bot/deliveries/replay  {delivery_id}
% 重放把 dead 行转回 pending（delivery_id 不变，attempt 续号），worker 拉取重投。
%%%

-export([init/2]).

-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            replay -> replay(<<"POST">>, Req0, State);
            list -> list(Req0);
            _ -> Req0
        end,
    {ok, Req1, State}.

list(Req0) ->
    Q = cowboy_req:parse_qs(Req0),
    Page =
        case lists:keyfind(<<"page">>, 1, Q) of
            {_, V} -> elib_cnv:safe_to_integer(V);
            false -> 1
        end,
    case bot_webhook_delivery_repo:list_dead(Page, 20) of
        {ok, P} -> elib_response:success(Req0, P);
        {error, _} -> elib_response:error(Req0, <<"读取失败"/utf8>>)
    end.

replay(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"mcp_clients:approve">>, Req0) of
        ok ->
            Post =
                try elib_param:post(Req0) of
                    M when is_map(M) -> M;
                    _ -> #{}
                catch
                    _:_ -> #{}
                end,
            Did = maps:get(<<"delivery_id">>, Post, maps:get(<<"deliveryId">>, Post, <<>>)),
            case bot_webhook_delivery_logic:replay_dead(Did) of
                {ok, reused_delivery} ->
                    elib_response:success(Req0, #{
                        <<"delivery_id">> => Did,
                        <<"status">> => <<"pending">>
                    });
                {error, not_dead} ->
                    elib_response:error(Req0, <<"仅死信可重放"/utf8>>, ?ERR_BAD_REQUEST);
                {error, notfound} ->
                    elib_response:error(Req0, <<"交付不存在"/utf8>>, ?ERR_NOT_FOUND);
                {error, _} ->
                    elib_response:error(Req0, <<"重放失败"/utf8>>)
            end;
        {error, Req1} ->
            Req1
    end;
replay(_, Req0, _State) ->
    elib_response:error(Req0, <<"Method Not Allowed">>).
