-module(micro_community_join_handler).
-export([init/2]).

init(Req, State) ->
    Result =
        case cowboy_req:method(Req) of
            <<"POST">> ->
                micro_community_access_logic:join(
                    maps:get(current_uid, State, 0),
                    cowboy_req:binding(channel_id, Req)
                );
            _ ->
                {error, {405, <<"Method not allowed">>}}
        end,
    Reply =
        case Result of
            {ok, Data} -> elib_response:success(Req, Data);
            {error, {Code, Msg}} -> elib_response:error(Req, Msg, Code)
        end,
    {ok, Reply, State}.
