-module(agent_hub_fake_llm).

-export([chat/3]).

chat(_Uid, Messages, Opts) ->
    Expected = maps:get(expected_prompt, Opts),
    Reply = maps:get(reply, Opts),
    case lists:last(Messages) of
        #{<<"role">> := <<"user">>, <<"content">> := Expected} ->
            {ok, #{<<"result">> => Reply}};
        _ ->
            {error, unexpected_prompt}
    end.
