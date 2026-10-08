-module(workspace_attachment_handler).
-export([init/2]).

init(Req, State) ->
    Reply =
        case cowboy_req:method(Req) of
            <<"GET">> -> search(Req, State);
            _ -> elib_response:error(Req, <<"Method not allowed">>, 405)
        end,
    {ok, Reply, State}.

search(Req, State) ->
    Id = elib_cnv:safe_to_integer(cowboy_req:binding(workspace_id, Req)),
    case is_integer(Id) andalso Id > 0 of
        false ->
            elib_response:error(Req, <<"Invalid workspace">>, 400);
        true ->
            Input = maps:from_list(cowboy_req:parse_qs(Req)),
            case action(Req, auth_ds:current_uid(State), Id, Input) of
                {ok, Result} -> elib_response:success(Req, Result);
                {error, {Code, Message}} -> elib_response:error(Req, Message, Code)
            end
    end.

action(Req, Uid, Ws, Input) ->
    case cowboy_req:binding(attachment_id, Req) of
        undefined ->
            workspace_attachment_logic:search(Uid, Ws, Input);
        Raw ->
            case elib_cnv:safe_to_integer(Raw) of
                Id when is_integer(Id), Id > 0 ->
                    case workspace_attachment_logic:open(Uid, Ws, Id) of
                        {ok, Url} ->
                            {ok, #{url => Url}};
                        {error, {Code, Message}} when is_integer(Code), is_binary(Message) ->
                            {error, {Code, Message}};
                        {error, _} ->
                            {error, {404, <<"Attachment unavailable">>}}
                    end;
                _ ->
                    {error, {400, <<"Invalid attachment">>}}
            end
    end.
