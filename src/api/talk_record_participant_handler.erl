-module(talk_record_participant_handler).
-moduledoc "受JWT保护的参与者GET/PUT；复用记录ETag和领域错误信封。".
-export([init/2]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req, State) ->
    Response =
        case cowboy_req:method(Req) of
            <<"GET">> -> dispatch(get, Req, State);
            <<"PUT">> -> dispatch(put, Req, State);
            _ -> talk_record_http:method_not_allowed(Req)
        end,
    {ok, Response, State}.

dispatch(Action, Req, State) ->
    case talk_record_http:path_id(Req) of
        {ok, Rid} ->
            Uid = maps:get(current_uid, State, 0),
            Result =
                case Action of
                    get ->
                        talk_record_participant_logic:list(Uid, Rid);
                    put ->
                        talk_record_participant_logic:replace(
                            Uid,
                            Rid,
                            cowboy_req:header(<<"if-match">>, Req),
                            elib_param:post(Req)
                        )
                end,
            case Result of
                {ok, #{<<"revision">> := Rev} = Payload} ->
                    Response = cowboy_req:set_resp_header(
                        <<"etag">>, talk_record_ds:etag(Rev), Req
                    ),
                    elib_response:success(Response, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req, Reason)
            end;
        {error, R} ->
            talk_record_http:respond_error(Req, R)
    end.
