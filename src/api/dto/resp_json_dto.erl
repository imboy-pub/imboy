-module (resp_json_dto).
%%%
% resp_json_dto 是 response json data transfer object 缩写
%%%
-export ([success/4, success/1, success/2, success/3]).
-export ([error/4, error/1, error/2, error/3]).

success(Req) ->
    reply_json(0, "success", [], Req).

success(Req, Data) ->
    reply_json(0, "success", Data, Req).

success(Req, Data, Msg) ->
    reply_json(0, Msg, Data, Req).

success(Req, Data, Msg, Options) ->
    reply_json(0, Msg, Data, Req, Options).

error(Req) ->
    reply_json(1, "error", [], Req).

error(Req, Msg) ->
    reply_json(1, Msg, [], Req).

error(Req, Msg, Code) ->
    reply_json(Code, Msg, [], Req).

error(Req, Msg, Code, Options) ->
    reply_json(Code, Msg, [], Req, Options).


%%%%% 私有的 %%%%%
reply_json(Code, Msg, Data, Req) ->
    LData = [
        {<<"code">>, Code}
        ,{<<"msg">>, unicode:characters_to_binary(Msg)}
        ,{<<"data">>, Data}
    ],
    Body = jsx:encode(LData),
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req
    ).

reply_json(Code, Msg, Data, Req, Options) ->
    LData = [
        {<<"code">>, Code}
        ,{<<"msg">>, unicode:characters_to_binary(Msg)}
        ,{<<"data">>, Data}
    ],
    Body = jsx:encode(LData ++ Options),
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req
    ).
