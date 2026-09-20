%%% @doc cors_face_tests 的回声 handler（test-only）：200 空体，方法不限。
%%% 只为让 cors/security_headers 中间件链有终点；不做任何业务。
-module(cors_face_echo_handler).

-export([init/2]).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, State) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"ok">>, Req0),
    {ok, Req, State}.
