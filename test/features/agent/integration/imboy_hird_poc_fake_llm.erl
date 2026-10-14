%%% @doc PoC fake LLM provider（实现 imboy_llm behaviour；响应脚本经
%%% Opts 的 resp 键注入——零真实调用、零密钥，仅离线测试用）。
-module(imboy_hird_poc_fake_llm).

-behaviour(imboy_llm).

-export([chat/3, capabilities/0]).

chat(_Uid, _Messages, #{resp := Resp}) ->
    {ok, Resp};
chat(_Uid, _Messages, _Opts) ->
    {error, no_script}.

capabilities() ->
    #{stream => false, vision => false, tools => false}.
