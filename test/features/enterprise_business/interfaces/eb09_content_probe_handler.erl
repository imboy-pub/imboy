%%% @doc EB-09 测试专用 handler：只为让**生产侧**的 content 响应构造器
%%% (`eb_enterprise_http:reply_content/2`) 在真 socket 上产生真实响应字节，
%%% 供 A05 的「响应不含存储能力」扫描使用。无业务逻辑、无路由登记、不进 release。
-module(eb09_content_probe_handler).

-export([init/2]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State) ->
    View = persistent_term:get({eb09_content_probe, view}),
    Req = eb_enterprise_http:reply_content(Req0, View),
    {ok, Req, State}.
