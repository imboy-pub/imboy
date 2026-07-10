-module(agent_card_handler).

%%%
% Phase 4 T4.1：Agent 能力发现端点 / Agent capability discovery
%
% GET /api/v1/agent-card —— 供外部 AI 发现本 imboy 实例的 agent 能力目录：
% 身份 + MCP 端点 + 插件 manifest 声明的 MCP tools（name/description）。
% 完整 tool schema 经 MCP 协议 /api/v1/mcp 的 tools/list 获取（Phase 3）。
%
% B 路（见 phase4-spike-gonogo-decision）：不采用完整 A2A Agent Card spec，
% 只做 imboy 原生精简能力描述。build→ETag→200 模式复用 app_manifest_handler。
%%%

-behavior(cowboy_rest).

-export([init/2]).
-export([build_card/0, compute_etag/1]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            card -> card(cowboy_req:method(Req0), Req0);
            _ -> Req0
        end,
    {ok, Req1, State}.

-spec card(binary(), cowboy_req:req()) -> cowboy_req:req().
card(<<"GET">>, Req0) ->
    Card = build_card(),
    Etag = compute_etag(Card),
    case cowboy_req:header(<<"if-none-match">>, Req0) of
        Etag when Etag =/= undefined ->
            cowboy_req:reply(304, #{<<"etag">> => Etag}, <<>>, Req0);
        _ ->
            Body = jsone:encode(Card, [native_utf8]),
            cowboy_req:reply(
                200,
                #{
                    <<"content-type">> => <<"application/json; charset=utf-8">>,
                    <<"etag">> => Etag,
                    <<"cache-control">> => <<"max-age=30">>
                },
                Body,
                Req0
            )
    end;
card(_, Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec build_card() -> map().
build_card() ->
    #{
        <<"name">> => <<"imboy">>,
        <<"protocol">> => <<"mcp">>,
        <<"mcp_endpoint">> => <<"/api/v1/mcp">>,
        <<"plugin_tools">> => plugin_tools(),
        <<"generated_at">> => erlang:system_time(millisecond)
    }.

-spec plugin_tools() -> [map()].
plugin_tools() ->
    [
        #{
            <<"name">> => maps:get(name, T),
            <<"description">> => maps:get(description, T, <<>>)
        }
     || T <- imboy_plugin_registry:mcp_tool_declarations(), is_map(T), is_map_key(name, T)
    ].

-spec compute_etag(map()) -> binary().
compute_etag(Card) ->
    Bin = jsone:encode(Card, [native_utf8]),
    <<"\"", (integer_to_binary(erlang:phash2(Bin)))/binary, "\"">>.
