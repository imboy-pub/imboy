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
-export([build_card/0, build_well_known_card/0, compute_etag/1]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            card -> serve(cowboy_req:method(Req0), build_card(), Req0);
            %% Phase 4 T4.1：标准 A2A 发现端点，挂载于 /.well-known/agent.json（无需认证）
            well_known -> serve(cowboy_req:method(Req0), build_well_known_card(), Req0);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% GET 才返回；命中 if-none-match 走 304，否则 200 + ETag。其余方法 405。
-spec serve(binary(), map(), cowboy_req:req()) -> cowboy_req:req().
serve(<<"GET">>, Card, Req0) ->
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
serve(_, _Card, Req0) ->
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

%% @doc Phase 4 T4.1：标准 A2A 风格 agent card（挂载 /.well-known/agent.json）。
%% 复用 build_card/0 的 mcp_endpoint 与 plugin_tools（后者同时以 skills 暴露），
%% 叠加 A2A 规范字段 name/description/url/version/capabilities，并汇总插件声明的 a2a_agent_card。
-spec build_well_known_card() -> map().
build_well_known_card() ->
    Base = build_card(),
    #{
        <<"name">> => <<"imboy">>,
        <<"description">> => <<"imboy 即时通讯平台 Agent 能力发现端点"/utf8>>,
        <<"url">> => <<"/.well-known/agent.json">>,
        <<"version">> => version(),
        <<"capabilities">> => #{
            <<"mcp">> => true,
            <<"streaming">> => false,
            <<"pushNotifications">> => false
        },
        <<"mcp_endpoint">> => maps:get(<<"mcp_endpoint">>, Base),
        <<"skills">> => maps:get(<<"plugin_tools">>, Base),
        <<"plugin_tools">> => maps:get(<<"plugin_tools">>, Base),
        <<"a2a_agent_cards">> => imboy_plugin_registry:a2a_agent_cards(),
        <<"generated_at">> => maps:get(<<"generated_at">>, Base)
    }.

%% 版本取自 OTP application vsn，避免硬编码。
-spec version() -> binary().
version() ->
    case application:get_key(imboy, vsn) of
        {ok, V} when is_list(V) -> list_to_binary(V);
        _ -> <<"unknown">>
    end.

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
