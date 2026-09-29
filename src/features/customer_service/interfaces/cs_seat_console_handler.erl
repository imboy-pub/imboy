%%% @doc 坐席控制台嵌入面的薄 Handler（seat-console-embed SC-BE）。
%%%
%%% 与 `cs_widget_handler` 的 public_frame 同职责同顺序（方法门 → 传输守卫 →
%%% 形状门 → facade），差异只在三条接入面纪律：
%%%
%%%   1. **零凭证导航面**：iframe src 落点，不读任何头凭证——查询串出现凭证
%%%      样式键即 400（`cs_http:credential_in_query_string/1`，值不读、不解析）；
%%%   2. **租户归属派生**：public_seat_console_id 全局反查的命中行权威派生
%%%      （facade OrgId=0 同构占位），HTML 壳零 org/workspace/secret/token；
%%%   3. **嵌入策略唯一真源**：CSP 逐 origin 列名（或 'none'）——复用
%%%      `cs_widget_frame_handler:frame_ancestors_csp/1` 纯函数；XFO 已由
%%%      共享形状谓词 `imboy_route_shape:is_cs_seat_console_frame_path/1` 豁免。
%%%
%%% 错误统一 404 `seat_console_unavailable`（missing/revoked 三态不区分，
%%% 无存在性枚举）；路径绑定形状非法 400。**本模块不做**：不读库、不写 SQL、
%%% 不做业务判定、不签发任何 URL。
-module(cs_seat_console_handler).

-export([init/2, handle/2]).

%% 帧文档构造纯函数（导出仅供套件零 socket 断言）。
-export([frame_document/1, frame_csp/1]).

%% 版本化静态资产路径常量（跨仓配对合同：priv/cs_widget_asset_pairing.json
%% 的 seat_console 面；升级 = 改此常量 + 产物同名 + 配对表同步）。
-define(SEAT_FRAME_ASSET_JS, <<"/seat-assets/cs-seat.v1.js">>).

%% CSP 输出的渲染防线：每个 origin 必须匹配 ^https?://[a-z0-9._:-]+$
%% （归一化输出的合法字符集超集收紧版——CRLF/引号/空白/通配形态即便因存量
%% 数据绕过写入门也进不了响应头；这是预发射守卫，不是校验真源）。
-define(CSP_ORIGIN_RE, <<"^https?://[a-z0-9._:-]+$">>).

%% cowboy 普通 handler：State = route Opts（含 route metadata + 中间件会话键）。
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Req = handle(Req0, State0),
    {ok, Req, State0}.

-spec handle(cowboy_req:req(), map()) -> cowboy_req:req().
handle(Req0, _State0) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            case cs_http:credential_in_query_string(Req0) of
                true ->
                    cs_http:reply_error(Req0, credential_in_query_string);
                false ->
                    frame_binding(Req0)
            end;
        _Other ->
            cs_http:reply_error(Req0, method_not_allowed)
    end.

%% 路径绑定形状门：非空、≤128、[A-Za-z0-9_-]（与
%% cs_seat_console:valid_public_seat_console_id/1 同口径——接口层先拒 400，
%% application 层再守 422，纵深防御不互信）。
frame_binding(Req0) ->
    case cowboy_req:binding(public_seat_console_id, Req0) of
        undefined ->
            cs_http:reply_error(Req0, {missing_path_param, public_seat_console_id});
        Raw when is_binary(Raw) ->
            case cs_seat_console:valid_public_seat_console_id(Raw) of
                true ->
                    %% OrgId=0 是 facade_call 同构占位：反查面无 Org 输入（命中
                    %% 行派生）。
                    Result =
                        cs_facade_call:call(
                            seat_console_frame_html,
                            0,
                            #{public_seat_console_id => Raw}
                        ),
                    frame_respond(Req0, Result);
                false ->
                    cs_http:reply_error(Req0, invalid_public_seat_console_id)
            end;
        _ ->
            cs_http:reply_error(Req0, invalid_public_seat_console_id)
    end.

frame_respond(Req0, {ok, Projection}) ->
    Origins = renderable_origins(maps:get(allowed_origins, Projection, [])),
    PublicId = maps:get(public_seat_console_id, Projection, <<>>),
    Headers = #{
        <<"content-type">> => <<"text/html; charset=utf-8">>,
        %% 嵌入策略唯一真源：逐 origin 列名（或 'none'）——复用 widget frame 的
        %% 同一纯函数（frame_ancestors_csp/1），XFO 已由共享形状谓词豁免。
        <<"content-security-policy">> => frame_csp(Origins),
        <<"cache-control">> => <<"no-store">>,
        <<"referrer-policy">> => <<"no-referrer">>
    },
    cowboy_req:reply(200, Headers, frame_document(PublicId), Req0);
frame_respond(Req0, {error, Reason}) ->
    cs_http:reply_error(Req0, Reason).

%% @doc frame-ancestors 指令：allowlist 逐个列出（已归一化 origin）；
%% 空名单 = `'none'`（任何宿主页都不许嵌）。渲染前每项过 CSP_ORIGIN_RE
%% 预发射守卫（CRLF 防线：不合法形状的条目被丢弃而不是进头）。
-spec frame_csp([binary()]) -> binary().
frame_csp([]) ->
    <<"frame-ancestors 'none'">>;
frame_csp(Origins) ->
    case renderable_origins(Origins) of
        [] -> <<"frame-ancestors 'none'">>;
        Safe -> cs_widget_frame_handler:frame_ancestors_csp(Safe)
    end.

renderable_origins(Origins) when is_list(Origins) ->
    [O || O <- Origins, is_binary(O), renderable_origin(O)];
renderable_origins(_Other) ->
    [].

renderable_origin(O) ->
    case re:run(O, ?CSP_ORIGIN_RE, [{capture, none}]) of
        match -> true;
        nomatch -> false
    end.

%% @doc /seat/ 帧文档（合同冻结形状）：`<title>IMBoy 客服工作台</title>` +
%% robots noindex + referrer no-referrer + 最小挂载点
%% `div#cs-seat-root` + `data-public-seat-console-id`（HTML 转义）+ 版本化
%% 样式与脚本。**不输出** organization_id / workspace_id / JWT / secret /
%% api-base（合同级禁令：壳内零租户键）。
-spec frame_document(binary()) -> binary().
frame_document(PublicId) when is_binary(PublicId) ->
    PubAttr = html_attr(PublicId),
    <<
        "<!DOCTYPE html>"
        "<html lang=\"zh-CN\">"
        "<head>"
        "<meta charset=\"utf-8\">"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        "<meta name=\"robots\" content=\"noindex\">"
        "<meta name=\"referrer\" content=\"no-referrer\">"
        "<title>IMBoy 客服工作台</title>"/utf8,
        "<link rel=\"stylesheet\" href=\"/seat-assets/cs-seat.v1.css\">"
        "</head>"
        "<body>"
        "<div id=\"cs-seat-root\" data-public-seat-console-id=\"",
        PubAttr/binary,
        "\"></div>"
        "<script src=\"",
        ?SEAT_FRAME_ASSET_JS/binary,
        "\" defer></script>"
        "</body>"
        "</html>"
    >>;
frame_document(_PublicId) ->
    <<>>.

%% HTML 属性转义（与 cs_widget_frame_handler:html_attr/1 同口径；该函数未导出，
%% 本面按同一四元集独立实现）。
html_attr(Bin) when is_binary(Bin) ->
    binary:replace(
        binary:replace(
            binary:replace(
                binary:replace(Bin, <<"&">>, <<"&amp;">>, [global]),
                <<"<">>,
                <<"&lt;">>,
                [global]
            ),
            <<">">>,
            <<"&gt;">>,
            [global]
        ),
        <<"\"">>,
        <<"&quot;">>,
        [global]
    );
html_attr(_) ->
    <<>>.
