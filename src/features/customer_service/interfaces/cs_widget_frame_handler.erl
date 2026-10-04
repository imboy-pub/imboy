%%% @doc Widget 动态 frame HTML 端点（BE-W01 A05，contracts frame_policy）。
%%%
%%% `GET /api/v1/cs/widget/frame/:installation_id?organization_id=...`：
%%% 按 public widget installation 返回嵌入文档（iframe src 的落点）——
%%%
%%%   * `Content-Security-Policy: frame-ancestors <origin...>` 按 installation
%%%     allowed_origins **逐个列出**（空 allowlist = `'none'`，全拒）；宿主页
%%%     只有列名在册才被浏览器允许嵌入；
%%%   * **不继承 X-Frame-Options DENY/SAMEORIGIN**（security_headers_middleware
%%%     与 cors_middleware 对本路径豁免 XFO；其余安全头 nosniff/no-store 照常）；
%%%   * 引用**版本化**静态 JS（路径常量 `?FRAME_ASSET_JS`，产物部署属 A6）；
%%%   * installation 不存在 / 已吊销（kill switch）→ 404；方法非 GET → 405；
%%%     installation_id / organization_id 非法 → 400；
%%%   * 查询串出现凭证样式键即 400（本端点无凭证面，防御纵深——
%%%     `cs_http:credential_in_query_string/1`）；
%%%   * 纯公开数据（installation_id / organization_id），URL/响应零 secret、
%%%     零 visit token。
%%%
%%% 分层纪律与 cs_widget_handler 同源：只经 `cs_facade_call` 进 application
%%% （禁直连 DB / 禁 apply / 禁 crypto）；TSID 出站 string。
%%%
%%% **路由注册不在本卡**：imboy_router 由持租约者按
%%% RUN_ROOT/artifacts/backend/be-w01-router-wiring-manifest.md 统一应用。
-module(cs_widget_frame_handler).

-moduledoc "Widget 动态 frame HTML 端点（BE-W01 A05，contracts frame_policy）。".
-export([init/2, handle/2]).

%% 帧文档构造纯函数（导出供套件零 socket 断言）。
-export([frame_document/2, frame_ancestors_csp/1]).

%% 版本化静态 JS 路径常量（部署产物挂载属 A6；升级 = 改此常量 + 产物同名）。
-define(FRAME_ASSET_JS, <<"/widget-assets/cs-widget.v1.js">>).

%% cowboy 普通 handler：State = route Opts。
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Req = handle(Req0, State0),
    {ok, Req, State0}.

-spec handle(cowboy_req:req(), map()) -> cowboy_req:req().
handle(Req0, _State0) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            dispatch(Req0);
        _Other ->
            cs_http:reply_error(Req0, method_not_allowed)
    end.

dispatch(Req0) ->
    case cs_http:credential_in_query_string(Req0) of
        true ->
            cs_http:reply_error(Req0, credential_in_query_string);
        false ->
            case org_id(Req0) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, OrgId} ->
                    case installation_id(Req0) of
                        {error, Reason} ->
                            cs_http:reply_error(Req0, Reason);
                        {ok, InstallationId} ->
                            invoke(Req0, OrgId, InstallationId)
                    end
            end
    end.

%% organization_id 是公开申报值（非凭证）：正确性由 store 的同语句
%% (Org, installation) 命中证明——错 Org 查不到行（not_found，无枚举）。
org_id(Req) ->
    Qs = cowboy_req:parse_qs(Req),
    case proplists:get_value(<<"organization_id">>, Qs) of
        undefined ->
            {error, missing_org_id};
        Raw ->
            case tsid(Raw) of
                {ok, Id} -> {ok, Id};
                error -> {error, invalid_org_id}
            end
    end.

installation_id(Req) ->
    case cowboy_req:binding(installation_id, Req) of
        undefined ->
            {error, {missing_path_param, installation_id}};
        Raw ->
            case tsid(Raw) of
                {ok, Id} -> {ok, Id};
                error -> {error, invalid_tsid}
            end
    end.

%% TSID 十进制字符串（与 cs_http:tsid/1 同口径；该函数未导出，此处直用
%% elib_tsid:from_binary/1 保持一致判据）。
tsid(Raw) when is_binary(Raw) ->
    elib_tsid:from_binary(Raw);
tsid(_Raw) ->
    error.

invoke(Req0, OrgId, InstallationId) ->
    Result = cs_facade_call:call(
        widget_frame_html, OrgId, #{installation_id => InstallationId}
    ),
    case Result of
        {ok, Installation} ->
            reply_html(Req0, Installation);
        {error, not_found} ->
            cs_http:reply_error(Req0, {not_found, installation});
        {error, installation_revoked} ->
            %% 冻结合同：frame 面对 revoked 一律 404（kill switch 不显形）。
            cs_http:reply_error(Req0, {not_found, installation});
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason)
    end.

reply_html(Req0, Installation) ->
    Origins = maps:get(allowed_origins, Installation, []),
    PublicWidgetId = maps:get(public_widget_id, Installation, <<>>),
    InstallationId = maps:get(id, Installation, 0),
    Headers = #{
        <<"content-type">> => <<"text/html; charset=utf-8">>,
        %% 嵌入策略唯一真源：逐 origin 列名（或 'none'）；XFO 已由中间件豁免。
        <<"content-security-policy">> => frame_ancestors_csp(Origins),
        %% 帧文档可被中间 CDN/代理短缓存，但默认跟随全局 no-store 口径，
        %% revocation 立即生效优先。
        <<"cache-control">> => <<"no-store">>
    },
    Body = frame_document(InstallationId, PublicWidgetId),
    cowboy_req:reply(200, Headers, Body, Req0).

%% @doc frame-ancestors 指令：allowlist 逐个列出（已归一化的 origin）；
%% 空名单 = `'none'`（任何宿主页都不许嵌）。
-spec frame_ancestors_csp([binary()]) -> binary().
frame_ancestors_csp([]) ->
    <<"frame-ancestors 'none'">>;
frame_ancestors_csp(Origins) ->
    List = lists:join(<<" ">>, [O || O <- Origins, is_binary(O), O =/= <<>>]),
    <<"frame-ancestors ", (iolist_to_binary(List))/binary>>.

%% @doc 帧文档：最小挂载点 + 版本化 JS 引用；属性值 HTML 转义。
-spec frame_document(integer() | binary(), binary()) -> binary().
frame_document(InstallationId, PublicWidgetId) when
    is_integer(InstallationId); is_binary(InstallationId)
->
    IdAttr = html_attr(value_to_binary(InstallationId)),
    PubAttr = html_attr(PublicWidgetId),
    <<
        "<!DOCTYPE html>"
        "<html lang=\"en\">"
        "<head>"
        "<meta charset=\"utf-8\">"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        "<title>Customer service</title>"
        "</head>"
        "<body>"
        "<div id=\"cs-widget-root\""
        " data-installation-id=\"",
        IdAttr/binary,
        "\" data-public-widget-id=\"",
        PubAttr/binary,
        "\"></div>"
        "<script src=\"",
        ?FRAME_ASSET_JS/binary,
        "\" defer></script>"
        "</body>"
        "</html>"
    >>;
frame_document(_InstallationId, _PublicWidgetId) ->
    <<>>.

value_to_binary(V) when is_integer(V) ->
    integer_to_binary(V);
value_to_binary(V) when is_binary(V) ->
    V;
value_to_binary(_) ->
    <<>>.

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
