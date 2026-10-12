-module(org_invite_landing_handler).
-moduledoc "组织邀请 H5 落地页（APP-M02）：GET /invite/org/:code。".
%%%===================================================================
%%% @doc 组织邀请 H5 落地页 `GET /invite/org/:code`（APP-M02 邀请同事）。
%%%
%%% 为什么存在：微信聊天内 `imboy://` 私有 scheme 不可点，分享文案必须带
%%% https 链接才有「点一下」的路径。本页由 :code 派生全部内容——
%%% **零 DB 查询、零凭证**（auth_middleware 对 /invite/ 前缀直通）：
%%%   * 邀请码本身就是凭证（8 位 A-Z2-9、7 天 TTL、一组织至多一个
%%%     active 码）；页面只做「展示 + 复制 + 尝试拉起 App」，**不做**
%%%     code → 组织反查——避免给公网增加一个按码枚举企业名的查询面；
%%%     组织名语境由分享文案本身携带；
%%%   * 不渲染除 :code 以外的任何请求输入，且 :code 先过形状门
%%%     （valid_code/1），非法一律 404 静态文案——零反射面；
%%%   * 文案现阶段固定 zh-CN（企业同事场景主市场），多语言待真实诉求。
%%%
%%% 合同：与 App 端 `parseOrgJoinDeepLink` 共用 `imboy://org/join?c=CODE`
%%% 深链契约（org_invite_qr_page / scanner_page 同款 payload）；分享文案由
%%% imboyapp `buildInviteShareText` 拼装 `https://imboy.pub/invite/org/CODE`
%%% （webBaseUrl + 本路由；nginx www.imboy.pub vhost `location ^~ /invite/`
%%% 转发到后端）。
%%% @end
%%%===================================================================

-export([init/2]).
%% 纯函数：EUnit 合同测试与 init/2 共用同一判据。
-export([valid_code/1, landing_html/1, invalid_html/0]).

%% 与 organization_invite_code_pg:?INVITE_CHARSET 同口径（8 位 A-Z2-9，
%% 排除 0/O/1/I 手抄混淆字符）——接口层形状门与生成器纵深不互信；
%% 生成器改字符集/长度时必须同步此处，否则有效码会被 404。
-define(INVITE_CODE_LEN, 8).
-define(INVITE_CHARS, "ABCDEFGHJKLMNPQRSTUVWXYZ23456789").

%% @doc 落地页统一响应头。no-store：码 7 天过期，任何中间层缓存旧页都会
%%% 让「已过期」误导用户；CSP 收紧到内联 style/script（页面零外部资源），
%%% frame-ancestors 'none' 与安全头中间件的 XFO DENY 叠加冗余但无害。
%%% @end
-spec landing_headers() -> map().
landing_headers() ->
    #{
        <<"content-type">> => <<"text/html; charset=utf-8">>,
        <<"cache-control">> => <<"no-store">>,
        <<"content-security-policy">> =>
            <<
                "default-src 'none'; style-src 'unsafe-inline'; "
                "script-src 'unsafe-inline'; frame-ancestors 'none'"
            >>
    }.

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State) ->
    case cowboy_req:method(Req0) of
        M when M =:= <<"GET">> orelse M =:= <<"HEAD">> ->
            landing(cowboy_req:binding(code, Req0, <<>>), Req0, State);
        _Other ->
            %% 落地页只读；写方法一律 405（allow 头指明能力）
            Req = cowboy_req:reply(
                405,
                #{
                    <<"content-type">> => <<"text/plain; charset=utf-8">>,
                    <<"allow">> => <<"GET, HEAD">>
                },
                <<"Method Not Allowed">>,
                Req0
            ),
            {ok, Req, State}
    end.

%% @doc 形状门 → 200 落地页 / 404 静态无效页（不回显输入，零反射）。
-spec landing(binary(), cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
landing(Code, Req0, State) ->
    case valid_code(Code) of
        true ->
            Req = cowboy_req:reply(200, landing_headers(), landing_html(Code), Req0),
            {ok, Req, State};
        false ->
            Req = cowboy_req:reply(404, landing_headers(), invalid_html(), Req0),
            {ok, Req, State}
    end.

%% @doc 邀请码形状门：恰 8 位、字符全在 A-Z2-9（排除 0/O/1/I）。
-spec valid_code(binary()) -> boolean().
valid_code(Code) when is_binary(Code) ->
    byte_size(Code) =:= ?INVITE_CODE_LEN andalso
        lists:all(
            fun(C) -> lists:member(C, ?INVITE_CHARS) end,
            binary_to_list(Code)
        );
valid_code(_) ->
    false.

%% @doc 200 落地页。:code 已过形状门（仅 A-Z2-9）才能进到这里，
%%% HTML 直接内插无注入面。主路径 = 一键复制（WeChat WebView 无
%%% navigator.clipboard 时回落 execCommand），打开 App 为辅助尝试——
%%% 引擎深链未开启的设备上只会拉起 App，用户仍需粘贴，故文案以
%%% 「复制 → 通讯录 → 加入企业 → 粘贴」为主线索。
%%% @end
-spec landing_html(binary()) -> binary().
landing_html(Code) when is_binary(Code) ->
    <<
        "<!DOCTYPE html><html lang=\"zh-CN\"><head>"
        "<meta charset=\"utf-8\">"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        "<title>imboy 企业邀请</title><style>"/utf8,
        (page_css())/binary,
        "</style></head>"
        "<body><div class=\"card\">",
        "<h1>邀请你加入企业</h1>"/utf8,
        "<p class=\"sub\">邀请码 7 天内有效</p>"/utf8,
        "<div class=\"code\">",
        Code/binary,
        "</div>",
        "<button id=\"copy\" onclick=\"copyCode()\">复制邀请码</button>"/utf8,
        "<a class=\"open\" href=\"imboy://org/join?c=",
        Code/binary,
        "\">已安装 imboy？点此打开 App</a>"/utf8,
        "<p class=\"hint\">打开 imboy App → 通讯录 → 加入企业，粘贴邀请码即可加入。<br>"/utf8,
        "链接由同事分享；码过期后请让对方在「邀请同事」页重新生成并分享。</p>"/utf8,
        "</div><script>var C='",
        Code/binary,
        "';"
        "function flash(b,t){var o=b.textContent;b.textContent=t;"
        "setTimeout(function(){b.textContent=o},2000)}"
        "function fallback(b){var i=document.createElement('textarea');i.value=C;"
        "document.body.appendChild(i);i.select();",
        "try{if(!document.execCommand('copy')){throw new Error('copy failed')}flash(b,'已复制，打开 imboy 粘贴即可')}"/utf8,
        "catch(e){flash(b,'复制失败，请长按上方邀请码')}document.body.removeChild(i)}"/utf8,
        "function copyCode(){var b=document.getElementById('copy');"
        "if(navigator.clipboard&&navigator.clipboard.writeText){"
        "navigator.clipboard.writeText(C).then(",
        "function(){flash(b,'已复制，打开 imboy 粘贴即可')},function(){fallback(b)})"/utf8,
        "}else{fallback(b)}}",
        "</script></body></html>"
    >>.

%% @doc 404 无效页：完全静态，不回显任何请求输入。
-spec invalid_html() -> binary().
invalid_html() ->
    <<
        "<!DOCTYPE html><html lang=\"zh-CN\"><head>"
        "<meta charset=\"utf-8\">"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        "<title>imboy</title><style>",
        (page_css())/binary,
        "</style></head>"
        "<body><div class=\"card\">",
        "<h1>邀请链接无效</h1>"/utf8,
        "<p class=\"sub\">链接可能已过期或被修改，请联系分享的同事重新获取。</p>"/utf8,
        "</div></body></html>"
    >>.

%% @doc 两页共用的内联样式（零外部资源，配合 default-src 'none' CSP）。
-spec page_css() -> binary().
page_css() ->
    <<
        "body{margin:0;min-height:100vh;display:flex;align-items:center;"
        "justify-content:center;background:#f2f3f5;color:#111;"
        "font-family:-apple-system,BlinkMacSystemFont,'PingFang SC',"
        "'Microsoft YaHei',sans-serif}"
        ".card{background:#fff;border-radius:16px;"
        "box-shadow:0 6px 24px rgba(0,0,0,.08);margin:16px;padding:36px 24px;"
        "max-width:360px;width:calc(100% - 32px);text-align:center}"
        "h1{font-size:18px;margin:0 0 6px}"
        ".sub{font-size:13px;color:#8a8a8e;margin:0 0 20px}"
        ".code{font-family:ui-monospace,Menlo,Consolas,monospace;font-size:30px;"
        "font-weight:700;letter-spacing:6px;background:#f6f7f9;border-radius:12px;"
        "padding:18px 0;margin:0 0 20px;-webkit-user-select:all;user-select:all}"
        "button{width:100%;border:0;border-radius:12px;padding:14px 0;"
        "font-size:16px;font-weight:600;background:#0a84ff;color:#fff;margin:0 0 16px}"
        "a.open{display:block;font-size:14px;color:#0a84ff;"
        "text-decoration:none;margin:0 0 16px}"
        ".hint{font-size:12px;color:#8a8a8e;line-height:1.8;margin:0;text-align:left}"
    >>.
