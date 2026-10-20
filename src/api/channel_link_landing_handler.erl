-module(channel_link_landing_handler).
-export([init/2, app_path/2]).

%% 公开落地页只含资源定位符，不查询或暴露频道名、正文和成员资料。
init(Req, State) ->
    ChannelId = cowboy_req:binding(channel_id, Req, <<>>),
    MessageId = cowboy_req:binding(message_id, Req, undefined),
    Reply =
        case {cowboy_req:method(Req), app_path(ChannelId, MessageId)} of
            {<<"GET">>, {ok, Path}} ->
                cowboy_req:reply(200, headers(), page(Path), Req);
            {<<"GET">>, error} ->
                cowboy_req:reply(404, headers(), <<"Unavailable">>, Req);
            _ ->
                cowboy_req:reply(405, #{<<"allow">> => <<"GET">>}, Req)
        end,
    {ok, Reply, State}.

app_path(ChannelId, MessageId) ->
    case valid_id(ChannelId) andalso (MessageId =:= undefined orelse valid_id(MessageId)) of
        true when MessageId =:= undefined -> {ok, <<"/channel/", ChannelId/binary>>};
        true -> {ok, <<"/channel/", ChannelId/binary, "/article/", MessageId/binary>>};
        false -> error
    end.

valid_id(Value) when is_binary(Value), byte_size(Value) =< 19 ->
    case re:run(Value, <<"^[1-9][0-9]*$">>, [{capture, none}]) of
        match -> binary_to_integer(Value) =< 9223372036854775807;
        _ -> false
    end;
valid_id(_) ->
    false.

headers() ->
    #{
        <<"content-type">> => <<"text/html; charset=utf-8">>,
        <<"cache-control">> => <<"no-store">>,
        <<"referrer-policy">> => <<"no-referrer">>,
        <<"x-content-type-options">> => <<"nosniff">>,
        <<"content-security-policy">> =>
            <<"default-src 'none'; style-src 'unsafe-inline'; base-uri 'none'; frame-ancestors 'none'">>
    }.

page(Path) ->
    [
        <<
            "<!doctype html><html lang='zh-CN'><meta charset='utf-8'>"
            "<meta name='viewport' content='width=device-width,initial-scale=1'>"
            "<title>IMBoy · 频道</title><style>body{font:17px system-ui;margin:0;"
            "padding:24px;color:#171717;background:#f5f5f7}main{max-width:480px;"
            "margin:12vh auto;background:white;padding:24px;border-radius:16px}"
            "a{display:block;padding:16px;margin-top:16px;color:#1677ff}"
            "p{line-height:1.6;color:#666}</style><main><h1>在 IMBoy 中查看频道</h1>"
            "<p>打开后将验证访问权限。尚未登录时，请先登录并继续。链接不会自动加入企业或订阅频道。</p>"
            "<a href='imboy://imboy.pub"/utf8
        >>,
        Path,
        <<
            "'>打开 IMBoy</a><p>未安装应用？请前往官网了解安装方式，安装后重新打开此链接。</p>"
            "<a href='/'>前往官网</a></main></html>"/utf8
        >>
    ].
