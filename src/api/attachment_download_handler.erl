-module(attachment_download_handler).
-moduledoc "附件下载入口（V3-18 EXC-1 边缘承载）：有界鉴权壳——逐请求重验票据，302 边缘承载正文".
%%%
%% V3-18 EXC-1 收口：本端点不再经 BEAM httpc 流式代理正文（旧形态已
%% 退役，正文零过 BEAM）——
%%   * GET/HEAD 逐请求 attach_logic:download 重验（授权/撤权语义不变，
%%     V3-11 合同保留）；
%%   * 验签通过 → 302 到边缘 /edge?ticket=<同一票据>；边缘 OS 进程
%%     （priv/workers/download_edge.escript → attachment_edge_proxy_handler）
%%     逐请求回调 authorize 端点换一次性 presigned GET 并承载
%%     Range/206/416/HEAD 语义；Location 只含边缘基址+票据，不泄私有签名；
%%   * disk 目标：边缘只承载 S3 → 404 隐藏拒绝（同 authorize 端点语义，
%%     V3-20 后 disk 面退役）；
%%   * 拒绝 → 403（不区分无权限/不存在以外的细节）；边缘未配置 →
%%     fail-closed 503，绝不回退 BEAM 取流。
-export([init/2]).

init(Req, State) ->
    Reply =
        case cowboy_req:method(Req) of
            <<"GET">> -> serve(Req);
            <<"HEAD">> -> serve(Req);
            _ -> cowboy_req:reply(405, #{}, Req)
        end,
    {ok, Reply, State}.

serve(Req) ->
    Query = cowboy_req:parse_qs(Req),
    Ticket = proplists:get_value(<<"ticket">>, Query, <<>>),
    case attach_logic:download(Ticket) of
        %% 边缘只承载 S3 目标；disk 目标按隐藏拒绝（V3-20 退役后不存在）。
        {ok, {disk, _}} ->
            cowboy_req:reply(404, private_headers(), Req);
        {ok, _Url} when is_binary(_Url) ->
            case attachment_edge_redirect:edge_ready() of
                true -> attachment_edge_redirect:redirect(Req, Ticket);
                false -> cowboy_req:reply(503, private_headers(), Req)
            end;
        _ ->
            cowboy_req:reply(403, private_headers(), Req)
    end.

private_headers() ->
    #{
        <<"cache-control">> => <<"private, no-store">>,
        <<"x-content-type-options">> => <<"nosniff">>,
        <<"content-security-policy">> => <<"sandbox">>
    }.
