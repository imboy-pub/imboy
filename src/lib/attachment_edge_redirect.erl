-module(attachment_edge_redirect).
-moduledoc "V3-18 P3/EXC 收口：旧下载入口的边缘承载重定向（有界鉴权壳公共件）".
%%%
%% V3-17 已验证的边缘形态（EB 域 eb_asset_app:issue_redirect 同构）：
%%   * 旧业务端点保留有界鉴权壳（逐请求 recheck / HEAD 元数据 / 416 语义），
%%     正文一律 302 到边缘 `/edge?ticket=`——字节由边缘 OS 进程
%%     （priv/workers/download_edge.escript → attachment_edge_proxy_handler）
%%     承载，逐请求回调 authorize 端点重放 ACL；
%%   * 302 Location 只含边缘基址 + 票据，**绝不泄露 Garage 签名 URL /
%%     endpoint / bucket / object key**（fail-closed：边缘未配置即异常，
%%     不回退 BEAM 取流——调用方应先以 edge_ready/0 判定并给出 503）；
%%   * 边缘基址：专用 `edge_base_url` 优先；缺省回退 `base_url` + `/edge`
%%     （同域反代形态）。两者皆空 → edge_base() 为 <<>>。

-export([edge_base/0, edge_ready/0, redirect/2, presign_attachment/2]).

-spec edge_base() -> binary().
edge_base() ->
    case config_ds:env(edge_base_url, <<>>) of
        <<>> ->
            case config_ds:env(base_url, <<>>) of
                <<>> -> <<>>;
                Base -> <<(string:trim(Base, trailing, "/"))/binary, "/edge">>
            end;
        Base ->
            string:trim(Base, trailing, "/")
    end.

-spec edge_ready() -> boolean().
edge_ready() -> byte_size(edge_base()) > 0.

%% @doc 有界 302：Location = <edge_base>?ticket=<Ticket>；缓存头私有不落盘。
%% Ticket 由调用方先行完成逐请求 recheck（本模块不再验票）；边缘未配置时
%% 调用方 fail-closed（不回退 BEAM 取流）。
-spec redirect(cowboy_req:req(), binary()) -> cowboy_req:req().
redirect(Req, Ticket) ->
    Base = edge_base(),
    true = byte_size(Base) > 0,
    Url = <<Base/binary, "?ticket=", Ticket/binary>>,
    cowboy_req:reply(
        302,
        #{
            <<"content-type">> => <<"text/plain; charset=utf-8">>,
            <<"location">> => Url,
            <<"cache-control">> => <<"private, no-store">>,
            %% 边缘响应带通配 ACAO（attachment_edge_proxy_handler 同款），
            %% 浏览器跨域跟随 302 需要。
            <<"access-control-allow-origin">> => <<"*">>
        },
        <<>>,
        Req
    ).

%% @doc 派生工件（attachment_derived_artifact 行）→ 一次性 presigned GET
%% （短窗；talk-record 回放/导出域边缘 authorize 共用——与
%% talk_record_audio_render:part_fetch_item 同一原语）。非 garage 位置
%% fail-closed（边缘只承载 S3）。
-spec presign_attachment(integer(), pos_integer()) -> {ok, binary()} | {error, term()}.
presign_attachment(Aid, WindowSeconds) when is_integer(Aid), Aid > 0 ->
    case attachment_derived_artifact:metadata(Aid) of
        {ok, Row} ->
            frozen_presign(Row, WindowSeconds);
        %% 缺陷⑬：final 成品是客户端 direct 直传附件（无 attachment_derived_write
        %% 行），派生读链必 404 → artifact_unavailable → 回放/导出 403。回退到
        %% direct live 附件读：live/status>0/物理定位齐备；授权仍由调用链先行
        %% 校验（playback verify_ticket+assembly 发布锚、export 授权），此处
        %% 只补「非派生工件」的定位解析。
        {error, artifact_unavailable} ->
            direct_presign(Aid, WindowSeconds);
        Error ->
            Error
    end.

-spec frozen_presign(map(), pos_integer()) -> {ok, binary()} | {error, term()}.
frozen_presign(Row, WindowSeconds) ->
    case attachment_storage:frozen_location(maps:get(<<"path">>, Row), Row) of
        {ok, #{driver := garage} = I} ->
            attachment_storage_garage:presign_snapshot(I, WindowSeconds);
        _ ->
            {error, historical_location_unverified}
    end.

-spec direct_presign(integer(), pos_integer()) -> {ok, binary()} | {error, term()}.
direct_presign(Aid, WindowSeconds) ->
    Sql =
        <<"SELECT a.* FROM attachment a WHERE a.id=$1 AND a.lifecycle_state='live'",
            " AND a.status=1 AND a.size>0",
            " AND a.storage_physical_key IS NOT NULL AND a.storage_driver IS NOT NULL">>,
    case elib_pg:query(Sql, [Aid]) of
        {ok, [Row]} ->
            frozen_presign(Row, WindowSeconds);
        {ok, []} ->
            {error, artifact_unavailable};
        Error ->
            Error
    end.
