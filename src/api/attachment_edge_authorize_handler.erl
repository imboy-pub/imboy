-module(attachment_edge_authorize_handler).
-moduledoc "V3-08 下载边缘逐请求授权端点（内部 token 鉴权，有界控制响应）".
%%%
%% POST /api/v1/attachment/edge/authorize
%%   body: {"ticket":…,"method":"GET"|"HEAD"}
%%   头:   x-edge-internal-token: <config edge_internal_token>
%% 行为：
%%   * 内部令牌缺失/不符 → 404（fail-closed，不暴露存在性）；
%%   * attach_logic:download 验证 ticket + ACL：{ok, Url} → 200 {"url":…}；
%%     disk 目标/拒绝 → 404（边缘只承载 S3；无权限与不存在统一隐藏）。
%% 本端点只回有界 JSON，绝不接触正文（BEAM=0 合同）。

-export([init/2]).

-include("generated/imboy_product_features.hrl").

-define(MAX_BODY, 16384).

init(Req0, State) ->
    Req =
        case
            {
                cowboy_req:method(Req0),
                internal_token_ok(cowboy_req:header(<<"x-edge-internal-token">>, Req0, <<>>))
            }
        of
            {<<"POST">>, true} -> serve(Req0);
            _ -> reply_json(Req0, 404, #{<<"error">> => <<"not_found">>})
        end,
    {ok, Req, State}.

serve(Req0) ->
    case cowboy_req:read_body(Req0, #{length => ?MAX_BODY, period => 5000}) of
        {ok, Body, Req1} ->
            try
                #{<<"ticket">> := Ticket, <<"method">> := Method} = jsone:decode(Body),
                true = is_binary(Ticket) andalso byte_size(Ticket) =< 8205,
                true = Method =:= <<"GET">> orelse Method =:= <<"HEAD">>,
                case authorize_target(Ticket) of
                    {ok, {verified_part, Url, Size, Hash}} when
                        is_binary(Url), is_integer(Size), Size > 0, is_binary(Hash)
                    ->
                        reply_json(Req1, 200, #{
                            <<"url">> => Url, <<"size">> => Size, <<"sha256">> => Hash
                        });
                    {ok, Url} when is_binary(Url) ->
                        reply_json(Req1, 200, #{<<"url">> => Url});
                    {ok, {Url, Filename}} when is_binary(Url), is_binary(Filename) ->
                        %% V3-18 P3/EXC-2：导出域附文件名（边缘透传
                        %% Content-Disposition，保留附件下载契约）。
                        reply_json(Req1, 200, #{<<"url">> => Url, <<"filename">> => Filename});
                    {ok, {disk, _}} ->
                        %% 边缘只承载 S3 目标；disk 目标按隐藏拒绝（V3-20 退役后不存在）
                        reply_json(Req1, 404, #{<<"error">> => <<"not_found">>});
                    _ ->
                        reply_json(Req1, 404, #{<<"error">> => <<"not_found">>})
                end
            catch
                _:_ -> reply_json(Req1, 400, #{<<"error">> => <<"bad_request">>})
            end;
        {more, _, Req1} ->
            reply_json(Req1, 413, #{<<"error">> => <<"too_large">>})
    end.

%% 票据分派（V3-17）：主域 verify 失败（sub/purpose 不符）再试 EB 域；两者
%% purpose 派生密钥不同，交叉验签不可能通过（fail-closed）。EB 域走
%% eb_asset_app:edge_authorize_content 重放完整 ACL（逐请求授权）。
%% V3-18 P3/EXC：追加 talk-record 回放/导出下载域（V3-18 边缘承载后，
%% 旧入口 302 到边缘，边缘逐请求回调本端点换 presigned GET；talk-record
%% 票据 audience 独立，交叉验签同样不可能通过）。
authorize_target(<<"parts:", Rest/binary>>) ->
    case binary:split(Rest, <<":">>) of
        [SeqBin, Ticket] when byte_size(SeqBin) > 0, byte_size(SeqBin) =< 6 ->
            try binary_to_integer(SeqBin) of
                Seq when Seq >= 0 -> talk_record_playback_parts:edge_target(Ticket, Seq);
                _ -> {error, forbidden}
            catch
                error:badarg -> {error, forbidden}
            end;
        _ ->
            {error, forbidden}
    end;
authorize_target(<<"client_audio:", Ticket/binary>>) ->
    talk_record_client_audio_logic:edge_target(Ticket);
authorize_target(<<"avatar:", Alias/binary>>) ->
    case
        re:run(
            Alias,
            <<"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-4[0-9a-fA-F]{3}-[89aAbB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$">>,
            [{capture, none}]
        )
    of
        match ->
            %% 层边界：API 不直连 repo，resolve+冻结+presign 收敛在 DS。
            attachment_ds:resolve_avatar_snapshot(Alias);
        _ ->
            {error, forbidden}
    end;
authorize_target(Ticket) ->
    case attach_logic:download(Ticket) of
        {ok, _} = Ok ->
            Ok;
        _ ->
            authorize_after_attachment(Ticket)
    end.

%% EB 域分支（enterprise_business 裁剪档，铁律 F-EB10-1 同类保护）：
%% 未选中档 eb_asset_app 按 ERLC_EXCLUDE 不参与编译，本分支整条省略——
%% EB 票据域恒不可达（fail-closed 落入 talk-record/拒绝分支，与选中档
%% 「EB verify 失败」外部形状一致）；选中档语义不变。
-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS).
authorize_after_attachment(Ticket) ->
    case eb_edge_authorize(Ticket) of
        {ok, _} = Ok -> Ok;
        _ -> authorize_talk_record(Ticket)
    end.

eb_edge_authorize(Ticket) ->
    case eb_asset_download_ticket:verify(Ticket) of
        {ok, Claims} ->
            eb_asset_app:edge_authorize_content(Claims);
        _ ->
            {error, forbidden}
    end.
-else.
authorize_after_attachment(Ticket) ->
    authorize_talk_record(Ticket).
-endif.

authorize_talk_record(Ticket) ->
    case talk_record_target(Ticket) of
        {ok, _} = Ok -> Ok;
        _ -> {error, forbidden}
    end.

%% talk-record 回放/导出域：verify 逐请求重放完整 ACL（claims 自带
%% uid/rid——边缘回调无请求者上下文，以票据 claim 为主体重验，撤权/
%% 过期/软删即拒），成功换**一次性 presigned GET**（ Garage frozen
%% location，短窗）。有界 JSON 只回 {"url":…}。
talk_record_target(Ticket) ->
    case talk_record_playback_stream:edge_target(Ticket) of
        {ok, _} = Ok ->
            Ok;
        _ ->
            talk_record_export_download:edge_target(Ticket)
    end.

internal_token_ok(Provided) when byte_size(Provided) =:= 0 ->
    false;
internal_token_ok(Provided) ->
    Expected = application:get_env(imboy, edge_internal_token, <<>>),
    byte_size(Expected) > 0 andalso
        byte_size(Expected) =:= byte_size(Provided) andalso
        crypto:hash_equals(Expected, Provided).

reply_json(Req, Code, Map) ->
    cowboy_req:reply(
        Code,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"cache-control">> => <<"private, no-store">>
        },
        jsone:encode(Map),
        Req
    ).
