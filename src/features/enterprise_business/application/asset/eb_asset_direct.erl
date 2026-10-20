-module(eb_asset_direct).
-moduledoc "EB 域 direct-v1 会话编排（step-14，协议 §3/§5）：application 主体复用 direct 内核 + 内容 Worker 复核。".
%%%
% 职责与边界（step-14 本地可验收核心；逐面真实 HTTP 证据归 step-20b）：
%   * EB Member/Platform/App 企业上传（登录/凭证主体）走 application 主体
%     direct 会话（org 声明进 registration；跨 org 写回被
%     ensure_bindable/credential 校验拒绝）；
%   * finalize CONFIRMED 后执行内容 Worker 受控扫描（现行 25MiB/魔数/文本
%     规则的流式等价实现 + 认证 receipt 五道门）——receipt 验证通过才置
%     `rules_checked`，规则未过/伪造/超时一律 fail-closed（不退化基线：
%     eb_asset_content 47/47 零改动）；
%   * transport 分派显式：direct descriptor = s3_put（存储直传，仅
%     required_headers）；既有同源 upload_ref + 业务 Bearer 合同（Seat/
%     Widget 前端现状）原样保留，不发送任何凭据到存储（T29）；
%   * Visitor（无登录态）与完整 enterprise_asset 业务绑定接线在 20b/后续
%     （本模块提供会话与复核内核，落位即占用）。
%%%

-export([
    register/6,
    scan_confirmed/4,
    read_fun_from_snapshot/1
]).

%% @doc EB 域 direct 会话登记（application 主体：principal=上传主体标识，
%% org_id=OrgId——跨 org/登录切换写回被声明幂等校验拒绝，T04/E10 语义）。
%% 返回 {ok, #{object_key, state, deadline}}（后续走
%% attach_direct_logic:declare/grant/finalize 通用链）。
-spec register(
    integer() | binary(),
    integer() | binary(),
    binary() | binary(),
    binary() | undefined,
    binary(),
    binary()
) ->
    {ok, map()} | {error, term()}.
register(OrgId, Subject, FileName, ClientUploadId, _Mime, _Purpose) ->
    %% Mime 声明由 declare 冻结（本入口不解释类型）；Purpose 暂无独立落库面
    attach_direct_logic:register_application(
        Subject, OrgId, <<"public">>, ClientUploadId, FileName
    ).

%% @doc 对 CONFIRMED 会话执行内容 Worker 扫描并验证 receipt（T30/T31）。
%% ReadChunkFun 来自 [read_fun_from_snapshot/1]（真实 Garage 流式读）或
%% 测试注入；Secret 为 receipt HMAC 密钥（实例内）。
%% 返回 {ok, #{sha256, size, receipt, verified => true}} |
%%       {error, {mime_content_mismatch, _}} | {error, timeout} |
%%       {error, not_confirmed} | {error, {receipt, Reason}}。
-spec scan_confirmed(binary(), binary(), binary(), non_neg_integer() | infinity) ->
    {ok, map()} | {error, term()}.
scan_confirmed(ObjectKey, Mime, Secret, BudgetMs) ->
    case direct_session_row(ObjectKey) of
        {ok, Row = #{<<"direct_state">> := <<"CONFIRMED">>}} ->
            case attachment_storage:frozen_location(ObjectKey, Row) of
                {ok, I} when is_map(I) ->
                    AttemptKey = attempt_key_of(Row, ObjectKey),
                    ReadFun = read_fun_from_snapshot(I),
                    Budget =
                        case BudgetMs of
                            infinity -> 600000;
                            N when is_integer(N), N > 0 -> N
                        end,
                    case
                        attachment_content_worker:scan(
                            ReadFun,
                            fun eb_asset_content:sniff/2,
                            Mime,
                            AttemptKey,
                            ObjectKey,
                            #{secret => Secret, deadline_ms => Budget}
                        )
                    of
                        {ok, Res = #{receipt := Receipt, sha256 := Sha, size := Size}} ->
                            case
                                attachment_content_receipt:verify(Receipt, Secret, #{
                                    attempt_key => AttemptKey,
                                    object_key => ObjectKey,
                                    sha256 => Sha,
                                    size => Size,
                                    mime => Mime,
                                    ruleset_digest =>
                                        attachment_content_receipt:ruleset_digest()
                                })
                            of
                                ok -> {ok, Res#{verified => true}};
                                {error, Reason} -> {error, {receipt, Reason}}
                            end;
                        {error, _} = E ->
                            E
                    end;
                _ ->
                    {error, location_unverified}
            end;
        {ok, #{}} ->
            {error, not_confirmed};
        {error, R} ->
            {error, R}
    end.

%% @doc 受控读注入器：garage 位置快照 I → 分块流式读函数
%% （presign 短时 GET + Range 顺序读；内存有界）。真实 Garage 的最终
%% 合同（反代/Range 行为）由 step-5a 实测；本注入器在 A0 前以 mock 验证。
-spec read_fun_from_snapshot(map()) ->
    fun((non_neg_integer(), non_neg_integer()) -> {ok, binary()} | eof).
read_fun_from_snapshot(I = #{driver := garage}) ->
    BaseUrl =
        case attachment_storage_garage:presign_snapshot(I, 120) of
            {ok, U} -> U;
            {error, _} -> erlang:error(bad_snapshot)
        end,
    fun(Offset, Len) ->
        Url = iolist_to_binary([
            BaseUrl,
            "&response-content-range=bytes=",
            integer_to_binary(Offset),
            "-",
            integer_to_binary(Offset + Len - 1)
        ]),
        case
            httpc:request(
                get,
                {binary_to_list(Url), [
                    {"range", io_lib:format("bytes=~B-~B", [Offset, Offset + Len - 1])}
                ]},
                [{timeout, 30000}],
                [{body_format, binary}]
            )
        of
            %% 206 等 Range 命中：Body 即请求区间内容
            {ok, {{_, S, _}, _, Body}} when S >= 201, S < 300 -> {ok, Body};
            {ok, {{_, 200, _}, _, Body}} ->
                %% 无 Range 支持的防御路径（不应发生）：按偏移截取，超出即 eof
                case Offset < byte_size(Body) of
                    true ->
                        End = min(Offset + Len, byte_size(Body)),
                        {ok, binary:part(Body, Offset, End - Offset)};
                    false ->
                        eof
                end;
            {ok, {{_, 416, _}, _, _}} ->
                eof;
            {ok, {{_, S, _}, _, _}} ->
                {error, {storage_status, S}};
            {error, R} ->
                {error, R}
        end
    end;
read_fun_from_snapshot(_) ->
    erlang:error(unsupported_driver).

%% ===== 内部 =====

%% direct 会话行读取经 infrastructure Port（铁律5：application 不直连
%% elib_pg），实现见 eb_pg_asset_meta:direct_session_row/1。
direct_session_row(ObjectKey) ->
    eb_pg_asset_meta:direct_session_row(ObjectKey).

attempt_key_of(Row, ObjectKey) ->
    Reg = maps:get(<<"upload_registration">>, Row, #{}),
    Subject = maps:get(<<"subject">>, Reg, #{}),
    Principal = maps:get(<<"principal">>, Subject, <<"0">>),
    <<"eb:", Principal/binary, ":", ObjectKey/binary>>.
