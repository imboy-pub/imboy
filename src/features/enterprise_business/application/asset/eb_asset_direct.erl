-module(eb_asset_direct).
-moduledoc "EB 域 direct-v1 会话编排（step-14 + V3-C04 20b 落位）：Member/Seat/Visitor 企业域主体复用 direct 内核 + 内容 Worker 复核 + enterprise_asset 绑定。".
%%%
% 职责与边界（V3-C04：企业与客服后端全认证面 S3-only）：
%   * application 主体（Admin 头像）走 register/6（step-14 既有）；
%   * 企业域三主体 member/seat/contact 走 direct_presign/3：一站式
%     register(fn_dv1_register_eb) → declare → grant，返回 s3_put staging
%     descriptor——正文自客户端直达 S3，BEAM 零正文（硬合同）；
%   * direct_finalize_scan/4：finalize（copy→proof→winner pin）+ 外部
%     Worker 受控扫描（receipt 五道门）——receipt 验证不过一律 fail-closed
%     （不产生任何消息附件引用）；
%   * direct_bind/4：CONFIRMED + scan 通过后把会话折叠进 enterprise_asset
%     active 行（object_key=winner target；advisory lock 幂等，重复 confirm
%     回读同一行不产生重复引用——C04-03）；
%   * transport 分派显式：descriptor = s3_put（存储直传，仅
%     required_headers），不发送任何凭据到存储（T29）；同源 api_byte /
%     upload_ref 正文链在 V3-C04 一并移除（不保留兼容）。
%%%

-export([
    register/6,
    scan_confirmed/4,
    read_fun_from_snapshot/1,
    direct_presign/3,
    direct_finalize_scan/4,
    direct_bind/4,
    direct_renew/1,
    session_owner/1,
    receipt_secret/0
]).

-include("log.hrl").
-include_lib("epgsql/include/epgsql.hrl").

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
                {ok, I = #{garage_config := Cfg}} when is_map(I) ->
                    %% V3-17：正文读移出 BEAM——外部 OS Worker（Range GET 流式
                    %% 扫描+receipt），本进程只收有界结果并二次验证；无 BEAM
                    %% 内 ReadFun 路径、无回退（fail-closed，S3-only 合同 §7）。
                    AttemptKey = attempt_key_of(Row, ObjectKey),
                    Budget =
                        case BudgetMs of
                            infinity -> 600000;
                            N when is_integer(N), N > 0 -> N
                        end,
                    Task = #{
                        endpoint => maps:get(endpoint, Cfg),
                        ak => maps:get(access_key, Cfg),
                        sk => maps:get(secret_key, Cfg),
                        bucket => maps:get(container, I),
                        region => maps:get(region, Cfg, <<"garage">>),
                        object_key => maps:get(key, I, ObjectKey),
                        attempt_key => AttemptKey,
                        mime => Mime,
                        secret => Secret,
                        deadline_ms => Budget
                    },
                    case attachment_worker_port:scan_external(Task) of
                        {ok, Res = #{verified := true}} ->
                            {ok, Res};
                        {ok, _} ->
                            %% port 层已验证但未回 verified 标记（不可达防御）
                            {error, {receipt, unverified}};
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

%% direct 会话读取经已装配的 asset 端口；application 不依赖 PG 实现。
direct_session_row(ObjectKey) ->
    {ok, Asset} = eb_infra_ports:resolve(asset),
    Asset:direct_session_row(ObjectKey).

attempt_key_of(Row, ObjectKey) ->
    Reg = maps:get(<<"upload_registration">>, Row, #{}),
    Subject = maps:get(<<"subject">>, Reg, #{}),
    Principal = maps:get(<<"principal">>, Subject, <<"0">>),
    <<"eb:", Principal/binary, ":", ObjectKey/binary>>.

%% ===================================================================
%% V3-C04 20b 落位：Member/Seat/Visitor 企业域 direct 一站式链
%% ===================================================================

%% @doc EB 企业域一站式 presign（V3-C04）：
%% register(fn_dv1_register_eb) → declare → grant，返回 s3_put staging
%% descriptor。每个 dv1 状态跳仍由 fn_dv1_* SQL 守卫强制（幂等/conflict/
%% quota/deadline 语义全保留）；BEAM 不经手正文（硬合同）。
%%
%% SubjectKind = member | seat | contact；
%% Principal = 登录 user uid（member/seat）或访客 contact id（contact）；
%% Decl 必填：org_id / workspace_id / conversation_id / mime / size_bytes /
%% sha256_hex；可选：message_id / client_upload_id(36B) / file_name /
%% scope（缺省 private）。
%%
%% 返回 {ok, #{object_key, state=GRANTED, attempt_id, grant, expires_at,
%% session_deadline}}；错误：declaration_conflict / quota_exceeded /
%% size_limit_exceeded / expected_conflict / illegal_state / ...
-spec direct_presign(member | seat | contact, integer(), map()) ->
    {ok, map()} | {error, term()}.
direct_presign(SubjectKind, Principal, Decl) when
    (SubjectKind =:= member orelse SubjectKind =:= seat orelse SubjectKind =:= contact),
    is_integer(Principal),
    is_map(Decl)
->
    OrgId = maps:get(org_id, Decl),
    WorkspaceId = maps:get(workspace_id, Decl),
    ObjectKey = eb_object_key(OrgId, WorkspaceId, maps:get(file_name, Decl, undefined)),
    Bucket = maps:get(bucket, application:get_env(imboy, garage, #{}), <<"imboy">>),
    Scope = maps:get(scope, Decl, <<"private">>),
    Cid =
        case maps:get(client_upload_id, Decl, undefined) of
            Bin when is_binary(Bin), byte_size(Bin) =:= 36 -> Bin;
            _ -> null
        end,
    case attachment_storage:registration_location(Scope, Bucket, ObjectKey) of
        {ok, #{
            driver := Driver,
            namespace := Ns,
            container := Container,
            locator_digest := Locator
        }} ->
            %% 铁律5：application 层不经 elib_pg——register SQL 收敛在
            %% infrastructure 层（eb_pg_asset_meta:register_eb/1）。
            RegisterArgs = [
                ObjectKey,
                Bucket,
                Scope,
                atom_to_binary(SubjectKind, utf8),
                integer_to_binary(Principal),
                integer_to_binary(OrgId),
                integer_to_binary(WorkspaceId),
                eb_int_bin(maps:get(conversation_id, Decl)),
                eb_int_bin(maps:get(message_id, Decl, undefined)),
                Cid,
                maps:get(mime, Decl),
                eb_file_name_bin(maps:get(file_name, Decl, undefined)),
                Driver,
                Ns,
                Container,
                Locator
            ],
            case eb_pg_asset_meta:register_eb(RegisterArgs) of
                {ok, [#{<<"key">> := Key}]} ->
                    declare_and_grant(
                        Key,
                        maps:get(sha256_hex, Decl),
                        maps:get(size_bytes, Decl),
                        maps:get(mime, Decl)
                    );
                {ok, []} ->
                    {error, register_not_returned};
                {error, #error{code = <<"23510">>}} ->
                    {error, declaration_conflict};
                {error, #error{code = <<"23514">>}} ->
                    {error, quota_exceeded};
                {error, R} ->
                    {error, R}
            end;
        {error, R} ->
            {error, R}
    end;
direct_presign(_Kind, _Principal, _Decl) ->
    {error, {invalid_argument, direct_presign}}.

%% declare→grant 两跳（expected 冻结 + attempt 签发）；错误原样透传
%% （size_limit_exceeded / expected_conflict / illegal_state）。
declare_and_grant(Key, Sha256Hex, Size, Mime) ->
    case attach_direct_logic:declare(Key, Sha256Hex, Size, Mime) of
        ok ->
            case attach_direct_logic:grant(Key, Sha256Hex) of
                {ok, Info} -> {ok, deadline_added(Info, Key)};
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

%% grant 投影补会话 deadline（客户端取消/重试窗口判定用）
deadline_added(Info, Key) ->
    case direct_session_row(Key) of
        {ok, #{<<"session_deadline">> := Deadline}} when Deadline =/= null ->
            Info#{session_deadline => deadline_ms(Deadline)};
        _ ->
            Info
    end.

%% @doc CONFIRMED 后折叠：finalize（copy→proof→winner pin）+ 外部 Worker
%% 同字节扫描（receipt 五道门）。任一失败 fail-closed——不得产生任何绑定
%% （C04 步骤 4）。AttemptNo 沿客户端申报（幂等重试=新 attempt 走 renew）。
-spec direct_finalize_scan(binary(), integer(), binary(), non_neg_integer()) ->
    {ok, map()} | {error, term()}.
direct_finalize_scan(ObjectKey, AttemptNo, Mime, BudgetMs) when
    is_binary(ObjectKey), is_integer(AttemptNo), AttemptNo >= 1
->
    case attach_direct_logic:finalize(ObjectKey, AttemptNo) of
        {ok, #{state := <<"CONFIRMED">>}} ->
            case receipt_secret() of
                {ok, Secret} ->
                    scan_confirmed(ObjectKey, Mime, Secret, BudgetMs);
                {error, _} = Err ->
                    %% secret 未装配：fail-closed（不产生消息附件引用）
                    Err
            end;
        {ok, #{outcome := <<"UNKNOWN_QUARANTINED">>}} ->
            {error, finalize_unknown};
        {error, _} = Err ->
            Err
    end;
direct_finalize_scan(_Key, _No, _Mime, _Budget) ->
    {error, {invalid_argument, direct_finalize_scan}}.

%% @doc 绑定（bind）：CONFIRMED + scan 通过后把会话折叠进 enterprise_asset
%% active 行（物理键=winner target，经 attachment 行取；advisory lock 幂等，
%% 重复 confirm 回读同一行——C04-03 反例不产生重复引用）。经 asset 端口，
%% application 不依赖 PG 实现。
-spec direct_bind(integer(), integer(), binary(), map()) ->
    {ok, map()} | {error, term()}.
direct_bind(OrgId, WorkspaceId, ObjectKey, Descriptor) when
    is_integer(OrgId), is_integer(WorkspaceId), is_binary(ObjectKey), is_map(Descriptor)
->
    {ok, Asset} = eb_infra_ports:resolve(asset),
    Asset:bind_direct(OrgId, WorkspaceId, ObjectKey, Descriptor);
direct_bind(_Org, _Ws, _Key, _Desc) ->
    {error, {invalid_argument, direct_bind}}.

%% @doc 会话归属投影（控制门校验用）：主体 kind/principal + EB 声明 +
%% direct_state。跨主体/跨空间复用在此 fail-closed（C04-02）。
-spec session_owner(binary()) -> {ok, map()} | {error, term()}.
session_owner(ObjectKey) when is_binary(ObjectKey) ->
    case direct_session_row(ObjectKey) of
        {ok, Row = #{<<"upload_registration">> := Reg0}} ->
            Reg =
                case Reg0 of
                    B when is_binary(B) -> jsone:decode(B);
                    M when is_map(M) -> M;
                    _ -> #{}
                end,
            Subject = maps:get(<<"subject">>, Reg, #{}),
            Decl = maps:get(<<"declaration">>, Reg, #{}),
            {ok, #{
                kind => eb_kind_atom(maps:get(<<"kind">>, Subject, <<>>)),
                principal => principal_int(maps:get(<<"principal">>, Subject, <<>>)),
                org_id => eb_int_val(maps:get(<<"org_id">>, Decl)),
                workspace_id => eb_int_val(maps:get(<<"workspace_id">>, Decl)),
                conversation_id => eb_int_val(maps:get(<<"conversation_id">>, Decl)),
                message_id => eb_int_val(maps:get(<<"message_id">>, Decl)),
                mime => maps:get(<<"mime">>, Decl, <<"application/octet-stream">>),
                file_name => maps:get(<<"file_name">>, Decl, undefined),
                scope => maps:get(<<"scope">>, Decl, <<"private">>),
                expected_sha => expected_sha_of(maps:get(<<"direct_expected">>, Row, null)),
                direct_state => maps:get(<<"direct_state">>, Row, null),
                session_state => maps:get(<<"session_state">>, Row, null),
                session_deadline => deadline_ms(maps:get(<<"session_deadline">>, Row, null))
            }};
        {ok, #{}} ->
            {error, not_found};
        {error, R} ->
            {error, R}
    end;
session_owner(_) ->
    {error, not_found}.

expected_sha_of(#{<<"sha256">> := Sha}) when is_binary(Sha) -> Sha;
expected_sha_of(_) -> undefined.

%% @doc direct 会话续期（session renew 用例）：GRANTED/FINALIZING → 新
%% attempt + 新 staging descriptor（expected sha 复用冻结声明）；CONFIRMED
%% 幂等终态投影（无新 attempt、无 presign）。
-spec direct_renew(binary()) -> {ok, map()} | {error, term()}.
direct_renew(ObjectKey) when is_binary(ObjectKey) ->
    case session_owner(ObjectKey) of
        {ok, #{direct_state := <<"CONFIRMED">>}} ->
            %% CONFIRMED 幂等终态优先：不进 renew_grant（少一次 session_row
            %% 查询；expected 冻结在 CONFIRMED 行上仍在，不能据此重开 attempt）
            {ok, #{state => <<"CONFIRMED">>, object_key => ObjectKey}};
        {ok, #{expected_sha := Sha}} when is_binary(Sha) ->
            %% GRANTED/FINALIZING：expected 已冻结，renew 换新 attempt
            attach_direct_logic:renew_grant(ObjectKey, Sha);
        {ok, #{}} ->
            {error, not_found};
        {error, R} ->
            {error, R}
    end;
direct_renew(_) ->
    {error, {invalid_argument, direct_renew}}.

%% @doc receipt HMAC 密钥（实例内）：env `imboy.attachment_receipt_secret`。
%% 未装配返回 {error, worker_scan_unavailable}——confirm 链 fail-closed。
-spec receipt_secret() -> {ok, binary()} | {error, term()}.
receipt_secret() ->
    case application:get_env(imboy, attachment_receipt_secret) of
        {ok, Bin} when is_binary(Bin), byte_size(Bin) >= 16 ->
            {ok, Bin};
        {ok, _} ->
            {error, worker_scan_unavailable};
        undefined ->
            {error, worker_scan_unavailable}
    end.

%% ===== EB 内部 =====

%% EB direct 会话键：`eb/<Org>/<Ws>/<Ymd>/<Hex><Ext>`——org/workspace 隔离
%% 前缀（跨空间不复用的键空间证据）；会话键终身不复用沿 V5 触发器。
eb_object_key(OrgId, WorkspaceId, FileName) ->
    Ymd = ymd_today(),
    Ext = string:lowercase(filename:extension(eb_file_name_bin(FileName))),
    Hex = binary:encode_hex(crypto:strong_rand_bytes(8), lowercase),
    iolist_to_binary([
        "eb/",
        integer_to_binary(OrgId),
        "/",
        integer_to_binary(WorkspaceId),
        "/",
        Ymd,
        "/",
        Hex,
        Ext
    ]).

eb_file_name_bin(undefined) -> <<"file">>;
eb_file_name_bin(Bin) when is_binary(Bin), byte_size(Bin) > 0 -> Bin;
eb_file_name_bin(_) -> <<"file">>.

eb_int_bin(undefined) -> null;
eb_int_bin(Id) when is_integer(Id) -> integer_to_binary(Id);
eb_int_bin(Bin) when is_binary(Bin) -> Bin.

eb_int_val(null) -> undefined;
eb_int_val(undefined) -> undefined;
eb_int_val(Bin) when is_binary(Bin) -> elib_cnv:safe_to_integer(Bin);
eb_int_val(Id) when is_integer(Id) -> Id.

eb_kind_atom(<<"member">>) -> member;
eb_kind_atom(<<"seat">>) -> seat;
eb_kind_atom(<<"contact">>) -> contact;
eb_kind_atom(_) -> unknown.

principal_int(Bin) when is_binary(Bin) -> elib_cnv:safe_to_integer(Bin);
principal_int(Id) when is_integer(Id) -> Id;
principal_int(_) -> undefined.

%% timestamptz（epgsql naive UTC datetime 元组）→ epoch ms
deadline_ms(Deadline) when is_tuple(Deadline) ->
    Gs = calendar:datetime_to_gregorian_seconds(Deadline),
    (Gs - 62167219200) * 1000;
deadline_ms(_) ->
    undefined.

ymd_today() ->
    {{Y, M, D}, _} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B~2..0B~2..0B", [Y, M, D])).
