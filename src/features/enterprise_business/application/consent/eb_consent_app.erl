%%% @doc 企业会话 consent（告知/同意）的 application 层用例与**唯一** consent 结论入口。
%%%
%%% 依据：plan v4.1 EB-D05、§2.1 #9/#14、§8 EB-06、§9；作业书 §3（★ 高危语义）。
%%%
%%% ## 本模块要解决的问题
%%%
%%% DB 侧（迁移 116）的 `ck_ec_consent` 只保证「`consent_at` 非空时 `notice_version`
%%% 必须非空」，即它只能证明「有一行看起来同意了」。而计划要求**合成 consent 不得被
%%% 当作真实同意或合规**（用户规则 8 / §2.1 #14 / §9）。因此：
%%%
%%%   * 本模块是全 feature **唯一**的 consent 结论入口：`verdict/1` 只可能给出
%%%     `synthetic` 或 `unknown` 两种结论，**永不给 `real`**；
%%%   * `synthetic_fields/2` 是唯一允许写入 consent 列的来源，写出的行带**冻结的
%%%     合成标记**（`notice_version = ?SYNTHETIC_NOTICE_VERSION`，`consent_subject`
%%%     以 `synthetic:eb06:` 开头并只含资源 ID），因此事后可从 DB 回读并识别；
%%%   * `evidence/1` 的 `real_consent_claim` / `compliance_claim` **恒为 `false`**，
%%%     类型上不存在「升级为真实同意」的表示。
%%%
%%% ## 为什么不可能被「升级」
%%%
%%% `eb_consent:classify/1`（EB-02 冻结合同）只有在入参 map 里**出现字面 atom
%%% `real`** 时才返回 `real`。本模块（以及本卡其它三个 application 模块）经静态
%%% 扫描确认不含该 token（见 `eb_consent_tests:a01_no_code_path_can_upgrade_synthetic_to_real/0`），
%%% 因此本卡不存在构造 `kind := real` 的代码路径。即便调用方自带
%%% `#{kind => real}`，`verdict/1` 也会把它归一为 `unknown`（只降不升）。
%%%
%%% ## 纯净性与依赖
%%%
%%% 本模块自身不做 I/O：只做 consent map 的判定与构造。写库由调用方
%%% （`eb_conversation_app`）经 `eb_store_port` 完成；需要「现在」时经注入时钟端口
%%% 取一次（`Params.clock` 可覆盖），domain 侧不读系统时间。
-module(eb_consent_app).

-export([
    synthetic_notice_version/0,
    synthetic_subject/1,
    synthetic_fields/2,
    consent_of/1,
    verdict/1,
    is_synthetic/1,
    report_label/1,
    gate/2,
    evidence/1,
    evidence_kind/1
]).

-type consent() :: map().
-type evidence() :: map().

-export_type([consent/0, evidence/0]).

%% 合成告知版本：名字里显式带 synthetic，任何报告都不可能把它读成真实告知文本。
-define(SYNTHETIC_NOTICE_VERSION, <<"eb06-synthetic-notice-v1">>).
%% 合成同意主体前缀：只允许前缀 + 资源 ID（合成 TSID），不得存放真实客户标识。
-define(SYNTHETIC_SUBJECT_PREFIX, <<"synthetic:eb06:">>).
%% `enterprise_conversation.consent_evidence_kind` 的**唯一**非空取值（迁移 122 的
%% `ck_ec_consent_evidence_kind` 逐字同口径：非空集合恰为 {synthetic}）。
-define(SYNTHETIC_EVIDENCE_KIND, <<"synthetic">>).

%% ===================================================================
%% 合成件的构造（consent 列的唯一写入来源）
%% ===================================================================

%% @doc 冻结的合成告知版本号。
-spec synthetic_notice_version() -> binary().
synthetic_notice_version() ->
    ?SYNTHETIC_NOTICE_VERSION.

%% @doc 合成同意主体：`synthetic:eb06:<resource_id>`（受控前缀 + 合成资源 ID）。
-spec synthetic_subject(integer()) -> binary().
synthetic_subject(ResourceId) when is_integer(ResourceId) ->
    <<?SYNTHETIC_SUBJECT_PREFIX/binary, (integer_to_binary(ResourceId))/binary>>;
synthetic_subject(Other) ->
    erlang:error({invalid_consent_resource_id, Other}).

%% @doc 构造待写入的合成 consent 字段。
%%
%% `Params.consent_at`（Unix 秒）可显式注入；缺省经注入时钟端口取一次。返回 map
%% 同时带 `notice_version/consent_at/consent_subject` 三列与 `kind => synthetic`
%% 标记，供 `eb_consent:gate/2` 自检与调用方透传。
-spec synthetic_fields(integer(), map()) -> {ok, consent()} | {error, term()}.
synthetic_fields(ResourceId, Params) when is_integer(ResourceId), is_map(Params) ->
    case consent_at(Params) of
        {error, _} = Err ->
            Err;
        {ok, At} ->
            {ok, #{
                notice_version => ?SYNTHETIC_NOTICE_VERSION,
                consent_at => At,
                consent_subject => synthetic_subject(ResourceId),
                kind => synthetic,
                synthetic => true
            }}
    end;
synthetic_fields(_ResourceId, _Params) ->
    {error, {invalid_argument, synthetic_fields}}.

%% ===================================================================
%% 从已落库的行还原 consent（识别合成件）
%% ===================================================================

%% @doc 从会话行还原 consent map；无 consent 列（未完成告知）返回 `undefined`。
%%
%% 只有「冻结合成版本 + 合成主体前缀」这一对同时成立才判为 `synthetic`；其余一律
%% `unknown`（**不是** `real`，也不是任何合规结论）。
-spec consent_of(term()) -> consent() | undefined.
consent_of(Row) when is_map(Row) ->
    case has_consent_columns(Row) of
        false -> undefined;
        true -> row_consent(Row)
    end;
consent_of(_Other) ->
    undefined.

has_consent_columns(Row) ->
    maps:is_key(notice_version, Row) orelse
        maps:is_key(consent_at, Row) orelse
        maps:is_key(consent_subject, Row).

row_consent(Row) ->
    NoticeVersion = maps:get(notice_version, Row, undefined),
    At = maps:get(consent_at, Row, undefined),
    Subject = maps:get(consent_subject, Row, undefined),
    case is_non_empty_binary(NoticeVersion) of
        false ->
            %% 未完成告知：与 `eb_consent:gate/2` 的 fail-closed 口径一致。
            undefined;
        true ->
            Base = #{
                notice_version => NoticeVersion,
                consent_at => At,
                consent_subject => Subject
            },
            case is_synthetic_pair(NoticeVersion, Subject) of
                true -> Base#{kind => synthetic, synthetic => true};
                false -> Base#{kind => unknown}
            end
    end.

is_synthetic_pair(?SYNTHETIC_NOTICE_VERSION, Subject) when is_binary(Subject) ->
    %% 前缀比较用 byte_size + binary:part（宏不可直接出现在 binary 模式串里）。
    PrefixSize = byte_size(?SYNTHETIC_SUBJECT_PREFIX),
    byte_size(Subject) > PrefixSize andalso
        binary:part(Subject, 0, PrefixSize) =:= ?SYNTHETIC_SUBJECT_PREFIX;
is_synthetic_pair(_NoticeVersion, _Subject) ->
    false.

%% ===================================================================
%% 唯一结论入口（只降不升）
%% ===================================================================

%% @doc 本 feature 对任意输入的 consent 结论。
%%
%% 只可能返回 `kind => synthetic | unknown`：无法从本模块得到真实同意结论。
-spec verdict(term()) -> map().
verdict(Term) ->
    Consent = canonical_consent(Term),
    Kind = eb_consent:classify(Consent),
    #{
        kind => Kind,
        is_synthetic => Kind =:= synthetic,
        report_label => eb_consent:report_label(Consent)
    }.

%% 把任意输入归一为「本模块能证明的两种结论」之一：
%%   * 已落库的行 → 按冻结标记识别（synthetic | unknown）；
%%   * 其它 map → 只有显式 synthetic 标记才认（synthetic），其余一律 unknown。
%% 注意：调用方自带 `kind => <非合成>` 都会被归一为 unknown，不透传。
canonical_consent(Term) ->
    case consent_of(Term) of
        undefined -> marker_consent(Term);
        Consent -> Consent
    end.

marker_consent(Term) when is_map(Term) ->
    case maps:get(synthetic, Term, undefined) of
        true ->
            #{kind => synthetic, synthetic => true};
        _ ->
            case maps:get(kind, Term, undefined) of
                synthetic -> #{kind => synthetic, synthetic => true};
                _ -> #{kind => unknown}
            end
    end;
marker_consent(_Term) ->
    #{kind => unknown}.

%% @doc 是否为合成 consent。合成件只能证明状态机，不构成真实客户同意。
-spec is_synthetic(term()) -> boolean().
is_synthetic(Term) ->
    maps:get(is_synthetic, verdict(Term)).

%% @doc 报告口径标签（合成件只能报 `synthetic_fixture_verified`）。
-spec report_label(term()) -> atom().
report_label(Term) ->
    maps:get(report_label, verdict(Term)).

%% @doc 内容写入门禁（透传到 `eb_consent:gate/2`）。
%%
%% 未同意 / 缺列 / 版本漂移一律 `{error, ...}`（fail-closed）。
-spec gate(term(), term()) -> ok | {error, term()}.
gate(Term, ExpectedNoticeVersion) ->
    eb_consent:gate(canonical_consent(Term), ExpectedNoticeVersion).

%% @doc 状态机证据（**只能**声明合成状态机 PASS）。
%%
%% 两个布尔标志恒为 `false`：本 feature 在任何路径上都不给出「已获真实同意」或
%% 「合规」结论。
-spec evidence(term()) -> evidence().
evidence(Term) ->
    Verdict = verdict(Term),
    Status =
        case maps:get(kind, Verdict) of
            synthetic -> synthetic_state_machine_only;
            _ -> requires_external_gate
        end,
    #{
        status => Status,
        kind => maps:get(kind, Verdict),
        report_label => maps:get(report_label, Verdict),
        real_consent_claim => false,
        compliance_claim => false
    }.

%% @doc 应用层对 `enterprise_conversation.consent_evidence_kind` 的**唯一**可表达取值
%%（EB-06-A17 / E6-10）。
%%
%% 返回值只有两种：
%%   * `undefined` —— 无 consent（或非合成）⇒ **不得**写任何证据类别（与迁移 122 的
%%     `ck_ec_consent_evidence_kind` 同口径：`consent_at IS NULL` 时该列必须 NULL）；
%%   * `<<"synthetic">>` —— V1 本地状态机可接受的**合成**证据。
%%
%% 不存在返回 `<<"real">>` / `<<"verified_real">>` 或任何其他取值的分支：这不是
%% 「应用层拒绝非法入参」，而是**能力上不存在** —— 连调用方自报
%% `#{kind => real}` / `#{consent_evidence_kind => <<"real">>}` 也会被 `verdict/1`
%% 归一为 `unknown` ⇒ `undefined`（只降不升）。
-spec evidence_kind(term()) -> binary() | undefined.
evidence_kind(Term) ->
    case maps:get(kind, verdict(Term)) of
        synthetic -> ?SYNTHETIC_EVIDENCE_KIND;
        _ -> undefined
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

%% 注入时钟：`Params.clock` 可覆盖装配默认（测试用），缺省取端口装配实现。
consent_at(Params) ->
    case maps:get(consent_at, Params, undefined) of
        At when is_integer(At) ->
            {ok, At};
        undefined ->
            case clock_port(Params) of
                {ok, Clock} ->
                    try
                        {ok, Clock:now()}
                    catch
                        Class:Reason -> {error, {clock_unavailable, {Class, Reason}}}
                    end;
                {error, _} = Err ->
                    Err
            end;
        Other ->
            {error, {invalid_consent_at, Other}}
    end.

clock_port(Params) ->
    case maps:get(clock, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(clock)
    end.

is_non_empty_binary(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.
