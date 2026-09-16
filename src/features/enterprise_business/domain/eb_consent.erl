%%% @doc 企业会话 consent（告知/同意）门禁的领域纯函数。
%%%
%%% 依据：plan v4.1 EB-D05、§2.1 #9/#14、§5.4、§2.3。
%%%
%%% 纯净性（铁律 4）：无 I/O、无进程、无隐式时间源。
%%%
%%% 冻结合同：
%%%   * **fail-closed**：没有 consent、`notice_version` 为空、缺 `consent_at`
%%%     或缺 `consent_subject` 时一律返回 `{error, consent_required}`，未同意
%%%     不得持久化企业内容；
%%%   * `classify/1` 必须能区分 synthetic 与 real，**严禁**任何把 synthetic
%%%     映射为 real / compliance 的分支；
%%%   * 本地只使用合成版本验证状态机，任何报告都不得声称真实客户同意、
%%%     法律或监管合规（plan §2.1 #14）。
%%%
%%% 命名说明：plan v4.1 §5.1 把谓词写作 `synthetic?/1`；Erlang 未加引号的
%%% atom 不允许 `?`。主 API 为 `is_synthetic/1`，并保留同义字面别名
%%% `'synthetic?'/1`（行为完全一致）以便与合同文本逐字对照。
-module(eb_consent).

-export([
    gate/2,
    classify/1,
    is_synthetic/1,
    'synthetic?'/1,
    report_label/1
]).

-type consent() :: map().
-type consent_kind() :: synthetic | real | unknown.
-type report_label() :: synthetic_fixture_verified | requires_external_gate.

-export_type([consent/0, consent_kind/0, report_label/0]).

%% ===================================================================
%% 内容写入门禁
%% ===================================================================

%% @doc 企业内容（message / asset）写入前的 consent 门禁。
%%
%% `ExpectedNoticeVersion` 为 `any` 或 `undefined` 时跳过版本比对；为 binary 时
%% 必须与 consent 记录的 `notice_version` 完全一致，否则
%% `{error, {notice_version_mismatch, Expected, Actual}}`。
%%
%% 一切缺失/空值路径都走 `{error, consent_required}`（fail-closed）。
-spec gate(term(), term()) -> ok | {error, term()}.
gate(Consent, ExpectedNoticeVersion) when is_map(Consent) ->
    case notice_version(Consent) of
        {ok, Actual} ->
            case consent_at(Consent) of
                {ok, _At} ->
                    case consent_subject(Consent) of
                        {ok, _Subject} -> check_notice_version(ExpectedNoticeVersion, Actual);
                        {error, _} = Err -> Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end;
gate(_MissingConsent, _ExpectedNoticeVersion) ->
    {error, consent_required}.

check_notice_version(Expected, Actual) when is_binary(Expected) ->
    case Expected =:= Actual of
        true -> ok;
        false -> {error, {notice_version_mismatch, Expected, Actual}}
    end;
check_notice_version(_Any, _Actual) ->
    ok.

notice_version(Consent) ->
    case maps:get(notice_version, Consent, undefined) of
        Version when is_binary(Version), Version =/= <<>> -> {ok, Version};
        _MissingOrEmpty -> {error, consent_required}
    end.

consent_at(Consent) ->
    case maps:get(consent_at, Consent, undefined) of
        At when is_integer(At) -> {ok, At};
        _MissingOrInvalid -> {error, consent_required}
    end.

consent_subject(Consent) ->
    case maps:get(consent_subject, Consent, undefined) of
        Subject when is_binary(Subject), Subject =/= <<>> -> {ok, Subject};
        _MissingOrEmpty -> {error, consent_required}
    end.

%% ===================================================================
%% synthetic / real 分类
%% ===================================================================

%% @doc 分类 consent 的来源。
%%
%% `synthetic` 只表示本地合成 fixture；`real` 只表示标记为真实的记录。
%% 本函数不产生任何合规结论。
-spec classify(term()) -> consent_kind().
classify(#{kind := synthetic}) ->
    synthetic;
classify(#{kind := real}) ->
    real;
classify(#{synthetic := true}) ->
    synthetic;
classify(_Other) ->
    unknown.

%% @doc 是否为合成 consent。合成件只能证明状态机，不构成真实客户同意。
-spec is_synthetic(term()) -> boolean().
is_synthetic(Consent) ->
    classify(Consent) =:= synthetic.

%% @doc `is_synthetic/1` 的合同字面别名（plan §5.1 写作 `synthetic?/1`）。
-spec 'synthetic?'(term()) -> boolean().
'synthetic?'(Consent) ->
    is_synthetic(Consent).

%% @doc 报告口径标签。
%%
%% 合成件只能报告为 `synthetic_fixture_verified`；其余（含被标记为 real 的记录）
%% 一律停在 `requires_external_gate`，绝不返回任何 real / compliance 结论。
-spec report_label(term()) -> report_label().
report_label(Consent) ->
    case is_synthetic(Consent) of
        true -> synthetic_fixture_verified;
        false -> requires_external_gate
    end.
