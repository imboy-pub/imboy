%%% @doc EB-06 合成 consent（★ 高危语义）应用层套件（真库）。
%%%
%%% 覆盖作业书 §3 与 EB-06-A01 的 consent 侧：
%%%   * 合成路径写入的行**事后可被 `eb_consent:is_synthetic/1` 识别**，且
%%%     `report_label/1` 恒为合成（`synthetic_fixture_verified`），永不变成
%%%     「已获真实同意」；
%%%   * 不存在任何把合成值改写成真实值的代码路径（静态断言 + 行为断言各一组）；
%%%   * synthetic 证据只能声明状态机 PASS（`real_consent_claim/compliance_claim`
%%%     恒为 false，无论输入是什么）；
%%%   * 无 consent 时 `gate/2` fail-closed（`consent_required`），未同意不得持久化内容。
%%%
%%% 本套件只证明**状态机**；不构成真实客户同意、法律合规或生产存档授权（plan §2.1 #14）。
-module(eb_consent_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).
-define(SYNTHETIC_SUBJECT_PREFIX, <<"synthetic:eb06:">>).

%% EB-06 本卡交付的 application 模块（静态「无升级路径」断言的扫描面）。
-define(EB06_APP_MODULES, [
    eb_consent_app,
    eb_conversation_app,
    eb_message_app,
    eb_retention_app
]).

consent_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a01_synthetic_consent_written_by_app_is_identified_as_synthetic/0},
        {timeout, 60, fun a01_report_label_stays_synthetic_for_tampered_and_crafted_inputs/0},
        {timeout, 60, fun a01_no_code_path_can_upgrade_synthetic_to_real/0},
        {timeout, 60, fun a01_evidence_only_declares_state_machine_pass/0},
        {timeout, 60, fun a01_gate_fails_closed_without_consent/0},
        {timeout, 60, fun a01_stored_columns_carry_frozen_synthetic_markers/0}
    ];
cases(Other) ->
    erlang:error({eb06_consent_suite_db_unavailable, Other}).

%% ===================================================================
%% ★ 合成 consent 可被识别，且 report_label 恒为合成
%% ===================================================================

a01_synthetic_consent_written_by_app_is_identified_as_synthetic() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        {ok, Opened} = open_synthetic_conversation(Scope, Org, Ws),
        ConversationId = maps:get(conversation_id, Opened),
        %% 从 DB 回读（不是用内存里的值）：识别必须对**落库的行**成立
        {ok, Row} = eb_pg_store:fetch_conversation(Org, Ws, ConversationId),
        Consent = eb_consent_app:consent_of(Row),
        ?assertNotEqual(undefined, Consent),
        ?assert(eb_consent:is_synthetic(Consent)),
        ?assert(eb_consent:'synthetic?'(Consent)),
        ?assertEqual(synthetic, eb_consent:classify(Consent)),
        ?assertEqual(synthetic_fixture_verified, eb_consent:report_label(Consent)),
        %% 本卡的应用层包装给出同一结论（不允许出现第二种口径）
        ?assert(eb_consent_app:is_synthetic(Row)),
        ?assertEqual(synthetic_fixture_verified, eb_consent_app:report_label(Row)),
        ?assertEqual(ok, eb_consent_app:gate(Row, any)),
        ?assertEqual(ok, eb_consent_app:gate(Row, eb_consent_app:synthetic_notice_version()))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 行为断言：任何输入都不能让本 feature 报告「真实同意」
%% ===================================================================

a01_report_label_stays_synthetic_for_tampered_and_crafted_inputs() ->
    %% ① 被篡改的合成行：notice_version 仍是合成版本，但 consent_subject 被改写成
    %%    看似「真实」的值 ⇒ 本 feature 只能退到 requires_external_gate，绝不能
    %%    变成 real / 合规结论。
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        {ok, Opened} = open_synthetic_conversation(Scope, Org, Ws),
        ConversationId = maps:get(conversation_id, Opened),
        ok = ?FIX:exec(
            <<
                "UPDATE enterprise_conversation SET consent_subject=$3"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$4"
            >>,
            [Org, Ws, <<"tampered-not-synthetic">>, ConversationId]
        ),
        {ok, Tampered} = eb_pg_store:fetch_conversation(Org, Ws, ConversationId),
        ?assertNot(eb_consent_app:is_synthetic(Tampered)),
        ?assertEqual(requires_external_gate, eb_consent_app:report_label(Tampered)),
        ?assertEqual(unknown, maps:get(kind, eb_consent_app:verdict(Tampered))),
        %% ② 构造一个显式标记为「非合成」的 consent：本 feature 的入口必须把它
        %%    归一为 unknown（不透传），report_label 也不得给出任何合规结论。
        Crafted = #{
            kind => real,
            notice_version => <<"legal-notice-v9">>,
            consent_at => now_secs(),
            consent_subject => <<"subject">>
        },
        ?assertNot(eb_consent_app:is_synthetic(Crafted)),
        ?assertEqual(requires_external_gate, eb_consent_app:report_label(Crafted)),
        ?assertEqual(unknown, maps:get(kind, eb_consent_app:verdict(Crafted))),
        %% ③ 只有域层认定的合成输入才允许报 synthetic_fixture_verified
        ?assertEqual(synthetic_fixture_verified, eb_consent_app:report_label(#{synthetic => true})),
        %% ④ 属性式复核：对所有输入，verdict 的 kind 只能是 synthetic | unknown；
        %%    evidence 的两个布尔标志恒为 false，status 只能取两个受控值。
        Inputs = [
            undefined,
            #{},
            #{kind => real},
            #{kind => real, synthetic => false},
            Crafted,
            #{synthetic => true},
            #{kind => synthetic, synthetic => true},
            eb_consent_app:consent_of(Tampered)
        ],
        lists:foreach(
            fun(Input) ->
                Kind = maps:get(kind, eb_consent_app:verdict(Input)),
                ?assert(lists:member(Kind, [synthetic, unknown])),
                Evidence = eb_consent_app:evidence(Input),
                ?assertEqual(false, maps:get(real_consent_claim, Evidence)),
                ?assertEqual(false, maps:get(compliance_claim, Evidence)),
                ?assert(
                    lists:member(
                        maps:get(status, Evidence),
                        [synthetic_state_machine_only, requires_external_gate]
                    )
                )
            end,
            Inputs
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 静态断言：「把合成值改写成真实值」的代码路径不存在
%% ===================================================================

%% 判据链（可机械核对）：
%%   1. `eb_consent:classify/1` 只有在 map 里**出现字面 atom `real`** 时才返回
%%      `real`（EB-02 冻结合同）；
%%   2. EB-06 的四个 application 模块源码（去注释后）**不含 `real` 这个 token**；
%%      ⇒ 本卡不存在能构造出 `kind := real` 的代码路径；
%%   3. `eb_consent_app` 的导出被白名单锁死（未来新增「升级」入口会立刻变红）。
a01_no_code_path_can_upgrade_synthetic_to_real() ->
    Expected = lists:sort([
        {consent_of, 1},
        {evidence, 1},
        %% EB-06 重开新增：应用层对该列的**唯一**可表达取值（synthetic | undefined）
        {evidence_kind, 1},
        {gate, 2},
        {is_synthetic, 1},
        {report_label, 1},
        {synthetic_fields, 2},
        {synthetic_notice_version, 0},
        {synthetic_subject, 1},
        {verdict, 1}
    ]),
    ?assertEqual(Expected, lists:sort(drop_module_info(eb_consent_app:module_info(exports)))),
    lists:foreach(
        fun(Mod) ->
            Code = module_code(Mod),
            ?assertEqual(
                {Mod, nomatch},
                {Mod, re:run(Code, <<"([^a-zA-Z0-9_]|^)real([^a-zA-Z0-9_]|$)">>, [{capture, none}])}
            ),
            ?assertEqual(
                {Mod, nomatch},
                {Mod, re:run(Code, <<"kind\\s*=>\\s*real">>, [{capture, none}])}
            )
        end,
        ?EB06_APP_MODULES
    ).

%% ===================================================================
%% 证据只能声明状态机 PASS
%% ===================================================================

a01_evidence_only_declares_state_machine_pass() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        {ok, Opened} = open_synthetic_conversation(Scope, Org, Ws),
        {ok, Row} = eb_pg_store:fetch_conversation(
            Org, Ws, maps:get(conversation_id, Opened)
        ),
        Evidence = eb_consent_app:evidence(Row),
        ?assertEqual(synthetic_state_machine_only, maps:get(status, Evidence)),
        ?assertEqual(synthetic, maps:get(kind, Evidence)),
        ?assertEqual(synthetic_fixture_verified, maps:get(report_label, Evidence)),
        ?assertEqual(false, maps:get(real_consent_claim, Evidence)),
        ?assertEqual(false, maps:get(compliance_claim, Evidence)),
        %% 无 consent 的行只能拿 requires_external_gate（同样不含任何合规结论）
        NoConsent = ?FIX:new_scope(#{with_consent => false}),
        try
            {NOrg, NWs} = tenant(NoConsent),
            {ok, NRow} = eb_pg_store:fetch_conversation(
                NOrg, NWs, maps:get(conversation_id, NoConsent)
            ),
            NEvidence = eb_consent_app:evidence(NRow),
            ?assertEqual(requires_external_gate, maps:get(status, NEvidence)),
            ?assertEqual(unknown, maps:get(kind, NEvidence)),
            ?assertEqual(requires_external_gate, maps:get(report_label, NEvidence)),
            ?assertEqual(false, maps:get(real_consent_claim, NEvidence)),
            ?assertEqual(false, maps:get(compliance_claim, NEvidence))
        after
            ?FIX:cleanup(NoConsent)
        end
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% fail-closed：无 consent 不写内容
%% ===================================================================

a01_gate_fails_closed_without_consent() ->
    NoConsent = ?FIX:new_scope(#{with_consent => false}),
    try
        {Org, Ws} = tenant(NoConsent),
        ConversationId = maps:get(conversation_id, NoConsent),
        {ok, Row} = eb_pg_store:fetch_conversation(Org, Ws, ConversationId),
        ?assertEqual(undefined, eb_consent_app:consent_of(Row)),
        ?assertNot(eb_consent_app:is_synthetic(Row)),
        ?assertEqual({error, consent_required}, eb_consent_app:gate(Row, any)),
        ?assertEqual({error, consent_required}, eb_consent_app:gate(undefined, any)),
        ?assertEqual({error, consent_required}, eb_consent_app:gate(#{}, any)),
        %% 只缺 consent_subject 也必须 fail-closed（不能只看 consent_at）
        ?assertEqual(
            {error, consent_required},
            eb_consent_app:gate(
                #{
                    notice_version => eb_consent_app:synthetic_notice_version(),
                    consent_at => now_secs()
                },
                any
            )
        ),
        %% notice_version 漂移（期望版本 ≠ 记录版本）也必须拒绝
        ok
    after
        ?FIX:cleanup(NoConsent)
    end,
    Synthetic = ?FIX:new_scope(),
    try
        {SOrg, SWs} = tenant(Synthetic),
        {ok, Opened} = open_synthetic_conversation(Synthetic, SOrg, SWs),
        {ok, SynthRow} = eb_pg_store:fetch_conversation(
            SOrg, SWs, maps:get(conversation_id, Opened)
        ),
        ?assertMatch(
            {error, {notice_version_mismatch, _, _}},
            eb_consent_app:gate(SynthRow, <<"some-other-notice-version">>)
        ),
        %% 版本一致时通过（同一条行，证明上一条不是「笼统拒绝」）
        ?assertEqual(ok, eb_consent_app:gate(SynthRow, eb_consent_app:synthetic_notice_version()))
    after
        ?FIX:cleanup(Synthetic)
    end.

%% ===================================================================
%% 落库列携带冻结的合成标记（可事后识别）
%% ===================================================================

a01_stored_columns_carry_frozen_synthetic_markers() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        {ok, Opened} = open_synthetic_conversation(Scope, Org, Ws),
        ConversationId = maps:get(conversation_id, Opened),
        Row = ?FIX:scalar(
            <<
                "SELECT (notice_version || '|' || consent_subject || '|' ||"
                "        (consent_at IS NOT NULL)::text) AS blob"
                "  FROM enterprise_conversation"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, ConversationId]
        ),
        Version = eb_consent_app:synthetic_notice_version(),
        Subject = eb_consent_app:synthetic_subject(ConversationId),
        ?assertEqual(
            iolist_to_binary([Version, <<"|">>, Subject, <<"|true">>]),
            iolist_to_binary(Row)
        ),
        %% 合成标记必须自证「非真实告知文本」：版本号里带 synthetic，主体带前缀
        ?assert(binary:match(Version, <<"synthetic">>) =/= nomatch),
        ?assertMatch({0, _}, binary:match(Subject, ?SYNTHETIC_SUBJECT_PREFIX)),
        %% 合成主体不得包含任何真实客户标识（只有受控前缀 + 资源 ID）
        ?assertEqual(Subject, eb_consent_app:synthetic_subject(ConversationId)),
        ?assertNotEqual(Subject, <<>>)
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

now_secs() ->
    eb_system_clock:now().

%% 走本卡的合成路径建立会话：服务端解析的默认 Workspace = 该 Org 的 Workspace。
open_synthetic_conversation(Scope, Org, Ws) ->
    eb_conversation_app:open_conversation(Org, #{
        workspace_id => Ws,
        contact_id => maps:get(contact_id, Scope),
        business_identity_id => maps:get(sales_identity_id, Scope),
        default_workspace => fun(_OrgId) -> {ok, Ws} end,
        consent_at => now_secs()
    }).

drop_module_info(Exports) ->
    [E || E <- Exports, E =/= {module_info, 0}, E =/= {module_info, 1}].

%% 读取模块源码并去掉行注释（`%` 之后），用于 token 级静态断言。
module_code(Mod) ->
    Path = source_path(Mod),
    case file:read_file(Path) of
        {ok, Bin} ->
            Lines = binary:split(Bin, <<"\n">>, [global]),
            iolist_to_binary([
                [
                    re:replace(Line, <<"%.*$">>, <<>>, [{return, binary}]),
                    <<"\n">>
                ]
             || Line <- Lines
            ]);
        {error, Reason} ->
            erlang:error({eb06_source_unreadable, Mod, Path, Reason})
    end.

source_path(Mod) ->
    Rel = "src/features/enterprise_business/application/" ++ source_rel(Mod),
    case [P || P <- [filename:join(Root, Rel) || Root <- root_candidates()], filelib:is_file(P)] of
        [Path | _] -> Path;
        [] -> erlang:error({eb06_source_missing, Mod, Rel, root_candidates()})
    end.

source_rel(eb_consent_app) -> "consent/eb_consent_app.erl";
source_rel(eb_conversation_app) -> "conversation/eb_conversation_app.erl";
source_rel(eb_message_app) -> "message/eb_message_app.erl";
source_rel(eb_retention_app) -> "retention/eb_retention_app.erl".

root_candidates() ->
    FromBeam =
        case code:which(eb_consent_app) of
            BeamPath when is_list(BeamPath) ->
                [filename:dirname(filename:dirname(BeamPath))];
            _ ->
                []
        end,
    FromLib =
        try
            [code:lib_dir(imboy)]
        catch
            _:_ -> []
        end,
    FromCwd = [element(2, file:get_cwd())],
    FromBeam ++ FromLib ++ FromCwd.
