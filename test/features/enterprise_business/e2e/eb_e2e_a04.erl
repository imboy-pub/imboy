%%% @doc EB-11-A04：current Base artifact/hash 完整；synthetic consent / 1095d / hold
%%% **只声明状态机与算法**（test-only）。
%%%
%%% 依据：EB-11-A04、plan §2.1 #9/#14/#17、`agents/a5/EB-11/ORDER.md` §4（F6/口径）。
%%%
%%% 本模块只做三类机械核对：
%%%   * **Base/构件**：HEAD == 冻结 Base；`.contract` 与 `include/generated` 内的企业条目在位；
%%%     竣工指纹（HEAD + 关键构件 SHA-256）落盘，供 A0 独立复核。
%%%   * **合成同意**：consent 证据类别在 DB 层**只可能**是 `synthetic`（尝试写 'real' 必被 CHECK 拒）；
%%%     因此任何「真实客户同意 / 合规」声明都不可由本 run 的数据支撑。
%%%   * **1095d / hold**：只核对**算法与状态机**（policy 版本快照、retain_until 推导、synthetic 必填），
%%%     明确**不**产生法律/监管/合规结论。
-module(eb_e2e_a04).

-export([run/1]).

run(Ctx) ->
    Scope = maps:get(scope, Ctx),
    Org1 = maps:get(org1, Scope),
    Ws1 = maps:get(ws1, Scope),
    ConvId = maps:get(conversation_id, Ctx),
    io:format("~n== EB-11-A04 Base 构件/hash 完整 + 合成同意/1095d/hold 只声明状态机 ==~n"),

    %% 1) Base / HEAD
    Head = env_or(<<"EB11_HEAD">>, <<"(unset)">>),
    Base = env_or(<<"EB11_BASE_SHA">>, <<"(unset)">>),
    eb_e2e_lib:assert(
        <<"EB-11-A04.1">>,
        io_lib:format("HEAD 与冻结 Base 逐字一致（HEAD=~ts base=~ts）", [Head, Base]),
        Head =/= <<"(unset)">> andalso Head =:= Base
    ),

    %% 2) 迁移头 + 企业迁移在位
    %% 迁移头只设下界（>= 124）：后续域（如 customer_service 125）追加迁移属正常
    %% 演进，EB 域关心的是库不落后于代码且自身 114..124 成对在位（A0 2026-09-16）。
    MigHead = eb_e2e_lib:scalar(
        <<"SELECT max(version) AS v FROM schema_migrations">>, [], undefined
    ),
    MigrationFiles = lists:sort(
        filelib:wildcard("priv/migrations/0000011[4-9]*") ++
            filelib:wildcard("priv/migrations/0000012[0-4]*")
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A04.2">>,
        io_lib:format(
            "DB 迁移头 >= 124（实测 ~p）；企业迁移 114..124 成对在位（~p 个文件）",
            [MigHead, length(MigrationFiles)]
        ),
        is_integer(MigHead) andalso MigHead >= 124 andalso length(MigrationFiles) =:= 22
    ),

    %% 3) 构件内容（contract / feature 生成物）
    ContractOk = file_contains(
        ".contract/api_contract.json", <<"/api/v1/enterprise/organizations/">>
    ),
    FeatureOk = file_contains(
        "include/generated/imboy_product_features.hrl",
        <<"IMBOY_FEATURE_ENTERPRISE_BUSINESS, true">>
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A04.3">>,
        io_lib:format(
            "契约与生成物含企业条目（.contract/api_contract.json=~p；generated hrl=~p）",
            [ContractOk, FeatureOk]
        ),
        ContractOk andalso FeatureOk
    ),
    Artifacts = [
        <<".contract/api_contract.json">>,
        <<"include/generated/imboy_product_features.hrl">>,
        <<"config/product-feature-manifest.json">>,
        <<"src/imboy_router.erl">>,
        <<"Makefile">>
    ],
    Fingerprints = [
        {A, file_sha256(A), file_size(A)}
     || A <- Artifacts
    ],
    eb_e2e_lib:evidence(
        "a04-artifacts.txt",
        "head=~ts base=~ts migration_head=~p artifacts=~p",
        [Head, Base, MigHead, Fingerprints]
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A04.4">>,
        io_lib:format("Base 构件指纹完整（~p 个文件均有 SHA-256 且非空）", [length(Fingerprints)]),
        lists:all(fun({_F, Hash, Size}) -> is_binary(Hash) andalso Size > 0 end, Fingerprints)
    ),

    %% 4) 合成同意：DB 层只可能 synthetic
    ConsentKind = eb_e2e_lib:scalar(
        <<
            "SELECT consent_evidence_kind AS v FROM enterprise_conversation"
            " WHERE organization_id=$1 AND id=$2"
        >>,
        [Org1, ConvId],
        undefined
    ),
    RealRejected = try_real_evidence_kind(Org1, ConvId),
    eb_e2e_lib:assert(
        <<"EB-11-A04.5">>,
        io_lib:format(
            "consent 证据类别与 consent 同 INSERT 固化为 synthetic（实测 ~p）；"
            "尝试写成 'real' 被 DB CHECK 拒（~p） ⇒ 本 run 只声明合成状态机",
            [ConsentKind, RealRejected]
        ),
        ConsentKind =:= <<"synthetic">> andalso RealRejected =/= ok
    ),
    eb_e2e_lib:evidence(
        "a04-consent-evidence.txt",
        "consent_evidence_kind=~p | update_to_real_result=~p（被 ck_ec_consent_evidence_kind 拒）",
        [ConsentKind, RealRejected]
    ),

    %% 5) hold 必须显式 synthetic
    NonSynthetic = enterprise_business_facade:create_hold(Org1, #{
        workspace_id => Ws1,
        scope => <<"conversation">>,
        scope_conversation_id => ConvId,
        reason_code => <<"eb11-non-synthetic">>
    }),
    eb_e2e_lib:assert(
        <<"EB-11-A04.6">>,
        io_lib:format("未声明 synthetic 的 hold 被拒（实测 ~p）⇒ 本 run 的 hold 只是合成状态机", [
            NonSynthetic
        ]),
        element(1, NonSynthetic) =:= error
    ),

    %% 6) 1095d：策略快照 + 推导算法（不含合规结论）
    PolicyRow = hd(
        eb_e2e_lib:rows(
            <<
                "SELECT id, version, retention_days, data_class, trigger_event"
                " FROM enterprise_retention_policy WHERE organization_id=$1 AND workspace_id=$2"
                " AND data_class='enterprise_message' ORDER BY version DESC LIMIT 1"
            >>,
            [Org1, Ws1]
        )
    ),
    MsgPolicy = eb_e2e_lib:scalar(
        <<"SELECT retention_days FROM enterprise_message WHERE organization_id=$1 LIMIT 1">>,
        [Org1],
        undefined
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A04.7">>,
        io_lib:format(
            "1095d 只是**算法口径**：策略行 retention_days=~p（data_class=~p）与消息行固化 retention_days=~p 一致",
            [
                maps:get(<<"retention_days">>, PolicyRow),
                maps:get(<<"data_class">>, PolicyRow),
                MsgPolicy
            ]
        ),
        maps:get(<<"retention_days">>, PolicyRow) =:= 1095 andalso MsgPolicy =:= 1095
    ),
    eb_e2e_lib:evidence(
        "a04-1095d-algorithm.txt",
        "policy=~p message_retention_days=~p（仅算法与状态机口径；不含法律/监管/合规结论）",
        [PolicyRow, MsgPolicy]
    ),
    %% 声明上限：本 run 的证据里**必须**出现 LOCAL_FOUNDATION_PASS 口径，且不得出现越界口径
    Ceiling = ceiling_scan(),
    eb_e2e_lib:assert(
        <<"EB-11-A04.8">>,
        io_lib:format("口径上限自检：证据/结果里出现越界口径=~p（应为空）", [Ceiling]),
        Ceiling =:= []
    ),
    Ctx.

%% ===================================================================
%% 辅助
%% ===================================================================

try_real_evidence_kind(OrgId, ConvId) ->
    case
        eb_e2e_lib:exec(
            <<
                "UPDATE enterprise_conversation SET consent_evidence_kind='real'"
                " WHERE organization_id=$1 AND id=$2"
            >>,
            [OrgId, ConvId]
        )
    of
        ok -> ok;
        {error, Reason} -> {rejected, normalize(Reason)}
    end.

normalize({error, _, _, Detail}) -> Detail;
normalize({error, Reason}) -> Reason;
normalize(Other) -> Other.

file_contains(Path, Needle) ->
    case file:read_file(Path) of
        {ok, Bin} -> binary:match(Bin, Needle) =/= nomatch;
        {error, _} -> false
    end.

file_sha256(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> binary:encode_hex(crypto:hash(sha256, Bin), lowercase);
        {error, _} -> undefined
    end.

file_size(Path) ->
    case file:read_file_info(Path) of
        {ok, Info} -> element(2, Info);
        _Other -> 0
    end.

env_or(Name, Default) ->
    case os:getenv(binary_to_list(Name)) of
        false -> Default;
        Value -> list_to_binary(Value)
    end.

%% 越界口径机械扫描（本 run 的全部证据文件 + 既有 RESULT.json）
ceiling_scan() ->
    Forbidden = [
        <<"PRODUCTION_READY">>,
        <<"REAL_CUSTOMER_ACCEPTED">>,
        <<"RELEASE_PASS">>,
        <<"real_garage_acceptance=PASS">>,
        <<"COMPLIANCE_PASS">>,
        <<"LEGAL_HOLD_VERIFIED">>
    ],
    Dir =
        case os:getenv("EB11_EVIDENCE_DIR") of
            false -> "/tmp";
            D -> D
        end,
    Files =
        filelib:wildcard(filename:join(Dir, "**/*.txt")) ++
            filelib:wildcard(filename:join(Dir, "*.json")),
    lists:flatmap(
        fun(File) ->
            case file:read_file(File) of
                {ok, Bin} ->
                    [{File, N} || N <- Forbidden, binary:match(Bin, N) =/= nomatch];
                _Other ->
                    []
            end
        end,
        Files
    ).
