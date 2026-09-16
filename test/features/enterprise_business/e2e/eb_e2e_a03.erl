%%% @doc EB-11-A03：DB / object / log / evidence **无明文 secret / PII / storage URL / object key**
%%% 的**机械扫描**（test-only）。
%%%
%%% 依据：plan §2.1 #13、EB-11-A03、`agents/a5/EB-11/ORDER.md` §5（「A03 的机械扫描证据」）。
%%%
%%% 扫描面（四层，逐层给原始输出）：
%%%   1. **DB（全库）**：public 下每张基表，行的文本形态里是否出现任一**金丝雀明文**
%%%      （客户 subject / 资料 / 备注 / 消息体 / 附件体）。`row_to_json`/`t::text` 判据是
%%%      「值层」，与列名无关 ⇒ 密文列、JSONB 审计 detail、普通文本列一视同仁。
%%%   2. **DB（企业表）**：存储能力串（`://` / object_key / bucket / endpoint / presign /
%%%      garage / X-Amz-）在**本 run 的作用域**内是否出现；`object_key` 是否 URL 形状。
%%%   3. **HTTP 响应留档**：企业路由的每个响应（头 + 体）是否含金丝雀或存储能力串。
%%%   4. **对象桶 / 证据目录**：替身桶键的形状与泄露串；证据目录（含 canaries.txt 之外的
%%%      全部文件）由 shell 侧再扫一遍（脚本 `--scan-evidence`）。
%%%
%%% 说明：日志侧（runner 的 stdout/stderr 与 lager 日志）由 shell 侧扫描（同一金丝雀集合，
%%% 见 `internal/canaries.txt`），因为日志在进程之外。
-module(eb_e2e_a03).

-export([run/1]).

run(Ctx) ->
    Scope = maps:get(scope, Ctx),
    Org1 = maps:get(org1, Scope),
    Ws1 = maps:get(ws1, Scope),
    Canaries = maps:get(canaries, Ctx),
    io:format("~n== EB-11-A03 DB/object/log/evidence 无明文 secret/PII/storage URL/object key ==~n"),
    %% 金丝雀清单落盘（shell 侧日志扫描用；放在 internal/ 下，证据扫描排除该目录）
    [
        eb_e2e_lib:evidence(<<"internal/canaries.txt">>, "~ts", [C])
     || C <- Canaries
    ],

    %% 1) 全库扫描：任一金丝雀明文出现在任何基表
    Tables = public_tables(),
    Regex = alternation(Canaries),
    {HitTables, Scanned} = scan_tables(Tables, Regex),
    eb_e2e_lib:assert(
        <<"EB-11-A03.1">>,
        io_lib:format(
            "全库（public ~p 张基表）无任何金丝雀明文：命中表=~p（金丝雀=~p 种子）",
            [Scanned, HitTables, length(Canaries)]
        ),
        HitTables =:= []
    ),
    eb_e2e_lib:evidence(
        "a03-db-fullscan.txt",
        "tables_scanned=~p canaries=~p hit_tables=~p",
        [Scanned, length(Canaries), HitTables]
    ),

    %% 2) 企业表：存储能力串 + object_key 形状
    EnterpriseTables = [
        <<"enterprise_asset">>,
        <<"enterprise_message">>,
        <<"enterprise_message_delivery">>,
        <<"enterprise_conversation">>,
        <<"enterprise_contact">>,
        <<"enterprise_contact_identity">>,
        <<"enterprise_note">>,
        <<"enterprise_retention_policy">>,
        <<"enterprise_retention_hold">>,
        <<"enterprise_audit_event">>,
        <<"enterprise_offboarding_case">>,
        <<"enterprise_offboarding_item">>,
        <<"organization_business_identity">>,
        <<"organization_business_identity_assignment">>
    ],
    LeakRegex = alternation([
        <<"://">>,
        <<"object_key">>,
        <<"storage_ref">>,
        <<"bucket">>,
        <<"endpoint">>,
        <<"presign">>,
        <<"garage">>,
        <<"X-Amz-">>
    ]),
    LeakHits = [
        {Table, Hits}
     || Table <- EnterpriseTables,
        Hits <- [count_matching(Table, LeakRegex, [Org1, Ws1])],
        Hits > 0
    ],
    eb_e2e_lib:assert(
        <<"EB-11-A03.2">>,
        io_lib:format(
            "企业表内无存储能力串（`://`/object_key/bucket/endpoint/presign/garage/X-Amz-）：命中=~p",
            [LeakHits]
        ),
        LeakHits =:= []
    ),
    UrlShapedKey = eb_e2e_lib:scalar(
        <<
            "SELECT count(*) AS n FROM enterprise_asset"
            " WHERE organization_id=$1 AND object_key ~* '^[a-zA-Z][a-zA-Z0-9+.\\-]*://'"
        >>,
        [Org1],
        0
    ),
    PrivatePrefix = eb_asset_object_stub:key_prefix(Org1, Ws1),
    KeyShapes = [
        maps:get(<<"object_key">>, R)
     || R <- eb_e2e_lib:rows(
            <<"SELECT object_key FROM enterprise_asset WHERE organization_id=$1">>, [Org1]
        )
    ],
    eb_e2e_lib:assert(
        <<"EB-11-A03.3">>,
        io_lib:format(
            "DB 内 object_key 一律非 URL 形状（URL 形状计数=~p）且带私有测试前缀；实测键=~p",
            [UrlShapedKey, KeyShapes]
        ),
        UrlShapedKey =:= 0 andalso
            lists:all(fun(K) -> eb_e2e_lib:binary_prefix(PrivatePrefix, K) end, KeyShapes)
    ),
    eb_e2e_lib:evidence("a03-db-keys.txt", "object_keys=~p url_shaped=~p", [KeyShapes, UrlShapedKey]),

    %% 3) HTTP 响应留档（企业路由）
    Responses = eb_e2e_lib:recorded_responses(),
    EnterpriseResponses = [
        R
     || R <- Responses, binary:match(maps:get(path, R), <<"/api/v1/enterprise/">>) =/= nomatch
    ],
    %% 被授权的代理下载（/assets/:id/content）**响应体就是对象字节**——那是调用方请求的数据，
    %% 不是泄露；它的存储侧泄露判据由 A01/A02 的同名断言覆盖，这里不算金丝雀命中。
    %% 其余企业响应一律纳入金丝雀扫描。
    NonContentResponses = [
        R
     || R <- EnterpriseResponses,
        binary:match(maps:get(path, R), <<"/content">>) =:= nomatch
    ],
    ResponseCanaryHits = [
        {
            maps:get(status, R),
            maps:get(path, R),
            eb_e2e_lib:contains_any(eb_e2e_lib:body(R), Canaries)
        }
     || R <- NonContentResponses,
        eb_e2e_lib:contains_any(eb_e2e_lib:body(R), Canaries) =/= []
    ],
    ResponseLeakHits = [
        {maps:get(status, R), maps:get(path, R), eb_e2e_lib:leak_scan(eb_e2e_lib:body(R))}
     || R <- EnterpriseResponses,
        eb_e2e_lib:leak_scan(eb_e2e_lib:body(R)) =/= []
    ],
    HeaderLeakHits = [
        {maps:get(path, R), eb_e2e_lib:leak_scan(maps:get(headers, R))}
     || R <- EnterpriseResponses,
        eb_e2e_lib:leak_scan(maps:get(headers, R)) =/= []
    ],
    eb_e2e_lib:assert(
        <<"EB-11-A03.4">>,
        io_lib:format(
            "企业路由响应（~p 条；含被授权下载）无金丝雀明文（排除下载体本身）、无存储能力串"
            "（体命中=~p，头命中=~p）",
            [length(EnterpriseResponses), ResponseCanaryHits, HeaderLeakHits]
        ),
        ResponseCanaryHits =:= [] andalso ResponseLeakHits =:= [] andalso HeaderLeakHits =:= []
    ),
    eb_e2e_lib:evidence(
        "a03-http-responses.txt",
        "enterprise_responses=~p canary_hits=~p body_leak_hits=~p header_leak_hits=~p",
        [length(EnterpriseResponses), ResponseCanaryHits, ResponseLeakHits, HeaderLeakHits]
    ),

    %% 4) 审计 detail（JSONB）与投递行同样在扫描面内（此处给出显式计数）
    AuditCanary = eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE detail::text ~ $1">>,
        [Regex],
        0
    ),
    eb_e2e_lib:assert_eq(
        <<"EB-11-A03.5">>,
        "企业审计事件 detail（JSONB）不含任何金丝雀明文",
        0,
        AuditCanary
    ),

    %% 5) 对象桶键形状
    BucketKeys = bucket_keys(),
    eb_e2e_lib:assert(
        <<"EB-11-A03.6">>,
        io_lib:format(
            "本地替身桶键（~p 个）带私有前缀且不含存储能力串：~p",
            [length(BucketKeys), BucketKeys]
        ),
        lists:all(
            fun(K) ->
                eb_e2e_lib:binary_prefix(PrivatePrefix, K) andalso eb_e2e_lib:leak_scan(K) =:= []
            end,
            BucketKeys
        )
    ),
    eb_e2e_lib:evidence(
        "a03-object-bucket.txt",
        "bucket_keys=~p private_prefix=~ts leak_per_key=~p",
        [BucketKeys, PrivatePrefix, [eb_e2e_lib:leak_scan(K) || K <- BucketKeys]]
    ),

    %% 6) 消息明文（金丝雀）未出现在任何企业响应/DB 的显式复述
    MsgCanaries = [C || C <- Canaries, binary:match(C, <<"MSG">>) =/= nomatch],
    MsgHits = [
        {T, count_matching(T, alternation(MsgCanaries), [Org1, Ws1])}
     || T <- [<<"enterprise_message">>, <<"enterprise_message_delivery">>],
        count_matching(T, alternation(MsgCanaries), [Org1, Ws1]) > 0
    ],
    eb_e2e_lib:assert(
        <<"EB-11-A03.7">>,
        io_lib:format("消息明文金丝雀（~p 条）不在消息/投递表的任何列：命中=~p", [
            length(MsgCanaries), MsgHits
        ]),
        MsgHits =:= []
    ),
    Ctx.

%% ===================================================================
%% 扫描辅助
%% ===================================================================

public_tables() ->
    [
        maps:get(<<"table_name">>, R)
     || R <- eb_e2e_lib:rows(
            <<
                "SELECT table_name FROM information_schema.tables"
                " WHERE table_schema='public' AND table_type='BASE TABLE' ORDER BY table_name"
            >>,
            []
        ),
        maps:get(<<"table_name">>, R) =/= <<"spatial_ref_sys">>
    ].

scan_tables(Tables, Regex) ->
    lists:foldl(
        fun(Table, {Hits, Scanned}) ->
            case count_matching_any(Table, Regex) of
                N when is_integer(N), N > 0 -> {Hits ++ [{Table, N}], Scanned + 1};
                N when is_integer(N) -> {Hits, Scanned + 1};
                _Skipped -> {Hits, Scanned}
            end
        end,
        {[], 0},
        Tables
    ).

%% 全库判定：整行文本匹配（表名做引号包裹，避免保留字/大小写问题）。
count_matching_any(Table, Regex) ->
    Quoted = <<"\"", Table/binary, "\"">>,
    Sql = iolist_to_binary(["SELECT count(*) AS n FROM ", Quoted, " t WHERE t::text ~ $1"]),
    case eb_e2e_lib:q(Sql, [Regex]) of
        {ok, [Row | _]} -> maps:get(<<"n">>, Row);
        _Other -> skipped
    end.

%% 企业表判定：限定本 run 的 Org（+Workspace，若该表有该列）。
count_matching(Table, Regex, [OrgId, WsId]) ->
    Quoted = <<"\"", Table/binary, "\"">>,
    HasWs =
        eb_e2e_lib:scalar(
            <<
                "SELECT count(*) AS n FROM information_schema.columns"
                " WHERE table_schema='public' AND table_name=$1 AND column_name='workspace_id'"
            >>,
            [Table],
            0
        ) > 0,
    Sql =
        case HasWs of
            true ->
                iolist_to_binary([
                    "SELECT count(*) AS n FROM ",
                    Quoted,
                    " t WHERE t.organization_id=$1 AND t.workspace_id=$2 AND t::text ~ $3"
                ]);
            false ->
                iolist_to_binary([
                    "SELECT count(*) AS n FROM ",
                    Quoted,
                    " t WHERE t.organization_id=$1 AND t::text ~ $2"
                ])
        end,
    Params =
        case HasWs of
            true -> [OrgId, WsId, Regex];
            false -> [OrgId, Regex]
        end,
    case eb_e2e_lib:q(Sql, Params) of
        {ok, [Row | _]} -> maps:get(<<"n">>, Row);
        _Other -> -1
    end.

alternation([]) ->
    <<"(?!)">>;
alternation(Values) ->
    iolist_to_binary([<<"(">>, lists:join(<<"|">>, [escape(B) || B <- Values]), <<")">>]).

escape(Bin) ->
    lists:foldl(
        fun(Char, Acc) ->
            case lists:member(Char, "\\^$.*+?()[]{}|") of
                true -> <<Acc/binary, "\\", Char>>;
                false -> <<Acc/binary, Char>>
            end
        end,
        <<>>,
        binary_to_list(Bin)
    ).

%% 本地替身桶里的全部键（只读）。用 element/2 显式取出，避免生成器里的元组模式
%% 在某些编译路径下不匹配（本次实撞后改为显式取元）。
bucket_keys() ->
    [
        element(2, K)
     || {K, _V} <- persistent_term:get(),
        is_tuple(K),
        tuple_size(K) =:= 2,
        is_tuple(element(1, K)),
        tuple_size(element(1, K)) =:= 2,
        element(1, element(1, K)) =:= eb_asset_object_stub,
        element(2, element(1, K)) =:= object,
        is_binary(element(2, K))
    ].
