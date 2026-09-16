%%% @doc EB-11 Foundation 独立 E2E 的**测试侧基础设施**（test-only，不进任何 release）。
%%%
%%% 依据：`agents/a5/EB-11/ORDER.md` §1/§5、plan §2.1 全链、EB-11-A01..A06。
%%%
%%% 本模块只做四件事，不含任何业务判定：
%%%
%%%   1. **断言记账**：`assert/3` 逐条打印 `[ASSERT <ID>]` / `[ASSERT-FAIL <ID>]`，
%%%      末尾由 runner 汇总。失败令牌只在**真的失败**时出现（`control/verify_evidence.py`
%%%      用 `^\s*\[FAIL\]` 判定「GREEN 里有没有混进失败」）。
%%%   2. **真 HTTP**：裸 TCP 发定长请求、按 `content-length` 读定长响应；响应同时进
%%%      「响应留档」（A03 的机械扫描输入之一）。
%%%   3. **真路由 / 真中间件**：`set_facts_mode/1` 通过 `cowboy:set_env(imboy_listener,
%%%      dispatch, ...)` 只替换企业路由的 `auth_facts` 装配键——**middleware 链、路由
%%%      表、handler、facade、SQL 全部是生产件**（F1/F2 的测试侧补偿，见 probe）。
%%%   4. **机械扫描**：存储能力泄露判据（与 `eb_enterprise_http:storage_leak_scan/1`
%%%      同口径）+ 金丝雀明文出现判定。
%%%
%%% 边界（如实声明）：本模块**不**读写生产模块；`key_ref` 的注入点在 facade 调用方
%%%（见 runner 的 F6 标注），HTTP 层无法携带（动作表白名单里没有该键）。
-module(eb_e2e_lib).

-export([
    %% 断言
    reset/0,
    assert/3,
    assert_eq/4,
    note/1,
    note/2,
    tally/0,
    failures/0,
    %% HTTP
    set_port/1,
    port/0,
    req/4,
    req/5,
    code/1,
    body/1,
    json/1,
    msg/1,
    payload/1,
    status/1,
    headers/1,
    raw/1,
    record/2,
    recorded_responses/0,
    auth/1,
    get/2,
    get/3,
    post/3,
    post/4,
    pget/2,
    tsid/1,
    tenant_path/3,
    %% 路由装配（F1/F2）
    install_dispatch/0,
    set_facts_mode/1,
    facts_mode/0,
    %% DB
    q/2,
    scalar/2,
    scalar/3,
    exec/2,
    rows/2,
    count_rows/3,
    %% 合成数据 / 工具
    run_token/0,
    id/0,
    key_ref/0,
    canary/1,
    now_sec/0,
    stable_hash/1,
    leak_patterns/0,
    leak_scan/1,
    contains_any/2,
    binary_prefix/2,
    evidence/2,
    evidence/3
]).

-define(ASSERT_TAB, eb_e2e_assert).
-define(RESP_TAB, eb_e2e_responses).
-define(MODE_KEY, {?MODULE, facts_mode}).
-define(ORIG_KEY, {?MODULE, original_routes}).
-define(PORT_KEY, {?MODULE, http_port}).
-define(TOKEN_KEY, {?MODULE, run_token}).

%% ===================================================================
%% 断言记账
%% ===================================================================

-spec reset() -> ok.
reset() ->
    ensure_tables(),
    ets:delete_all_objects(?ASSERT_TAB),
    ets:insert(?ASSERT_TAB, [{passed, 0}, {failed, 0}, {failures, []}]),
    ok.

ensure_tables() ->
    case ets:info(?ASSERT_TAB) of
        undefined ->
            _ = ets:new(?ASSERT_TAB, [named_table, public, set, {write_concurrency, true}]),
            ok;
        _ ->
            ok
    end,
    case ets:info(?RESP_TAB) of
        undefined ->
            _ = ets:new(?RESP_TAB, [named_table, public, bag, {write_concurrency, true}]),
            ok;
        _ ->
            ok
    end,
    ok.

%% @doc 一条断言。`true` 即通过；`false` 记失败并打印 `[ASSERT-FAIL <ID>]`。
%% 刻意**不**抛异常：让同一 Acceptance 的其余子断言继续取证（失败集合完整可见）。
-spec assert(binary() | string(), iodata(), boolean()) -> boolean().
assert(Id, Desc0, true) ->
    Desc = flatten(Desc0),
    bump(0, 1),
    io:format("[ASSERT ~s] ~ts~n", [idb(Id), Desc]),
    true;
assert(Id, Desc0, false) ->
    Desc = flatten(Desc0),
    bump(1, 1),
    add_failure(Id, Desc),
    io:format("[ASSERT-FAIL ~s] ~ts~n", [idb(Id), Desc]),
    false.

%% 断言描述必须是**单行**（证据脚本按行解析），换行一律折成空格。
%% 注意：描述里含中文（码点 > 255）⇒ 必须用 `unicode:characters_to_binary/1`，
%% 用 `iolist_to_binary/1` 会 badarg（本次实撞后修正）。
flatten(Value) ->
    Bin = unicode:characters_to_binary(Value),
    binary:replace(binary:replace(Bin, <<"\n">>, <<" ">>, [global]), <<"\r">>, <<" ">>, [global]).

%% @doc 取值相等断言（失败时打印两侧取值——取值本身是合成夹具，不含 PII）。
-spec assert_eq(binary() | string(), iodata(), term(), term()) -> boolean().
assert_eq(Id, Desc, Expected, Actual) ->
    case Expected =:= Actual of
        true ->
            assert(Id, Desc, true);
        false ->
            assert(
                Id,
                io_lib:format("~ts :: expected=~p actual=~p", [Desc, Expected, Actual]),
                false
            )
    end.

-spec note(iodata()) -> ok.
note(Format) ->
    note(Format, []).

-spec note(iodata(), list()) -> ok.
note(Format, Args) ->
    io:format("-- " ++ to_list(Format) ++ "~n", Args).

%% @doc `{Passed, Failed}`。
-spec tally() -> {non_neg_integer(), non_neg_integer()}.
tally() ->
    Passed = lookup(passed, 0),
    Failed = lookup(failed, 0),
    {Passed, Failed}.

-spec failures() -> [{binary(), iodata()}].
failures() ->
    lookup(failures, []).

lookup(Key, Default) ->
    case ets:lookup(?ASSERT_TAB, Key) of
        [{Key, Value}] -> Value;
        [] -> Default
    end.

bump(FailDelta, PassDelta) ->
    ets:update_counter(?ASSERT_TAB, failed, FailDelta),
    ets:update_counter(?ASSERT_TAB, passed, PassDelta),
    ok.

add_failure(Id, Desc) ->
    Prev = failures(),
    ets:insert(?ASSERT_TAB, {failures, Prev ++ [{idb(Id), flatten(Desc)}]}),
    ok.

idb(Id) when is_binary(Id) -> Id;
idb(Id) -> unicode:characters_to_binary(Id).

to_list(X) when is_list(X) -> X;
to_list(X) when is_binary(X) -> binary_to_list(X).

%% ===================================================================
%% HTTP
%% ===================================================================

-spec set_port(integer()) -> ok.
set_port(Port) ->
    persistent_term:put(?PORT_KEY, Port),
    ok.

-spec port() -> integer().
port() ->
    persistent_term:get(?PORT_KEY).

-spec req(binary(), binary(), term(), map()) -> map().
req(Method, Path, Body, Headers) ->
    req(Method, Path, Body, Headers, #{}).

%% @doc 发一次请求；`Opts` 支持 `record => boolean()`（默认 true，进响应留档）。
-spec req(binary(), binary(), term(), map(), map()) -> map().
req(Method, Path, Body0, Headers0, Opts) ->
    Body = encode_body(Body0),
    Headers = maps:merge(
        #{
            <<"host">> => <<"localhost">>,
            <<"connection">> => <<"close">>,
            <<"content-length">> => integer_to_binary(byte_size(Body))
        },
        Headers0
    ),
    HeaderBin = iolist_to_binary([
        [K, <<": ">>, V, <<"\r\n">>]
     || {K, V} <- maps:to_list(Headers)
    ]),
    Req = iolist_to_binary([Method, <<" ">>, Path, <<" HTTP/1.1\r\n">>, HeaderBin, <<"\r\n">>, Body]),
    Resp =
        case gen_tcp:connect({127, 0, 0, 1}, port(), [binary, {active, false}], 20000) of
            {ok, Socket} ->
                ok = gen_tcp:send(Socket, Req),
                Raw = recv_all(Socket, []),
                _ = gen_tcp:close(Socket),
                parse(Raw);
            {error, Reason} ->
                #{status => 0, headers => #{}, body => <<>>, raw => <<>>, transport_error => Reason}
        end,
    case maps:get(record, Opts, true) of
        true -> record(Path, Resp);
        false -> ok
    end,
    Resp.

encode_body(Body) when is_binary(Body) -> Body;
encode_body(Body) when is_map(Body) -> jsx:encode(Body);
encode_body(Body) when is_list(Body) -> jsx:encode(Body);
encode_body(undefined) -> <<>>.

recv_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 20000) of
        {ok, Data} -> recv_all(Socket, [Data | Acc]);
        {error, _Reason} -> iolist_to_binary(lists:reverse(Acc))
    end.

parse(Raw) ->
    case binary:split(Raw, <<"\r\n\r\n">>) of
        [Head, BodyRaw] ->
            [StatusLine | HeaderLines] = binary:split(Head, <<"\r\n">>, [global]),
            [_, StatusBin | _] = binary:split(StatusLine, <<" ">>, [global]),
            Headers = headers_of(HeaderLines),
            #{
                raw => Raw,
                status => binary_to_integer(StatusBin),
                headers => Headers,
                body => decode_chunked(BodyRaw, Headers)
            };
        [_Only] ->
            #{raw => Raw, status => 0, headers => #{}, body => <<>>}
    end.

headers_of(Lines) ->
    maps:from_list([
        {string:lowercase(K), V}
     || Line <- Lines,
        [K, V] <- [binary:split(Line, <<": ">>)],
        K =/= <<>>
    ]).

decode_chunked(Body, #{<<"transfer-encoding">> := <<"chunked">>}) -> dechunk(Body, []);
decode_chunked(Body, _Headers) -> Body.

dechunk(<<>>, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
dechunk(Bin, Acc) ->
    case binary:split(Bin, <<"\r\n">>) of
        [SizeBin, Rest] ->
            case hex(SizeBin) of
                0 ->
                    iolist_to_binary(lists:reverse(Acc));
                Size when is_integer(Size), Size > 0 ->
                    case Rest of
                        <<Chunk:Size/binary, _CRLF:2/binary, Tail/binary>> ->
                            dechunk(Tail, [Chunk | Acc]);
                        _Incomplete ->
                            iolist_to_binary(lists:reverse(Acc))
                    end;
                _Bad ->
                    iolist_to_binary(lists:reverse(Acc))
            end;
        _Incomplete ->
            iolist_to_binary(lists:reverse(Acc))
    end.

hex(Bin) ->
    try binary_to_integer(Bin, 16) of
        N -> N
    catch
        _:_ -> -1
    end.

-spec status(map()) -> integer().
status(Resp) -> maps:get(status, Resp, 0).

-spec headers(map()) -> map().
headers(Resp) -> maps:get(headers, Resp, #{}).

-spec body(map()) -> binary().
body(Resp) -> maps:get(body, Resp, <<>>).

-spec raw(map()) -> binary().
raw(Resp) -> maps:get(raw, Resp, <<>>).

%% @doc 响应信封（`elib_response`：`#{code, msg, payload, sv_ts}`）解码；非 JSON 返回 `#{}`。
-spec json(map()) -> map().
json(Resp) ->
    case maps:get(body, Resp, <<>>) of
        <<>> ->
            #{};
        Bin ->
            try jsx:decode(Bin, [return_maps]) of
                Map when is_map(Map) -> Map;
                _NotObject -> #{}
            catch
                _:_ -> #{}
            end
    end.

-spec code(map()) -> integer() | undefined.
code(Resp) ->
    maps:get(<<"code">>, json(Resp), undefined).

-spec msg(map()) -> binary() | undefined.
msg(Resp) ->
    maps:get(<<"msg">>, json(Resp), undefined).

-spec payload(map()) -> term().
payload(Resp) ->
    maps:get(<<"payload">>, json(Resp), undefined).

%% @doc 响应留档（A03 的机械扫描输入：只留 status / 头 / 体，键值都是本 run 的合成值）。
-spec record(binary(), map()) -> ok.
record(Path, Resp) ->
    Entry = {
        {maps:get(status, Resp, 0), Path},
        #{
            status => maps:get(status, Resp, 0),
            path => Path,
            headers => maps:get(headers, Resp, #{}),
            body => maps:get(body, Resp, <<>>)
        }
    },
    ets:insert(?RESP_TAB, Entry),
    ok.

-spec recorded_responses() -> [map()].
recorded_responses() ->
    [Entry || {_Key, Entry} <- ets:tab2list(?RESP_TAB)].

%% @doc `Authorization: Bearer <JWT>`（真 JWT：`token_ds:encrypt_token/1` 产出，
%% 由生产中同一中间件 `auth_ds:verify_token/1` 校验）。
-spec auth(binary() | undefined) -> map().
auth(undefined) -> #{};
auth(Token) -> #{<<"authorization">> => <<"Bearer ", Token/binary>>}.

-spec get(binary() | undefined, binary()) -> map().
get(Token, Path) ->
    get(Token, Path, #{}).

-spec get(binary() | undefined, binary(), map()) -> map().
get(Token, Path, Opts) ->
    req(<<"GET">>, Path, <<>>, auth(Token), Opts).

-spec post(binary() | undefined, binary(), term()) -> map().
post(Token, Path, Body) ->
    post(Token, Path, Body, #{}).

-spec post(binary() | undefined, binary(), term(), map()) -> map().
post(Token, Path, Body, Opts) ->
    req(<<"POST">>, Path, Body, auth(Token), Opts).

%% @doc JSON payload 取值（atom 或 binary 键都接受）。
-spec pget(map() | undefined, atom() | binary()) -> term().
pget(Payload, Key) when is_map(Payload) ->
    K =
        case is_atom(Key) of
            true -> atom_to_binary(Key, utf8);
            false -> Key
        end,
    maps:get(K, Payload, undefined);
pget(_Other, _Key) ->
    undefined.

%% @doc TSID 字符串 → 整数（`undefined`/非法值 → undefined）。
-spec tsid(term()) -> integer() | undefined.
tsid(Value) when is_integer(Value) -> Value;
tsid(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Int -> Int
    catch
        _:_ -> undefined
    end;
tsid(_Other) ->
    undefined.

%% @doc 企业租户面路径（自带 `workspace_id` 查询参数）。
-spec tenant_path(integer(), binary(), integer()) -> binary().
tenant_path(OrgId, Suffix, WorkspaceId) ->
    Base = <<"/api/v1/enterprise/organizations/", (integer_to_binary(OrgId))/binary>>,
    Query =
        case WorkspaceId of
            0 -> <<>>;
            _ -> <<"?workspace_id=", (integer_to_binary(WorkspaceId))/binary>>
        end,
    <<Base/binary, Suffix/binary, Query/binary>>.

%% ===================================================================
%% 路由装配：只替换企业路由的 `auth_facts` 键（F1/F2 的测试侧补偿）
%% ===================================================================

%% @doc 记录原始路由表并装配「探针」模式（`{probe, Permissions}` 见 `eb_e2e_facts_probe`）。
-spec install_dispatch() -> ok.
install_dispatch() ->
    Routes = imboy_router:get_routes(),
    persistent_term:put(?ORIG_KEY, Routes),
    install(facts_rewrite(Routes)),
    ok.

%% @doc 原始路由表（生产装配，未经任何改写）。
-spec original_routes() -> list().
original_routes() ->
    persistent_term:get(?ORIG_KEY).

%% @doc 切回 `real`（生产 `auth_facts` 装配）或 `probe`（测试侧补偿装配）。
-spec set_facts_mode(real | probe) -> ok.
set_facts_mode(Mode) when Mode =:= real; Mode =:= probe ->
    persistent_term:put(?MODE_KEY, Mode),
    Routes = original_routes(),
    install(
        case Mode of
            real -> Routes;
            probe -> facts_rewrite(Routes)
        end
    ),
    ok.

-spec facts_mode() -> real | probe.
facts_mode() ->
    persistent_term:get(?MODE_KEY, real).

install(Routes) ->
    ok = cowboy:set_env(imboy_listener, dispatch, cowboy_router:compile(Routes)).

%% 只动企业**租户**面的 `auth_facts`（平台面在生产由 `eb_platform_auth_facts` 投影既有
%% Admin ACL，本 E2E 不替换它）。其余路由逐字不动。
facts_rewrite(Routes) ->
    [{Host, [rewrite_route(R) || R <- Paths]} || {Host, Paths} <- Routes].

rewrite_route({Path, eb_tenant_handler, Opts}) when is_map(Opts) ->
    {Path, eb_tenant_handler, Opts#{auth_facts => eb_e2e_facts_probe}};
rewrite_route(Route) ->
    Route.

%% ===================================================================
%% DB（只经 `elib_pg`，与生产同一条连接池）
%% ===================================================================

-spec q(iodata(), list()) -> {ok, [map()]} | {error, term()}.
q(Sql, Params) ->
    elib_pg:query(Sql, Params).

-spec rows(iodata(), list()) -> [map()].
rows(Sql, Params) ->
    case q(Sql, Params) of
        {ok, Rows} -> Rows;
        {error, _Reason} -> []
    end.

-spec scalar(iodata(), list()) -> term().
scalar(Sql, Params) ->
    scalar(Sql, Params, undefined).

-spec scalar(iodata(), list(), term()) -> term().
scalar(Sql, Params, Default) ->
    case q(Sql, Params) of
        {ok, [Row | _]} ->
            case maps:values(Row) of
                [Value | _] -> Value;
                [] -> Default
            end;
        _Other ->
            Default
    end.

-spec exec(iodata(), list()) -> ok | {error, term()}.
exec(Sql, Params) ->
    case elib_pg:execute(Sql, Params) of
        {ok, _Count} -> ok;
        {ok, _Count, _Rows} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 以「本 run 的作用域」计数某表行数（每张表都带 organization_id；带 workspace_id 的
%% 表同时约束 Workspace）。
count_rows(Org, Workspace, Table) ->
    Sql =
        case has_workspace(Table) of
            true ->
                <<"SELECT count(*) AS n FROM ", Table/binary,
                    " WHERE organization_id=$1 AND workspace_id=$2">>;
            false ->
                <<"SELECT count(*) AS n FROM ", Table/binary, " WHERE organization_id=$1">>
        end,
    Params =
        case has_workspace(Table) of
            true -> [Org, Workspace];
            false -> [Org]
        end,
    case scalar(Sql, Params, -1) of
        N when is_integer(N) -> N;
        _Other -> -1
    end.

has_workspace(<<"enterprise_contact">>) -> false;
has_workspace(<<"enterprise_contact_identity">>) -> false;
has_workspace(<<"enterprise_contact_assignment">>) -> false;
has_workspace(<<"enterprise_note">>) -> false;
has_workspace(<<"organization_business_identity">>) -> false;
has_workspace(<<"organization_business_identity_assignment">>) -> false;
has_workspace(<<"enterprise_audit_event">>) -> false;
has_workspace(<<"enterprise_offboarding_case">>) -> false;
has_workspace(<<"enterprise_offboarding_item">>) -> false;
has_workspace(_Other) -> true.

%% ===================================================================
%% 合成数据 / 工具
%% ===================================================================

%% @doc 本 run 的随机令牌：所有合成标识都带它，使证据可归因且不与其他 run 混淆。
-spec run_token() -> binary().
run_token() ->
    case persistent_term:get(?TOKEN_KEY, undefined) of
        undefined ->
            Token = binary:encode_hex(crypto:strong_rand_bytes(4), lowercase),
            persistent_term:put(?TOKEN_KEY, Token),
            Token;
        Token ->
            Token
    end.

%% @doc 合成 TSID（时间有序；未启动 imboy app 时退化为唯一整数）。
-spec id() -> integer().
id() ->
    try
        elib_tsid:generate(default)
    catch
        _:_ -> 3000000000000000000 + erlang:unique_integer([positive, monotonic])
    end.

%% @doc 合成企业托管主密钥引用（**测试侧**注入；生产侧无提供者，见 F6）。
-spec key_ref() -> map().
key_ref() ->
    eb_pg_test_fixture:key_ref().

%% @doc 可归因的合成明文金丝雀（ASCII，便于在 DB / 日志 / 响应里做子串断言）。
-spec canary(binary() | string()) -> binary().
canary(Kind) ->
    K = iolist_to_binary([Kind]),
    <<"EB11-E2E-PLAINTEXT-", K/binary, "-", (run_token())/binary>>.

-spec now_sec() -> integer().
now_sec() ->
    os:system_time(second).

%% @doc 稳定指纹（排序后的 term_to_binary → sha256 hex）。
-spec stable_hash(term()) -> binary().
stable_hash(Term) ->
    binary:encode_hex(crypto:hash(sha256, term_to_binary(Term)), lowercase).

%% @doc 存储能力泄露判据（与 `eb_enterprise_http:storage_leak_scan/1` 同口径 + 常见 S3 串）。
-spec leak_patterns() -> [binary()].
leak_patterns() ->
    [
        <<"://">>,
        <<"object_key">>,
        <<"object-key">>,
        <<"storage_ref">>,
        <<"bucket">>,
        <<"endpoint">>,
        <<"presign">>,
        <<"garage">>,
        <<"X-Amz-">>,
        <<"AWSAccessKeyId">>
    ].

%% @doc 返回命中的泄露模式列表（空列表 = 干净）。
-spec leak_scan(term()) -> [binary()].
leak_scan(Value) ->
    Bin = elib_cnv:safe_to_binary(Value),
    [P || P <- leak_patterns(), binary:match(Bin, P) =/= nomatch].

%% @doc 二进制前缀判定（`lists:prefix/2` 只吃 list，binary 会 function_clause —— 本次实撞）。
-spec binary_prefix(binary(), binary()) -> boolean().
binary_prefix(Prefix, Bin) when is_binary(Prefix), is_binary(Bin) ->
    Size = byte_size(Prefix),
    byte_size(Bin) >= Size andalso binary:part(Bin, 0, Size) =:= Prefix.

%% @doc 任一面值为真即返回命中的 needle 列表。
-spec contains_any(term(), [binary()]) -> [binary()].
contains_any(Value, Needles) ->
    Bin = elib_cnv:safe_to_binary(Value),
    [N || N <- Needles, binary:match(Bin, N) =/= nomatch].

%% ===================================================================
%% 证据落盘（原始输出，供 A0 复核）
%% ===================================================================

%% @doc 追加原始证据行到 `$EB11_EVIDENCE_DIR/<File>`（缺省 `/tmp`）。
-spec evidence(iodata(), iodata()) -> ok.
evidence(File, Line) ->
    evidence(File, "~ts", [Line]).

-spec evidence(iodata(), iodata(), list()) -> ok.
evidence(File, Format, Args) ->
    Dir =
        case os:getenv("EB11_EVIDENCE_DIR") of
            false -> "/tmp";
            D -> D
        end,
    Path = filename:join(Dir, to_list(File)),
    _ = filelib:ensure_dir(Path),
    Line = unicode:characters_to_binary(io_lib:format("~ts\n", [io_lib:format(Format, Args)])),
    ok = file:write_file(Path, Line, [append]),
    ok.
