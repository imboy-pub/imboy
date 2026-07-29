%%%-------------------------------------------------------------------
%%% E2EE-061 Slice 5 验收 —— 附件密文判别位（迁移 000050）
%%%
%%% ⚠️ **真 PostgreSQL 集成测试**：无 DB 时整组 skip，因此**不进** `make e2ee-verify`
%%% 硬门禁（进了只会得到假绿）。手动运行命令见
%%% `evidence/E2EE-061-slice5-backend-cipher-column.md` §6。
%%%
%%% 拍板 ②「暂不回迁，但预留判别位」的验收对象：
%%% - 旧调用形状（不传 cipher）落库必须是 NULL，**语义完全不变**；
%%% - 新的加密上传写入套件名；
%%% - 未知套件 **fail-closed 拒绝**，绝不落成 NULL——那会把密文对象标成明文。
%%%-------------------------------------------------------------------
-module(attachment_cipher_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% 纯函数部分：不做套件协商（无需 DB）
%%%===================================================================

%% ⚠️ 这是本刀最关键的一条：把未知套件落成 NULL 才是真正危险的降级——
%% 日后回迁盘点会漏掉它，读取侧也会把密文当明文直读。
normalize_cipher_is_fail_closed_test() ->
    %% 明文（含全部旧客户端的三种缺省形态）
    ?assertEqual(null, attach_logic:normalize_cipher(undefined)),
    ?assertEqual(null, attach_logic:normalize_cipher(null)),
    ?assertEqual(null, attach_logic:normalize_cipher(<<>>)),
    %% 唯一支持的套件（与客户端 AttachmentDescriptor.supportedCipher 同值）
    ?assertEqual(<<"AES-256-GCM">>, attach_logic:normalize_cipher(<<"AES-256-GCM">>)),
    %% 任何其它取值一律拒绝，**不降级为 null**
    [
        ?assertEqual(
            {error, unsupported_cipher},
            attach_logic:normalize_cipher(V),
            lists:flatten(io_lib:format("~p 未被拒绝", [V]))
        )
     || V <- [
            <<"AES-128-GCM">>,
            <<"none">>,
            <<"NONE">>,
            <<"aes-256-gcm">>,
            <<"AES-256-GCM ">>,
            <<"plaintext">>,
            <<"1">>,
            true
        ]
    ].

%%%===================================================================
%%% 真 PG 部分
%%%===================================================================

db_ready() ->
    try elib_pg:query(<<"SELECT 1 AS ok">>, []) of
        {ok, _} -> true;
        _ -> false
    catch
        _:_ -> false
    end.

column_exists() ->
    case
        elib_pg:query(
            <<
                "SELECT count(*) AS c FROM information_schema.columns "
                "WHERE table_name='attachment' AND column_name='cipher'"
            >>,
            []
        )
    of
        {ok, [#{<<"c">> := C}]} -> C > 0;
        _ -> false
    end.

cipher_column_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Ctx) -> cases(Ctx) end}.

setup() ->
    _ = application:load(imboy),
    _ = application:ensure_all_started(imboy),
    #{ready => db_ready()}.

cleanup(_) ->
    _ = elib_pg:query(<<"DELETE FROM public.attachment WHERE path LIKE 'e2ee-061-test/%'">>, []),
    ok.

cases(#{ready := false}) ->
    [{"无 PostgreSQL，整组 skip（不得据此认为通过）", fun() -> ?assert(true) end}];
cases(#{ready := true}) ->
    [
        {"对照组：迁移 000050 已生效，cipher 列存在且可空", fun col_exists/0},
        {"正向可用性：旧调用形状（不传 cipher）落库为 NULL，语义不变", fun legacy_null/0},
        {"加密上传：写入套件名", fun encrypted_row/0},
        {"confirm 重放（ON CONFLICT）不翻转 cipher", fun conflict_keeps_cipher/0},
        {"盘点查询可区分明文与密文（拍板 ② 预留的用途）", fun backlog_query/0}
    ].

col_exists() ->
    ?assert(column_exists()),
    {ok, [#{<<"is_nullable">> := N}]} = elib_pg:query(
        <<
            "SELECT is_nullable FROM information_schema.columns "
            "WHERE table_name='attachment' AND column_name='cipher'"
        >>,
        []
    ),
    ?assertEqual(<<"YES">>, N).

save(Path, Extra) ->
    Now = elib_dt:now(),
    Attach = maps:merge(
        #{
            <<"file_hash256">> => <<"deadbeef">>,
            <<"mime_type">> => <<"application/octet-stream">>,
            <<"name">> => <<"x.bin">>,
            <<"path">> => Path,
            <<"url">> => Path,
            <<"size">> => 116
        },
        Extra
    ),
    ok = elib_pg:with_tx(fun(Conn) ->
        attachment_repo:save(Conn, Now, 1001, [Attach])
    end).

cipher_of(Path) ->
    {ok, Rows} = elib_pg:query(
        <<"SELECT cipher, referer_time FROM public.attachment WHERE path = $1">>, [Path]
    ),
    Rows.

legacy_null() ->
    Path = <<"e2ee-061-test/legacy.bin">>,
    save(Path, #{}),
    [#{<<"cipher">> := C}] = cipher_of(Path),
    ?assertEqual(null, C).

encrypted_row() ->
    Path = <<"e2ee-061-test/enc.bin">>,
    save(Path, #{<<"cipher">> => <<"AES-256-GCM">>}),
    [#{<<"cipher">> := C}] = cipher_of(Path),
    ?assertEqual(<<"AES-256-GCM">>, C).

conflict_keeps_cipher() ->
    Path = <<"e2ee-061-test/retry.bin">>,
    save(Path, #{<<"cipher">> => <<"AES-256-GCM">>}),
    %% confirm 重试：同 object_key 再落一次，ON CONFLICT 只递增引用计数
    save(Path, #{<<"cipher">> => <<"AES-256-GCM">>}),
    [#{<<"cipher">> := C, <<"referer_time">> := R}] = cipher_of(Path),
    ?assertEqual(<<"AES-256-GCM">>, C),
    ?assert(R >= 2).

backlog_query() ->
    save(<<"e2ee-061-test/p1.bin">>, #{}),
    save(<<"e2ee-061-test/e1.bin">>, #{<<"cipher">> => <<"AES-256-GCM">>}),
    {ok, [#{<<"plain">> := P, <<"enc">> := E}]} = elib_pg:query(
        <<
            "SELECT count(*) FILTER (WHERE cipher IS NULL) AS plain, "
            "count(*) FILTER (WHERE cipher IS NOT NULL) AS enc "
            "FROM public.attachment WHERE path LIKE 'e2ee-061-test/%'"
        >>,
        []
    ),
    ?assert(P >= 1),
    ?assert(E >= 1).
