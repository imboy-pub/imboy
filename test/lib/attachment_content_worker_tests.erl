-module(attachment_content_worker_tests).
-moduledoc "step-12：内容 Worker 规则等价（oracle=eb_asset_content:sniff/2）+ receipt 认证（T30/T31）".
%%%
% 规则样本逐项等价：scan_stream 判定 ≡ 现行 sniff/2 全量判定；
% receipt：伪造/跨 attempt/过期/ruleset 漂移/verdict 非 pass 全部 fail-closed；
% 超时预算：deadline 到即停（不无限读）。
%%%

-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------- 样本

sample(<<"image/png">>) -> <<137, 80, 78, 71, 13, 10, 26, 10, 0, 1, 2, 3>>;
sample(<<"image/jpeg">>) -> <<16#FF, 16#D8, 16#FF, 224, 1, 2>>;
sample(<<"image/gif">>) -> <<"GIF89a", 1, 2, 3>>;
sample(<<"image/webp">>) -> <<"RIFF", 0, 0, 0, 0, "WEBP", 9, 9>>;
sample(<<"image/bmp">>) -> <<"BM", 1, 2, 3>>;
sample(<<"image/svg+xml">>) -> <<"<svg xmlns='x'>", 10, "hello", 10>>;
sample(<<"application/pdf">>) -> <<"%PDF-1.7 body">>;
sample(<<"application/zip">>) -> <<80, 75, 3, 4, 1, 2>>;
sample(<<"application/x-7z-compressed">>) -> <<55, 122, 188, 175, 39, 28, 1>>;
sample(<<"application/gzip">>) -> <<16#1F, 16#8B, 1, 2>>;
sample(<<"application/msword">>) -> <<16#D0, 16#CF, 16#11, 16#E0, 16#A1, 16#B1, 16#1A, 16#E1>>;
sample(<<"video/mp4">>) -> <<0, 0, 0, 24, "ftyp", "isom", 0, 0>>;
sample(<<"video/webm">>) -> <<16#1A, 16#45, 16#DF, 16#A3, 1, 2, 3>>;
sample(<<"image/heic">>) -> <<0, 0, 0, 24, "ftyp", "heic", 0, 0>>;
sample(<<"image/heif">>) -> <<0, 0, 0, 24, "ftyp", "mif1", 0, 0>>;
sample(<<"video/quicktime">>) -> <<0, 0, 0, 20, "ftyp", "qt  ", 0, 0>>;
%% 2026-10-30 扩表家族的代表样本（EBML/RIFF/ASF/rar/tar 前缀都在探测块内）
sample(<<"video/x-matroska">>) -> <<16#1A, 16#45, 16#DF, 16#A3, 1, 2>>;
sample(<<"audio/x-wav">>) -> <<"RIFF", 0, 0, 0, 0, "WAVE", 1>>;
sample(<<"video/x-msvideo">>) -> <<"RIFF", 0, 0, 0, 0, "AVI ", "LIST", 1>>;
sample(<<"application/x-rar-compressed">>) -> <<"Rar!", 16#1A, 16#07, 1, 0>>;
sample(<<"application/x-tar">>) -> <<0:(257 * 8), "ustar", 0, 1>>;
sample(<<"audio/mpeg">>) -> <<"ID3", 4, 4, 4>>;
sample(<<"text/plain">>) -> <<"hello clean text", 10>>;
sample(<<"text_bad_nul">>) -> <<"bad", 0, "text">>;
sample(<<"text_bad_ctrl">>) -> <<"bad", 16#01, "text">>.

%% ---------------------------------------------------------------- 逐项等价

rule_equivalence_test_() ->
    MimeList = [
        <<"image/png">>,
        <<"image/jpeg">>,
        <<"image/gif">>,
        <<"image/webp">>,
        <<"image/bmp">>,
        <<"image/svg+xml">>,
        <<"application/pdf">>,
        <<"application/zip">>,
        <<"application/x-7z-compressed">>,
        <<"application/gzip">>,
        <<"application/msword">>,
        <<"video/mp4">>,
        <<"video/webm">>,
        <<"image/heic">>,
        <<"image/heif">>,
        <<"video/quicktime">>,
        <<"video/x-matroska">>,
        <<"audio/x-wav">>,
        <<"video/x-msvideo">>,
        <<"application/x-rar-compressed">>,
        <<"application/x-tar">>,
        <<"audio/mpeg">>,
        <<"text/plain">>
    ],
    [{timeout, 35, ?_test(pure_mock_fixture:run(fun() -> rule_case(M) end))} || M <- MimeList] ++
        [
            {timeout, 35, ?_test(pure_mock_fixture:run(fun text_bad_nul_case/0))},
            {timeout, 35, ?_test(pure_mock_fixture:run(fun text_bad_ctrl_case/0))},
            {timeout, 35, ?_test(pure_mock_fixture:run(fun mime_mismatch_case/0))},
            {timeout, 35, ?_test(pure_mock_fixture:run(fun empty_object_case/0))}
        ].

rule_case(M) ->
    Bin = sample(M),
    {ok, ReadFun} = reader_of(Bin),
    R =
        try
            attachment_content_worker:scan_stream(
                ReadFun, fun eb_asset_content:sniff/2, M, forever()
            )
        catch
            throw:Reason -> {error, Reason}
        end,
    Oracle = eb_asset_content:sniff(M, Bin),
    ?assertEqual(oracle_ok(Oracle), oracle_ok(R)).

text_bad_nul_case() ->
    Bin = sample(<<"text_bad_nul">>),
    {ok, ReadFun} = reader_of(Bin),
    R =
        try
            attachment_content_worker:scan_stream(
                ReadFun, fun eb_asset_content:sniff/2, <<"text/plain">>, forever()
            )
        catch
            throw:Reason2 -> {error, Reason2}
        end,
    ?assertMatch({error, _}, R),
    ?assertMatch(
        {error, {mime_content_mismatch, _}},
        eb_asset_content:sniff(<<"text/plain">>, Bin)
    ).

text_bad_ctrl_case() ->
    Bin = sample(<<"text_bad_ctrl">>),
    {ok, ReadFun} = reader_of(Bin),
    R =
        try
            attachment_content_worker:scan_stream(
                ReadFun, fun eb_asset_content:sniff/2, <<"text/plain">>, forever()
            )
        catch
            throw:Reason3 -> {error, Reason3}
        end,
    ?assertMatch({error, _}, R).

mime_mismatch_case() ->
    Bin = sample(<<"image/png">>),
    {ok, ReadFun} = reader_of(Bin),
    R =
        try
            attachment_content_worker:scan_stream(
                ReadFun, fun eb_asset_content:sniff/2, <<"image/jpeg">>, forever()
            )
        catch
            throw:Reason4 -> {error, Reason4}
        end,
    ?assertMatch({error, {mime_content_mismatch, <<"image/jpeg">>}}, R).

%% review P2 修复验证：空对象（size=0，ReadChunkFun 恒 eof）——
%% text 家族空集洁净通过（空摘要 e3b0…）；非 text 家族魔数必然 mismatch。
empty_object_case() ->
    ReadFun = fun(_Offset, _Len) -> eof end,
    TextR =
        try
            attachment_content_worker:scan_stream(
                ReadFun, fun eb_asset_content:sniff/2, <<"text/plain">>, forever()
            )
        catch
            throw:TR -> {error, TR}
        end,
    ?assertEqual(
        {ok, #{
            sha256 =>
                <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>,
            size => 0
        }},
        TextR
    ),
    BinR =
        try
            attachment_content_worker:scan_stream(
                ReadFun, fun eb_asset_content:sniff/2, <<"image/png">>, forever()
            )
        catch
            throw:BR -> {error, BR}
        end,
    ?assertMatch({error, {mime_content_mismatch, <<"image/png">>}}, BinR).

reader_of(Bin) ->
    {ok, fun
        (Offset, Len) when Offset >= byte_size(Bin) -> eof;
        (Offset, Len) ->
            End = min(Offset + Len, byte_size(Bin)),
            {ok, binary:part(Bin, Offset, End - Offset)}
    end}.

oracle_ok(ok) -> ok;
oracle_ok({ok, _}) -> ok;
oracle_ok({error, _}) -> error.

forever() -> erlang:monotonic_time(millisecond) + 60000.

%% ---------------------------------------------------------------- receipt

receipt_test_() ->
    {foreach, fun setup_receipt/0, fun cleanup_receipt/1, [
        fun scan_issues_verifiable_receipt/0,
        fun forged_receipt_rejected/0,
        fun cross_attempt_rejected/0,
        fun expired_receipt_rejected/0,
        fun ruleset_drift_rejected/0,
        fun dirty_verdict_never_pass/0,
        fun timeout_budget/0
    ]}.

setup_receipt() ->
    application:ensure_all_started(crypto),
    ok.

cleanup_receipt(_) ->
    ok.

-define(SECRET, <<"dv1-receipt-test-secret">>).
-define(AK, <<"u1/dv1/a">>).
-define(OK1, <<"direct-c/u1/dv1/a/p1">>).

scan_issues_verifiable_receipt() ->
    Bin = sample(<<"image/png">>),
    {ok, ReadFun} = reader_of(Bin),
    {ok, #{sha256 := Sha, size := Size, receipt := Receipt}} =
        attachment_content_worker:scan(
            ReadFun,
            fun eb_asset_content:sniff/2,
            <<"image/png">>,
            ?AK,
            ?OK1,
            #{secret => ?SECRET, deadline_ms => 60000}
        ),
    ?assertEqual(byte_size(Bin), Size),
    ?assertEqual(binary:encode_hex(crypto:hash(sha256, Bin), lowercase), Sha),
    ?assertEqual(
        ok,
        attachment_content_receipt:verify(Receipt, ?SECRET, #{
            attempt_key => ?AK,
            object_key => ?OK1,
            sha256 => Sha,
            size => Size,
            mime => <<"image/png">>,
            ruleset_digest => attachment_content_receipt:ruleset_digest()
        })
    ),
    ok.

forged_receipt_rejected() ->
    Bin = sample(<<"image/png">>),
    {ok, ReadFun} = reader_of(Bin),
    {ok, #{receipt := Receipt}} =
        attachment_content_worker:scan(
            ReadFun,
            fun eb_asset_content:sniff/2,
            <<"image/png">>,
            ?AK,
            ?OK1,
            #{secret => ?SECRET, deadline_ms => 60000}
        ),
    %% 改一位：伪造/篡改
    Decoded = base64:decode(Receipt),
    Byte = binary:at(Decoded, 20),
    Forged = binary:replace(Decoded, <<Byte>>, <<(Byte bxor 16#01)>>, [{scope, {20, 1}}]),
    ?assertMatch(
        {error, forged},
        attachment_content_receipt:verify(base64:encode(Forged), ?SECRET, #{
            attempt_key => ?AK,
            object_key => ?OK1,
            sha256 => binary:encode_hex(crypto:hash(sha256, Bin)),
            size => byte_size(Bin),
            mime => <<"image/png">>,
            ruleset_digest => attachment_content_receipt:ruleset_digest()
        })
    ),
    ok.

cross_attempt_rejected() ->
    Bin = sample(<<"image/png">>),
    {ok, ReadFun} = reader_of(Bin),
    {ok, #{sha256 := Sha, size := Size, receipt := Receipt}} =
        attachment_content_worker:scan(
            ReadFun,
            fun eb_asset_content:sniff/2,
            <<"image/png">>,
            ?AK,
            ?OK1,
            #{secret => ?SECRET, deadline_ms => 60000}
        ),
    %% 同字节绑定到另一 attempt：拒绝（T31 跨 attempt）
    ?assertMatch(
        {error, binding_mismatch},
        attachment_content_receipt:verify(Receipt, ?SECRET, #{
            attempt_key => <<"u1/dv1/OTHER">>,
            object_key => ?OK1,
            sha256 => Sha,
            size => Size,
            mime => <<"image/png">>,
            ruleset_digest => attachment_content_receipt:ruleset_digest()
        })
    ),
    ok.

expired_receipt_rejected() ->
    Receipt = attachment_content_receipt:issue(
        #{
            attempt_key => ?AK,
            object_key => ?OK1,
            sha256 => binary:encode_hex(crypto:hash(sha256, <<1, 2, 3>>)),
            size => 3,
            mime => <<"image/png">>,
            verdict => <<"pass">>,
            expires_at => erlang:system_time(second) - 10
        },
        ?SECRET
    ),
    ?assertMatch(
        {error, expired},
        attachment_content_receipt:verify(Receipt, ?SECRET, #{
            attempt_key => ?AK,
            object_key => ?OK1,
            sha256 => binary:encode_hex(crypto:hash(sha256, <<1, 2, 3>>)),
            size => 3,
            mime => <<"image/png">>,
            ruleset_digest => attachment_content_receipt:ruleset_digest()
        })
    ),
    ok.

ruleset_drift_rejected() ->
    Receipt = attachment_content_receipt:issue(
        #{
            attempt_key => ?AK,
            object_key => ?OK1,
            sha256 => binary:encode_hex(crypto:hash(sha256, <<1>>)),
            size => 1,
            mime => <<"image/png">>,
            verdict => <<"pass">>
        },
        ?SECRET
    ),
    ?assertMatch(
        {error, binding_mismatch},
        attachment_content_receipt:verify(Receipt, ?SECRET, #{
            attempt_key => ?AK,
            object_key => ?OK1,
            sha256 => binary:encode_hex(crypto:hash(sha256, <<1>>)),
            size => 1,
            mime => <<"image/png">>,
            ruleset_digest => binary:copy(<<"0">>, 64)
        })
    ),
    ok.

dirty_verdict_never_pass() ->
    %% verdict=dirty 的 receipt：签名合法也永不通过（worker 失败停待验证）
    Receipt = attachment_content_receipt:issue(
        #{
            attempt_key => ?AK,
            object_key => ?OK1,
            sha256 => binary:encode_hex(crypto:hash(sha256, <<1>>)),
            size => 1,
            mime => <<"image/png">>,
            verdict => <<"dirty">>
        },
        ?SECRET
    ),
    ?assertMatch(
        {error, verdict_not_pass},
        attachment_content_receipt:verify(Receipt, ?SECRET, #{
            attempt_key => ?AK,
            object_key => ?OK1,
            sha256 => binary:encode_hex(crypto:hash(sha256, <<1>>)),
            size => 1,
            mime => <<"image/png">>,
            ruleset_digest => attachment_content_receipt:ruleset_digest()
        })
    ),
    ok.

timeout_budget() ->
    %% deadline=0：立即超时，不产生 receipt（T31 超时 fail-closed）
    Bin = sample(<<"image/png">>),
    {ok, ReadFun} = reader_of(Bin),
    R =
        try
            attachment_content_worker:scan(
                ReadFun,
                fun eb_asset_content:sniff/2,
                <<"image/png">>,
                ?AK,
                ?OK1,
                #{secret => ?SECRET, deadline_ms => 0}
            )
        catch
            throw:timeout -> {error, timeout}
        end,
    ?assertMatch({error, timeout}, R),
    ok.
