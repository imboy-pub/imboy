-module(attachment_worker_port_gate).
-moduledoc "V3-07 外部内容 Worker gate：真实 Garage 上的端到端与 fail-closed 负向断言".
-export([main/0]).

main() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    try
        e2e(),
        negative_bad_attempt(),
        negative_same_size_swap(),
        negative_bad_mime(),
        negative_missing_object(),
        negative_bad_cred(),
        negative_deadline_timeout(),
        beam_zero_canary(),
        io:format("GATE_ALL_PASS~n"),
        halt(0)
    catch
        Class:Reason ->
            io:format("GATE_FAIL ~p:~p~n", [Class, Reason]),
            halt(1)
    end.

cfg() ->
    #{
        endpoint => list_to_binary(os:getenv("A0_ENDPOINT")),
        container => <<"imboy">>,
        access_key => list_to_binary(os:getenv("A0_AK")),
        secret_key => list_to_binary(os:getenv("A0_SK")),
        region => <<"garage">>
    }.

put_png(Key, Body) ->
    C = cfg(),
    Ck = base64:encode(crypto:hash(sha256, Body)),
    Url = elib_s3_sign:presign_signed_url_at(
        <<"PUT">>,
        maps:get(endpoint, C),
        <<"imboy">>,
        Key,
        [{<<"content-type">>, <<"image/png">>}, {<<"x-amz-checksum-sha256">>, Ck}],
        calendar:universal_time(),
        300,
        C
    ),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {
            binary_to_list(Url),
            [{"Content-Type", "image/png"}, {"x-amz-checksum-sha256", binary_to_list(Ck)}],
            "image/png",
            Body
        },
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ok.

e2e() ->
    Png =
        <<137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, "IHDR", "v3-worker-gate-payload",
            0:(16 * 8)>>,
    Key = <<"v3run1/worker-gate.png">>,
    put_png(Key, Png),
    Secret = <<"v3run1-worker-secret">>,
    Task = #{
        endpoint => map_endpoint(),
        ak => list_to_binary(os:getenv("A0_AK")),
        sk => list_to_binary(os:getenv("A0_SK")),
        bucket => <<"imboy">>,
        region => <<"garage">>,
        object_key => Key,
        attempt_key => <<"eb:990921:", Key/binary>>,
        mime => <<"image/png">>,
        secret => Secret
    },
    {ok, #{sha256 := Sha, size := Size, receipt := Receipt, verified := true}} =
        attachment_worker_port:scan_external(Task),
    Size = byte_size(Png),
    Sha = binary:encode_hex(crypto:hash(sha256, Png), lowercase),
    %% 回执重放验证（五道门同字段集）
    ok = attachment_content_receipt:verify(Receipt, Secret, #{
        attempt_key => maps:get(attempt_key, Task),
        object_key => Key,
        sha256 => Sha,
        size => Size,
        mime => <<"image/png">>,
        ruleset_digest => attachment_content_receipt:ruleset_digest()
    }),
    %% 篡改 sha 绑定 → 拒
    {error, _} = attachment_content_receipt:verify(Receipt, Secret, #{
        attempt_key => maps:get(attempt_key, Task),
        object_key => Key,
        sha256 => binary:encode_hex(crypto:hash(sha256, <<"tampered">>), lowercase),
        size => Size,
        mime => <<"image/png">>,
        ruleset_digest => attachment_content_receipt:ruleset_digest()
    }),
    io:format(
        "e2e: size=~p sha=~s verified=true receipt_replay_ok tamper_rejected~n",
        [Size, binary:part(Sha, 0, 12)]
    ).

map_endpoint() -> list_to_binary(os:getenv("A0_ENDPOINT")).

negative_bad_attempt() ->
    %% 回执跨 attempt 不可冒充：Worker 按任务 A 签发的 receipt，
    %% 用任务 B 的期望字段验证必须拒绝（T31 绑定语义）。
    Key = <<"v3run1/worker-gate.png">>,
    AKey = <<"eb:990921:", Key/binary>>,
    Secret = <<"v3run1-worker-secret">>,
    Task = #{
        endpoint => map_endpoint(),
        ak => list_to_binary(os:getenv("A0_AK")),
        sk => list_to_binary(os:getenv("A0_SK")),
        bucket => <<"imboy">>,
        region => <<"garage">>,
        object_key => Key,
        attempt_key => AKey,
        mime => <<"image/png">>,
        secret => Secret
    },
    {ok, #{sha256 := Sha, size := Size, receipt := Receipt}} =
        attachment_worker_port:scan_external(Task),
    {error, _} = attachment_content_receipt:verify(Receipt, Secret, #{
        attempt_key => <<"eb:other-attacker:", Key/binary>>,
        object_key => Key,
        sha256 => Sha,
        size => Size,
        mime => <<"image/png">>,
        ruleset_digest => attachment_content_receipt:ruleset_digest()
    }),
    {error, _} = attachment_content_receipt:verify(Receipt, <<"wrong-secret">>, #{
        attempt_key => AKey,
        object_key => Key,
        sha256 => Sha,
        size => Size,
        mime => <<"image/png">>,
        ruleset_digest => attachment_content_receipt:ruleset_digest()
    }),
    io:format("negative_bad_attempt: cross-attempt/wrong-secret replay rejected~n").

negative_same_size_swap() ->
    %% L-T31：同 size 不同对象，receipt 互换（object_key 绑定）必须拒。
    K1 = <<"v3run1/worker-gate.png">>,
    K2 = <<"v3run1/worker-gate-b.png">>,
    PngB =
        <<137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, "IHDR", "v3-worker-gate-pB", 0:(16 * 8)>>,
    put_png(K2, PngB),
    Secret = <<"v3run1-worker-secret">>,
    Task2 = #{
        endpoint => map_endpoint(),
        ak => list_to_binary(os:getenv("A0_AK")),
        sk => list_to_binary(os:getenv("A0_SK")),
        bucket => <<"imboy">>,
        region => <<"garage">>,
        object_key => K2,
        attempt_key => <<"eb:990921:", K2/binary>>,
        mime => <<"image/png">>,
        secret => Secret
    },
    {ok, #{sha256 := Sha2, size := Size2, receipt := R2}} =
        attachment_worker_port:scan_external(Task2),
    {error, _} = attachment_content_receipt:verify(R2, Secret, #{
        attempt_key => <<"eb:990921:", K2/binary>>,
        object_key => K1,
        sha256 => Sha2,
        size => Size2,
        mime => <<"image/png">>,
        ruleset_digest => attachment_content_receipt:ruleset_digest()
    }),
    io:format(
        "negative_same_size_swap: cross-object binding rejected (size=~p)~n",
        [Size2]
    ).

negative_deadline_timeout() ->
    %% L-T31：deadline=1ms → fail-closed（超时不视通过、不回退）。
    Key = <<"v3run1/worker-gate.png">>,
    Task = #{
        endpoint => map_endpoint(),
        ak => list_to_binary(os:getenv("A0_AK")),
        sk => list_to_binary(os:getenv("A0_SK")),
        bucket => <<"imboy">>,
        region => <<"garage">>,
        object_key => Key,
        attempt_key => <<"eb:990921:", Key/binary>>,
        mime => <<"image/png">>,
        secret => <<"v3run1-worker-secret">>,
        deadline_ms => 1
    },
    {error, _} = attachment_worker_port:scan_external(Task),
    io:format("negative_deadline_timeout: fail-closed~n").

beam_zero_canary() ->
    %% L-T59：8MiB 唯一 canary 对象经 Worker 扫描；BEAM 侧 binary 内存增长
    %% 须远小于对象（正文未进入 BEAM）。canary 尾部 16 字节唯一标记。
    Tail = crypto:strong_rand_bytes(16),
    Canary = <<137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, "IHDR">>,
    Body = binary:copy(<<"c4n4ry-">>, (8 * 1048576) div 7),
    Key = <<"v3run1/worker-canary.bin">>,
    put_png(Key, <<Canary/binary, Body/binary, Tail/binary>>),
    erlang:garbage_collect(),
    Before = erlang:memory(binary),
    Task = #{
        endpoint => map_endpoint(),
        ak => list_to_binary(os:getenv("A0_AK")),
        sk => list_to_binary(os:getenv("A0_SK")),
        bucket => <<"imboy">>,
        region => <<"garage">>,
        object_key => Key,
        attempt_key => <<"eb:990921:", Key/binary>>,
        mime => <<"image/png">>,
        secret => <<"v3run1-worker-secret">>
    },
    {ok, #{size := Size, verified := true}} = attachment_worker_port:scan_external(Task),
    erlang:garbage_collect(),
    Growth = erlang:memory(binary) - Before,
    Size = byte_size(Canary) + byte_size(Body) + 16,
    true = Growth < 1048576,
    io:format(
        "beam_zero_canary: object=~pB beam_binary_growth=~pB (<1MiB)~n",
        [Size, Growth]
    ).

negative_bad_mime() ->
    Key = <<"v3run1/worker-gate.png">>,
    Task = #{
        endpoint => map_endpoint(),
        ak => list_to_binary(os:getenv("A0_AK")),
        sk => list_to_binary(os:getenv("A0_SK")),
        bucket => <<"imboy">>,
        region => <<"garage">>,
        object_key => Key,
        attempt_key => <<"eb:990921:", Key/binary>>,
        mime => <<"text/plain">>,
        secret => <<"v3run1-worker-secret">>
    },
    {error, _} = attachment_worker_port:scan_external(Task),
    io:format("negative_bad_mime: rule mismatch rejected~n").

negative_missing_object() ->
    Task = #{
        endpoint => map_endpoint(),
        ak => list_to_binary(os:getenv("A0_AK")),
        sk => list_to_binary(os:getenv("A0_SK")),
        bucket => <<"imboy">>,
        region => <<"garage">>,
        object_key => <<"v3run1/absent-gate.bin">>,
        attempt_key => <<"eb:990921:v3run1/absent-gate.bin">>,
        mime => <<"application/octet-stream">>,
        secret => <<"v3run1-worker-secret">>
    },
    {error, _} = attachment_worker_port:scan_external(Task),
    io:format("negative_missing_object: fail-closed no fallback~n").

negative_bad_cred() ->
    Key = <<"v3run1/worker-gate.png">>,
    Task = #{
        endpoint => map_endpoint(),
        ak => list_to_binary(os:getenv("A0_AK")),
        sk => <<"deadbeefdeadbeef">>,
        bucket => <<"imboy">>,
        region => <<"garage">>,
        object_key => Key,
        attempt_key => <<"eb:990921:", Key/binary>>,
        mime => <<"image/png">>,
        secret => <<"v3run1-worker-secret">>
    },
    {error, _} = attachment_worker_port:scan_external(Task),
    io:format("negative_bad_cred: rejected~n").
