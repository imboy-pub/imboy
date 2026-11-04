-module(attachment_worker_port_tests).
-moduledoc "V3-07：外部内容 Worker 端到端（真 Garage fixture）——正文零过 BEAM、\n"
"回执五道门 fail-closed、Worker 崩溃/超时不回退".
%%%
% 前置：环境变量 A0_ENDPOINT/A0_AK/A0_SK 指向真实一次性 Garage
% （状态根 v3/fixture/garage-v3run1.env）；无则本套件 fail（不得静默跳过）。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(EP, os:getenv("A0_ENDPOINT")).

worker_runtime_escript_test() ->
    ?assertEqual(
        filename:join([code:root_dir(), "bin", "escript"]),
        attachment_worker_port:escript_executable()
    ).

worker_e2e_test_() ->
    {timeout, 120, fun worker_e2e/0}.

worker_e2e() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    Cfg = garage_cfg(),
    Secret = <<"v3run1-worker-secret">>,
    %% 1) 放一个真实 PNG 头对象（魔数头通过 sniff）
    Png =
        <<137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, "IHDR", "v3-worker-e2e-payload",
            0:(16 * 8)>>,
    Key = <<"v3run1/worker-e2e.png">>,
    put_object(Cfg, Key, Png, <<"image/png">>),
    AttemptKey = <<"eb:990921:", Key/binary>>,
    Task = #{
        endpoint => maps:get(endpoint, Cfg),
        ak => maps:get(access_key, Cfg),
        sk => maps:get(secret_key, Cfg),
        bucket => maps:get(container, Cfg),
        region => <<"garage">>,
        object_key => Key,
        attempt_key => AttemptKey,
        mime => <<"image/png">>,
        secret => Secret
    },
    %% 成功：sha/size/receipt 与本地独立计算一致，verified=true
    erlang:display(probe_before_scan),
    ScanRes = attachment_worker_port:scan_external(Task),
    erlang:display({probe_scan_result, element(1, ScanRes)}),
    {ok, #{sha256 := Sha, size := Size, receipt := Receipt, verified := true}} = ScanRes,
    ?assertEqual(Size, byte_size(Png)),
    ?assertEqual(
        binary:encode_hex(crypto:hash(sha256, Png), lowercase),
        Sha
    ),
    %% 回执重放验证（同一字段集）
    ok = attachment_content_receipt:verify(Receipt, Secret, #{
        attempt_key => AttemptKey,
        object_key => Key,
        sha256 => Sha,
        size => Size,
        mime => <<"image/png">>,
        ruleset_digest => attachment_content_receipt:ruleset_digest()
    }),
    %% 2) 负向：篡改 sha256 绑定 → verify 拒（fail-closed）
    ?assertMatch(
        {error, binding_mismatch},
        tamper_verify(Receipt, Secret, AttemptKey, Key, Size)
    ),
    %% 3) 负向：回执跨 attempt 冒充（Worker 按任务签发后用攻击者期望字段
    %%    验证）→ 拒。注意 port 层按 Task 自身字段验证天然一致（设计使然），
    %%    跨主体冒充发生在上层用错误期望字段 verify 时。
    {error, _} = attachment_content_receipt:verify(
        Receipt, Secret, #{
            attempt_key => <<"eb:attacker:", Key/binary>>,
            object_key => Key,
            sha256 => Sha,
            size => Size,
            mime => <<"image/png">>,
            ruleset_digest => attachment_content_receipt:ruleset_digest()
        }
    ),
    %% 4) 负向：mime 不符（文本规则对 PNG 头）→ Worker 规则拒
    TaskText = Task#{mime => <<"text/plain">>},
    {error, _} = attachment_worker_port:scan_external(TaskText),
    %% 5) 负向：对象不存在 → Worker 读失败 fail-closed（不回退 BEAM）
    TaskMiss = Task#{object_key => <<"v3run1/absent.bin">>},
    {error, _} = attachment_worker_port:scan_external(TaskMiss),
    %% 6) 负向：坏凭据 → 拒
    TaskBadCred = Task#{sk => <<"deadbeef">>},
    {error, _} = attachment_worker_port:scan_external(TaskBadCred),
    ok.

tamper_verify(Receipt, Secret, AK, OK, Size) ->
    attachment_content_receipt:verify(Receipt, Secret, #{
        attempt_key => AK,
        object_key => OK,
        sha256 => binary:encode_hex(crypto:hash(sha256, <<"tampered">>), lowercase),
        size => Size,
        mime => <<"image/png">>,
        ruleset_digest => attachment_content_receipt:ruleset_digest()
    }).

garage_cfg() ->
    {Endpoint, Bucket, Access, Secret} =
        case ?EP of
            false ->
                {
                    os:getenv("W202_GARAGE_ENDPOINT"),
                    os:getenv("W202_GARAGE_BUCKET"),
                    os:getenv("W202_GARAGE_ACCESS_KEY"),
                    os:getenv("W202_GARAGE_SECRET_KEY")
                };
            E ->
                {E, os:getenv("A0_BUCKET", "imboy"), os:getenv("A0_AK"), os:getenv("A0_SK")}
        end,
    true = lists:all(fun(V) -> is_list(V) andalso V =/= [] end, [Endpoint, Bucket, Access, Secret]),
    #{
        endpoint => list_to_binary(Endpoint),
        container => list_to_binary(Bucket),
        access_key => list_to_binary(Access),
        secret_key => list_to_binary(Secret),
        region => <<"garage">>
    }.

put_object(Cfg, Key, Body, Mime) ->
    Ck = base64:encode(crypto:hash(sha256, Body)),
    Url = elib_s3_sign:presign_signed_url_at(
        <<"PUT">>,
        maps:get(endpoint, Cfg),
        maps:get(container, Cfg),
        Key,
        [{<<"content-type">>, Mime}, {<<"x-amz-checksum-sha256">>, Ck}],
        calendar:universal_time(),
        300,
        Cfg
    ),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {
            binary_to_list(Url),
            [{"Content-Type", binary_to_list(Mime)}, {"x-amz-checksum-sha256", binary_to_list(Ck)}],
            binary_to_list(Mime),
            Body
        },
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ok.
