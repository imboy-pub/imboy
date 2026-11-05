-module(attachment_worker_port).
-moduledoc "V3-07 外部内容 Worker 端口：正文零过 BEAM 的 scan 派发（S3-only 合同）".
%%%
%% 将内容扫描派发给独立 OS 进程（priv/workers/content_worker.escript）：
%%   * 任务体（含存储凭据与 receipt 密钥）写入 0600 临时文件、路径经 argv
%%     传递——凭据不进 ps 参数表，文件随调用即删；
%%   * Worker 进程内 Range GET 读正文并流式扫描/签发 receipt，结果写
%%     <任务文件>.out（有界 ≤ ?MAX_RESULT_BYTES）；
%%   * 本模块读结果并二次验证 receipt（绑定 Task 的 object_key/attempt_key/
%%     mime，任一不符即拒）；
%%   * fail-closed：Worker 崩溃/超时/输出越界/回执不符/残留清理失败 →
%%     {error, _}，绝不回退 BEAM 内读正文（本模块无任何正文读路径）。

-export([
    scan_external/1,
    fetch_files/1,
    upload_file/1,
    render_export/1,
    upload_export/1,
    validate_audio/1,
    worker_script_path/0
]).

-define(MAX_RESULT_BYTES, 65536).
-define(DEFAULT_DEADLINE_MS, 600000).
-define(PORT_TIMEOUT_MS, 650000).

-ifdef(TEST).
-export([escript_executable/0]).
-endif.

%% Task 字段与 content_worker.escript 对齐：
%%   endpoint/ak/sk/bucket/region/object_key/attempt_key/mime/deadline_ms/secret
%% 返回 {ok, #{sha256 := binary(), size := non_neg_integer(),
%%             receipt := binary(), verified := true}} | {error, term()}。
scan_external(Task) ->
    Task1 = Task#{
        deadline_ms => maps:get(deadline_ms, Task, ?DEFAULT_DEADLINE_MS),
        ebin => ebin_dir(),
        code_paths => worker_code_paths()
    },
    case {escript_executable(), worker_script_path()} of
        {false, _} ->
            {error, {worker_missing, escript}};
        {_, false} ->
            {error, {worker_missing, script}};
        {Es, Script} ->
            run_worker(Es, Script, Task1)
    end.

%% ===== V3-18 P2：汇编 IO Worker 派发（fetch/upload；正文零过 BEAM） =====

%% Items：[{url, path, sha256_hex, size}]（有序，顺序即拼接序）。
%% 返回 {ok, #{fetched => N}}（N=实际成功项数，逐项 size+sha 校验在
%% worker 内完成）| {error, _}（fail-closed，不回退 BEAM 下载）。
fetch_files(Items) ->
    Task = #{
        action => fetch,
        items => [I#{path => iolist_to_binary(maps:get(path, I))} || I <- Items],
        deadline_ms => 600000
    },
    run_io_worker(Task).

%% UploadTask：src_path/endpoint/ak/sk/bucket/region/object_key/mime/
%% issued_at/expires_at（与 put_snapshot 同构窗口）。
%% 返回 {ok, #{sha256, size}} | {error, _}。
upload_file(UploadTask) ->
    run_io_worker(UploadTask#{action => upload}).

%% Audio validation always runs in the OS worker, including test mode.
validate_audio(Task) ->
    case {os:find_executable("escript"), io_worker_script_path()} of
        {false, _} -> {error, {worker_missing, escript}};
        {_, false} -> {error, {worker_missing, script}};
        {Es, Script} -> run_worker(Es, Script, prepare_task(Task#{action => validate_audio}))
    end.

render_export(Task) ->
    case {escript_executable(), io_worker_script_path()} of
        {false, _} -> {error, {worker_missing, escript}};
        {_, false} -> {error, {worker_missing, script}};
        {Es, Script} -> run_worker(Es, Script, prepare_task(Task#{action => render_export}))
    end.

upload_export(Task) ->
    case {escript_executable(), io_worker_script_path()} of
        {false, _} -> {error, {worker_missing, escript}};
        {_, false} -> {error, {worker_missing, script}};
        {Es, Script} -> run_worker(Es, Script, prepare_task(Task#{action => upload_export}))
    end.

run_io_worker(Task) ->
    Task1 = maps:merge(Task, #{deadline_ms => maps:get(deadline_ms, Task, 600000)}),
    %% V3-18 W5b：eunit VM fork 楔死旁路（测试开关）。取证：relay 套件
    %% 在 eunit VM 下 ~40 次 fork（escript+ffmpeg）中偶发 1-2 次楔死一个
    %% 调度线程（fork/exec 卡死不返回），其后落在该调度器上的任意进程
    %% 永久停滞（receive after 不触发、gen_server:call 无响应——r3 挂
    %% run_worker receive、r4 挂 epgsql gen call；同代码普通 VM 150 连跑
    %% 零丢失）。开关仅供 eunit 套件注入：IO 段在本 VM 进程内以 httpc
    %% 等价路径执行（与 assembly_io_worker.escript 逐块同构），fork 类
    %% 真链路由 G18 gate / assembly_io_worker_tests 在非 eunit VM 承载。
    case application:get_env(imboy, attachment_worker_in_process) of
        {ok, true} ->
            run_io_worker_in_process(Task1);
        _ ->
            case {escript_executable(), io_worker_script_path()} of
                {false, _} ->
                    {error, {worker_missing, escript}};
                {_, false} ->
                    {error, {worker_missing, script}};
                {Es, Script} ->
                    run_worker(Es, Script, prepare_task(Task1))
            end
    end.

%% 进程内 IO 等价路径（仅测试开关打开时；正文语义与 escript 一致——
%% fetch 逐项 size+sha256 校验、upload 自签 presign PUT+checksum）。
run_io_worker_in_process(#{action := fetch, items := Items}) ->
    do_fetch_in_process(Items);
run_io_worker_in_process(#{action := upload} = Task) ->
    do_upload_in_process(Task);
run_io_worker_in_process(_) ->
    {error, worker_bad_task}.

do_fetch_in_process([]) ->
    {ok, #{fetched => 0}};
do_fetch_in_process([Item | Rest]) ->
    Url = maps:get(url, Item),
    Path = maps:get(path, Item),
    WantSha = maps:get(sha256_hex, Item),
    WantSize = maps:get(size, Item),
    case
        httpc:request(get, {binary_to_list(Url), []}, [{timeout, 60000}, {autoredirect, false}], [
            {body_format, binary}
        ])
    of
        {ok, {{_, 200, _}, _, Body}} ->
            Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
            case byte_size(Body) =:= WantSize andalso Sha =:= WantSha of
                true ->
                    ok = file:write_file(Path, Body),
                    %% C09 修正：与 assembly_io_worker.escript 同构——
                    %% 回执 fetched = 实际成功项数（原实现递归丢弃计数）。
                    case do_fetch_in_process(Rest) of
                        {ok, #{fetched := C}} -> {ok, #{fetched => C + 1}};
                        Error -> Error
                    end;
                false ->
                    {error, part_integrity_mismatch}
            end;
        {ok, {{_, S, _}, _, _}} ->
            {error, {fetch_status, S}};
        {error, R} ->
            {error, {fetch_error, R}}
    end.

do_upload_in_process(Task) ->
    SrcPath = maps:get(src_path, Task),
    case file:read_file(SrcPath) of
        {ok, Bytes} when byte_size(Bytes) > 0 ->
            Endpoint = maps:get(endpoint, Task),
            Container = maps:get(bucket, Task, <<"imboy">>),
            Cfg = #{
                endpoint => Endpoint,
                container => Container,
                access_key => maps:get(ak, Task),
                secret_key => maps:get(sk, Task),
                region => maps:get(region, Task, <<"garage">>)
            },
            Ck = base64:encode(crypto:hash(sha256, Bytes)),
            Mime = maps:get(mime, Task, <<"application/octet-stream">>),
            Url = elib_s3_sign:presign_signed_url_at(
                <<"PUT">>,
                Endpoint,
                Container,
                maps:get(object_key, Task),
                [{<<"content-type">>, Mime}, {<<"x-amz-checksum-sha256">>, Ck}],
                maps:get(issued_at, Task, calendar:universal_time()),
                window_seconds(Task),
                Cfg
            ),
            case
                httpc:request(
                    put,
                    {
                        binary_to_list(Url),
                        [
                            {"Content-Type", binary_to_list(Mime)},
                            {"x-amz-checksum-sha256", binary_to_list(Ck)}
                        ],
                        binary_to_list(Mime),
                        Bytes
                    },
                    [{timeout, 60000}, {autoredirect, false}],
                    [{body_format, binary}]
                )
            of
                {ok, {{_, S, _}, _, _}} when S >= 200, S < 300 ->
                    {ok, #{
                        sha256 => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
                        size => byte_size(Bytes)
                    }};
                {ok, {{_, S, _}, _, B}} ->
                    {error, {upload_status, S, B}};
                {error, R} ->
                    {error, {upload_error, R}}
            end;
        {ok, _} ->
            {error, empty_source};
        Error ->
            {error, {read_source, Error}}
    end.

prepare_task(Task) ->
    Task#{
        deadline_ms => maps:get(deadline_ms, Task, 600000),
        ebin => ebin_dir(),
        code_paths => worker_code_paths()
    }.

io_worker_script_path() ->
    case code:which(attachment_worker_port) of
        P when is_list(P) ->
            EbinDir = filename:dirname(P),
            Path = filename:join(
                filename:dirname(EbinDir), <<"priv/workers/assembly_io_worker.escript">>
            ),
            case filelib:is_file(Path) of
                true -> Path;
                false -> false
            end;
        _ ->
            false
    end.

run_worker(Es, Script, Task) ->
    Budget = min(port_timeout_ms(), maps:get(deadline_ms, Task, ?DEFAULT_DEADLINE_MS)),
    EffectiveTask = Task#{deadline_ms => Budget},
    Base =
        "imboy-worker-" ++ os:getpid() ++ "-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(16), lowercase)),
    Dir = filename:join(worker_tmp_dir(), Base),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    ok = file:change_mode(Dir, 8#700),
    TaskFile = filename:join(Dir, "worker.task"),
    OutFile = TaskFile ++ ".out",
    Encoded = base64:encode(term_to_binary(EffectiveTask)),
    ok = file:write_file(TaskFile, Encoded),
    ok = file:change_mode(TaskFile, 8#600),
    try
        Port = erlang:open_port(
            {spawn_executable, Es},
            [
                {args, [unicode:characters_to_list(Script), TaskFile]},
                {parallelism, false},
                exit_status
            ]
        ),
        Res =
            receive
                %% OTP port exit_status 消息实形是 2-tuple {Port, {exit_status, N}}
                %%（trap_exit 与否同形；旧 3-tuple 模式永不匹配——eunit 等断言
                %% 进程 trap_exit=true 时滞留邮箱直至超时挂死）。
                {Port, {exit_status, 0}} -> read_result(OutFile, EffectiveTask);
                {Port, {exit_status, N}} -> {error, {worker_exit, N}};
                {'EXIT', Port, Reason} -> {error, {port_exit, Reason}}
            after Budget ->
                try
                    erlang:port_close(Port)
                catch
                    _:_ -> ok
                end,
                {error, timeout}
            end,
        uncertain_result(EffectiveTask, Res)
    after
        %% 任务/结果文件短存即删（凭据不留盘面）
        _ = file:delete(TaskFile),
        _ = file:delete(OutFile),
        _ = file:del_dir(Dir)
    end.

%% Any worker failure after dispatch may have left a PUT or local file behind.
uncertain_result(#{action := Action}, {error, _}) when
    Action =:= upload; Action =:= fetch; Action =:= upload_export
->
    {error, worker_outcome_unknown};
uncertain_result(_, Result) ->
    Result.

read_result(OutFile, Task) ->
    case file:read_file(OutFile) of
        {ok, Bin} when byte_size(Bin) =< ?MAX_RESULT_BYTES ->
            decode_result(binary:replace(Bin, <<"\n">>, <<>>, [global]), Task);
        {ok, _} ->
            {error, worker_output_unbounded};
        {error, _} ->
            {error, worker_result_missing}
    end.

decode_result(<<>>, _Task) ->
    {error, worker_empty_output};
decode_result(Bin, Task) ->
    try
        Result = binary_to_term(base64:decode(Bin)),
        case Result of
            {ok, #{audio_validation := Binding, summary := Summary}} when
                is_map_key(binding, Task), is_map(Summary)
            ->
                case maps:get(binding, Task) =:= Binding of
                    true -> {ok, Summary};
                    false -> {error, worker_binding_mismatch}
                end;
            {ok, _} when map_get(action, Task) =:= validate_audio ->
                {error, worker_bad_result_shape};
            {ok, #{export_binding := Binding, sha256 := Sha, size := Size} = Export} when
                (map_get(action, Task) =:= render_export orelse
                    map_get(action, Task) =:= upload_export),
                is_binary(Sha),
                is_integer(Size),
                Size > 0
            ->
                case maps:get(binding, Task) =:= Binding of
                    true -> {ok, Export};
                    false -> {error, worker_binding_mismatch}
                end;
            {ok, _} when map_get(action, Task) =:= render_export ->
                {error, worker_bad_result_shape};
            {ok, _} when map_get(action, Task) =:= upload_export ->
                {error, worker_bad_result_shape};
            {ok, #{sha256 := Sha, size := Size, receipt := Receipt}} ->
                verify_receipt(Receipt, Sha, Size, Task);
            %% V3-18 P2 汇编 IO Worker（assembly_io_worker.escript）合同：
            %% upload 返回 {ok, #{sha256, size}}（无 receipt——IO 搬运不是
            %% 内容扫描；fail-closed 由 worker 非零退出+错误形态承担）；
            %% fetch 返回 {ok, #{fetched => N}}（逐项 size+sha 校验在 worker
            %% 内完成，任一不符即非零退出）。
            {ok, #{sha256 := _Sha, size := _Size} = Ok} when not is_map_key(receipt, Ok) ->
                {ok, Ok};
            {ok, #{fetched := _N} = OkF} when not is_map_key(receipt, OkF) ->
                {ok, OkF};
            {error, R} ->
                {error, R};
            _ ->
                {error, worker_bad_result_shape}
        end
    catch
        _:_ -> {error, worker_bad_output}
    end.

%% 二次验证（T31 fail-closed）：receipt 必须与 Task 绑定一致。
verify_receipt(Receipt, Sha, Size, Task) ->
    Fields = #{
        attempt_key => maps:get(attempt_key, Task),
        object_key => maps:get(object_key, Task),
        sha256 => Sha,
        size => Size,
        mime => maps:get(mime, Task),
        ruleset_digest => attachment_content_receipt:ruleset_digest()
    },
    case attachment_content_receipt:verify(Receipt, maps:get(secret, Task), Fields) of
        ok -> {ok, #{sha256 => Sha, size => Size, receipt => Receipt, verified => true}};
        {error, Reason} -> {error, {receipt, Reason}}
    end.

window_seconds(Task) ->
    case maps:get(expires_at, Task, undefined) of
        undefined ->
            30;
        Exp ->
            calendar:datetime_to_gregorian_seconds(Exp) -
                calendar:datetime_to_gregorian_seconds(
                    maps:get(issued_at, Task, calendar:universal_time())
                )
    end.

ebin_dir() ->
    case code:which(attachment_content_worker) of
        P when is_list(P) -> iolist_to_binary(filename:dirname(P));
        _ -> iolist_to_binary(<<".">>)
    end.

%% Worker 需要的 code path：当前 VM 全量 + backend deps（jsone 等）。
worker_code_paths() ->
    BackendDir = filename:dirname(binary_to_list(ebin_dir())),
    DepsGlob = filename:join([BackendDir, "deps", "*", "ebin"]),
    [iolist_to_binary(P) || P <- code:get_path()] ++
        [iolist_to_binary(D) || D <- filelib:wildcard(DepsGlob)].

worker_tmp_dir() ->
    case os:getenv("IMBOY_WORKER_TMPDIR") of
        false -> "/tmp";
        Dir -> Dir
    end.

%% 端口等待上限（ms）。默认 ?PORT_TIMEOUT_MS（≥ worker deadline 余量）；
%% 测试环境经 {imboy, attachment_worker_port_timeout_ms} 注入更小界——
%% 必须小于外层 eunit timeout，否则挂死时收割分支永远轮不到。
port_timeout_ms() ->
    case application:get_env(imboy, attachment_worker_port_timeout_ms) of
        {ok, N} when is_integer(N), N >= 1000 -> N;
        _ -> ?PORT_TIMEOUT_MS
    end.

escript_executable() ->
    Path = filename:join([code:root_dir(), "bin", "escript"]),
    case filelib:is_file(Path) of
        true -> Path;
        false -> os:find_executable("escript")
    end.

worker_script_path() ->
    case code:which(attachment_worker_port) of
        P when is_list(P) ->
            EbinDir = filename:dirname(P),
            Path = filename:join(
                filename:dirname(EbinDir), <<"priv/workers/content_worker.escript">>
            ),
            case filelib:is_file(Path) of
                true -> Path;
                false -> false
            end;
        _ ->
            false
    end.
