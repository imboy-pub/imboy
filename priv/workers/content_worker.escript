#!/usr/bin/env escript
%%! -noshell +Bd -mode interactive
%%%
%% V3-07 最小外部内容 Worker（独立 OS 进程，S3-only 合同 §5.4/§7）：
%%   * 任务文件路径经 argv 传入；任务体为 base64(term_to_binary(Task))，
%%     文件由调用方以 0600 短存（凭据不进 ps 参数表）；
%%   * 正文只在本进程内存流动：Range GET 流块 ≤256KiB，无全对象缓存；
%%   * 复用生产规则/回执模块（attachment_content_worker:scan_stream、
%%     attachment_content_receipt:issue、eb_asset_content:sniff）；
%%   * 结果写 <任务文件>.out（单行 base64(term_to_binary(Result))）；
%%     BEAM 主节点只读该有界小文件，绝不触碰正文（BEAM=0）；
%%   * 任一失败以非零退出，调用方 fail-closed，不回退 BEAM 读。
%% Task 字段：ebin/code_paths/endpoint/ak/sk/bucket/region/object_key/
%%           attempt_key/mime/deadline_ms/secret
%%!
main([TaskFile | _]) ->
    try
        {ok, _} = application:ensure_all_started(inets),
        {ok, _} = application:ensure_all_started(crypto),
        {ok, _} = application:ensure_all_started(ssl),
        {ok, Encoded0} = file:read_file(TaskFile),
        Task = binary_to_term(base64:decode(strip_nl(Encoded0))),
        code:add_patha(binary_to_list(maps:get(ebin, Task))),
        [code:add_patha(binary_to_list(P)) || P <- maps:get(code_paths, Task, [])],
        Cfg = #{
            endpoint => maps:get(endpoint, Task),
            container => maps:get(bucket, Task),
            access_key => maps:get(ak, Task),
            secret_key => maps:get(sk, Task),
            region => maps:get(region, Task, <<"garage">>)
        },
        ObjectKey = maps:get(object_key, Task),
        Mime = maps:get(mime, Task),
        Deadline = erlang:monotonic_time(millisecond) + maps:get(deadline_ms, Task, 600000),
        %% V3-C04：escript 进程对 add_patha 后的模块延迟解析不可靠（OTP29
        %% 实测：path 已含目录但首调 undef）。关键依赖显式 ensure_loaded。
        io:format(standard_error, "DBG ebin=~p s3sign_beam_exists=~p path_hit=~p~n", [
            maps:get(ebin, Task),
            filelib:is_file(binary_to_list(
                <<(maps:get(ebin, Task))/binary, "/elib_s3_sign.beam">>)),
            [P || P <- code:get_path(),
                  filelib:is_file(filename:join(P, "elib_s3_sign.beam"))]]),
        lists:foreach(
            fun(M) ->
                Res = code:ensure_loaded(M),
                io:format(standard_error, "ENSURE ~p=~p which=~p~n",
                    [M, Res, code:which(M)])
            end,
            [
                elib_s3_sign, attachment_content_receipt,
                attachment_content_worker, eb_asset_content
            ]
        ),
        ReadFun = range_read_fun(Cfg, ObjectKey),
        Result =
            case attachment_content_worker:scan_stream(
                ReadFun, fun eb_asset_content:sniff/2, Mime, Deadline
            ) of
                {ok, #{sha256 := Sha, size := Size}} ->
                    Receipt = attachment_content_receipt:issue(
                        #{
                            attempt_key => maps:get(attempt_key, Task),
                            object_key => ObjectKey,
                            sha256 => Sha,
                            size => Size,
                            mime => Mime,
                            verdict => <<"pass">>
                        },
                        maps:get(secret, Task)
                    ),
                    {ok, #{sha256 => Sha, size => Size, receipt => Receipt}};
                {error, R} ->
                    {error, R}
            end,
        Out = base64:encode(term_to_binary(Result)),
        ok = file:write_file(TaskFile ++ ".out", Out),
        halt(0)
    catch
        Class:Reason:Stack ->
            io:format(standard_error, "WORKER_CRASH ~p:~p~n", [Class, Reason]),
            [io:format(standard_error, "  ~p~n", [T]) || T <- lists:sublist(Stack, 6)],
            Err = base64:encode(term_to_binary({error, {worker_crash, Class, Reason}})),
            _ = file:write_file(TaskFile ++ ".out", Err),
            halt(1)
    end;
main(_) ->
    halt(2).

strip_nl(B) -> binary:replace(B, <<"\n">>, <<>>, [global]).

%% Worker 内 ReadChunkFun：presign GET + Range（Range 不在 presign 签名内，
%% S3 允许附加）。与 eb_asset_direct:read_fun_from_snapshot/1 语义逐项一致；
%% 正文只进本进程。
range_read_fun(Cfg, ObjectKey) ->
    fun(Offset, Len) ->
        Url = elib_s3_sign:presign_signed_url_at(
            <<"GET">>, maps:get(endpoint, Cfg), maps:get(container, Cfg), ObjectKey,
            [], calendar:universal_time(), 300, Cfg
        ),
        case
            httpc:request(
                get,
                {binary_to_list(Url), [
                    {"range", io_lib:format("bytes=~B-~B", [Offset, Offset + Len - 1])}
                ]},
                [{timeout, 30000}, {autoredirect, false}],
                [{body_format, binary}]
            )
        of
            {ok, {{_, S, _}, _, Body}} when S >= 201, S < 300 -> {ok, Body};
            {ok, {{_, 200, _}, _, Body}} ->
                case Offset < byte_size(Body) of
                    true ->
                        End = min(Offset + Len, byte_size(Body)),
                        {ok, binary:part(Body, Offset, End - Offset)};
                    false ->
                        eof
                end;
            {ok, {{_, 416, _}, _, _}} ->
                eof;
            {ok, {{_, S, _}, _, _}} ->
                {error, {storage_status, S}};
            {error, R} ->
                {error, R}
        end
    end.
