#!/usr/bin/env escript
%%! -noshell +Bd
%%%
%%% V3-18 P2 汇编 IO Worker（独立 OS 进程，S3-only 合同 §5.4/§7）：
%%%   * 派生工件链的 S3↔盘字节搬运全部在本进程——主 BEAM 零音频字节
%%%     （no-backend-ffmpeg V1 后无服务端解码/编码，本 Worker 只补 IO 段）；
%%%   * fetch：按序下载 parts 到工作目录（逐项 size+sha256 校验，任一
%%%     不符即失败——完整性不降于原 BEAM spool 校验）；
%%%   * upload：本地文件 → 对象存储（Worker 本地算 sha256/b64 checksum、
%%%     自签 presign PUT——与 put_snapshot 同构的 issued/expires 窗口）；
%%%   * 任务文件 base64(term_to_binary(Task)) 0600 短存（凭据不进 ps），
%%%     结果写 <任务文件>.out（有界），任一失败非零退出 fail-closed。
%%% Task 字段：action=fetch|upload；ebin/code_paths/deadline_ms 公共；
%%%   fetch: items=[{url,path,sha256_hex,size}]（有序）；
%%%   upload: src_path/endpoint/ak/sk/bucket/region/object_key/mime/
%%%           issued_at={{Y,M,D},{H,M,S}}/expires_at={{..}}（UTC）
%%!
main([TaskFile | _]) ->
    try
        {ok, _} = application:ensure_all_started(inets),
        {ok, _} = application:ensure_all_started(crypto),
        {ok, _} = application:ensure_all_started(ssl),
        {ok, Encoded0} = file:read_file(TaskFile),
        Task = binary_to_term(base64:decode(strip_nl(Encoded0))),
        code:add_patha(to_path(maps:get(ebin, Task))),
        [code:add_patha(to_path(P)) || P <- maps:get(code_paths, Task, [])],
        Result =
            case maps:get(action, Task) of
                fetch -> do_fetch(maps:get(items, Task, []));
                upload -> do_upload(Task)
            end,
        Out = base64:encode(term_to_binary(Result)),
        ok = file:write_file(TaskFile ++ ".out", Out),
        halt(0)
    catch
        Class:Reason:_Stack ->
            Err = base64:encode(term_to_binary({error, {worker_crash, Class, Reason}})),
            _ = file:write_file(TaskFile ++ ".out", Err),
            halt(1)
    end;
main(_) ->
    halt(2).

strip_nl(B) -> binary:replace(B, <<"\n">>, <<>>, [global]).

%% 路径字段兼容 binary|string（调用方两种形态都合法，禁止 badarg 崩）。
to_path(P) when is_binary(P) -> binary_to_list(P);
to_path(P) when is_list(P) -> P.

%% parts 按序下载（顺序即拼接序）；逐项 size+sha256 全量校验。
do_fetch(Items) -> do_fetch(Items, 1).
do_fetch([], _N) -> {ok, #{fetched => 0}};
do_fetch([Item | Rest], N) ->
    Url = maps:get(url, Item),
    Path = to_path(maps:get(path, Item)),
    WantSha = maps:get(sha256_hex, Item),
    WantSize = maps:get(size, Item),
    case httpc:request(get, {binary_to_list(Url), []}, [{timeout, 60000}, {autoredirect, false}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
            case byte_size(Body) =:= WantSize andalso Sha =:= WantSha of
                true ->
                    ok = file:write_file(Path, Body),
                    do_fetch(Rest, N + 1);
                false ->
                    {error, {part_integrity_mismatch, N}}
            end;
        {ok, {{_, S, _}, _, _}} ->
            {error, {fetch_status, N, S}};
        {error, R} ->
            {error, {fetch_error, N, R}}
    end.

%% 本地文件上传：sha256 本地算（不回 BEAM）、presign PUT + checksum 头。
do_upload(Task) ->
    SrcPath = to_path(maps:get(src_path, Task)),
    case file:read_file(SrcPath) of
        {ok, Bytes} when byte_size(Bytes) > 0 ->
            Cfg = #{
                endpoint => maps:get(endpoint, Task),
                container => maps:get(bucket, Task),
                access_key => maps:get(ak, Task),
                secret_key => maps:get(sk, Task),
                region => maps:get(region, Task, <<"garage">>)
            },
            Ck = base64:encode(crypto:hash(sha256, Bytes)),
            Mime = maps:get(mime, Task, <<"application/octet-stream">>),
            Url = elib_s3_sign:presign_signed_url_at(
                <<"PUT">>, maps:get(endpoint, Cfg), maps:get(container, Cfg),
                maps:get(object_key, Task),
                [{<<"content-type">>, Mime}, {<<"x-amz-checksum-sha256">>, Ck}],
                maps:get(issued_at, Task, calendar:universal_time()),
                window_seconds(Task),
                Cfg
            ),
            case
                httpc:request(
                    put,
                    {binary_to_list(Url),
                     [{"Content-Type", binary_to_list(Mime)},
                      {"x-amz-checksum-sha256", binary_to_list(Ck)}],
                     binary_to_list(Mime), Bytes},
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

window_seconds(Task) ->
    case maps:get(expires_at, Task, undefined) of
        undefined -> 30;
        Exp ->
            calendar:datetime_to_gregorian_seconds(Exp) -
                calendar:datetime_to_gregorian_seconds(maps:get(issued_at, Task, calendar:universal_time()))
    end.
