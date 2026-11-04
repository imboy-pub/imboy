-module(talk_record_processor_audio).
-moduledoc "本机处理器临时音频；私有目录与已发布成品spool回调。".
-export([with_audio/2]).

%% no-backend-ffmpeg V1：读取权威 = assembly 已发布音频（客户端直传成品
%% 附件，assemble job succeeded 锚）——不再要求 derived_write 完成行。
%%
%% V3-18/C09 收口（source-audit C09-recording-body）：音频正文退出 BEAM。
%% 原实现 attachment_artifact_reader:with_source → BEAM spool copy_to 把
%% 全量音频字节经本 VM 搬运到临时文件；现改为 BEAM 只产出冻结位置快照的
%% 短时 presign GET（任务仅含精确对象能力 url + 期望 size/sha256 + 本地
%% 目标路径），下载由最小受限 Worker（assembly_io_worker fetch，独立 OS
%% 进程）执行：Worker 进程内逐项 size+sha256 校验（任一不符不写盘、非零
%% 退出）——回执 {ok, #{fetched => N}} 绑定输入 identity/hash/size。BEAM
%% 全程零音频字节，无 BEAM 内读回退路径（fail-closed）：Worker kill/超时/
%% 对象被换/凭据失效均以 {error,_} 结束，模型作业按既有 classify 走
%% requeue/suspend，绝不以缺字节的临时文件进入 processor 发布链。
with_audio(Aid, Fun) ->
    {ok, Row} = published_row(Aid),
    {Dir, Path} = tmp_target(),
    ok = file:make_dir(Dir),
    ok = file:change_mode(Dir, 8#700),
    case fetch_audio(Row, Path) of
        ok ->
            try
                Fun(list_to_binary(Path))
            after
                cleanup(Path, Dir)
            end;
        {error, _} = E ->
            %% Worker 校验失败不写盘（fetch 先验 size+sha 再落盘）；残留
            %% 目录尽力清理，不遮蔽原始失败（作业按 {error,_} settle）。
            discard(Path, Dir),
            E
    end.

published_row(Aid) ->
    elib_pg:with_conn(fun(C) ->
        case
            elib_pg:query(
                C,
                <<
                    "SELECT a.path, a.size, a.file_hash256"
                    " FROM talk_record_assembly m"
                    " JOIN attachment a ON a.id = m.audio_attachment_id"
                    " JOIN talk_record_job j ON j.record_id = m.record_id"
                    " AND j.kind='assemble' AND j.state='succeeded'"
                    " WHERE m.audio_attachment_id = $1"
                    " AND a.lifecycle_state='live' AND a.status=1 AND a.size>0"
                >>,
                [Aid]
            )
        of
            {ok, [R]} -> {ok, R};
            _ -> {error, audio_source_denied}
        end
    end).

%% 冻结位置 → 短时 presign GET（600s ≥ fetch deadline 600s）→ 受限 Worker
%% fetch（任务仅精确对象能力与期望字节身份）。size/hash 界与原
%% with_source 的有界读一致（≤128MiB、64 位 hex hash）。
fetch_audio(#{<<"path">> := P, <<"size">> := Size, <<"file_hash256">> := Hash} = Row, TmpPath) when
    is_integer(Size),
    Size > 0,
    Size =< 134217728,
    is_binary(Hash),
    byte_size(Hash) =:= 64
->
    case attachment_storage:frozen_location(P, Row) of
        {ok, #{driver := garage} = I} ->
            case attachment_storage_garage:presign_snapshot(I, 600) of
                {ok, Url} ->
                    case
                        attachment_worker_port:fetch_files([
                            #{url => Url, path => TmpPath, sha256_hex => Hash, size => Size}
                        ])
                    of
                        {ok, _} -> ok;
                        {error, _} = E -> E
                    end;
                {error, _} = E ->
                    E
            end;
        {ok, #{driver := disk}} ->
            {error, disk_driver_retired};
        _ ->
            {error, location_unverified}
    end;
fetch_audio(_, _) ->
    {error, artifact_integrity_mismatch}.

tmp_target() ->
    Base = config_ds:env(imboy, talk_record_render_tmp_dir, "/tmp"),
    Name =
        "talk-model-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(16), lowercase)),
    Dir = filename:join(Base, Name),
    {Dir, filename:join(Dir, "audio.aac")}.

cleanup(Path, Dir) ->
    case {file:delete(Path), file:del_dir(Dir)} of
        {ok, ok} -> ok;
        _ -> throw(processor_cleanup_unconfirmed)
    end.

discard(Path, Dir) ->
    _ = file:delete(Path),
    _ = file:del_dir(Dir),
    ok.
