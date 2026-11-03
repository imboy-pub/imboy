-module(talk_record_processor_audio).
-moduledoc "本机处理器临时音频；私有目录与已发布成品spool回调。".
-export([with_audio/2]).

%% no-backend-ffmpeg V1：读取权威 = assembly 已发布音频（客户端直传成品
%% 附件，assemble job succeeded 锚）——不再要求 derived_write 完成行。
with_audio(Aid, Fun) ->
    {ok, Row} = published_row(Aid),
    attachment_artifact_reader:with_source(Row, 134217728, fun(Source) ->
        Base = config_ds:env(imboy, talk_record_render_tmp_dir, "/tmp"),
        {ok, W} = attachment_upload_spool:new(Base),
        ok = attachment_upload_spool:close(W),
        Name =
            "talk-model-" ++
                binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(16), lowercase)),
        Dir = filename:join(Base, Name),
        ok = file:make_dir(Dir),
        ok = file:change_mode(Dir, 8#700),
        Path = filename:join(Dir, "audio.aac"),
        try
            {ok, F} = file:open(Path, [write, binary, exclusive]),
            ok = file:change_mode(Path, 8#600),
            try
                ok = attachment_upload_spool:copy_to(Source, F)
            after
                file:close(F)
            end,
            Fun(list_to_binary(Path))
        after
            case {file:delete(Path), file:del_dir(Dir)} of
                {ok, ok} -> ok;
                _ -> throw(processor_cleanup_unconfirmed)
            end
        end
    end).

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
