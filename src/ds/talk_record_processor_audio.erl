-module(talk_record_processor_audio).
-moduledoc "本机处理器临时音频；私有目录与真实附件spool回调。".
-export([with_audio/2]).

with_audio(Aid, Fun) ->
    attachment_derived_artifact:with_spool(Aid, fun(Source) ->
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
