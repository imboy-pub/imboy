-module(talk_record_playback_stream).
-moduledoc "回放字节流：JWT主体与票据绑定，HEAD只读元数据，Range仅读必要字节。".
-export([metadata/3, with_range/7, recheck/3, edge_target/1]).

metadata(Uid, Rid, Ticket) ->
    case recheck(Uid, Rid, Ticket) of
        ok ->
            %% no-backend-ffmpeg V1：读取权威 = assembly 已发布音频（客户端
            %% 直传成品附件，assemble job succeeded 锚），票据撤权/过期/软
            %% 删语义不变。
            case elib_pg:with_conn(fun(C) -> published_audio_tx(C, Rid) end) of
                {ok, Row} ->
                    {ok, #{
                        attachment_id => maps:get(<<"id">>, Row),
                        size => maps:get(<<"size">>, Row),
                        mime => <<"audio/aac">>
                    }};
                {error, _} ->
                    {error, playback_media_unavailable};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

published_audio_tx(C, Rid) ->
    Sql =
        <<
            "SELECT a.id, a.size, a.path, a.file_hash256"
            " FROM talk_record_assembly m"
            " JOIN attachment a ON a.id = m.audio_attachment_id"
            " JOIN talk_record_job j ON j.record_id = m.record_id"
            " AND j.kind='assemble' AND j.state='succeeded'"
            " WHERE m.record_id = $1 AND a.lifecycle_state='live'"
            " AND a.status=1 AND a.size>0"
        >>,
    case elib_pg:query(C, Sql, [Rid]) of
        {ok, [Row]} -> {ok, Row};
        _ -> {error, playback_media_unavailable}
    end.

with_range(Uid, Rid, Ticket, Aid, Offset, Length, Fun) ->
    case metadata(Uid, Rid, Ticket) of
        {ok, #{attachment_id := Aid}} ->
            case
                elib_pg:with_conn(fun(C) ->
                    published_audio_tx(C, Rid)
                end)
            of
                {ok, Row} ->
                    attachment_artifact_reader:with_range(Row, Offset, Length, fun(Source) ->
                        case recheck(Uid, Rid, Ticket) of
                            ok -> Fun(Source);
                            Error -> Error
                        end
                    end);
                _ ->
                    {error, playback_media_unavailable}
            end;
        {ok, _} ->
            {error, playback_media_unavailable};
        Error ->
            Error
    end.

recheck(Uid, Rid, Ticket) ->
    case talk_record_grant_playback:verify_ticket(Ticket, Uid, Rid) of
        {ok, _} -> ok;
        Error -> Error
    end.

%% V3-18 P3 边缘承载：回放票据 → 一次性 presigned GET（BEAM 零字节）。
%% 边缘 authorize 回调无请求者上下文，以票据 claim（uid/rid）为主体重放
%% 完整 ACL（verify_ticket/3 绑定校验——撤权/过期/软删即拒），止于
%% attachment 元数据（零字节读），presign 交边缘流式。
edge_target(Ticket) ->
    case talk_record_grant_playback:verify_ticket(Ticket) of
        {ok, #{<<"uid">> := Uid, <<"rid">> := Rid}} ->
            case metadata(Uid, Rid, Ticket) of
                {ok, #{attachment_id := Aid}} ->
                    attachment_edge_redirect:presign_attachment(Aid, 600);
                {ok, _} ->
                    {error, playback_media_unavailable};
                Error ->
                    Error
            end;
        _ ->
            {error, playback_ticket_invalid}
    end.
