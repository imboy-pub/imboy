-module(talk_record_playback_stream).
-moduledoc "回放字节流：JWT主体与票据绑定，HEAD只读元数据，Range仅读必要字节。".
-export([metadata/3, with_range/7, recheck/3]).

metadata(Uid, Rid, Ticket) ->
    case recheck(Uid, Rid, Ticket) of
        ok ->
            case elib_pg:with_conn(fun(C) -> talk_record_assembly_logic:find_tx(C, Rid) end) of
                {ok, #{<<"audio_attachment_id">> := Aid}} when is_integer(Aid), Aid > 0 ->
                    case attachment_derived_artifact:metadata(Aid) of
                        {ok, Row} ->
                            {ok, #{
                                attachment_id => Aid,
                                size => maps:get(<<"size">>, Row),
                                mime => <<"audio/aac">>
                            }};
                        Error ->
                            Error
                    end;
                {ok, _} ->
                    {error, playback_media_unavailable};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

with_range(Uid, Rid, Ticket, Aid, Offset, Length, Fun) ->
    case metadata(Uid, Rid, Ticket) of
        {ok, #{attachment_id := Aid}} ->
            attachment_derived_artifact:with_range(Aid, Offset, Length, fun(Source) ->
                case recheck(Uid, Rid, Ticket) of
                    ok -> Fun(Source);
                    Error -> Error
                end
            end);
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
