-module(talk_record_content_http).
-moduledoc "面聊记录职责拆分；保留原端点、权限与事务语义。".
-include("error_code.hrl").
-export([
    segment_update/2,
    segments_list/2,
    speaker_claim/2,
    speaker_update/2,
    speakers_list/2,
    summary_get/2,
    summary_update/2
]).

%% ===================================================================
%% T15 内容域 Actions（原文/发言人/摘要；§4）
%% ===================================================================

%% GET /api/v1/talk-records/:id/segments（cursor/limit/speaker_id/q；正序）
-spec segments_list(cowboy_req:req(), map()) -> cowboy_req:req().
segments_list(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            Qs = cowboy_req:parse_qs(Req0),
            Raw = #{
                <<"speaker_id">> => proplists:get_value(<<"speaker_id">>, Qs, <<>>),
                <<"q">> => proplists:get_value(<<"q">>, Qs, <<>>)
            },
            Cursor = proplists:get_value(<<"cursor">>, Qs, undefined),
            Limit = talk_record_logic:parse_limit(proplists:get_value(<<"limit">>, Qs, undefined)),
            case parse_segment_filters(Raw) of
                {ok, Filters} ->
                    talk_record_http:respond_ok(
                        Req0,
                        talk_record_content_segments:list(Uid, Id, Filters, Cursor, Limit)
                    );
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% @private segments 过滤参数（speaker_id 正整数可缺省；q ≤100 codepoint）。
-spec parse_segment_filters(map()) -> {ok, map()} | {error, atom()}.
parse_segment_filters(Raw) ->
    Speaker = maps:get(<<"speaker_id">>, Raw, <<>>),
    Q = maps:get(<<"q">>, Raw, <<>>),
    case {valid_speaker_filter(Speaker), io_list_char_count(Q) =< 100} of
        {false, _} ->
            {error, invalid_param};
        {_, false} ->
            {error, invalid_filter};
        {_, true} ->
            {ok, #{
                speaker_id => speaker_filter(Speaker),
                q => norm_query(Q)
            }}
    end.

-spec valid_speaker_filter(term()) -> boolean().
valid_speaker_filter(<<>>) ->
    true;
valid_speaker_filter(Speaker) when is_binary(Speaker) ->
    case talk_record_content_common:parse_pos_int(Speaker) of
        {ok, _} -> true;
        error -> false
    end;
valid_speaker_filter(_) ->
    false.

-spec speaker_filter(binary()) -> integer() | undefined.
speaker_filter(<<>>) ->
    undefined;
speaker_filter(Speaker) ->
    {ok, Id} = talk_record_content_common:parse_pos_int(Speaker),
    Id.

-spec norm_query(binary()) -> binary().
norm_query(undefined) ->
    <<>>;
norm_query(Q) when is_binary(Q) ->
    Q.

-spec io_list_char_count(term()) -> non_neg_integer().
io_list_char_count(Q) when is_binary(Q) ->
    try
        string:length(Q)
    catch
        _:_ -> 999999
    end;
io_list_char_count(_) ->
    999999.

%% PATCH /api/v1/talk-records/:id/segments/:segment_id（text/speaker_id + If-Match）
-spec segment_update(cowboy_req:req(), map()) -> cowboy_req:req().
segment_update(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    case talk_record_http:path_ids(Req0, [id, segment_id]) of
        {ok, [Id, SegmentId]} ->
            Body = elib_param:post(Req0),
            case talk_record_content_segments:update(Uid, Id, SegmentId, IfMatch, Body) of
                {ok, #{<<"segment">> := Segment} = Payload} ->
                    reply_content_etag(Req0, Segment, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% GET /api/v1/talk-records/:id/speakers（不暴露 embedding；§4）
-spec speakers_list(cowboy_req:req(), map()) -> cowboy_req:req().
speakers_list(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            talk_record_http:respond_ok(Req0, talk_record_content_speakers:list(Uid, Id));
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% PATCH /api/v1/talk-records/:id/speakers/:speaker_id（display_name/matched_user_id + If-Match）
-spec speaker_update(cowboy_req:req(), map()) -> cowboy_req:req().
speaker_update(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    case talk_record_http:path_ids(Req0, [id, speaker_id]) of
        {ok, [Id, SpeakerId]} ->
            Body = elib_param:post(Req0),
            case talk_record_content_speakers:update(Uid, Id, SpeakerId, IfMatch, Body) of
                {ok, #{<<"speaker">> := Speaker} = Payload} ->
                    reply_content_etag(Req0, Speaker, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records/:id/speakers/:speaker_id/claim（仅本人；§4）
-spec speaker_claim(cowboy_req:req(), map()) -> cowboy_req:req().
speaker_claim(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_ids(Req0, [id, speaker_id]) of
        {ok, [Id, SpeakerId]} ->
            talk_record_http:respond_ok(
                Req0, talk_record_content_speakers:claim(Uid, Id, SpeakerId)
            );
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% GET /api/v1/talk-records/:id/summary（未生成返回状态对象，不伪装 404）
-spec summary_get(cowboy_req:req(), map()) -> cowboy_req:req().
summary_get(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            case talk_record_content_summary:get(Uid, Id) of
                {ok, #{<<"etag">> := ETag} = Payload} ->
                    Req1 = cowboy_req:set_resp_header(
                        <<"etag">>, talk_record_http:quote_etag(ETag), Req0
                    ),
                    elib_response:success(Req1, Payload);
                {ok, StatusObject} ->
                    elib_response:success(Req0, StatusObject);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% PATCH /api/v1/talk-records/:id/summary（人工编辑与 confirm；If-Match）
-spec summary_update(cowboy_req:req(), map()) -> cowboy_req:req().
summary_update(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            Body = elib_param:post(Req0),
            case talk_record_content_summary:update(Uid, Id, IfMatch, Body) of
                {ok, #{<<"summary">> := Summary} = Payload} ->
                    Req1 = cowboy_req:set_resp_header(
                        <<"etag">>,
                        talk_record_http:quote_etag(maps:get(<<"etag">>, Summary, <<>>)),
                        Req0
                    ),
                    elib_response:success(Req1, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% @private 内容域 200 响应：ETag 头（RFC 引号形态）+ payload etag 令牌本体。
-spec reply_content_etag(cowboy_req:req(), map(), map()) -> cowboy_req:req().
reply_content_etag(Req0, Resource, Payload) ->
    ETag = maps:get(<<"etag">>, Resource, <<>>),
    Req1 = cowboy_req:set_resp_header(<<"etag">>, talk_record_http:quote_etag(ETag), Req0),
    elib_response:success(Req1, Payload).
