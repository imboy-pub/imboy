-module(attachment_artifact_reader).
-moduledoc "已授权附件冻结位置的有界私有spool读取；完整大小/hash通过后才向业务交付句柄。".
-export([with_source/3, with_range/4]).

with_source(Row, Max, Fun) when is_integer(Max), Max > 0, is_function(Fun, 1) ->
    Size = maps:get(<<"size">>, Row, 0),
    Hash = maps:get(<<"file_hash256">>, Row, <<>>),
    case
        is_integer(Size) andalso Size > 0 andalso Size =< Max andalso
            is_binary(Hash) andalso byte_size(Hash) =:= 64
    of
        true ->
            case attachment_storage:frozen_location(maps:get(<<"path">>, Row), Row) of
                {ok, I} -> spool(I, Size, Hash, Fun);
                Error -> Error
            end;
        false ->
            {error, artifact_integrity_mismatch}
    end.

%% Only immutable completed artifacts use partial reads: publication already
%% verified the full-object hash; an arbitrary range cannot recompute that hash.
with_range(Row, Offset, Length, Fun) when
    is_integer(Offset), Offset >= 0, is_integer(Length), Length > 0
->
    Total = maps:get(<<"size">>, Row, 0),
    case Offset + Length =< Total andalso Length =< 134217728 of
        true ->
            case attachment_storage:frozen_location(maps:get(<<"path">>, Row), Row) of
                {ok, I} -> range_spool(I, Offset, Length, Total, Fun);
                Error -> Error
            end;
        false ->
            {error, range_not_satisfiable}
    end.

range_spool(I, Offset, Length, Total, Fun) ->
    case attachment_upload_spool:new() of
        {ok, W} ->
            try
                case copy_range(I, W, Offset, Length, Total) of
                    {ok, Length} ->
                        case attachment_upload_spool:seal(W) of
                            {ok, S} -> Fun(S);
                            Error -> Error
                        end;
                    {ok, _} ->
                        {error, artifact_integrity_mismatch};
                    Error ->
                        Error
                end
            after
                attachment_upload_spool:close(W)
            end;
        Error ->
            Error
    end.
copy_range(#{driver := disk} = I, W, Offset, Length, Total) ->
    attachment_storage_disk:fold_range_snapshot(
        I,
        Offset,
        Length,
        Total,
        fun(B, N) -> append(W, B, N, Length) end,
        0
    );
copy_range(#{driver := garage} = I, W, Offset, Length, Total) ->
    %% presign_snapshot takes a relative TTL in seconds, not a datetime.
    case attachment_storage_garage:presign_snapshot(I, 300) of
        {ok, Url} ->
            Range = lists:flatten(io_lib:format("bytes=~B-~B", [Offset, Offset + Length - 1])),
            Expected = list_to_binary(
                lists:flatten(io_lib:format("bytes ~B-~B/~B", [Offset, Offset + Length - 1, Total]))
            ),
            download(Url, W, Length, [{"range", Range}], Expected);
        Error ->
            Error
    end;
copy_range(_, _, _, _, _) ->
    {error, location_unverified}.

spool(I, Size, Hash, Fun) ->
    case attachment_upload_spool:new() of
        {ok, Writer} ->
            try
                case copy(I, Writer, Size) of
                    {ok, Size} ->
                        case attachment_upload_spool:seal(Writer) of
                            {ok, Source} ->
                                case attachment_upload_spool:sha256(Source) =:= Hash of
                                    true -> Fun(Source);
                                    false -> {error, artifact_integrity_mismatch}
                                end;
                            Error ->
                                Error
                        end;
                    {ok, _} ->
                        {error, artifact_integrity_mismatch};
                    Error ->
                        Error
                end
            after
                attachment_upload_spool:close(Writer)
            end;
        Error ->
            Error
    end.

copy(#{driver := disk} = I, Writer, Size) ->
    attachment_storage_disk:fold_snapshot(I, fun(B, N) -> append(Writer, B, N, Size) end, 0);
copy(#{driver := garage} = I, Writer, Size) ->
    %% presign_snapshot takes a relative TTL in seconds, not a datetime.
    case attachment_storage_garage:presign_snapshot(I, 300) of
        {ok, Url} -> download(Url, Writer, Size);
        Error -> Error
    end;
copy(_, _, _) ->
    {error, location_unverified}.

append(Writer, Bytes, N, Size) when byte_size(Bytes) + N =< Size ->
    case attachment_upload_spool:append(Writer, Bytes) of
        {ok, _} -> {ok, N + byte_size(Bytes)};
        Error -> Error
    end;
append(_, _, _, _) ->
    {error, artifact_integrity_mismatch}.

%% Storage endpoints come only from verified frozen registry snapshots. Never
%% follow a redirect or expose this signed storage URL to the media client.
download(Url, Writer, Size) -> download(Url, Writer, Size, [], undefined).
download(Url, Writer, Size, Headers, ExpectedRange) ->
    Options =
        [{timeout, 300000}, {connect_timeout, 10000}, {autoredirect, false}] ++ tls_options(Url),
    case
        httpc:request(
            get,
            {binary_to_list(Url), Headers},
            Options,
            [{sync, false}, {stream, {self, once}}, {body_format, binary}]
        )
    of
        {ok, Id} ->
            Deadline = erlang:monotonic_time(millisecond) + 300000,
            try
                first(Id, Writer, Size, Deadline, ExpectedRange)
            after
                httpc:cancel_request(Id),
                drain(Id)
            end;
        Error ->
            Error
    end.

first(Id, Writer, Size, Deadline, ExpectedRange) ->
    Timeout = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {http, {Id, stream_start, Headers, Pid}} ->
            case range_headers(Headers, ExpectedRange, Size) of
                true ->
                    httpc:stream_next(Pid),
                    collect(Id, Pid, Writer, Size, 0, Deadline);
                false ->
                    {error, storage_range_mismatch}
            end;
        {http, {Id, {{_, Status, _}, _, _}}} ->
            {error, {storage_http_status, Status}};
        {http, {Id, {error, Reason}}} ->
            {error, Reason}
    after Timeout -> {error, timeout}
    end.

collect(Id, Pid, Writer, Size, N, Deadline) ->
    Timeout = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {http, {Id, stream, Bytes}} when is_binary(Bytes) ->
            case append(Writer, Bytes, N, Size) of
                {ok, Next} ->
                    httpc:stream_next(Pid),
                    collect(Id, Pid, Writer, Size, Next, Deadline);
                Error ->
                    Error
            end;
        {http, {Id, stream_end, _}} ->
            {ok, N};
        {http, {Id, {error, Reason}}} ->
            {error, Reason}
    after Timeout -> {error, timeout}
    end.

drain(Id) ->
    receive
        {http, {Id, _, _}} -> drain(Id);
        {http, {Id, _}} -> drain(Id)
    after 0 -> ok
    end.

range_headers(_, undefined, _) ->
    true;
range_headers(Headers, Expected, Size) ->
    Normal = [{string:lowercase(to_list(K)), to_list(V)} || {K, V} <- Headers],
    proplists:get_value("content-range", Normal) =:= binary_to_list(Expected) andalso
        proplists:get_value("content-length", Normal) =:= integer_to_list(Size).
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.

tls_options(<<"https://", _/binary>>) ->
    [
        {ssl, [
            {verify, verify_peer},
            {cacerts, public_key:cacerts_get()},
            {customize_hostname_check, [
                {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
            ]}
        ]}
    ];
tls_options(_) ->
    [].
