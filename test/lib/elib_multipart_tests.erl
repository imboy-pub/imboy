-module(elib_multipart_tests).
%%%
% elib_multipart 流式 multipart 收集器单元测试
% 覆盖：整包/逐字节分块喂入一致性、多 part（字段+文件）、二进制体含
% "部分 boundary" 字节序列、无 file part、超限、缺 closing boundary。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(BOUNDARY, <<"XyZbOuNdArY123">>).

%% 组装典型 wx.uploadFile / curl -F 形态的 multipart body
make_body(Name, Filename, ContentType, FileBytes) ->
    CD =
        case Filename of
            <<>> ->
                <<"Content-Disposition: form-data; name=\"", Name/binary, "\"\r\n">>;
            _ ->
                <<"Content-Disposition: form-data; name=\"", Name/binary, "\"; filename=\"",
                    Filename/binary, "\"\r\n">>
        end,
    CT =
        case ContentType of
            <<>> -> <<>>;
            T -> <<"Content-Type: ", T/binary, "\r\n">>
        end,
    [
        <<"--", ?BOUNDARY/binary, "\r\n">>,
        CD,
        CT,
        <<"\r\n">>,
        FileBytes,
        <<"\r\n--", ?BOUNDARY/binary, "--\r\n">>
    ].

%% 按固定块大小把 body 喂进收集器。
%% 返回 {done, Bytes, FinalSt} | {more, Bytes, FinalSt} | {error, Reason, undefined}。
collect(Body, ChunkSize, MaxSize) ->
    Parent = self(),
    Write = fun(Data) ->
        Parent ! {chunk, Data},
        ok
    end,
    St0 = elib_multipart:new(?BOUNDARY, Write, MaxSize),
    Res = feed(iolist_to_binary(Body), ChunkSize, St0),
    Chunks = recv_chunks([]),
    Bytes = iolist_to_binary(Chunks),
    case Res of
        {done, FinalSt} -> {done, Bytes, FinalSt};
        {more, FinalSt} -> {more, Bytes, FinalSt};
        {error, R} -> {error, R, undefined}
    end.

feed(<<>>, _ChunkSize, St) ->
    case elib_multipart:stream(<<>>, St) of
        {done, St2} -> {done, St2};
        {more, St2} -> {more, St2};
        {error, R} -> {error, R}
    end;
feed(Body, ChunkSize, St) ->
    N = min(ChunkSize, byte_size(Body)),
    <<Chunk:N/binary, Rest/binary>> = Body,
    case elib_multipart:stream(Chunk, St) of
        {more, St2} -> feed(Rest, ChunkSize, St2);
        {done, St2} -> {done, St2};
        {error, R} -> {error, R}
    end.

recv_chunks(Acc) ->
    receive
        {chunk, Data} -> recv_chunks([Data | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

%% ===================================================================

whole_body_single_chunk_test() ->
    File = <<"hello upload world">>,
    Body = make_body(<<"file">>, <<"a.jpg">>, <<"image/jpeg">>, File),
    {done, Got, _St} = collect(Body, 100000, 1024 * 1024),
    ?assertEqual(File, Got).

chunked_feed_matches_whole_test() ->
    File = crypto:strong_rand_bytes(5000),
    Body = iolist_to_binary(make_body(<<"file">>, <<"v.mp4">>, <<"video/mp4">>, File)),
    {done, Got1, _} = collect(Body, 1, 1024 * 1024),
    {done, Got2, _} = collect(Body, 3, 1024 * 1024),
    {done, Got7, _} = collect(Body, 7, 1024 * 1024),
    ?assertEqual(File, Got1),
    ?assertEqual(File, Got2),
    ?assertEqual(File, Got7).

extra_field_then_file_test() ->
    File = <<"binary payload \0\1\2 here">>,
    FieldPart =
        [
            <<"--", ?BOUNDARY/binary, "\r\n">>,
            <<"Content-Disposition: form-data; name=\"comment\"\r\n\r\n">>,
            <<"hello">>,
            <<"\r\n">>
        ],
    FilePart =
        [
            <<"--", ?BOUNDARY/binary, "\r\n">>,
            <<"Content-Disposition: form-data; name=\"file\"; filename=\"trae测试.jpg\"\r\n">>,
            <<"Content-Type: image/jpeg\r\n\r\n">>,
            File,
            <<"\r\n--", ?BOUNDARY/binary, "--\r\n">>
        ],
    {done, Got, _} = collect(FieldPart ++ FilePart, 17, 1024 * 1024),
    ?assertEqual(File, Got).

file_before_field_test() ->
    File = <<"AAAABBBBCCCC">>,
    FilePart =
        [
            <<"--", ?BOUNDARY/binary, "\r\n">>,
            <<"Content-Disposition: form-data; name=\"file\"; filename=\"a.png\"\r\n">>,
            <<"Content-Type: image/png\r\n\r\n">>,
            File,
            <<"\r\n">>
        ],
    FieldPart =
        [
            <<"--", ?BOUNDARY/binary, "\r\n">>,
            <<"Content-Disposition: form-data; name=\"sha256\"\r\n\r\n">>,
            <<"deadbeef">>,
            <<"\r\n--", ?BOUNDARY/binary, "--\r\n">>
        ],
    {done, Got, _} = collect(FilePart ++ FieldPart, 5, 1024 * 1024),
    ?assertEqual(File, Got).

binary_body_with_partial_boundary_bytes_test() ->
    %% 文件体含 "\r\n--XyZ"（boundary 前缀但非完整 boundary）→ 必须原样写出
    File = <<16#00, 16#01, $\r, $\n, $-, $-, "XyZ", 16#FF, 16#FE, "tail">>,
    Body = make_body(<<"file">>, <<"b.bin">>, <<"application/octet-stream">>, File),
    {done, Got, _} = collect(Body, 2, 1024 * 1024),
    ?assertEqual(File, Got).

no_file_part_test() ->
    Body =
        [
            <<"--", ?BOUNDARY/binary, "\r\n">>,
            <<"Content-Disposition: form-data; name=\"comment\"\r\n\r\n">>,
            <<"no file here">>,
            <<"\r\n--", ?BOUNDARY/binary, "--\r\n">>
        ],
    Write = fun(_) -> ok end,
    St0 = elib_multipart:new(?BOUNDARY, Write, 1024 * 1024),
    {done, FinalSt} = feed(iolist_to_binary(Body), 4, St0),
    ?assertEqual({error, no_file_part}, elib_multipart:result(FinalSt)).

oversize_aborts_test() ->
    File = crypto:strong_rand_bytes(1024),
    Body = iolist_to_binary(
        make_body(<<"file">>, <<"big.bin">>, <<"application/octet-stream">>, File)
    ),
    {error, file_too_large, _} = collect(Body, 128, 100).

missing_closing_boundary_stays_more_test() ->
    Body = iolist_to_binary(
        [
            <<"--", ?BOUNDARY/binary, "\r\n">>,
            <<"Content-Disposition: form-data; name=\"file\"; filename=\"a.jpg\"\r\n\r\n">>,
            <<"truncated">>
        ]
    ),
    {more, _Bytes, _St} = collect(Body, 9, 1024 * 1024).

result_size_reflects_written_bytes_test() ->
    File = crypto:strong_rand_bytes(3210),
    Body = iolist_to_binary(make_body(<<"file">>, <<"s.mp4">>, <<"video/mp4">>, File)),
    Write = fun(_) -> ok end,
    St0 = elib_multipart:new(?BOUNDARY, Write, 1024 * 1024),
    {done, FinalSt} = feed(Body, 997, St0),
    {ok, #{size := 3210}} = elib_multipart:result(FinalSt),
    ok.
