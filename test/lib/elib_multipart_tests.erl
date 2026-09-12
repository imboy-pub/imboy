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

%% ===================================================================
%% 边界用例（安全审查补齐）：epilogue 注入、多 file part、空 multipart、
%% 无 name 参数 part、非 file 字段超限
%% ===================================================================

%% closing boundary 之后的 epilogue 里嵌套完整新 boundary+headers+文件数据：
%% 注入内容绝不能被当作新 part 追加进输出（P2-2 安全判定用例）。
epilogue_injection_after_closing_test() ->
    InjectedPart =
        [
            <<"\r\n--", ?BOUNDARY/binary, "\r\n">>,
            <<"Content-Disposition: form-data; name=\"file\"; filename=\"evil.jpg\"\r\n\r\n">>,
            <<"INJECTED-PAYLOAD">>,
            <<"\r\n--", ?BOUNDARY/binary, "--\r\n">>
        ],
    Body =
        iolist_to_binary(
            make_body(<<"file">>, <<"a.jpg">>, <<"image/jpeg">>, <<"FIRST">>) ++ InjectedPart
        ),
    {done, Got, _} = collect(Body, 3, 1024 * 1024),
    ?assertEqual(<<"FIRST">>, Got).

%% 两个 name="file" part：当前契约=顺序追加（handler 层语义），锁定行为。
multiple_file_parts_accumulate_test() ->
    Part = fun(Fn, Bytes) ->
        [
            <<"--", ?BOUNDARY/binary, "\r\n">>,
            <<"Content-Disposition: form-data; name=\"file\"; filename=\"", Fn/binary,
                "\"\r\n\r\n">>,
            Bytes,
            <<"\r\n">>
        ]
    end,
    Closing = <<"--", ?BOUNDARY/binary, "--\r\n">>,
    Body = iolist_to_binary(
        Part(<<"a.jpg">>, <<"AAA">>) ++ Part(<<"b.jpg">>, <<"BBB">>) ++ Closing
    ),
    {done, Got, _} = collect(Body, 2, 1024 * 1024),
    ?assertEqual(<<"AAABBB">>, Got).

%% 立即 closing 的空 multipart：正常 done，无 file part。
empty_multipart_immediate_closing_test() ->
    Body = <<"--", ?BOUNDARY/binary, "--\r\n">>,
    Write = fun(_) -> ok end,
    St0 = elib_multipart:new(?BOUNDARY, Write, 1024 * 1024),
    {done, FinalSt} = feed(Body, 3, St0),
    ?assertEqual({error, no_file_part}, elib_multipart:result(FinalSt)).

%% part 头只有 filename 没有 name 参数：跳过不崩，最终 no_file_part。
noname_part_test() ->
    Body =
        iolist_to_binary(
            [
                <<"--", ?BOUNDARY/binary, "\r\n">>,
                <<"Content-Disposition: form-data; filename=\"x.jpg\"\r\n\r\n">>,
                <<"orphan">>,
                <<"\r\n--", ?BOUNDARY/binary, "--\r\n">>
            ]
        ),
    Write = fun(_) -> ok end,
    St0 = elib_multipart:new(?BOUNDARY, Write, 1024 * 1024),
    {done, FinalSt} = feed(Body, 4, St0),
    ?assertEqual({error, no_file_part}, elib_multipart:result(FinalSt)).

%% 非 file 字段的超大 body 也必须触发总量上限（P1-1：原始字节一并计数）。
oversize_nonfile_field_aborts_test() ->
    FieldPart =
        [
            <<"--", ?BOUNDARY/binary, "\r\n">>,
            <<"Content-Disposition: form-data; name=\"comment\"\r\n\r\n">>,
            crypto:strong_rand_bytes(2048),
            <<"\r\n--", ?BOUNDARY/binary, "--\r\n">>
        ],
    {error, file_too_large, _} = collect(FieldPart, 64, 100).

%% WriteFun（临时文件写入）抛错必须分流为 {write_failed, _}，
%% 不得与协议解析错误混为 {bad_part, _}（后者映射 400，前者映射 5xx）。
write_fun_failure_isolated_test() ->
    Body = iolist_to_binary(
        make_body(<<"file">>, <<"a.jpg">>, <<"image/jpeg">>, <<"DATA">>)
    ),
    Write = fun(_) -> erlang:error(disk_full) end,
    St0 = elib_multipart:new(?BOUNDARY, Write, 1024 * 1024),
    {error, {write_failed, _}} = feed(Body, 5, St0),
    ok.
