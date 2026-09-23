%%% @doc eb_asset_content 纯函数单元测试（mime 白名单 + 魔数嗅探）。
%%% 无 DB 依赖：跑法 `make eunit t=eb_asset_content_tests`。
%%% 扩表纪律：?ALLOWED_MIMES 每个 MIME 在此必须有「真魔数正例」+
%%% 「声明与内容不符负例」，两条腿缺一不可（2026-09-23 扩表时钉死）。
-module(eb_asset_content_tests).

-include_lib("eunit/include/eunit.hrl").

mime_and_sniff_matrix_test_() ->
    Cases = [
        %% {Mime, 合法内容（真魔数）, 非法内容（声明与内容不符）}
        {<<"image/png">>, <<137, 80, 78, 71, 13, 10, 26, 10, "rest">>, <<"GIF89a">>},
        {<<"image/jpeg">>, <<16#FF, 16#D8, 16#FF, 16#E0, "junk">>, <<137, 80, 78, 71>>},
        {<<"image/gif">>, <<"GIF89a", 1, 2, 3>>, <<"PNGNOT">>},
        {<<"image/webp">>, <<"RIFF", 0, 0, 0, 0, "WEBP", "VP8 ">>, <<"RIFF", 0, 0, 0, 0, "WAV ">>},
        {<<"image/bmp">>, <<"BM", 0, 0, 0>>, <<"GIF89a">>},
        {<<"image/svg+xml">>, <<"<svg xmlns=\"...\">hi</svg>">>, <<"<notsvg/>">>},
        {<<"application/pdf">>, <<"%PDF-1.7 rest">>, <<"PLAINTEXT">>},
        {<<"text/plain">>, <<"hello world\n">>, <<"a", 0, "b">>},
        {<<"text/markdown">>, <<"# Title\n\ntext\n">>, <<0, 1, 2>>},
        {<<"text/csv">>, <<"a,b,c\n1,2,3\n">>, <<255, 254, 0>>},
        {<<"application/zip">>, <<80, 75, 3, 4, "payload">>, <<"Rar!">>},
        {<<"application/x-7z-compressed">>, <<55, 122, 188, 175, 39, 28, "rest">>, <<"PK", 3, 4>>},
        {<<"application/gzip">>, <<16#1F, 16#8B, 8, 0>>, <<"MZ">>},
        {<<"application/msword">>,
            <<16#D0, 16#CF, 16#11, 16#E0, 16#A1, 16#B1, 16#1A, 16#E1, "ole">>, <<80, 75, 3, 4>>},
        {<<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>,
            <<80, 75, 3, 4, "ooxml">>, <<"OLE8">>},
        {<<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>,
            <<80, 75, 3, 4, "xlsx">>, <<"text">>},
        {<<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>,
            <<80, 75, 3, 4, "pptx">>, <<"%PDF-1.4">>},
        {<<"video/mp4">>, <<"    ftypisom", 0, 0>>, <<"ftypat-start">>},
        {<<"video/webm">>, <<16#1A, 16#45, 16#DF, 16#A3, 9>>, <<"ID3abc">>},
        {<<"audio/mpeg">>, <<"ID3", 3, 0, 0, 0>>, <<"RIFF", 0, 0, 0, 0, "WEBP">>}
    ],
    [
        [
            {binary_to_list(Mime), fun() ->
                ?assertEqual(ok, eb_asset_content:validate_mime(Mime)),
                ?assertEqual(ok, eb_asset_content:sniff(Mime, Good))
            end},
            {binary_to_list(Mime) ++ "_mismatch", fun() ->
                ?assertEqual(ok, eb_asset_content:validate_mime(Mime)),
                ?assertMatch(
                    {error, {mime_content_mismatch, Mime}}, eb_asset_content:sniff(Mime, Bad)
                )
            end}
        ]
     || {Mime, Good, Bad} <- Cases
    ].

reject_octet_stream_test() ->
    ?assertMatch(
        {error, {invalid_mime, _}},
        eb_asset_content:validate_mime(<<"application/octet-stream">>)
    ).

reject_unknown_mime_test() ->
    ?assertMatch(
        {error, {invalid_mime, _}}, eb_asset_content:validate_mime(<<"video/x-matroska">>)
    ),
    ?assertMatch({error, {invalid_mime, _}}, eb_asset_content:validate_mime(<<">not-a-mime">>)),
    ?assertMatch({error, {invalid_mime, _}}, eb_asset_content:validate_mime(plain_string)).

sniff_empty_and_non_binary_test() ->
    ?assertMatch(
        {error, {mime_content_mismatch, _}}, eb_asset_content:sniff(<<"image/png">>, <<>>)
    ),
    ?assertMatch(
        {error, {mime_content_mismatch, _}},
        eb_asset_content:sniff(<<"image/png">>, not_binary)
    ).

svg_marker_must_appear_near_head_test() ->
    Tail = binary:copy(<<"x">>, 3000),
    LateSvg = <<Tail/binary, "<svg">>,
    ?assertMatch(
        {error, {mime_content_mismatch, _}},
        eb_asset_content:sniff(<<"image/svg+xml">>, LateSvg)
    ).

short_prefix_content_still_matches_test() ->
    %% 前缀规则只要求内容不短于前缀：恰好等长也要命中
    ?assertEqual(ok, eb_asset_content:sniff(<<"image/bmp">>, <<"BM">>)),
    ?assertEqual(ok, eb_asset_content:sniff(<<"application/gzip">>, <<16#1F, 16#8B>>)).
