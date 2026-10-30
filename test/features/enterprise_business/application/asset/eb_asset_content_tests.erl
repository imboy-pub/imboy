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
        %% 2026-10-30 扩表：iOS 直出 HEIC / 录屏 MOV。负例一律取「同为
        %% ISO BMFF、但主品牌不属于该家族」——只判 `ftyp` 存在会放行任意容器。
        {<<"image/heic">>, <<"    ftypheic", 0, 0>>, <<"    ftypisom", 0, 0>>},
        {<<"image/heif">>, <<"    ftypmif1", 0, 0>>, <<"    ftypavif", 0, 0>>},
        {<<"video/quicktime">>, <<"    ftypqt  ", 0, 0>>, <<"    ftypheic", 0, 0>>},
        {<<"audio/mpeg">>, <<"ID3", 3, 0, 0, 0>>, <<"RIFF", 0, 0, 0, 0, "WEBP">>},
        %% —— 2026-10-30 二次扩表：常见图片/视频/音频/文档/压缩包 ——
        %% 负例一律取「同族或同容器但不属于该 mime」的内容，避免用无关
        %% 字节把「规则写宽了」也判成通过。
        {<<"image/tiff">>, <<16#49, 16#49, 16#2A, 0, 8, 0, 0, 0>>, <<"GIF89a">>},
        {<<"image/avif">>, <<"    ftypavif", 0, 0>>, <<"    ftypheic", 0, 0>>},
        {<<"image/x-icon">>, <<0, 0, 1, 0, 1, 0>>, <<0, 0, 2, 0>>},
        {<<"image/vnd.adobe.photoshop">>, <<"8BPS", 0, 1>>, <<"8BPX">>},
        {<<"image/vnd.dwg">>, <<"AC1015", 0>>, <<"ACME01">>},
        {<<"video/x-matroska">>, <<16#1A, 16#45, 16#DF, 16#A3, 1>>, <<"RIFF", 0, 0, 0, 0, "AVI ">>},
        {<<"video/x-msvideo">>, <<"RIFF", 0, 0, 0, 0, "AVI ", "LIST">>,
            <<"RIFF", 0, 0, 0, 0, "WAVE">>},
        {<<"video/mpeg">>, <<0, 0, 1, 16#BA, 16#44>>, <<0, 0, 1, 16#BB>>},
        {<<"video/x-ms-wmv">>,
            <<16#30, 16#26, 16#B2, 16#75, 16#8E, 16#66, 16#CF, 16#11, 16#A6, 16#D9, 0, 16#AA, 0,
                16#62, 16#CE, 16#6C>>,
            <<"FLV", 1>>},
        {<<"video/x-flv">>, <<"FLV", 1, 5>>, <<"FLV", 9>>},
        {<<"video/3gpp">>, <<"    ftyp3gp5", 0>>, <<"    ftypisom", 0>>},
        {<<"video/x-m4v">>, <<"    ftypM4V ", 0>>, <<"    ftyp3gp5", 0>>},
        {<<"audio/mp4">>, <<"    ftypM4A ", 0>>, <<"    ftypavis", 0>>},
        {<<"audio/aac">>, <<16#FF, 16#F1, 16#50>>, <<16#FF, 16#F2, 16#50>>},
        {<<"audio/wav">>, <<"RIFF", 0, 0, 0, 0, "WAVE">>, <<"RIFF", 0, 0, 0, 0, "AVI ">>},
        {<<"audio/x-wav">>, <<"RIFF", 0, 0, 0, 0, "WAVE">>, <<"RIFF", 0, 0, 0, 0, "WEBP">>},
        {<<"audio/ogg">>, <<"OggS", 0>>, <<"OggX">>},
        {<<"audio/flac">>, <<"fLaC", 0>>, <<"FLAC">>},
        {<<"audio/x-flac">>, <<"fLaC", 0>>, <<"fLaX">>},
        {<<"application/json">>, <<"{\"a\":1}">>, <<"plain text">>},
        {<<"application/xml">>, <<"<?xml version=\"1.0\"?><a/>">>, <<"plain text">>},
        {<<"text/xml">>, <<"<a/>">>, <<"plain text">>},
        {<<"application/vnd.ms-excel">>, <<16#D0, 16#CF, 16#11, 16#E0, 16#A1, 16#B1, 16#1A, 16#E1>>,
            <<80, 75, 3, 4>>},
        {<<"application/vnd.ms-powerpoint">>,
            <<16#D0, 16#CF, 16#11, 16#E0, 16#A1, 16#B1, 16#1A, 16#E1>>, <<"%PDF-1.4">>},
        {<<"application/vnd.oasis.opendocument.text">>, <<80, 75, 3, 4, "odt">>, <<"BM">>},
        {<<"application/vnd.oasis.opendocument.spreadsheet">>, <<80, 75, 3, 4, "ods">>,
            <<"BM", 1>>},
        {<<"application/vnd.oasis.opendocument.presentation">>, <<80, 75, 3, 4, "odp">>,
            <<"Rar!">>},
        {<<"application/epub+zip">>, <<80, 75, 3, 4, "epub">>, <<"8BPS">>},
        {<<"application/rtf">>, <<"{\\rtf1\\ansi">>, <<"{\\notrtf">>},
        {<<"text/rtf">>, <<"{\\rtf1">>, <<"{\\x">>},
        {<<"application/x-zip-compressed">>, <<80, 75, 3, 4, "zip">>, <<"BM">>},
        {<<"application/x-rar-compressed">>, <<"Rar!", 16#1A, 16#07, 1, 0>>, <<"RarX">>},
        {<<"application/vnd.rar">>, <<"Rar!", 16#1A, 16#07, 0>>, <<80, 75, 3, 4>>},
        {<<"application/x-tar">>, <<0:(257 * 8), "ustar", 0>>, <<"short">>},
        {<<"application/x-bzip2">>, <<"BZh9">>, <<"BZx9">>},
        {<<"application/x-xz">>, <<16#FD, 16#37, 16#7A, 16#58, 16#5A, 0, 0>>,
            <<16#FD, 16#37, 16#7A, 16#58, 16#5B>>}
    ],
    %% 扩表纪律的可执行断面：白名单里每个 MIME 都必须在上面有正负例。
    %% 只改 `?ALLOWED_MIMES` 不补规则时，这条断言让套件立刻变红。
    ?assertEqual(
        lists:sort(eb_asset_content:allowed_mimes()),
        lists:sort([M || {M, _, _} <- Cases])
    ),
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
        {error, {invalid_mime, _}},
        eb_asset_content:validate_mime(<<"application/x-msdownload">>)
    ),
    %% 明确拒绝的常见类型：HTML 附件可被当作同源页面回显，APK 是可执行体派发面
    ?assertMatch(
        {error, {invalid_mime, _}}, eb_asset_content:validate_mime(<<"text/html">>)
    ),
    ?assertMatch(
        {error, {invalid_mime, _}},
        eb_asset_content:validate_mime(<<"application/vnd.android.package-archive">>)
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

iso_bmff_families_do_not_leak_test() ->
    %% 只判「偏移 4..8 是 ftyp」会把所有 ISO BMFF 容器放成同一类：
    %% HEIC 能以 image/png 名义上送，MOV 能以 HEIC 名义上送。品牌必须归属。
    Heic = <<"    ftypheic", 0, 0>>,
    ?assertMatch(
        {error, {mime_content_mismatch, _}},
        eb_asset_content:sniff(<<"image/png">>, Heic)
    ),
    %% 反方向：HEIC 品牌不得混进 HEIF 之外的静态图家族（image/webp 走 RIFF）
    ?assertMatch(
        {error, {mime_content_mismatch, _}},
        eb_asset_content:sniff(<<"image/webp">>, Heic)
    ),
    %% 截断到只有 ftyp、没有品牌时不得放行
    ?assertMatch(
        {error, {mime_content_mismatch, _}},
        eb_asset_content:sniff(<<"image/heic">>, <<"    ftyp">>)
    ).

quicktime_legacy_atom_test() ->
    %% 旧版 .mov 没有 ftyp，首原子直接是 moov/mdat/wide/free/skip
    ?assertEqual(ok, eb_asset_content:sniff(<<"video/quicktime">>, <<"    mdat", 0, 0, 0>>)),
    ?assertEqual(ok, eb_asset_content:sniff(<<"video/quicktime">>, <<"    moov", 0>>)),
    %% 不足一个原子的长度不得命中（原子类型需完整 4 字节在偏移 4..8）
    ?assertMatch(
        {error, {mime_content_mismatch, _}},
        eb_asset_content:sniff(<<"video/quicktime">>, <<"    mda">>)
    ).

text_byte_predicate_equivalence_test() ->
    %% Keep the historical byte whitelist exactly, without allocating a list
    %% proportional to the complete upload size.
    lists:foreach(
        fun(B) ->
            Expected =
                case B >= 32 orelse B =:= 9 orelse B =:= 10 orelse B =:= 13 of
                    true -> ok;
                    false -> {error, {mime_content_mismatch, <<"text/plain">>}}
                end,
            ?assertEqual(Expected, eb_asset_content:sniff(<<"text/plain">>, <<B>>))
        end,
        lists:seq(0, 255)
    ).

text_late_control_byte_test() ->
    Prefix = binary:copy(<<"a">>, 65536 * 3),
    ?assertEqual(ok, eb_asset_content:sniff(<<"text/plain">>, Prefix)),
    ?assertMatch(
        {error, {mime_content_mismatch, _}},
        eb_asset_content:sniff(<<"text/plain">>, <<Prefix/binary, 1>>)
    ).
