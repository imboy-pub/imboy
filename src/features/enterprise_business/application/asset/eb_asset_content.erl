%%% @doc EB-07：企业附件的**内容校验与对外视图**（纯函数，零 I/O、零端口）。
%%%
%%% 依据：plan §EB-07「hash/mime/size 校验」，以及硬约束 3
%%% （content 响应**不得**含 storage URL / endpoint / object key；**不持久化 URL**）。
%%%
%%% ## 为什么单独成模块
%%%
%%% 「哪些值可以出对外返回体」必须是**可机械审阅的白名单**，而不是散落在用例里的
%%% `maps:remove/2`。本模块只导出两类东西：
%%%   * 校验：`validate_mime/1` / `validate_size/1` / `validate_hash/1` / `sniff/2`；
%%%   * 视图：`public_asset_view/1` / `public_content_view/2` —— **白名单投影**，
%%%     输入里任何存储侧字段（`object_key` 等）都不可能出现在输出里。
%%%
%%% ## 单位口径（踩过的坑，写死在这里）
%%%
%%% `enterprise_asset.retain_until` 在**写入参数**上是毫秒（基础设施侧用
%%% `to_timestamp($n/1000)` 落库），而**回读**是秒（归一化把 timestamptz 转成
%%% Unix 秒）。用例层对外统一用**秒**（与 `enterprise_message.retain_until` 的
%%% 回读一致），毫秒换算只在本模块与 `eb_asset_app` 的边界处发生。
%%%
%%% 注明：本层的注释刻意**不写**持久化实现模块名 —— 该层的边界声明是
%%% 「零 SQL、零实现模块名」，引用实现模块名会让「说明边界」与「越过边界」
%%% 在文本上无法区分（`scripts/check_eb_port_closure.sh` 的 A04 判定）。
-module(eb_asset_content).

-export([
    allowed_mimes/0,
    max_size_bytes/0,
    validate_mime/1,
    validate_size/1,
    validate_hash/1,
    validate_file_name/1,
    sniff/2,
    sha256_hex/1,
    to_retain_ms/1,
    from_retain_ms/1,
    public_asset_view/1,
    public_content_view/2,
    with_hold_count/2
]).

%% 企业附件 MIME 白名单：只放开可**按魔数复核**的类型。
%% 刻意不含 `application/octet-stream` —— 那会让 mime 校验变成恒真。
%% 扩表纪律：每个 MIME 必须在 `has_magic/2` 有对应可机械判定规则，
%% 两者同步演进（见各子句注释），且 `eb_asset_content_tests` 里必须有
%% 「真魔数正例 + 错配负例」两条腿。
%%
%% 2026-10-30 大幅扩表：座席/客服场景的真实诉求是「常见文件都能发」，
%% 原先 21 项的窄表把 WAV/MKV/AVI/XLS/PPT/RAR/TAR/JSON 等全部挡在
%% presign（422 invalid_mime），而客户端又给出「重试」入口 —— 同一份
%% 字节重试必然再败，用户看到的是死循环。这里按「家庭」成组放开，
%% 同族别名（`audio/x-wav` 与 `audio/wav` 这类浏览器/Dart 各自叫法）
%% 一并收下，否则同一份文件换个端就 422。
-define(ALLOWED_MIMES, [
    %% —— 静态图 ——
    <<"image/png">>,
    <<"image/jpeg">>,
    <<"image/gif">>,
    <<"image/webp">>,
    <<"image/bmp">>,
    <<"image/svg+xml">>,
    <<"image/tiff">>,
    <<"image/avif">>,
    <<"image/x-icon">>,
    <<"image/heic">>,
    <<"image/heif">>,
    <<"image/vnd.adobe.photoshop">>,
    <<"image/vnd.dwg">>,
    %% —— 视频 ——
    <<"video/mp4">>,
    <<"video/webm">>,
    <<"video/quicktime">>,
    <<"video/x-matroska">>,
    <<"video/x-msvideo">>,
    <<"video/mpeg">>,
    <<"video/x-ms-wmv">>,
    <<"video/x-flv">>,
    <<"video/3gpp">>,
    <<"video/x-m4v">>,
    %% —— 音频 ——
    <<"audio/mpeg">>,
    <<"audio/mp4">>,
    <<"audio/aac">>,
    <<"audio/wav">>,
    <<"audio/x-wav">>,
    <<"audio/ogg">>,
    <<"audio/flac">>,
    <<"audio/x-flac">>,
    %% —— 文档 ——
    <<"application/pdf">>,
    <<"text/plain">>,
    <<"text/markdown">>,
    <<"text/csv">>,
    <<"application/json">>,
    <<"application/xml">>,
    <<"text/xml">>,
    <<"application/msword">>,
    <<"application/vnd.ms-excel">>,
    <<"application/vnd.ms-powerpoint">>,
    <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>,
    <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>,
    <<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>,
    <<"application/vnd.oasis.opendocument.text">>,
    <<"application/vnd.oasis.opendocument.spreadsheet">>,
    <<"application/vnd.oasis.opendocument.presentation">>,
    <<"application/epub+zip">>,
    <<"application/rtf">>,
    <<"text/rtf">>,
    %% —— 压缩包 ——
    <<"application/zip">>,
    <<"application/x-zip-compressed">>,
    <<"application/x-7z-compressed">>,
    <<"application/gzip">>,
    <<"application/x-rar-compressed">>,
    <<"application/vnd.rar">>,
    <<"application/x-tar">>,
    <<"application/x-bzip2">>,
    <<"application/x-xz">>
]).

%% 刻意**不**收的常见类型及原因（写在这里，避免下次又被当成漏配）：
%%   * `text/html` —— 内容端点若按声明 mime 回显，等于把附件变成同源页面，
%%     与 `x-content-type-options: nosniff` 之外没有第二道闸；聊天附件里
%%     .html 也不是高频需求，故继续拒绝。
%%   * `application/vnd.android.package-archive`（.apk）与各类可执行体 ——
%%     通过 IM 派发安装包是明确的滥用面，不进白名单。
%%   * `application/octet-stream` —— 见上，会让整层校验恒真。

%% 企业附件单文件上限（25 MiB）。超过即拒，不进入对象存储。
-define(MAX_SIZE_BYTES, 25 * 1024 * 1024).

%% 对外可见的资产字段白名单（**不含** object_key / 任何 URL 语义字段）。
%% CS-BE-01：补 file_name（冻结契约 assets[].file_name 的数据源，迁移 146 起有列）。
-define(ASSET_VIEW_KEYS, [
    id,
    status,
    object_hash,
    mime,
    size_bytes,
    file_name,
    conversation_id,
    message_id,
    business_identity_id,
    retain_until,
    created_at,
    deleted_at,
    version
]).

-spec allowed_mimes() -> [binary()].
allowed_mimes() ->
    ?ALLOWED_MIMES.

-spec max_size_bytes() -> pos_integer().
max_size_bytes() ->
    ?MAX_SIZE_BYTES.

%% @doc MIME 校验：白名单内、且是规范化的小写 `type/subtype`。
-spec validate_mime(term()) -> ok | {error, {invalid_mime, term()}}.
validate_mime(Mime) when is_binary(Mime) ->
    case lists:member(Mime, ?ALLOWED_MIMES) of
        true -> ok;
        false -> {error, {invalid_mime, Mime}}
    end;
validate_mime(Mime) ->
    {error, {invalid_mime, Mime}}.

%% @doc 大小校验：正整数且不超过上限。
-spec validate_size(term()) -> ok | {error, {invalid_size_bytes, term()}}.
validate_size(Size) when is_integer(Size), Size > 0, Size =< ?MAX_SIZE_BYTES ->
    ok;
validate_size(Size) ->
    {error, {invalid_size_bytes, Size}}.

%% @doc 哈希校验：64 位小写 hex 的 SHA-256（上传方在 presign 时声明，PUT 后由服务端复核）。
-spec validate_hash(term()) -> ok | {error, {invalid_object_hash, term()}}.
validate_hash(Hash) when is_binary(Hash), byte_size(Hash) =:= 64 ->
    case re:run(Hash, "^[0-9a-f]{64}$", [{capture, none}]) of
        match -> ok;
        nomatch -> {error, {invalid_object_hash, Hash}}
    end;
validate_hash(Hash) ->
    {error, {invalid_object_hash, Hash}}.

%% @doc CS-BE-01：展示文件名校验（presign 可选声明）。
%% `undefined` 合法（未声明 = 列保持 NULL）；声明时须为 1..256 字节的二进制，
%% 且含basename 后非空（与 src/logic/enterprise_asset_logic.erl valid_name/1 同口径）。
%% 文件名是**展示值**：不参与 object_key 派生、不得含路径语义（basename 判定
%% 拒绝纯路径分隔符），存储引用仍由白名单投影排除。
-spec validate_file_name(term()) -> ok | {error, {invalid_file_name, term()}}.
validate_file_name(undefined) ->
    ok;
validate_file_name(Name) when is_binary(Name), byte_size(Name) > 0, byte_size(Name) =< 256 ->
    case filename:basename(Name) =/= <<>> of
        true -> ok;
        false -> {error, {invalid_file_name, Name}}
    end;
validate_file_name(Name) ->
    {error, {invalid_file_name, Name}}.

%% @doc 按声明的 MIME 复核内容魔数（与 ?ALLOWED_MIMES 一一对应，同步演进）。
-spec sniff(term(), term()) -> ok | {error, {mime_content_mismatch, term()}}.
sniff(Mime, Bytes) when is_binary(Mime), is_binary(Bytes), byte_size(Bytes) > 0 ->
    case has_magic(Mime, Bytes) of
        true -> ok;
        false -> {error, {mime_content_mismatch, Mime}}
    end;
sniff(Mime, _Bytes) ->
    {error, {mime_content_mismatch, Mime}}.

has_magic(<<"image/png">>, Bytes) ->
    prefix(<<137, 80, 78, 71, 13, 10, 26, 10>>, Bytes);
has_magic(<<"image/jpeg">>, Bytes) ->
    prefix(<<16#FF, 16#D8, 16#FF>>, Bytes);
has_magic(<<"image/gif">>, Bytes) ->
    prefix(<<"GIF87a">>, Bytes) orelse prefix(<<"GIF89a">>, Bytes);
has_magic(<<"image/webp">>, Bytes) ->
    %% RIFF 容器：前 4 字节 RIFF + 偏移 8..12 为 WEBP
    prefix(<<"RIFF">>, Bytes) andalso byte_size(Bytes) >= 12 andalso
        binary:part(Bytes, 8, 4) =:= <<"WEBP">>;
has_magic(<<"image/bmp">>, Bytes) ->
    prefix(<<"BM">>, Bytes);
has_magic(<<"image/svg+xml">>, Bytes) ->
    %% SVG 是文本：先过纯文本判定，再要求前 2KB 内出现 <svg 标记
    text_plain_ok(Bytes) andalso svg_marker(Bytes);
has_magic(<<"application/pdf">>, Bytes) ->
    prefix(<<"%PDF-">>, Bytes);
has_magic(<<"text/plain">>, Bytes) ->
    text_plain_ok(Bytes);
has_magic(<<"text/markdown">>, Bytes) ->
    text_plain_ok(Bytes);
has_magic(<<"text/csv">>, Bytes) ->
    text_plain_ok(Bytes);
has_magic(<<"application/zip">>, Bytes) ->
    zip_magic(Bytes);
has_magic(<<"application/x-7z-compressed">>, Bytes) ->
    prefix(<<55, 122, 188, 175, 39, 28>>, Bytes);
has_magic(<<"application/gzip">>, Bytes) ->
    prefix(<<16#1F, 16#8B>>, Bytes);
has_magic(<<"application/msword">>, Bytes) ->
    ole2(Bytes);
%% OOXML（.docx/.xlsx/.pptx）本质是 zip 容器：魔数只复核到 PK 前缀
%%（弱校验——容器内条目形状由消费方按声明 mime 解释；V1 接受此粒度）
has_magic(<<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>, Bytes) ->
    zip_magic(Bytes);
has_magic(<<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>, Bytes) ->
    zip_magic(Bytes);
has_magic(<<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>, Bytes) ->
    zip_magic(Bytes);
has_magic(<<"video/mp4">>, Bytes) ->
    %% ISO BMFF：偏移 4..8 为 ftyp
    byte_size(Bytes) >= 12 andalso binary:part(Bytes, 4, 4) =:= <<"ftyp">>;
has_magic(<<"video/webm">>, Bytes) ->
    prefix(<<16#1A, 16#45, 16#DF, 16#A3>>, Bytes);
has_magic(<<"audio/mpeg">>, Bytes) ->
    prefix(<<"ID3">>, Bytes) orelse mpeg_frame_sync(Bytes);
%% HEIC / HEIF：ISO BMFF 容器，`ftyp` 后 4 字节为主品牌。品牌表取
%% ISO/IEC 23008-12（heic/heix/hevc/hevx/heim/heis）与 MIAF 通用 HEIF
%% （mif1/msf1）。刻意**不**复用 video/mp4 的「任意 ftyp」规则——那会让
%% 任何容器都被认成静态图，校验退化为恒真。
has_magic(<<"image/heic">>, Bytes) ->
    heif_brand(Bytes);
has_magic(<<"image/heif">>, Bytes) ->
    heif_brand(Bytes);
%% QuickTime（.mov）有两种形态：
%%   ① 新版带 `ftyp`，主品牌为 `qt  `（含尾随两空格）；
%%   ② 旧版无 `ftyp`，文件首原子直接是 moov/mdat/wide/free/skip，
%%      此时偏移 4..8 即原子类型。
%% 与 OOXML 的 PK 前缀同级——弱校验，容器内条目形状由消费方按声明 mime 解释。
has_magic(<<"video/quicktime">>, Bytes) ->
    iso_bmff_brand(Bytes, [<<"qt  ">>]) orelse qt_legacy_atom(Bytes);
%% ————————————————————— 2026-10-30 扩表：静态图 —————————————————————
has_magic(<<"image/tiff">>, Bytes) ->
    %% 小端 `II*\0` / 大端 `MM\0*`
    prefix(<<16#49, 16#49, 16#2A, 16#00>>, Bytes) orelse
        prefix(<<16#4D, 16#4D, 16#00, 16#2A>>, Bytes);
has_magic(<<"image/avif">>, Bytes) ->
    iso_bmff_brand(Bytes, [<<"avif">>, <<"avis">>]);
has_magic(<<"image/x-icon">>, Bytes) ->
    prefix(<<0, 0, 1, 0>>, Bytes);
has_magic(<<"image/vnd.adobe.photoshop">>, Bytes) ->
    prefix(<<"8BPS">>, Bytes);
has_magic(<<"image/vnd.dwg">>, Bytes) ->
    %% AutoCAD R13 起每个版本以 "AC10xx" 开头；与 PK 同级弱校验
    prefix(<<"AC10">>, Bytes);
%% ————————————————————— 视频 —————————————————————
has_magic(<<"video/x-matroska">>, Bytes) ->
    %% .mkv 与 .webm 同为 EBML 容器，魔数层不可区分：两者都放行，
    %% 差异（VP8/VP9/H.264 等）由消费方按声明 mime 解释。
    prefix(<<16#1A, 16#45, 16#DF, 16#A3>>, Bytes);
has_magic(<<"video/x-msvideo">>, Bytes) ->
    riff_type(Bytes, <<"AVI ">>);
has_magic(<<"video/mpeg">>, Bytes) ->
    %% MPEG-PS pack start code；0xB3 为 sequence header 起始
    prefix(<<0, 0, 1, 16#BA>>, Bytes) orelse prefix(<<0, 0, 1, 16#B3>>, Bytes);
has_magic(<<"video/x-ms-wmv">>, Bytes) ->
    %% ASF 容器头 GUID
    prefix(
        <<16#30, 16#26, 16#B2, 16#75, 16#8E, 16#66, 16#CF, 16#11, 16#A6, 16#D9, 16#00, 16#AA, 16#00,
            16#62, 16#CE, 16#6C>>,
        Bytes
    );
has_magic(<<"video/x-flv">>, Bytes) ->
    prefix(<<16#46, 16#4C, 16#56, 1>>, Bytes);
has_magic(<<"video/3gpp">>, Bytes) ->
    %% 3gp/3g2 家族主品牌一律以 "3g" 起头（3gp4/3gp5/3g2a…）
    iso_bmff_brand_prefix(Bytes, <<"3g">>);
has_magic(<<"video/x-m4v">>, Bytes) ->
    iso_bmff_brand_prefix(Bytes, <<"M4V">>);
%% ————————————————————— 音频 —————————————————————
has_magic(<<"audio/mp4">>, Bytes) ->
    %% .m4a/.m4b/.m4p 都是 ISO BMFF；部分编码器主品牌写 mp42/isom
    iso_bmff_brand(Bytes, [
        <<"M4A ">>, <<"M4B ">>, <<"M4P ">>, <<"mp42">>, <<"isom">>, <<"iso2">>
    ]);
has_magic(<<"audio/aac">>, Bytes) ->
    %% ADTS 帧同步：FF F1（MPEG-4）/ FF F9（MPEG-2）
    prefix(<<16#FF, 16#F1>>, Bytes) orelse prefix(<<16#FF, 16#F9>>, Bytes);
has_magic(<<"audio/wav">>, Bytes) ->
    riff_type(Bytes, <<"WAVE">>);
has_magic(<<"audio/x-wav">>, Bytes) ->
    riff_type(Bytes, <<"WAVE">>);
has_magic(<<"audio/ogg">>, Bytes) ->
    prefix(<<"OggS">>, Bytes);
has_magic(<<"audio/flac">>, Bytes) ->
    prefix(<<"fLaC">>, Bytes);
has_magic(<<"audio/x-flac">>, Bytes) ->
    prefix(<<"fLaC">>, Bytes);
%% ————————————————————— 文档 —————————————————————
has_magic(<<"application/json">>, Bytes) ->
    text_plain_ok(Bytes) andalso json_head(Bytes);
has_magic(<<"application/xml">>, Bytes) ->
    text_plain_ok(Bytes) andalso xml_head(Bytes);
has_magic(<<"text/xml">>, Bytes) ->
    text_plain_ok(Bytes) andalso xml_head(Bytes);
has_magic(<<"application/vnd.ms-excel">>, Bytes) ->
    ole2(Bytes);
has_magic(<<"application/vnd.ms-powerpoint">>, Bytes) ->
    ole2(Bytes);
has_magic(<<"application/vnd.oasis.opendocument.text">>, Bytes) ->
    zip_magic(Bytes);
has_magic(<<"application/vnd.oasis.opendocument.spreadsheet">>, Bytes) ->
    zip_magic(Bytes);
has_magic(<<"application/vnd.oasis.opendocument.presentation">>, Bytes) ->
    zip_magic(Bytes);
has_magic(<<"application/epub+zip">>, Bytes) ->
    zip_magic(Bytes);
has_magic(<<"application/rtf">>, Bytes) ->
    prefix(<<"{\\rtf">>, Bytes);
has_magic(<<"text/rtf">>, Bytes) ->
    prefix(<<"{\\rtf">>, Bytes);
%% ————————————————————— 压缩包 —————————————————————
has_magic(<<"application/x-zip-compressed">>, Bytes) ->
    zip_magic(Bytes);
has_magic(<<"application/x-rar-compressed">>, Bytes) ->
    rar_magic(Bytes);
has_magic(<<"application/vnd.rar">>, Bytes) ->
    rar_magic(Bytes);
has_magic(<<"application/x-tar">>, Bytes) ->
    %% POSIX tar：偏移 257 起为 "ustar"（前 257 字节是文件名/权限等头字段）
    byte_size(Bytes) >= 262 andalso binary:part(Bytes, 257, 5) =:= <<"ustar">>;
has_magic(<<"application/x-bzip2">>, Bytes) ->
    %% "BZh" + 块大小等级（'1'..'9'），只复核到 3 字节前缀即够
    prefix(<<"BZh">>, Bytes);
has_magic(<<"application/x-xz">>, Bytes) ->
    prefix(<<16#FD, 16#37, 16#7A, 16#58, 16#5A, 16#00>>, Bytes);
has_magic(_Other, _Bytes) ->
    false.

%% OLE2 复合文档（.doc/.xls/.ppt/.msg 老格式共用同一容器头）
ole2(Bytes) ->
    prefix(<<16#D0, 16#CF, 16#11, 16#E0, 16#A1, 16#B1, 16#1A, 16#E1>>, Bytes).

%% zip 容器（.zip/.docx/.xlsx/.pptx/.odt/.ods/.odp/.epub）：只复核 PK 前缀
zip_magic(Bytes) ->
    prefix(<<80, 75, 3, 4>>, Bytes).

%% RAR：4.x 为 `Rar!\x1A\x07\x00`，5.x 为 `Rar!\x1A\x07\x01\x00`，前缀到 6 字节即可
rar_magic(Bytes) ->
    prefix(<<"Rar!", 16#1A, 16#07>>, Bytes).

%% RIFF 容器（.webp/.wav/.avi/.cda…）：前 4 字节 RIFF + 偏移 8..12 为具体类型
riff_type(Bytes, Type) ->
    byte_size(Bytes) >= 12 andalso
        binary:part(Bytes, 0, 4) =:= <<"RIFF">> andalso
        binary:part(Bytes, 8, 4) =:= Type.

%% HEIF 家族主品牌（图片侧）。image/heic 与 image/heif 共用同一品牌集：
%% 二者在容器层不可靠区分（同一颗照片可能主品牌 `heic`、兼容品牌 `mif1`），
%% 强行细分只会造成「同一条字节换扩展名就 422」的假拒。
heif_brand(Bytes) ->
    iso_bmff_brand(Bytes, [
        <<"heic">>,
        <<"heix">>,
        <<"hevc">>,
        <<"hevx">>,
        <<"heim">>,
        <<"heis">>,
        <<"mif1">>,
        <<"msf1">>
    ]).

%% ISO BMFF：偏移 4..8 为 `ftyp`，偏移 8..12 为主品牌且须在给定集合内。
iso_bmff_brand(Bytes, Brands) ->
    byte_size(Bytes) >= 12 andalso
        binary:part(Bytes, 4, 4) =:= <<"ftyp">> andalso
        lists:member(binary:part(Bytes, 8, 4), Brands).

%% 无 ftyp 的旧版 QuickTime：首原子类型落在已知顶层原子集合内。
qt_legacy_atom(Bytes) ->
    byte_size(Bytes) >= 8 andalso
        lists:member(
            binary:part(Bytes, 4, 4),
            [<<"moov">>, <<"mdat">>, <<"wide">>, <<"free">>, <<"skip">>, <<"pnot">>]
        ).

%% ISO BMFF 的「品牌前缀」变体：3GPP 与 M4V 家族的主品牌有多个版本号
%% （3gp4/3gp5/3gp6、M4V /M4VH/M4VP），按前缀比对避免逐个登记。
iso_bmff_brand_prefix(Bytes, Prefix) ->
    byte_size(Bytes) >= 12 andalso
        binary:part(Bytes, 4, 4) =:= <<"ftyp">> andalso
        prefix(Prefix, binary:part(Bytes, 8, byte_size(Prefix))).

%% JSON：纯文本且首个非空白字节是对象/数组/字符串起始。
%% 只判「非空文本」会让任意 .txt 都能冒充 .json，收敛一个字节即可区分。
json_head(Bytes) ->
    %% 34 = `"`（Erlang 里 $"; 这里写数字以免与列表分隔符混淆）
    lists:member(first_significant(Bytes), [${, $[, 34]).

%% XML：纯文本且（前 2KB 里有 `<?xml` 声明 或 首个非空白字节是 `<`）。
%% 允许 UTF-8 BOM：命中声明分支时 BOM 不影响判定。
xml_head(Bytes) ->
    Head = head_bytes(Bytes, 2048),
    case binary:match(Head, <<"<?xml">>) of
        nomatch -> first_significant(Head) =:= $<;
        _ -> true
    end.

%% 跳过 ASCII 空白后的首个字节；全空白返回 `none`。
first_significant(<<C, Rest/binary>>) when C =:= 32; C =:= 9; C =:= 10; C =:= 13 ->
    first_significant(Rest);
first_significant(<<C, _/binary>>) ->
    C;
first_significant(<<>>) ->
    none.

head_bytes(Bytes, Limit) when byte_size(Bytes) > Limit ->
    binary:part(Bytes, 0, Limit);
head_bytes(Bytes, _Limit) ->
    Bytes.

%% text/plain：不得含 NUL 或非法 UTF-8 控制字节（避免把二进制伪装成文本）
text_plain_ok(Bytes) ->
    binary:match(Bytes, <<0>>) =:= nomatch andalso no_control_noise(Bytes).

svg_marker(Bytes) ->
    Head =
        case byte_size(Bytes) > 2048 of
            true -> binary:part(Bytes, 0, 2048);
            false -> Bytes
        end,
    binary:match(Head, <<"<svg">>) =/= nomatch.

%% MP3 裸帧同步字：FF Ex/F F2/F3/FB/FA（11 位帧同步 + 层/保护位常见组合）
mpeg_frame_sync(<<16#FF, X, _/binary>>) when
    X =:= 16#FB orelse X =:= 16#FA orelse X =:= 16#F3 orelse X =:= 16#F2 orelse X =:= 16#E3
->
    true;
mpeg_frame_sync(_Bytes) ->
    false.

%% 同一字节规则，直接遍历 binary，避免最大文本体膨胀为逐字节 cons 列表。
%% Preserve the byte predicate without expanding large bodies into cons lists.
no_control_noise(<<>>) ->
    true;
no_control_noise(<<B, Rest/binary>>) when B >= 32; B =:= 9; B =:= 10; B =:= 13 ->
    no_control_noise(Rest);
no_control_noise(_) ->
    false.

prefix(Prefix, Bytes) when byte_size(Bytes) >= byte_size(Prefix) ->
    binary:part(Bytes, 0, byte_size(Prefix)) =:= Prefix;
prefix(_Prefix, _Bytes) ->
    false.

%% @doc 内容哈希（64 位小写 hex）。对象完整性校验的唯一判据。
-spec sha256_hex(binary()) -> binary().
sha256_hex(Bytes) when is_binary(Bytes) ->
    binary:encode_hex(crypto:hash(sha256, Bytes), lowercase).

%% @doc 秒 → 毫秒（元数据写入参数一侧）。`undefined` 原样透传。
-spec to_retain_ms(term()) -> term().
to_retain_ms(Seconds) when is_integer(Seconds) -> Seconds * 1000;
to_retain_ms(Other) -> Other.

%% @doc 毫秒 → 秒（若调用方拿到的是毫秒形态）。
-spec from_retain_ms(term()) -> term().
from_retain_ms(Ms) when is_integer(Ms) -> Ms div 1000;
from_retain_ms(Other) -> Other.

%% @doc 资产元数据 → 对外视图（白名单投影；`object_key` 一类的存储侧字段不可能出现）。
-spec public_asset_view(map()) -> map().
public_asset_view(Row) when is_map(Row) ->
    Base = maps:with(?ASSET_VIEW_KEYS, Row),
    rename_id(Base).

rename_id(Map) ->
    case maps:get(id, Map, undefined) of
        undefined -> maps:without([id], Map);
        Id -> maps:put(asset_id, Id, maps:without([id], Map))
    end.

%% @doc content 视图：只含标识 + 完整性摘要 + 字节体，**绝不含** key / URL / endpoint。
-spec public_content_view(binary(), map()) -> map().
public_content_view(Bytes, Row) when is_binary(Bytes), is_map(Row) ->
    View = public_asset_view(Row),
    #{
        asset_id => maps:get(asset_id, View, undefined),
        object_hash => maps:get(object_hash, View, undefined),
        mime => maps:get(mime, View, undefined),
        size_bytes => maps:get(size_bytes, View, undefined),
        file_name => maps:get(file_name, View, undefined),
        conversation_id => maps:get(conversation_id, View, undefined),
        message_id => maps:get(message_id, View, undefined),
        status => maps:get(status, View, undefined),
        body => Bytes
    }.

%% @doc 附上「覆盖该附件的 active hold 条数」（A06 的 hold 继承可观测面）。
-spec with_hold_count(map(), non_neg_integer()) -> map().
with_hold_count(View, Count) when is_map(View), is_integer(Count) ->
    maps:put(hold_count, Count, View).
