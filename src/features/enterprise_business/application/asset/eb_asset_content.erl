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

%% V1 企业附件 MIME 白名单：只放开可**按魔数复核**的少数类型。
%% 刻意不含 `application/octet-stream` —— 那会让 mime 校验变成恒真。
%% 扩表纪律：每个 MIME 必须在 `has_magic/2` 有对应可机械判定规则，
%% 两者同步演进（见各子句注释）。
-define(ALLOWED_MIMES, [
    <<"image/png">>,
    <<"image/jpeg">>,
    <<"image/gif">>,
    <<"image/webp">>,
    <<"image/bmp">>,
    <<"image/svg+xml">>,
    <<"application/pdf">>,
    <<"text/plain">>,
    <<"text/markdown">>,
    <<"text/csv">>,
    <<"application/zip">>,
    <<"application/x-7z-compressed">>,
    <<"application/gzip">>,
    <<"application/msword">>,
    <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>,
    <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>,
    <<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>,
    <<"video/mp4">>,
    <<"video/webm">>,
    <<"audio/mpeg">>
]).

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
    prefix(<<80, 75, 3, 4>>, Bytes);
has_magic(<<"application/x-7z-compressed">>, Bytes) ->
    prefix(<<55, 122, 188, 175, 39, 28>>, Bytes);
has_magic(<<"application/gzip">>, Bytes) ->
    prefix(<<16#1F, 16#8B>>, Bytes);
has_magic(<<"application/msword">>, Bytes) ->
    %% OLE2 复合文档（.doc/.xls/.ppt 老格式）
    prefix(<<16#D0, 16#CF, 16#11, 16#E0, 16#A1, 16#B1, 16#1A, 16#E1>>, Bytes);
%% OOXML（.docx/.xlsx/.pptx）本质是 zip 容器：魔数只复核到 PK 前缀
%%（弱校验——容器内条目形状由消费方按声明 mime 解释；V1 接受此粒度）
has_magic(<<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>, Bytes) ->
    prefix(<<80, 75, 3, 4>>, Bytes);
has_magic(<<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>, Bytes) ->
    prefix(<<80, 75, 3, 4>>, Bytes);
has_magic(<<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>, Bytes) ->
    prefix(<<80, 75, 3, 4>>, Bytes);
has_magic(<<"video/mp4">>, Bytes) ->
    %% ISO BMFF：偏移 4..8 为 ftyp
    byte_size(Bytes) >= 12 andalso binary:part(Bytes, 4, 4) =:= <<"ftyp">>;
has_magic(<<"video/webm">>, Bytes) ->
    prefix(<<16#1A, 16#45, 16#DF, 16#A3>>, Bytes);
has_magic(<<"audio/mpeg">>, Bytes) ->
    prefix(<<"ID3">>, Bytes) orelse mpeg_frame_sync(Bytes);
has_magic(_Other, _Bytes) ->
    false.

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

no_control_noise(Bytes) ->
    %% 只允许 TAB/LF/CR 三个控制字符
    Allowed = [9, 10, 13],
    lists:all(
        fun(B) -> B >= 32 orelse lists:member(B, Allowed) end,
        binary_to_list(Bytes)
    ).

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
