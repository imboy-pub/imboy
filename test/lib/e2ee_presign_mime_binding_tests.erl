%%% E2EE-061 Slice 1：**presigned PUT 的 MIME 绑定形态**。
%%%
%%% == 为什么需要它 ==
%%%
%%% `27-e2ee-061-attachment-encryption-design.md` §3.2 写着：
%%%   「presigned URL 的签名**通常覆盖** Content-Type：只改 PUT 不改 presign 会导致
%%%    签名失配、直传直接失败」，并把它标为 **未实证**、要求先在本地 Garage 验证，
%%%    「不得凭 S3 通例推断」。
%%%
%%% 本地 Garage 未运行（3900 无监听），但该问题的**权威来源其实是我方的签名实现**
%%% ——签名覆盖哪些内容由 `elib_s3_sign:presign_url/6` 决定，不由 Garage 决定。
%%% 本文件把该函数的实际行为钉死。
%%%
%%% == 守护的性质 ==
%%%
%%% 1. 【对照组】签名确实随输入变化（不同 object_key → 不同签名）。
%%%    这条红 = 探针没在测签名，后面的结论都不成立；
%%% 2. `X-Amz-SignedHeaders` **只有 host** —— PUT 请求的 Content-Type **请求头**
%%%    不在签名覆盖范围内；
%%% 3. 但 MIME 以 **query 参数** `Content-Type=` 进入 canonical query string，
%%%    因而**被绑进签名**：换 MIME 必然换 URL，改不了「只改客户端请求头」就完事；
%%% 4. 【正向可用性】MIME 为空时不得凭空产出 `Content-Type=` 参数
%%%    （presign_get / presign_delete 走的就是这条路，不得因本查证被破坏）。
%%%
%%% **本文件不改任何生产代码**，只读 `elib_s3_sign:presign_put/5` 的产物。
-module(e2ee_presign_mime_binding_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ENDPOINT, <<"http://127.0.0.1:3900">>).
-define(BUCKET, <<"imboy-private">>).
-define(KEY, <<"u1001/file_1_abc/photo.jpg">>).
-define(EXPIRES, 600).

sig_of(Url) ->
    [_, Sig] = binary:split(Url, <<"&X-Amz-Signature=">>),
    Sig.

%% ===================================================================
%% 1. 对照组：签名确实随输入变化
%% ===================================================================

%% 这条红说明探针根本没测到签名（例如 URL 形状变了、split 取错段），
%% 此时后面「MIME 是否影响签名」的任何结论都不成立，必须停下重估。
signature_varies_with_object_key_test() ->
    A = elib_s3_sign:presign_put(?ENDPOINT, ?BUCKET, <<"u1/a.bin">>, <<"image/jpeg">>, ?EXPIRES),
    B = elib_s3_sign:presign_put(?ENDPOINT, ?BUCKET, <<"u1/b.bin">>, <<"image/jpeg">>, ?EXPIRES),
    ?assertNotEqual(
        sig_of(A),
        sig_of(B),
        "不同 object_key 必须产出不同签名；相同说明探针没取到真签名"
    ).

%% ===================================================================
%% 2. SignedHeaders 只有 host —— 请求头 Content-Type 不被签名覆盖
%% ===================================================================

signed_headers_is_host_only_test() ->
    Url = elib_s3_sign:presign_put(?ENDPOINT, ?BUCKET, ?KEY, <<"image/jpeg">>, ?EXPIRES),
    ?assertNotEqual(
        nomatch,
        binary:match(Url, <<"X-Amz-SignedHeaders=host">>),
        "presign 只签 host"
    ),
    %% 不得出现 content-type 被列入 SignedHeaders 的形态
    ?assertEqual(
        nomatch,
        binary:match(Url, <<"SignedHeaders=content-type">>)
    ),
    ?assertEqual(
        nomatch,
        binary:match(Url, <<"X-Amz-SignedHeaders=host%3Bcontent-type">>)
    ).

%% ===================================================================
%% 3. MIME 以 query 参数进签名 —— 换 MIME 必然换 URL
%% ===================================================================

mime_appears_as_query_param_test() ->
    Url = elib_s3_sign:presign_put(?ENDPOINT, ?BUCKET, ?KEY, <<"image/jpeg">>, ?EXPIRES),
    ?assertNotEqual(
        nomatch,
        binary:match(Url, <<"Content-Type=image">>),
        "MIME 以 query 参数出现在 presigned URL 里——URL 本身就泄漏了文件类型"
    ).

mime_is_bound_into_signature_test() ->
    Jpeg = elib_s3_sign:presign_put(?ENDPOINT, ?BUCKET, ?KEY, <<"image/jpeg">>, ?EXPIRES),
    Octet = elib_s3_sign:presign_put(
        ?ENDPOINT, ?BUCKET, ?KEY, <<"application/octet-stream">>, ?EXPIRES
    ),
    ?assertNotEqual(
        sig_of(Jpeg),
        sig_of(Octet),
        "MIME 进 canonical query string → 被绑进签名。"
        "因此改 MIME **必须重新 presign**，光改客户端 PUT 请求头没有意义"
    ).

%% ===================================================================
%% 4. 正向可用性：MIME 为空时不得凭空产出 Content-Type 参数
%% ===================================================================

%% presign_get / presign_delete 走的就是空 MIME 这条路。
empty_mime_omits_content_type_param_test() ->
    Url = elib_s3_sign:presign_get(?ENDPOINT, ?BUCKET, ?KEY, ?EXPIRES),
    ?assertEqual(
        nomatch,
        binary:match(Url, <<"Content-Type=">>),
        "空 MIME 不得凭空产出 Content-Type 参数，否则 GET/DELETE 签名形态被改坏"
    ),
    ?assertNotEqual(nomatch, binary:match(Url, <<"X-Amz-Signature=">>)).
