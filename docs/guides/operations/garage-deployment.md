# Garage 对象存储部署指南 / Garage Object Storage Deployment Guide

> **版本 / Version**: Garage v2.3.0 (2026-04-16)
> **适用场景 / Scope**: 单节点二进制部署，替代 go-fastdfs，供 Erlang 后端和 Flutter 客户端使用
> **最后更新 / Last updated**: 2026-05-28

---

## 目录 / Table of Contents

1. [架构概览](#架构概览)
2. [安装](#安装)
3. [配置](#配置)
4. [systemd 服务](#systemd-服务)
5. [初始化](#初始化)
6. [Erlang 集成](#erlang-集成)
7. [Flutter 集成](#flutter-集成)
8. [验证与排障](#验证与排障)

---

## 架构概览

```
Flutter App
  ├── GET /api/v1/attachment/presign  →  Erlang 后端生成 presigned PUT URL
  ├── PUT <presigned_url>         →  直传 Garage（不经 Erlang）
  └── GET <public_url>            →  Garage 直读（bucket 公开读，无需签名）

Erlang 后端
  ├── 生成 presigned PUT URL       →  elib_s3_sign（AWS Sig V4，零额外依赖）
  ├── 服务端上传（群文件/群相册）    →  httpc PUT → Garage
  └── 孤儿附件物理删除              →  httpc DELETE → Garage

Garage
  ├── S3 API  :3900  （上传 / 下载 / 签名）
  ├── RPC     :3901  （集群内部通信）
  └── Admin   :3903  （管理 API）
```

**URL 格式（path-style，Garage 默认）**

```
http://<host>:3900/<bucket>/<object-key>

示例：http://127.0.0.1:3900/imboy/file_1748000000_123456/photo.jpg
```

---

## 部署方式选择 / Deployment Options

本项目提供两种 Garage 部署方式，按场景择一 / Two methods are provided; pick one:

| 方式 / Method | 脚本 / Script | 适用 / Use case |
|---|---|---|
| 二进制 + systemd | `script/garage-install.sh` | **生产推荐**；自动识别 macOS(开发)/Linux(生产)，二进制安装，无 docker 依赖 |
| Docker | `script/garage-local-setup.sh` | 快速本地试用；依赖 docker 运行时 |

> 生产服务器优先二进制方式（`garage-install.sh`），避免引入 docker 运行时依赖；该脚本自动随机生成 `rpc_secret`/`admin_token`，默认不开放整桶公开读。
> Production prefers the binary installer to avoid a docker runtime dependency; it auto-generates secrets and never enables bucket-wide public-read.

下面手动步骤适用于不使用脚本、需逐步理解配置的场景 / The manual steps below apply when not using the script.

---

## 安装

### 下载二进制

> Ubuntu / Debian 生产服务器推荐使用 `x86_64-unknown-linux-musl`（静态链接，无 glibc 依赖，在所有主流发行版通用）。

```bash
# macOS Apple Silicon（开发机）
curl -o garage \
  'https://garagehq.deuxfleurs.fr/api/v1/download?version=v2.3.0&platform=aarch64-apple-darwin'

# Linux x86_64 —— Ubuntu / Debian 生产服务器
curl -o garage \
  'https://garagehq.deuxfleurs.fr/api/v1/download?version=v2.3.0&platform=x86_64-unknown-linux-musl'

# Linux ARM64（ARM 服务器 / 树莓派）
curl -o garage \
  'https://garagehq.deuxfleurs.fr/api/v1/download?version=v2.3.0&platform=aarch64-unknown-linux-musl'

chmod +x garage
sudo mv garage /usr/local/bin/garage
garage --version
```

---

## 配置

### `/etc/garage.toml`（生产）

```toml
metadata_dir = "/var/lib/garage/meta"
data_dir     = "/var/lib/garage/data"
db_engine    = "lmdb"           # lmdb 性能最佳
replication_factor = 1          # 单节点固定为 1
rpc_bind_addr = "127.0.0.1:3901"  # 顶层字段，非 section / top-level field, NOT a section
rpc_secret    = "<openssl rand -hex 32 生成 / generate with openssl rand -hex 32>"

[s3_api]
# 必须与 Erlang sys.config 和 Flutter 中的 region 完全一致
s3_region     = "garage"
api_bind_addr = "0.0.0.0:3900"  # 开发用 127.0.0.1:3900

[admin]
api_bind_addr = "127.0.0.1:3903"
```

### `~/garage.toml`（本地开发最简版）

```toml
metadata_dir = "/tmp/garage/meta"
data_dir     = "/tmp/garage/data"
db_engine    = "lmdb"
replication_factor = 1
rpc_bind_addr = "127.0.0.1:3901"
rpc_secret    = "<openssl rand -hex 32>"

[s3_api]
s3_region     = "garage"
api_bind_addr = "127.0.0.1:3900"
```

---

## systemd 服务

```ini
# /etc/systemd/system/garage.service
[Unit]
Description=Garage S3-compatible object store
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
ExecStart=/usr/local/bin/garage -c /etc/garage.toml server
Restart=on-failure
RestartSec=5s
User=garage
Group=garage
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ReadWritePaths=/var/lib/garage

[Install]
WantedBy=multi-user.target
```

```bash
useradd -r -s /bin/false garage
mkdir -p /var/lib/garage/meta /var/lib/garage/data
chown -R garage:garage /var/lib/garage
cp garage.toml /etc/garage.toml

systemctl daemon-reload
systemctl enable --now garage
systemctl status garage
journalctl -u garage -f
```

---

## 初始化

> 首次部署执行一次，重启无需重复。

```bash
# 可选：设置别名
alias gg='garage -c /etc/garage.toml'

# 1. 查看节点 ID（启动约 3 秒后可用）
gg status
# 示例输出：
# ==== HEALTHY NODES ====
# ID                  Addr            Zone  Cap
# b10c110d4f8b3f73…   127.0.0.1:3901

# 2. 配置单节点布局（必须执行，否则无法存储）
gg layout assign -z dc1 -c 200G b10c110d   # 取节点 ID 前 8 位

# 3. 预览并应用布局
gg layout show
gg layout apply --version 1

# 4. 创建 bucket
gg bucket create imboy

# 5. 创建 Access Key（凭证只显示一次，立即保存到安全位置）
gg key create imboy-key
# 输出：
#   Key ID:     GKxxxxxxxxxxxxxxxxxx
#   Secret key: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx...

# 6. 授权（仅服务端密钥可读写，不开放匿名访问）
#    Authorize (server key only; NO anonymous public access)
gg bucket allow imboy --read --write --owner --key imboy-key

# 注意：不要设置 bucket 公开读。私有附件（聊天图片/文件）必须保密，
# 下载一律经后端 GET /api/v1/attachment/view_url 按需签发短时 presigned GET URL。
# DO NOT enable public-read. Private attachments must stay confidential;
# downloads are served via short-lived presigned GET URLs issued by the backend
# endpoint GET /api/v1/attachment/view_url. Never run `bucket allow imboy --read --public`.

# 验证 / Verify
gg bucket list
gg key list
```

> ⚠️ **安全 / Security**：整桶公开读会让任何人凭 URL（且 ObjectKey 含时间戳可被推测）匿名读取私有聊天附件。本方案改为后端签发短时（默认 600s）presigned GET，配合 `u<Uid>/` 命名空间前缀做归属隔离。
> Bucket-wide public-read would expose private chat attachments to anyone holding (or guessing) the URL. This design instead issues short-lived (default 600s) presigned GET URLs from the backend, combined with a `u<Uid>/` key-prefix namespace for ownership isolation.

---

## Erlang 集成

### sys.config

```erlang
%% config/sys.local.config（开发，.gitignore 中，不入 git）
{garage, #{
    endpoint   => <<"http://127.0.0.1:3900">>,
    region     => <<"garage">>,
    bucket     => <<"imboy">>,
    access_key => <<"GKxxxxxxxxxxxxxxxxxx">>,
    secret_key => <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>
}},

%% config/sys.config（生产模板，密钥通过环境变量 IMBOY_GARAGE_* 注入）
{garage, #{
    endpoint   => <<"http://127.0.0.1:3900">>,
    region     => <<"garage">>,
    bucket     => <<"imboy">>,
    access_key => <<"REPLACE_WITH_ENV">>,
    secret_key => <<"REPLACE_WITH_ENV">>
}},
```

### 确认 inets 依赖

`imboy.app.src` 的 `applications` 列表必须包含 `inets`（`httpc` 所在 OTP 应用）：

```erlang
{applications, [kernel, stdlib, inets, ssl, crypto, ...]},
```

### elib_s3_sign.erl（新建）

纯 OTP 实现 AWS Signature V4，零额外依赖。

```erlang
-module(elib_s3_sign).
%%% AWS Signature Version 4 — 供 Garage S3 API 鉴权
%%% 依赖：OTP crypto（HMAC-SHA256）、inets（httpc）
-export([presign_put/5, presign_get/4, auth_header/6]).
-include("log.hrl").

%% @doc 生成 presigned PUT URL（供 Flutter 直传 Garage，不经 Erlang）
-spec presign_put(binary(), binary(), binary(), binary(), pos_integer()) -> binary().
presign_put(Endpoint, Bucket, ObjKey, MimeType, Expires) ->
    {Date, AmzDate} = amz_dates(),
    Region  = gconf(region),
    Access  = gconf(access_key),
    Secret  = gconf(secret_key),
    Cred    = <<Access/binary, "/", Date/binary, "/",
                Region/binary, "/s3/aws4_request">>,
    QS = iolist_to_binary([
        "X-Amz-Algorithm=AWS4-HMAC-SHA256",
        "&X-Amz-Credential=", uri_encode(Cred),
        "&X-Amz-Date=",       AmzDate,
        "&X-Amz-Expires=",    integer_to_binary(Expires),
        "&X-Amz-SignedHeaders=host",
        "&Content-Type=",     uri_encode(MimeType)
    ]),
    Host     = endpoint_host(Endpoint),
    CanonReq = <<"PUT\n/", Bucket/binary, "/", ObjKey/binary, "\n",
                  QS/binary, "\nhost:", Host/binary,
                  "\n\nhost\nUNSIGNED-PAYLOAD">>,
    STS      = string_to_sign(AmzDate, Date, Region, CanonReq),
    SignKey  = signing_key(Secret, Date, Region, <<"s3">>),
    Sig      = hex(hmac256(SignKey, STS)),
    <<Endpoint/binary, "/", Bucket/binary, "/", ObjKey/binary,
      "?", QS/binary, "&X-Amz-Signature=", Sig/binary>>.

%% @doc 生成 presigned GET URL（bucket 非公开读时用）
-spec presign_get(binary(), binary(), binary(), pos_integer()) -> binary().
presign_get(Endpoint, Bucket, ObjKey, Expires) ->
    {Date, AmzDate} = amz_dates(),
    Region  = gconf(region),
    Access  = gconf(access_key),
    Secret  = gconf(secret_key),
    Cred    = <<Access/binary, "/", Date/binary, "/",
                Region/binary, "/s3/aws4_request">>,
    QS = iolist_to_binary([
        "X-Amz-Algorithm=AWS4-HMAC-SHA256",
        "&X-Amz-Credential=", uri_encode(Cred),
        "&X-Amz-Date=",       AmzDate,
        "&X-Amz-Expires=",    integer_to_binary(Expires),
        "&X-Amz-SignedHeaders=host"
    ]),
    Host     = endpoint_host(Endpoint),
    CanonReq = <<"GET\n/", Bucket/binary, "/", ObjKey/binary, "\n",
                  QS/binary, "\nhost:", Host/binary,
                  "\n\nhost\nUNSIGNED-PAYLOAD">>,
    STS      = string_to_sign(AmzDate, Date, Region, CanonReq),
    SignKey  = signing_key(Secret, Date, Region, <<"s3">>),
    Sig      = hex(hmac256(SignKey, STS)),
    <<Endpoint/binary, "/", Bucket/binary, "/", ObjKey/binary,
      "?", QS/binary, "&X-Amz-Signature=", Sig/binary>>.

%% @doc 生成 Authorization Header（Erlang 服务端 PUT/DELETE 使用）
-spec auth_header(binary(), binary(), binary(), binary(), binary(), binary()) -> binary().
auth_header(Method, Bucket, ObjKey, MimeType, Body, AmzDate) ->
    {Date, _} = amz_dates(),
    Region    = gconf(region),
    Access    = gconf(access_key),
    Secret    = gconf(secret_key),
    BodyHash  = hex(crypto:hash(sha256, Body)),
    Host      = endpoint_host(gconf(endpoint)),
    SignedHdrs = <<"host;x-amz-content-sha256;x-amz-date">>,
    CanonReq = <<Method/binary, "\n/", Bucket/binary, "/", ObjKey/binary,
                 "\n\nhost:", Host/binary,
                 "\nx-amz-content-sha256:", BodyHash/binary,
                 "\nx-amz-date:", AmzDate/binary,
                 "\n\n", SignedHdrs/binary, "\n", BodyHash/binary>>,
    STS      = string_to_sign(AmzDate, Date, Region, CanonReq),
    SignKey  = signing_key(Secret, Date, Region, <<"s3">>),
    Sig      = hex(hmac256(SignKey, STS)),
    Cred     = <<Access/binary, "/", Date/binary, "/",
                 Region/binary, "/s3/aws4_request">>,
    <<"AWS4-HMAC-SHA256 Credential=", Cred/binary,
      ",SignedHeaders=", SignedHdrs/binary,
      ",Signature=", Sig/binary>>.

%% ===== 内部函数 =====

hmac256(Key, Data) -> crypto:mac(hmac, sha256, Key, Data).

signing_key(Secret, Date, Region, Service) ->
    K1 = hmac256(<<"AWS4", Secret/binary>>, Date),
    K2 = hmac256(K1, Region),
    K3 = hmac256(K2, Service),
    hmac256(K3, <<"aws4_request">>).

string_to_sign(AmzDate, Date, Region, CanonReq) ->
    Scope = <<Date/binary, "/", Region/binary, "/s3/aws4_request">>,
    Hash  = hex(crypto:hash(sha256, CanonReq)),
    <<"AWS4-HMAC-SHA256\n", AmzDate/binary, "\n",
      Scope/binary, "\n", Hash/binary>>.

amz_dates() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    Date = iolist_to_binary(
               io_lib:format("~4..0B~2..0B~2..0B", [Y, Mo, D])),
    AmzDate = iolist_to_binary(
                  io_lib:format("~4..0B~2..0B~2..0BT~2..0B~2..0B~2..0BZ",
                                [Y, Mo, D, H, Mi, S])),
    {Date, AmzDate}.

endpoint_host(Endpoint) ->
    [_, Host] = binary:split(Endpoint, <<"://">>),
    Host.

gconf(Key) ->
    maps:get(Key, application:get_env(imboy, garage, #{}), <<>>).

hex(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [X]) || <<X>> <= Bin]).

uri_encode(B) ->
    << <<(ue(C))/binary>> || <<C>> <= B >>.

ue(C) when C >= $A, C =< $Z -> <<C>>;
ue(C) when C >= $a, C =< $z -> <<C>>;
ue(C) when C >= $0, C =< $9 -> <<C>>;
ue($-) -> <<"-">>; ue($_) -> <<"_">>;
ue($.) -> <<".">>; ue($~) -> <<"~">>;
ue(C)  -> iolist_to_binary(io_lib:format("%~2.16.0B", [C])).
```

### elib_oss.erl — 核心函数（改写）

```erlang
%% 服务端上传（群文件 / 群相册等后端处理场景）
upload_to_storage(FileId, FileName, FileBinary, MimeType) ->
    #{endpoint := EP, bucket := Bkt} = garage_cfg(),
    ObjKey  = object_key(FileId, FileName),
    Url     = <<EP/binary, "/", Bkt/binary, "/", ObjKey/binary>>,
    {_, AmzDate} = elib_s3_sign:amz_dates(),
    Auth = elib_s3_sign:auth_header(
               <<"PUT">>, Bkt, ObjKey, MimeType, FileBinary, AmzDate),
    Headers = [
        {"content-type",           binary_to_list(MimeType)},
        {"x-amz-date",             binary_to_list(AmzDate)},
        {"x-amz-content-sha256",   binary_to_list(
                                       hex(crypto:hash(sha256, FileBinary)))},
        {"authorization",          binary_to_list(Auth)}
    ],
    case httpc:request(put,
                       {binary_to_list(Url), Headers,
                        binary_to_list(MimeType), FileBinary},
                       [{timeout, 30000}], []) of
        {ok, {{_, C, _}, _, _}} when C =:= 200; C =:= 204 ->
            {ok, public_url(EP, Bkt, ObjKey)};
        {ok, {{_, C, _}, _, Body}} ->
            ?ERROR_LOG(["elib_oss:upload_to_storage failed: ", C, " ", Body]),
            {error, {http_error, C}};
        {error, R} ->
            ?ERROR_LOG(["elib_oss:upload_to_storage httpc error: ", R]),
            {error, R}
    end.

%% Flutter 直传：生成 presigned PUT URL（文件不经过 Erlang）
%% 返回 {PutUrl, ObjectKey, PublicUrl}
presign_put(FileName, MimeType, ExpiresSeconds) ->
    #{endpoint := EP, bucket := Bkt} = garage_cfg(),
    FileId    = generate_file_id(),
    ObjKey    = object_key(FileId, FileName),
    PutUrl    = elib_s3_sign:presign_put(EP, Bkt, ObjKey, MimeType,
                    min(86400, max(60, ExpiresSeconds))),
    PublicUrl = public_url(EP, Bkt, ObjKey),
    {PutUrl, ObjKey, PublicUrl}.

%% 物理删除（孤儿清理 Phase 2）
delete_object(ObjKey) ->
    #{endpoint := EP, bucket := Bkt} = garage_cfg(),
    Url = <<EP/binary, "/", Bkt/binary, "/", ObjKey/binary>>,
    {_, AmzDate} = elib_s3_sign:amz_dates(),
    Auth = elib_s3_sign:auth_header(
               <<"DELETE">>, Bkt, ObjKey, <<>>, <<>>, AmzDate),
    EmptyHash = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    Headers = [
        {"x-amz-date",           binary_to_list(AmzDate)},
        {"x-amz-content-sha256", EmptyHash},
        {"authorization",        binary_to_list(Auth)}
    ],
    case httpc:request(delete, {binary_to_list(Url), Headers},
                       [{timeout, 10000}], []) of
        {ok, {{_, C, _}, _, _}} when C =:= 204; C =:= 200 -> ok;
        {ok, {{_, C, _}, _, B}} -> {error, {http_error, C, B}};
        {error, R}              -> {error, R}
    end.

%% 内部工具
object_key(FileId, FileName) ->
    SafeName = filename:basename(FileName),
    <<FileId/binary, "/", SafeName/binary>>.

public_url(EP, Bkt, ObjKey) ->
    <<EP/binary, "/", Bkt/binary, "/", ObjKey/binary>>.

garage_cfg() ->
    application:get_env(imboy, garage, #{}).

hex(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [X]) || <<X>> <= Bin]).
```

### attach_handler.erl — presign 接口（新建）

```erlang
%% 路由：GET /api/v1/attachment/presign?filename=x.jpg&mime_type=image/jpeg&expires=600
%% 需要 JWT 认证，放在普通认证路由区（非 open 路由）
presign(<<"GET">>, Req0, _State) ->
    Qs       = cowboy_req:parse_qs(Req0),
    FileName = proplists:get_value(<<"filename">>,  Qs, <<"file">>),
    MimeType = proplists:get_value(<<"mime_type">>, Qs, <<"application/octet-stream">>),
    ExpiresRaw = proplists:get_value(<<"expires">>, Qs, <<"600">>),
    Expires  = min(86400, max(60, binary_to_integer(ExpiresRaw))),
    case elib_oss:validate_file_type(MimeType) of
        false ->
            elib_response:error(Req0, <<"不支持的文件类型"/utf8>>, ?ERR_BAD_REQUEST);
        true ->
            {PutUrl, ObjKey, PublicUrl} =
                elib_oss:presign_put(FileName, MimeType, Expires),
            elib_response:success(Req0, #{
                <<"put_url">>    => PutUrl,
                <<"object_key">> => ObjKey,
                <<"public_url">> => PublicUrl,
                <<"expires_at">> => erlang:system_time(second) + Expires
            }, "success.")
    end;
presign(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
```

路由注册（`imboy_router.erl`）：

```erlang
%% 认证路由区（非 open）
{"/api/v1/attachment/presign", attach_handler, #{action => presign}},
```

---

## Flutter 集成

> Flutter 不需要任何 AWS SDK。
> 上传使用 `dio` PUT 到 presigned URL（直连 Garage）。
> 查看使用 public_url 直接展示（bucket 已设 public-read，无需签名）。

### pubspec.yaml

```yaml
dependencies:
  dio: ^5.4.0     # 已有
  mime: ^1.0.4    # 新增，用于探测 MIME 类型
```

### attachment_api.dart

```dart
import 'dart:io';
import 'package:dio/dio.dart';
import 'package:mime/mime.dart';

class PresignResult {
  final String putUrl;
  final String objectKey;
  final String publicUrl;
  final int expiresAt;

  const PresignResult({
    required this.putUrl,
    required this.objectKey,
    required this.publicUrl,
    required this.expiresAt,
  });

  factory PresignResult.fromJson(Map<String, dynamic> j) => PresignResult(
        putUrl:    j['put_url']    as String,
        objectKey: j['object_key'] as String,
        publicUrl: j['public_url'] as String,
        expiresAt: j['expires_at'] as int,
      );
}

class AttachmentUploadResult {
  final String objectKey;
  final String publicUrl;
  final String mimeType;
  final int size;

  const AttachmentUploadResult({
    required this.objectKey,
    required this.publicUrl,
    required this.mimeType,
    required this.size,
  });

  /// 消息 payload 格式
  Map<String, dynamic> toPayload() => {
        'object_key': objectKey,
        'url':        publicUrl,   // 直接当图片/文件 URL 使用
        'mime_type':  mimeType,
        'size':       size,
      };
}

class AttachmentApi {
  /// 专用 Dio 实例：不带业务拦截器，不加 Authorization header
  /// ⚠️ presigned URL 已含签名，绝对不能再加 Authorization，否则签名冲突
  static final _garageDio = Dio(
    BaseOptions(
      connectTimeout: const Duration(seconds: 30),
      sendTimeout:    const Duration(minutes: 3),
      receiveTimeout: const Duration(seconds: 30),
      followRedirects: false,
      validateStatus: (s) => s != null,
    ),
  );

  /// Step 1：向 Erlang 后端请求 presigned PUT URL
  static Future<PresignResult> requestPresignUrl({
    required String filename,
    required String mimeType,
    int expires = 600,
  }) async {
    final resp = await apiClient.get<Map<String, dynamic>>(
      '/api/v1/attachment/presign',
      queryParameters: {
        'filename':  filename,
        'mime_type': mimeType,
        'expires':   expires,
      },
    );
    return PresignResult.fromJson(
        resp.data!['payload'] as Map<String, dynamic>);
  }

  /// Step 2：直接 PUT 文件到 Garage（不经 Erlang）
  static Future<void> _putToGarage({
    required String presignedUrl,
    required List<int> bytes,
    required String mimeType,
    void Function(int sent, int total)? onProgress,
  }) async {
    final resp = await _garageDio.put<dynamic>(
      presignedUrl,
      data: bytes,                 // dio 直接接受 Uint8List / List<int>
      options: Options(
        contentType: mimeType,
        headers: {Headers.contentLengthHeader: bytes.length},
      ),
      onSendProgress: onProgress,
    );

    if (resp.statusCode != 200 && resp.statusCode != 204) {
      throw Exception(
          'Garage upload failed: HTTP ${resp.statusCode}\n${resp.data}');
    }
  }

  /// 一步完成：拿 presigned URL → 上传 → 返回结果
  static Future<AttachmentUploadResult> uploadFile(
    File file, {
    void Function(int sent, int total)? onProgress,
  }) async {
    final bytes    = await file.readAsBytes();
    final filename = file.path.split('/').last;
    final mimeType = lookupMimeType(file.path,
                         headerBytes: bytes.sublist(0, 12)) ??
                     'application/octet-stream';

    final presign = await requestPresignUrl(
        filename: filename, mimeType: mimeType);

    await _putToGarage(
      presignedUrl: presign.putUrl,
      bytes:        bytes,
      mimeType:     mimeType,
      onProgress:   onProgress,
    );

    return AttachmentUploadResult(
      objectKey: presign.objectKey,
      publicUrl: presign.publicUrl,
      mimeType:  mimeType,
      size:      bytes.length,
    );
  }
}
```

### 替换 AssetsService.viewUrl

```dart
// 旧：自制 HMAC-MD5 签名（废弃）
// String viewUrl(String path) { ... MD5(uploadKey + ts) ... }

// 新：bucket 公开读，直接返回 public_url，无需签名
String viewUrl(String urlOrKey) {
  // 已是完整 HTTP URL（新数据）
  if (urlOrKey.startsWith('http')) return urlOrKey;
  // 纯 object_key（兜底）
  return '${Env.garageEndpoint}/${Env.garageBucket}/$urlOrKey';
}
```

### 消息 payload 格式变化

```dart
// 旧（go-fastdfs）
// { md5, url, path, size }

// 新（Garage）
final result = await AttachmentApi.uploadFile(file);
final payload = result.toPayload();
// { object_key, url, mime_type, size }
```

---

## 验证与排障

### 逐步验证

```bash
# 1. Garage 健康状态
garage -c /etc/garage.toml status        # 期望：HEALTHY

# 2. aws-cli 功能验证（最直接的方式）
export AWS_ACCESS_KEY_ID=GKxxxxxxxxxxxxxxxxxx
export AWS_SECRET_ACCESS_KEY=xxxxxxxxxxxxxxxxxxxxxxxx
export AWS_ENDPOINT_URL=http://127.0.0.1:3900
export AWS_DEFAULT_REGION=garage

echo "hello garage" > /tmp/test.txt
aws s3 cp /tmp/test.txt s3://imboy/test.txt
aws s3 ls s3://imboy/

# 3. 验证公开读（无需凭证）
curl http://127.0.0.1:3900/imboy/test.txt
# 期望：返回 "hello garage"，HTTP 200

# 4. Erlang shell 验证
IMBOYENV=local make run

# 服务端上传
elib_oss:upload(<<"hello">>, <<"test.txt">>, #{mime_type => <<"text/plain">>}).
% 期望：{ok, <<"http://127.0.0.1:3900/imboy/file_xxx.../test.txt">>, <<"file_xxx...">>}

# 生成 presigned URL
{PutUrl, ObjKey, PubUrl} = elib_oss:presign_put(<<"photo.jpg">>, <<"image/jpeg">>, 600).

# 用 curl 验证 presigned URL
curl -X PUT \
  -H "Content-Type: image/jpeg" \
  --data-binary @/tmp/test.jpg \
  "$PutUrl"
# 期望：HTTP 200 或 204

curl "$PubUrl"
# 期望：返回图片内容

# 5. presign API 接口验证
curl -H "Authorization: Bearer <jwt_token>" \
  "http://localhost:8080/api/v1/attachment/presign?filename=a.jpg&mime_type=image/jpeg"
# 期望：{ "put_url": "...", "object_key": "...", "public_url": "..." }
```

### 常见问题

| 症状 | 原因 | 解决 |
|---|---|---|
| `SignatureDoesNotMatch` | Content-Type 与签名时不一致 | Erlang presign 传的 mime_type 必须与 Flutter PUT 时 Content-Type 完全一致 |
| Flutter PUT 返回 `403` | Presigned URL 已过期 | 增大 `expires` 参数（上限 86400s），缩短拿到 URL 到上传的间隔 |
| Flutter PUT 返回 `400` | 多余的 Authorization header | 确保 `_garageDio` 实例没有业务 Authorization 拦截器 |
| Erlang `{error, econnrefused}` | Garage 未启动或端口错误 | `systemctl status garage`，确认 3900 端口监听 |
| GET public_url 返回 `403` | Bucket 未设 public-read | `garage bucket allow imboy --read --public` |
| `clock skew` 错误 | 服务器时间偏差 > 5 分钟 | `timedatectl set-ntp true` 同步 NTP |
| `layout not configured` | 未执行 layout assign/apply | 重新执行初始化步骤 2-4 |

### 端口速查

| 端口 | 用途 | 对外暴露 |
|---|---|---|
| **3900** | S3 API（Erlang 和 Flutter 使用） | 是（生产走 Nginx 代理） |
| 3901 | RPC 集群内部通信 | 否 |
| 3902 | Web 静态网站托管（可选） | 按需 |
| 3903 | Admin API | 否（仅本机） |
