# ADR: 附件存储采用可切换驱动并固化每个对象的位置

- Status: Proposed
- Date: 2026-09-25
- 关联：ADR 0001（四层单向依赖）、ADR 0007（Feature Slice）、
  `docs/architecture/2026-09-25-attachment-storage-driver-plan.md`

## Context

IMBoy 当前附件链路固定依赖 Garage：`elib_oss` 生成 S3 SigV4 预签名 URL，
Flutter 对 `put_url` 执行裸 PUT，`attach_logic` 在 confirm 时查询对象，读取时由
`view_url` 返回短时 URL，孤儿清理再调用 S3 DELETE。

广州单机交付要求附件直接保存在服务器磁盘，不安装 Garage；其他客户仍可能使用
Garage、腾讯云 COS 或阿里云 OSS。仅在 `sys.config` 增加一个分支不够：

1. 切换驱动后，历史附件不能被误认为位于新驱动；
2. 已签发但尚未 confirm 的上传必须继续由原驱动完成；
3. `disk` 没有 S3 预签名 URL，需要由 IMBoy 提供等价的短时 PUT/GET 网关；
4. COS 新 bucket 采用 virtual-hosted-style，OSS 的原生 V4 与 AWS SigV4 不是同一协议，
   不能假设改 endpoint 就能共用 Garage 签名器；
5. 客户端不应因为运维切换存储驱动而重新发布。

成熟自托管软件通常把本地文件系统限定在简单单机场景，把 S3/NAS 用于多节点；
GitLab 还明确区分本地存储的代理上传/下载与对象存储的直传。本决策采用相同边界。

## Decision

### 1. 配置使用附件存储名称，不叫 `s3_driver`

在 `sys.config` 中增加一个统一配置段：

```erlang
{attachment_storage, #{
    driver => garage, %% garage | disk | cos | oss
    upload_url_ttl_seconds => 3600,
    download_url_ttl_seconds => 600,
    drivers => #{
        garage => #{
            endpoint => <<"http://127.0.0.1:3900">>,
            public_endpoint => <<"https://api.example.com/s3">>,
            region => <<"garage">>,
            bucket => <<"imboy">>,
            public_bucket => <<"imboy-public">>,
            public_base_url => <<"https://files.example.com">>,
            access_key => {env, <<"IMBOY_GARAGE_ACCESS_KEY">>},
            secret_key => {env, <<"IMBOY_GARAGE_SECRET_KEY">>}
        },
        disk => #{
            root_dir => <<"/var/lib/imboy/attachments">>,
            public_base_url => <<"https://api.example.com/api/v1/attachment/public">>,
            signing_secret => {env, <<"IMBOY_ATTACHMENT_DISK_SIGNING_SECRET">>},
            min_free_bytes => 5368709120
        },
        cos => #{
            endpoint => <<"https://cos.ap-guangzhou.myqcloud.com">>,
            region => <<"ap-guangzhou">>,
            bucket => <<"REPLACE_PRIVATE_BUCKET">>,
            public_bucket => <<"REPLACE_PUBLIC_BUCKET">>,
            public_base_url => <<"https://REPLACE_PUBLIC_COS_DOMAIN">>,
            addressing_style => virtual_host,
            access_key => {env, <<"IMBOY_COS_SECRET_ID">>},
            secret_key => {env, <<"IMBOY_COS_SECRET_KEY">>}
        },
        oss => #{
            endpoint => <<"https://oss-cn-guangzhou.aliyuncs.com">>,
            region => <<"cn-guangzhou">>,
            bucket => <<"REPLACE_PRIVATE_BUCKET">>,
            public_bucket => <<"REPLACE_PUBLIC_BUCKET">>,
            public_base_url => <<"https://REPLACE_PUBLIC_OSS_DOMAIN">>,
            signing_mode => oss_v4,
            access_key => {env, <<"IMBOY_OSS_ACCESS_KEY_ID">>},
            secret_key => {env, <<"IMBOY_OSS_ACCESS_KEY_SECRET">>}
        }
    }
}},
```

运维只改 `driver` 即可决定**新上传**的目标。环境变量
`IMBOY_ATTACHMENT_STORAGE_DRIVER` 可覆盖该值。启动时校验当前启用驱动，以及数据库中
仍被 attachment/pending 行引用的历史驱动；全新 disk 数据库不要求 Garage 凭据，但有
Garage 历史行时缺少 Garage 配置必须 fail-fast。未知驱动、相对磁盘路径、空密钥或
不可写目录同样 fail-fast。

阿里云名称固定写作 `oss`，不接受 `oos`，避免形成永久错误配置名。

### 2. 对外附件 API 保持不变

保留以下客户端契约：

- `GET /api/v1/attachment/presign` 返回 `put_url`、`object_key`、`expires_at`；
- 客户端对 `put_url` 执行原始字节 PUT；
- `POST /api/v1/attachment/confirm` 确认上传；
- `GET /api/v1/attachment/view_url` 返回可访问 URL。

Garage/COS/OSS 的 `put_url` 和私有查看 URL 指向对象存储，public scope 使用各自独立
公开桶的稳定 URL；disk 的 URL 指向 IMBoy 自身的短时签名上传/下载端点。Flutter 无需
知道当前驱动，也不新增 SDK。

### 3. 在数据库固化对象位置

`attachment` 增加非空 `storage_driver` 和可空 `storage_bucket`；`attach_pending` 增加
`storage_driver`，并允许现有 `bucket` 在 disk 行为 NULL。存量附件回填为 `garage`，
bucket 按现有 scope 规则回填。数据库约束要求 Garage/COS/OSS 的 bucket 非空，disk 的
bucket 为 NULL。

- presign 时把当时的 driver 和 bucket 写入 pending；
- pending 写入失败时 presign 必须失败，不能再以 best-effort 签发无登记 URL；
- confirm 必须读取 pending 中的 driver，不能读取“当前配置”；
- attachment 落库后，查看、删除、孤儿清理按该行的 driver/bucket 分派；
- 改配置只影响新上传，历史附件继续从原位置读取；
- 数据迁移由独立、可恢复的迁移命令完成，不在启动时隐式搬文件。

### 4. 增加一个存储 facade 和四个驱动

业务层只依赖 `attachment_storage` facade。驱动实现统一能力：

```erlang
-callback create_put_url(map()) -> {ok, binary()} | {error, term()}.
-callback put_file(map(), file:filename_all()) -> ok | {error, term()}.
-callback stat(map()) -> {ok, #{size := non_neg_integer(), content_type := binary()}}
    | {error, not_found | term()}.
-callback create_get_url(map()) -> {ok, binary()} | {error, term()}.
-callback delete(map()) -> ok | {error, term()}.
```

- `garage`：复用现有 `elib_s3_sign` 和已验证行为；
- `disk`：使用 OTP 文件 API 和 IMBoy 签名网关；
- `cos`：单独处理 virtual-hosted-style、COS endpoint 与真实兼容性；
- `oss`：默认实现 OSS 原生 V4；若采用 S3 兼容模式，必须显式配置并完成实桶验收。

`elib_oss` 先保留为兼容 facade，现有调用者逐步转入新 facade，不做无关重命名。

### 5. disk 驱动只支持单机或共享持久卷

disk 文件保存为 `<root_dir>/<object_key>`，写入同目录临时文件，完成后原子 rename。
所有 key 必须通过现有 owner/scope 规则并经过路径规范化；拒绝绝对路径、`..`、NUL、
符号链接逃逸和超长路径。

disk 上传 URL 允许在有效期内重试，但 attachment 已 confirm 后禁止覆盖。上传端点流式
写盘并执行大小上限与剩余空间检查，不把 100 MB 文件整体读入 BEAM heap。下载端点支持
`GET`、`HEAD` 和单区间 `Range`，以满足图片、音频和视频播放。

disk 不支持多节点各自本地盘。多节点部署必须挂载同一可靠共享卷，或选择
Garage/COS/OSS；启动检查和文档必须明确该限制。

### 6. 安全和可观测性保持一致

- 私有附件始终先走现有 DB ACL，再签发短时 GET；
- disk 签名绑定 `op + object_key + mime_type + expires`，使用独立 secret 和常量时间比较；
- disk 公共附件通过 IMBoy public gateway 查表确认 `scope=public`；对象存储只有独立
  public bucket 可匿名读，private bucket 永不开放 Website/ACL 公共读取；
- 日志记录 driver、耗时、大小、结果和 request id，不记录签名 URL、密钥或附件内容；
- 指标至少覆盖上传/读取/删除成功率、延迟、字节量、磁盘余量和签名拒绝数。

## Consequences

### Positive

- 广州单机可不安装 Garage，附件直接进入指定磁盘目录；
- 同一客户端可连接不同存储驱动的部署；
- 配置切换不会破坏历史附件，也不会打断 pending 上传；
- COS/OSS 差异被限制在驱动内，业务 ACL、对象 key、confirm 契约保持单一真源；
- 后续可做逐对象在线迁移，不需要停机一次性搬空。

### Negative

- disk 上传/下载占用 IMBoy 进程和主机带宽，不适合横向扩容或高并发大文件；
- 数据库需要记录对象位置，并增加迁移、备份和孤儿清理测试；
- COS/OSS 必须用真实 bucket 做独立验收，mock SigV4 不能证明供应商兼容性；
- 混合驱动期间，运维必须同时备份仍被引用的旧存储和新存储。

### Neutral

- `object_key` 和附件消息协议不变；
- 切换配置不自动迁移存量文件；
- Garage 仍是社区版默认驱动，disk 是广州单机交付选项。

## Alternatives Considered

### 只把 Garage 数据目录改成本地磁盘

不采用。Garage 本来就把对象保存在磁盘，但仍需运行 Garage 进程，不满足“不用 Garage”。

### 所有驱动都由后端代理上传下载

不采用。会让 COS/OSS/Garage 失去直传优势，并把全部大文件流量压到 Erlang 节点。
只有 disk 走后端网关。

### 切配置后让所有历史对象跟着新驱动读取

不采用。配置切换瞬间会让所有历史附件 404，也无法安全回滚。

### 只做 `garage | disk`，预留未实现的 `cos | oss`

MVP 实施顺序允许先交付 Garage+disk，但配置解析器不得把未实现驱动当成可用。
COS/OSS 只有完成各自真实供应商验收后才进入支持矩阵。

## References

- [实施计划](../architecture/2026-09-25-attachment-storage-driver-plan.md)
- [Garage 安装指南](../guides/operations/garage-deployment.md)
- [Mattermost 文件存储建议](https://docs.mattermost.com/deployment-guide/server/prepare-file-storage)
- [GitLab Object Storage](https://docs.gitlab.com/administration/object_storage/)
- [腾讯云 COS 的 S3 兼容说明](https://intl.cloud.tencent.com/document/product/436/34688?lang=en)
- [阿里云 OSS URL V4 签名](https://help.aliyun.com/zh/oss/developer-reference/add-signatures-to-urls)
