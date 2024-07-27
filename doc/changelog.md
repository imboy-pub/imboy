# 0.4.2
* 群二维码签名tk还原为md5生成；
* 修复扫描群二维码报错的问题；

# 0.4.1
* fix init config_ds:set/2-4
* 升级一些otp依赖
* 升级相关Dockerfile依赖版本（TIMESCALEDB_VERSION 2.15.3 OTP_VERSION=25.3.2.13）
*

# 0.4.0
* 优化 config_ds 模块（solidified_key AES加密存储在 public.config 里面 config_ds:set/4.）;
* 优化调整接口签名算规则；
* 新增 public.user_device/public_key 字段，存储客户端登录上次的RSA公钥；
* 新增 app_version_ds模块，设置、获取 签名秘钥；
* 整理 ./imboy/config/sys.config 文件，尽可能把安全验证相关配置放入 public.config 表；


# 0.3.14
* 引入pure-migrations 管理数据库版本，数据库版本脚本文件使用8位数做前缀 00000000_xxx.sql
* 为了数据库安全，添加 super_account 数据库账户，和api的账号分类；
* 修正 public.user 为空的时候，IMBOYENV=local make run 启动报错的问题；

# 0.3.13
* 更新docker里面的 OTP_VERSION=25.3.2.12 TIMESCALEDB_VERSION 2.15.2
* 新增 ./docker/pg-update-timescaledb.sh 文件，解决升级报错的问题；

# 0.3.12
* 修复 /group/qrcode 接口获取，重新获取 member_count ，而不是之前+=1，增加验证二维码有效期功能；
* 修复 每次调用 group_member_logic:join/4-5 的收，都还发送 S2C消息 group_member_join，客户端要做消息幂等处理

# 0.3.11
* 修复 添加群聊报错的问题；
* 修复 在加入群聊的时候判断是否已经是成员了；
* 调整： 用户二维码扫描结果响应 type = user的字段；

# 0.3.10
* 新增 /group/qrcode 接口；
* 新增 msg_c2g_timeline 添加 client_ack字段，相关业务逻辑做调整；
* 其他一些细节修复

# 0.3.9
* 调整 /group_member/leave 接口请求和响应参数；
* 调整 /group_member/join 接口请求和响应参数；
* 调试面对面建群功能，接口响应参数做调整；
* 新增group_member表新增join_mode “进群方式 :  invite_[uid]_[nickname] 邀请进群  scan_qr_code 扫描二维码加入 face2face_join 面对面建群”
* 新增 public.user_group 表，存储“用户保存到通讯录的群”


# 0.3.8
* 新增 /group/detail 接口；
* 新增 /group/face2face_save 接口；
* 调整 /group/face2face 接口，使得查询 group_random_code，表，不存在code,的是只写入 group_random_code；
* 修复 group_ds:member_uids/1 group_ds:join/2 group_ds:leave/2 换成过期会报错的问题；

# 0.3.7
* 修正group/creator_uid 字段命令错误
* 创建群接口 group/face2face 新增响应参数 member_list，借口成功之后发送 group_member_join 消息；
* 暂时移除 khepri 库依赖，调整相关代码；
* erlang.mk 升级，relx 升级到4.9.0
* ./docker/pg15_Dockerfile_dev 细微调整

# 0.3.6
* 创建群接口 group/add 新增请求参数 member_uids；
* 其他一些文档细节调整；

# 0.3.5
* 优化 app_version 表，删除 app_db_vsn 字段，修改相关代码；
* 优化面对面建群功能，使用 rabbitmq/khepri 存储群成员信息；
* 新增 C2S 消息类型，调整相关数据库表结构；
* 新增 C2S 消息、 C2G 消息话题功能，以支持机器人聊天功能；
* 新增 重新部署流程文档 ./doc/redeployment_process.md
* 更新 ./docker/pg15_Dockerfile_dev

# 0.3.4
* 修复 Email没有注册，确提示“Email已经被占用了”的问题

# 0.3.3
* 新增用户注销接口 /user/cancel;
* 新增 imboy_db/find/2 方法；
* user_repo/find_by_xxx 方法响应数据类型调整为 map，修改相关代码；
* 删除 /friend/find 接口，有 /fts/user_search 替代功能；
* 新增 5个 S2C 消息，具体细节参考 . /doc/api/消息类型.md S2C 消息介绍


# 0.3.2
* 新增 群消息分页列表新增 last_time（utc0 的毫秒时间戳） 过滤参数；
* 新增 postgresql 新增 CREATE OR REPLACE FUNCTION public.timezone_offset() 删除 unix_time() 方法；

# 0.3.1
* 修复 /app_ddl/get 接口，升降级相关ddl排序不正确的问题；
* 修复 加入群、离开群，清理群成员ID换成 group_member_ds:flush_cache(Gid)；
* 修改 websocket_logic:c2c_revoke/3 为 websocket_logic:revoke/4，以兼容群聊撤回消息

# 0.3.0
* 新增 创建群组功能、修改群信息、面对面添加群、解散群等4个接口；
* 新增 加入群聊、主动退群、设置群内别名、群成员分页列表等4个接口；
* 新增 我加入的群、管理的群 分页列表功能；
* 新增 群聊消息加密存储到时序数据库，按群的级别拉去特定时间段的消息；
* 新增 群通知相关6个接口；
* 修复 一些操作数据库的相关方法的调整优化；


# 0.2.16
* 新增 app_version 表添加sort字段，值为 vsn 的 Major * 1_000_000 + Minor * 1_000 + Patch
* 修正 发布版本默认值设置，版本管理列表排序调整；
* 修复 /app_version/check 获取值问题（调整排序为 order by sort desc, updated_at desc limit 1）；

# 0.2.15
* APP判断token过期的当前时间去 utc+0 的时间戳，服务器端的 token 的 exp 也去 utc+0的时间戳，以避免时区差异影响；
* 修复已经反馈回复列表获取数据为空的问题；
* 其他细节调整；

# 0.2.14
* 凌晨3点执行 pgsql备份；
* 升级 cowboy 到 2.21.0；

# 0.2.13
* 修复c2c_client_ack 之后没删除离线消息的问题；
* websocket 的 idle_timeout 参数的值调整为128秒(需要比客户端的心跳时间间隔大点儿);

# 0.2.12
* 因为客户端第一次安装APP的时候，使用了“Copy from asset”方案，所以 app_ddl 不在需要 type字段，做响应修改
*

# 0.2.11
* 调整 /init API 获取AES必要的方法；
* 修复 CLIENT_ACK 的时候 erlang:cancel_timer/1 之后 imboy_cache:flush/1 掉消息的 TimerRef
* 调整 C2C 消息重试策略为 [0, 5000, 7000, 11000]
* 调整 S2C 消息 please_refresh_token 重试策略为  [7000, 11000]
* imboy_sup 代码格式调整

# 0.2.10
* 删除 imboy_func:to_binary/1 方法，使用 ec_cnv:to_binary/1 替代；
* 删除 imboy_func:to_int/1-2 方法，使用 ec_cnv:to_integer/1 替代；
* 删除 imboy_func:start_at/0 方法；
* 移除 samovar 包依赖，使用 erlware_commons/src/ec_semver.erl 替代；
* 调整用于API验证签名的auth_keys 规则；

# 0.2.9
* 引入 layuid的 tabPage.js 模块，实现管理后台多菜单切换功能；
* 实现 APP版本 分页列表、编辑、删除功能；
* 实现 APP数据库版本 分页列表、编辑、删除功能；
* 实现APP版本检查功能、实现APP数据库版本升级、降级，创建获取相关DDL语句功能；
* imboy_db 模块新增 count_for_where/2, page_for_where/6 assemble_where/1，调整相关代码；
* 新增 imboy_func/to_int/1-2 方法
* 升级 layui 到 2.9.3

# 0.2.8
* 用户反馈添加联系方式字段；
* app/version 接口响应 updatable 字段等；
* 修复 ./apps/imadm/priv/template/feedback_index_dtl.html 表个数据错了样式问题；
* 升级 layui 到 2.9.2；

# 0.2.7
* 新增 appup.sh 脚本 vsn=0.2.7 ./appup.sh

# 0.2.6
* 引入 samovar library ，调整 auth_middleware 相关代码 VsnXY = samovar:major_minor(Vsn)
* 新增 app_version_repo.erl ,实现 app/version API
* 现在 ./doc/app_sqlite3/db_vsn_5.sql 文件等

# 0.2.5
* 添加 imboy.appup 文件
* debug relup

# 0.2.4
* 调整代码布局；

# 0.2.3
* 新增管理员用户表、用户角色表；
* 新增 apps/imadm/priv/static/js/jsencrypt.js
* 新增 apps/imadm/priv/static/js/md5.min.js
* 新增管理员登录功能（登录的时候，提交表单之前经过，密码一次md5加密，保证调试代码的打印日志的时候也不泄露用户密码；）
* 模块化管理后台html模板布局，参考 https://sight-wcg.github.io/layui-theme-dark/
* 新增 反馈列表；回复反馈功能；
* 调整代码布局；

# 0.2.2
* 新增依赖  simple_captcha uid erlydtl;
* 引入 simple_captcha layui ；
* 新增 imadm 应用；
* 新增后台 api (/adm/passport/login  /adm/passport/captcha)
* 新增用户反馈相关功能（新增反馈、反馈列表、反馈回复列表、移除反馈功能)
*

# 0.2.1
* 备注收藏功能；
* 新增 imboy_dt:now/1 imboy_dt:now/2 imboy_dt:to_rfc3339/2 方法；
* 新增 config_ds:set/2 config_ds:save/2 方法；
* 删除没有被使用过的 imboy_dt/utc_date/1 imboy_dt/to_rfc3339/1 方法；
* 修复我的收藏添加标签报错问题；
* 文档修改调整；
* 格式化代码
*

# 0.2.0
* 项目支持 multi app （引入 apps/imlib apps/imcron apps/imapi），调整相关目录
* 重新整理dockerfile数据库引入， vector roaringbitmap timescaledb 等插件
* 新增 docker-compose.yml
* imboy_session module 修改为 imboy_syn module
* 引入 aho_corasick
* 用户登录响应登录Email

# 0.1.13
* 调整收藏方案：清除 收藏专有的存储服务，改用定时任务扫描，在判断资源配如果没收藏嘞，就不删除；否则，过期删除之
* json 库只用jsone，移除 jsx依赖
* 引入 redbug 库

# 0.1.12
* "/passport/" 开通的api也需要做签名验证；
* 优化 Makefile
* 整理一些文档；

# 0.1.11
* 新增 imboy_hasher:hmac_sha512/2 方法，API验证同时支持 sha256 sha512 md5 等方法
* 新增错误码 707 ，表示：签名错误，需要下载最新版本APP；
* 新增配置 api_auth_switch ，为原子 on 的时候开启API签名验证
* 初始化配置、刷新token 和 WebSocket 链接这3个 API 必须做签名校验，其他 open 类型的API不用做签名校验；

# 0.1.10
* 新增新的WebSocket 链接token校验机制，提示APP体验
* 修改 message_ds:send_next/5 为 message_ds:send_next/6 解决消息不按频率发送的问题
* 新增 message_ds:send_next/6 第4个参数的元素支持 fun/1

# 0.1.9
* 添加 imboy_hasher:encoded_val/1 imboy_hasher:decoded_payload/0 imboy_hasher:decoded_field/1 等方法，替换相关逻辑；
* 修正 msg_type = apply_friend_confirm 的消息，结构错误导致APP端无法解析的问题；
* 非好友情况下添加标签，只是先插入标签，标签和朋友关系在确认好友的时候绑定
* 删除朋友关系的时候清理相关tag关系

# 0.1.8
* 修正 verification_code_repo:save/4 报错问题；
* 添加 imboy_db:insert_into/4 ；
* 添加 /fts/recently_user API ；
* 添加 /fts/user_search API 新增响应 is_friend 、 remark ；

# 0.1.7
* /fts/user_search 新增响应 is_friend 、 remark ；
* 添加 /fts/recently_user

# 0.1.6
* 调整 websocket链接频率控制（1 秒钟内3次链接，1分钟内5次年级）；
* 修复 /friend/list 接口报错；

# 0.1.5
* 用户标签_联系人标签设置标签;
* 用户标签，单独新建标签功能

# 0.1.4
* 用户标签_标签详情-标签联系人列表
* 用户标签_标签详情-标签联系人列表-移除对象

# 0.1.3
* 标签分页列表（按关键词like查询）
* 标签添加、标签删除、标签修改名称
*

# 0.1.2
* fix refreshtoken error
* 新增 config_ds 模块，替换相关代码； 使用 pgcrypto 组件aes算法存储c2c c2g消息 payload字段信息收藏的 info字段信息
* 其他一些数据安全相关优化

# 0.1.1
* 实现给联系人添加、删除、列表标签的功能
* 实现给收藏信息添加、删除、列表标签的功能

# 0.1.0
* 基于cowboy框架搭建系统，基于postgresql15 数据存储
* 实现了一对一聊天（支持文本、表情、图片、视频、文件、个人名片、位置等消息）
* 实现了基于gen_smtp发送Email的功能
* 实现了基于 postgis  的附近的朋友的功能
* 实现了基于websocket的消息重发、确认系统
* 实现了基于websocket的WebRTC信令系统
* 实现了使用Email注册用户
* 实现了用户登录、退出登录、刷新token、通过Email找回密码
* 实现了对7种聊天消息的收藏管理功能
* 实现了登录设备管理功能
* 实现了添加朋友，添加备注功能、联系人管理功能
* 实现了联系人黑名单功能
* 实现了基于全文索引的用户搜索功能
*
