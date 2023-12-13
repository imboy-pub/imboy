
# 0.2.2
* 新增依赖  simple_captcha uid erlydtl;
* 引入 simple_captcha layui ；
* 新增 imadm 应用；
* 新增后台 api (/adm/passport/login  /adm/passport/captcha)
* 新增用户反馈相关功能

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
