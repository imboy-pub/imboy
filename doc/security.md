
一些数据安全相关的说明

* 公钥加密私钥解密是密送，保证消息即使公开也只有私钥持有者能读懂；
* 私钥加密、公钥解密是签名，保证消息来源是私钥持有者。


## 配置与数据安全
* 服务端配置模块 value 字段 使用 pgcrypto 组件aes算法存储数据
* 服务端聊天信息 payload 字段 使用 pgcrypto 组件aes算法存储数据
* 服务端收藏信息 info 字段 使用 pgcrypto 组件aes算法存储数据
* 服务端响应公钥api 使用 aes算法加密
* 服务端基于jwt实现用户安全认证

* 用户聊天信息在客户端确认收到信息后删除
* 使用系统环境变量 IMBOYENV 加载相关配置，sys.local.config vm.local.args 等配置不加入软件git仓储
* 服务端和客户端通过约定 solidified_key 交互安全数据
* 客户端通过请求 /init 获取经过 aes cbc 加密的rsa公钥
* 客户端登录密码使用 rsa 公钥加密提交数据
* 客户端获取所有的用户ID都是经过 hashids 算法加密的6位+字符串
*

* 系统中的附近信息（上传的图片、视频、语音、文件等数据）都是需要经过服务授权才能够访问的（授权码有效期可配置）

## RSA公钥、私钥生成

```
openssl genrsa -des3 -out test_rsa_private_key.pem
# 去除掉密钥文件保护密码
openssl rsa -in test_rsa_private_key.pem -out test_rsa_private_key.pem
# 分离出公钥
openssl rsa -in test_rsa_private_key.pem -pubout -out test_rsa_public_key.pem
```
