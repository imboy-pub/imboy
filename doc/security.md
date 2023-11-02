
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

## 使用 OpenSSL 工具来生成自签名的 HTTPS 证书

自制证书在浏览器中通常会被标记为不受信任

```
brew install openssl
```


生成私钥文件（key.pem）
```

openssl genrsa -out ./priv/ssl/server.key 2048
```

生成自签名中间证书（chain.crt）
```
openssl req -new -sha256 -key ./priv/ssl/server.key -out ./priv/ssl/chain.csr
openssl x509 -req -in ./priv/ssl/chain.csr -signkey ./priv/ssl/server.key -out ./priv/ssl/chain.crt -days 365

    Signature ok
    subject=C = CN, ST = Guangdong, L = Shenzhen, O = imboy.pub, OU = imboy, CN = imboy, emailAddress = leeyisoft@qq.com
    Getting Private key
```


生成公共证书（ public.crt）：
```
openssl req -new -sha256 -key ./priv/ssl/server.key -out ./priv/ssl/public.csr
openssl x509 -req -in ./priv/ssl/public.csr -CA ./priv/ssl/chain.crt -CAkey ./priv/ssl/server.key -CAcreateserial -out ./priv/ssl/public.crt -days 365
    Signature ok
    subject=C = CN, ST = Guangdong, L = Shenzhen, O = imboy..pub, OU = imboy, CN = imboy, emailAddress = leeyisoft@qq.com
    Getting CA Private Key

```
已经生成了一个自制的 HTTPS 证书，其中 key.pem 是私钥文件，cert.pem 是自签名证书文件。
自制证书在浏览器中通常会被标记为不受信任

## 内容安全

调用华为云提供的 API，可自由设置过滤内容类型，分别为：politics（涉政）、porn（涉黄）、ad（广告）、abuse（辱骂）、contraband（违禁品）、flood（灌水）

原理：根据图片或者图片链接，华为云 API 返回三个维度对应的比例，分别是正常比例、色情比例、性感比例，返回值里的参数 suggestion 结果为 block，则判定为色情图片；性感图片的返回值里的参数 suggestion 结果为 pass，在三个维度的比例中性感比例最大，则认为该图片是性感图片。对于正常与色情比例接近的会返回 review，需要人工确认。


### 内容安全 相关介绍
* IM敏感词算法原理和实现  https://blog.csdn.net/xmcy001122/article/details/118000803
* 华为云与鉴黄师不得不说的那些事 https://www.infoq.cn/article/x-a6mhtxe3shsc3qecey
* https://gitee.com/humingzhang/wordfilter
* Serverless 实战：3 分钟实现文本敏感词过滤 https://www.infoq.cn/article/d0jzjh0lpttjqhj32vg1
    * 什么是 AC 自动机？简单来说，AC 自动机就是字典树+kmp 算法+失配指针，一个常见的例子就是给出 n 个单词，再给出一段包含 m 个字符的文章，让你找出有多少个单词在文章里出现过。

