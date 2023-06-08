
# https://dev.imboy.pub/init

```
{
    "code": 0,
    "msg": "success.",
    "payload": {
        "res": "encrypt by aes_256_cbc"
    }
}
```

res 解密后为:
```
{
    "code": 0,
    "msg": "success.",
    "payload": {
        "login_pwd_rsa_encrypt": "1",
        "login_rsa_pub_key": "-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDxKL1RrEcd4szM8Df4HqsJdOvK\nrSQO7BBvBVsvXKfpWrM+8XGL1SP7nsQd6alhntotPSDezaHnFvhnP/sr8bwzzorr\n1dWoBVabqDFZgZ2awB7iTk4k/3RN1TEPoD08kaJQ0xBHZ14395q8bVh22Uh10eCO\n/xtHnso3I6penSvRawIDAQAB\n-----END PUBLIC KEY-----"
    }
}
```
