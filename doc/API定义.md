
# api 约定  (api convention)
```
{
  "code": 0,
  "msg": "success.",
  "payload": {}
}
```
## code
* websocket Status 101 正常
* websocket Status 400 - 请求无效 为定义 sec-websocket-protocol
* websocket Status 406 - 无法接受 sec-websocket-protocol 不包含 text
* websocket Status 412 - 先决条件失败 缺少token参数
* api json code 0 成功
* api json code 1 失败（通用编码，前端不弹出提示）
* api json code 2 失败（通用编码，前端弹出提示）
* api json code 705 请刷新token
* api json code 706 token无效 (包含缺失token情况) 或者设备ID不存在
* api json code 786 - 在其他设备登录了

## msg
* 错误提示

## payload For List
```
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "list": []
  }
}
```

## payload For Page
```
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "total": 100, // 总记录数量
    "page": 1, // 当前页
    "size": 10, // 每页记录数
    "list": []
  }
}
```

