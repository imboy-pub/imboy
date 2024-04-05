
## 消息确认机制实现

1. 判断用户是否在线，如果用户离线，直接存储离线消息
2. 用户在线（or 用户上线），判断 erlang is_process_alive(Pid) 马上投递一次
3. 没有收到消息之前， 2 S -> 5 S -> 7S -> 11 S 投递4次
4. 如果收到消息 清理定时器，清理数据库消息
5. 4次投递都未确认消息，待用户下次登录再投递

以上消息确认重复机制，可以确保消息不丢失

服务端，发送消息代码如下：
```
    MillisecondList = [0, 1500, 1500, 3000, 1000, 3000, 5000],
    message_ds:send_next(Uid, DType, MsgId, Msg, MillisecondList),
```
* MillisecondList 频率控制列表，列表元素以单位为毫秒；
    * 0 标识第1条消息马上发送；
    * 1500 表示，消息1500毫米内没有ack，的情况下会再次投递
    * 其他逻辑元素逻辑如上
* Uid 用户ID
* MsgId 消息ID，消息唯一标识
* Msg 消息json文本

客户端收到消息后发送以下文本数据(对erlang来说是binary文本)，用于消息收到确认
```
CLIENT_ACK,type,msgid,did
```
4段信息以半角逗号（ , ）分隔：

* 第1段： CLIENT_ACK 为固定消息确认前缀
* 第2段： type 是IM消息类型
* 第3段： msgid 消息唯一ID
* 第4段： did 登录用户设备ID


测试验证数据，收到测试观察，该消息确认机制有效
```
http://coolaf.com/tool/chattest

ws://192.168.1.4:9800/ws/?authorization=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2Njg1Njk3MTY0MDksInN1YiI6InRrIiwidWlkIjoiYnltajVnIn0.zPojzN6IfxzIfU4CCJodguaAMcGPDx3XLTvou6-U9A8

CLIENT_ACK,type,msgid,did

CurrentUid = 13238,
MsgId = <<"msgid">>,
DID = <<"did">>.
webrtc_ws_logic:event(13238, <<"ios">>, MsgId, <<"Msg bin">>).


http://coolaf.com/tool/chattest

// 1
ws://192.168.1.4:9800/ws/?authorization=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2NjExMDM5NDQ5MTgsInN1YiI6InRrIiwidWlkIjoia3licWRwIn0.FYhYR0KzHZe9kHEeTbcYWwahyqLXBE7rUWaQgyI5I14

1 = imboy_hashids:uid_decode("kybqdp").
108 = imboy_hashids:uid_decode("7b4v1b").
{"id":"cdsgrbgppoodp0gvpb60","type":"C2C","from":"kybqdp","to":"7b4v1b","payload":{"msg_type":"text","text":"1to108"},"created_at":1668877742828}

108
ws://192.168.1.4:9800/ws/?authorization=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2NjkwNDc2Nzc3MDgsInN1YiI6InRrIiwidWlkIjoiN2I0djFiIn0.n19M6-kR_p4EtqJMst4kO1cqgdG5F2gyNY6QL46xpR8
{"id":"cdsgrbgppoodp0gvpb61","type":"C2C","to":"kybqdp","from":"7b4v1b","payload":{"msg_type":"text","text":"108to1"},"created_at":1668877742828}
```

token_ds:decrypt_token(<<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2OTQwNTI2NDEyNjQsInN1YiI6InJ0ayIsInVpZCI6IjlkNG5uayJ9.N7X7mpInEbiawIP7qiDOf00Gbmm4H-4HM2cAukQ-040">>).

{ok,513244,1694052641264,<<"rtk">>}

1694052641264 - imboy_dt:millisecond().
rtmp_proxy:run(1935, <<"rtmp://192.168.0.144/live_room">>).
