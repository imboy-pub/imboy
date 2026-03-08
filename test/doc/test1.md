# 100 万级 WebSocket 在线连接压测记录（历史归档）

> 文档类型：历史压测记录  
> 说明：本文档保留原始监控截图与现场摘录，方便后续做容量估算、问题回溯与测试方法复用；不建议直接作为 2026 年对外交付承诺口径。

## 1. 测试目标

验证在长时间在线场景下，服务端维持 100 万级 TCP / WebSocket 在线连接时的资源占用和整体稳定性表现。

本次测试关注：

- 长连接在线规模；
- 服务端 CPU / 内存 / 网络指标；
- 客户端压测机负载情况；
- 长时间运行下的连接稳定性。

## 2. 测试环境

### 2.1 系统环境

- 操作系统：CentOS Linux release 8.1.1911 (Core)

### 2.2 客户端环境

- 客户端机器数：17 台阿里云服务器
- 规格：CentOS 8，2 vCPU，4 GB 内存
- 带宽：上行 12 Mb/s，下行 1000 Mb/s

### 2.3 服务端环境

- 操作系统：CentOS 8
- 云主机规格：阿里云 `ecs.sn1ne.2xlarge`
- 配置：8 核 16 GB
- 带宽：上行 12 Mb/s，下行 1000 Mb/s

### 2.4 网络拓扑

- 本次记录未单独保存网络拓扑图。

## 3. 测试工具

- 自制 Erlang WebSocket 客户端：`wsc1.erl`
- 自制 Erlang WebSocket 客户端：`wsc2.erl`
- 阿里云监控平台

## 4. 测试场景

- 协议形态：WebSocket 长连接在线
- 持续时长：约 90 分钟
- 在线规模：TCP 并发连接数 100 万+
- 本轮重点：在线承载与稳定性，不关注业务 TPS 峰值
- 记录结果：成功率约 `99.8994%`

## 5. 关键观察

- 服务端在线连接数稳定在 100 万级别；
- 服务端 CPU 大致在 `500%` 左右波动；
- 服务端内存占用约 `10 GB`；
- 停服前抽样观察到 `ESTABLISHED` 连接数约在 `103 万 ~ 105 万` 区间。

## 6. 服务端监控截图

### 6.1 TCP 连接数

<img src="test1Service/TCP连接数(Count).png" width="95%"/>

### 6.2 内存使用量

<img src="test1Service/内存使用量.png" width="95%"/>

### 6.3 CPU 使用率

<img src="test1Service/CPU使用率.png" width="95%"/>

### 6.4 网络流入流出数据包数（pps）

<img src="test1Service/网络流入流出数据包数(pps).png" width="95%"/>

### 6.5 网络流入流出速率（bps）

<img src="test1Service/网络流入流出速率(bps).png" width="95%"/>

### 6.6 磁盘与系统负载补充截图

#### 磁盘使用率

<img src="test1Service/磁盘使用率.png" width="95%"/>

#### Inode 使用率

<img src="test1Service/Inode使用率.png" width="95%"/>

#### 系统平均负载

<img src="test1Service/系统平均负载.png" width="95%"/>

#### 读写字节数（Bps）

<img src="test1Service/读写字节数(Bps).png" width="95%"/>

#### 读写请求数（CountPerSecond）

<img src="test1Service/读写请求数(CountPerSecond).png" width="95%"/>

## 7. 客户端监控截图

### 7.1 CPU 使用率

<img src="test1Client/CPU使用率.png" width="95%"/>

### 7.2 内存使用量

<img src="test1Client/内存使用量.png" width="95%"/>

### 7.3 TCP 连接数

<img src="test1Client/TCP连接数(Count).png" width="95%"/>

### 7.4 网络流入流出数据包数（pps）

<img src="test1Client/网络流入流出数据包数(pps).png" width="95%"/>

### 7.5 网络流入流出速率（bps）

<img src="test1Client/网络流入流出速率(bps).png" width="95%"/>

### 7.6 磁盘与系统负载补充截图

#### 磁盘使用率

<img src="test1Client/磁盘使用率.png" width="95%"/>

#### Inode 使用率

<img src="test1Client/Inode使用率.png" width="95%"/>

#### 系统平均负载

<img src="test1Client/系统平均负载.png" width="95%"/>

#### 读写字节数（Bps）

<img src="test1Client/读写字节数(Bps).png" width="95%"/>

#### 读写请求数（CountPerSecond）

<img src="test1Client/读写请求数(CountPerSecond).png" width="95%"/>

## 8. Erlang 虚拟机参数

```text
[root@launch-master test]# cat config/vm.args
-name test@127.0.0.1
-setcookie test
-heart

+K true
+A 1024
+P 20480000
+Q 20480000
+S 6
+MSe true
```

## 9. 停服前现场摘录

以下内容保留原始终端摘录，便于回看测试现场：

```text
[root@launch-master ~]# free -h
              total        used        free      shared  buff/cache   available
Mem:           15Gi        10Gi       3.8Gi       0.0Ki       1.4Gi       6.0Gi
Swap:            0B          0B          0B
[root@launch-master ~]# netstat -n | awk '/^tcp/ {++S[$NF]} END {for(a in S) print a, S[a]}'
SYN_RECV 346
ESTABLISHED 1030314
FIN_WAIT1 540
FIN_WAIT2 702
TIME_WAIT 33
[root@launch-master ~]# ps aux|grep cloudmonitor
root      1897  0.0  0.0  23020  3264 ?        Ss   16:58   0:00 /usr/local/cloudmonitor/bin/argusagent -d
root      1899  5.6  0.1 970604 19468 ?        Sl   16:58   3:29 /usr/local/cloudmonitor/bin/argusagent
root      4800  0.0  0.0  12320  1068 pts/0    S+   17:59   0:00 grep --color=auto cloudmonitor
[root@launch-master ~]# free -h
              total        used        free      shared  buff/cache   available
Mem:           15Gi        10Gi       3.7Gi       0.0Ki       1.4Gi       5.7Gi
Swap:            0B          0B          0B
[root@launch-master ~]# netstat -n | awk '/^tcp/ {++S[$NF]} END {for(a in S) print a, S[a]}'
SYN_RECV 864
ESTABLISHED 1043283
FIN_WAIT1 1209
FIN_WAIT2 1163
SYN_SENT 1
TIME_WAIT 150
[root@launch-master ~]# free -h
              total        used        free      shared  buff/cache   available
Mem:           15Gi        10Gi       3.6Gi       0.0Ki       1.4Gi       5.3Gi
Swap:            0B          0B          0B
[root@launch-master ~]# ps aux|grep cloudmonitor
root      1897  0.0  0.0  23020  3264 ?        Ss   16:58   0:00 /usr/local/cloudmonitor/bin/argusagent -d
root      1899  6.2  0.1 970604 19884 ?        Sl   16:58   4:45 /usr/local/cloudmonitor/bin/argusagent
root      4840  0.0  0.0  12320  1076 pts/0    S+   18:14   0:00 grep --color=auto cloudmonitor
[root@launch-master ~]# netstat -n | awk '/^tcp/ {++S[$NF]} END {for(a in S) print a, S[a]}'
SYN_RECV 498
ESTABLISHED 1054101
FIN_WAIT1 937
FIN_WAIT2 1017
SYN_SENT 2
TIME_WAIT 67
[root@launch-master ~]# netstat -n | awk '/^tcp/ {++S[$NF]} END {for(a in S) print a, S[a]}'
SYN_RECV 686
ESTABLISHED 1051642
FIN_WAIT1 991
FIN_WAIT2 1139
SYN_SENT 2
TIME_WAIT 114
[root@launch-master ~]# netstat -n | awk '/^tcp/ {++S[$NF]} END {for(a in S) print a, S[a]}'
SYN_RECV 523
ESTABLISHED 1053550
FIN_WAIT1 1038
FIN_WAIT2 1143
TIME_WAIT 80
[root@launch-master ~]# netstat -n | awk '/^tcp/ {++S[$NF]} END {for(a in S) print a, S[a]}'
SYN_RECV 502
ESTABLISHED 1053268
FIN_WAIT1 999
FIN_WAIT2 1036
SYN_SENT 1
```
