
CentOS服务器配置
```
cat /etc/redhat-release
CentOS Linux release 8.1.1911 (Core)
CentOS Linux release 8.1.1911 (Core)

getconf LONG_BIT
64
64

grep MemTotal /proc/meminfo
MemTotal:       15723044 kB
MemTotal:        3724936 kB

grep 'model name' /proc/cpuinfo
model name  : Intel(R) Xeon(R) Platinum 8269CY CPU @ 2.50GHz
model name  : Intel(R) Xeon(R) Platinum 8269CY CPU @ 2.50GHz
model name  : Intel(R) Xeon(R) Platinum 8269CY CPU @ 2.50GHz
model name  : Intel(R) Xeon(R) Platinum 8269CY CPU @ 2.50GHz
model name  : Intel(R) Xeon(R) Platinum 8269CY CPU @ 2.50GHz
model name  : Intel(R) Xeon(R) Platinum 8269CY CPU @ 2.50GHz
model name  : Intel(R) Xeon(R) Platinum 8269CY CPU @ 2.50GHz
model name  : Intel(R) Xeon(R) Platinum 8269CY CPU @ 2.50GHz

model name  : Intel(R) Xeon(R) Platinum 8269CY CPU @ 2.50GHz
model name  : Intel(R) Xeon(R) Platinum 8269CY CPU @ 2.50GHz

100Mbps 按量收费
按量12Mbps 峰值带宽

服务端
公网入带宽最大值： 2000Mb/s
公网出带宽最大值： 100Mb/s

客服端
公网入带宽最大值： 1000Mb/s
公网出带宽最大值： 12Mb/s

实例规格： ecs.sn1ne.2xlarge
网络(内网)内网带宽：2.00Gbps 内网收发包：100万PPS

实例规格： ecs.c6.large
网络(内网)内网带宽：1.00Gbps 内网收发包：30万PPS


//查看磁盘挂载情况
df -l
40G
40G
```

CentOS7/8服务器优化

文件句柄限制
```
ulimit -n
// 临时修改：
ulimit -n 2000000
```
vim /etc/security/limits.conf
```
root soft nofile 2000000
root hard nofile 2000000
* soft nofile 2000000
* hard nofile 2000000


       说明：* 代表针对所有用户
            noproc 是代表最大进程数
            nofile 是代表最大文件打开数

vim /etc/sysctl.conf
    32768   60999
net.ipv4.ip_local_port_range = 1025 65535
net.ipv4.tcp_tw_reuse = 1
# 超过这个数量，系统将不再接受新的TCP连接请求，一定程度上可以防止系统资源耗尽。可根据情况增加该值以接受更多的连接请求。默认值1024##
net.ipv4.tcp_max_syn_backlog = 65536


sysctl -p /etc/sysctl.conf

cat /proc/sys/net/ipv4/ip_local_port_range

Linux系统级的最大打开文件数限制
cat /proc/sys/fs/file-max
echo 381987 > /proc/sys/fs/file-max

查看TCP 数量
netstat -n | awk '/^tcp/ {++S[$NF]} END {for(a in S) print a, S[a]}'
```
ws 空闲下有 erlang:system_info(process_count). 1172  TCP （ESTABLISHED 2055 TIME_WAIT 4）

ws://local.imboy.leeyi.net:9800/stress_testing?token=ef9a3HMAhATfw40LZ38w1Ilim9zprY4B9SfsMgb4ucw=

erl +Q 1048576 +P 1048576 -env ERL_MAX_PORTS 1048576 -env ERTS_MAX_PORTS 1048576 -pa ../ebin


message_1(etimedout) -> <<"connection timed out">>;
message_1(nxdomain) -> <<"non-existing domain">>;
message_1(enotconn) -> <<"socket is not connected">>;

message_1(eaddrinuse) -> <<"address already in use">>;
message_1(eaddrnotavail) -> <<"can't assign requested address">>;
message_1(econnreset) -> <<"connection reset by peer">>;
message_1(econnrefused) -> <<"connection refused">>;
message_1(enfile) -> <<"file table overflow">>;
message_1(ehostunreach) -> <<"host is unreachable">>;
message_1(enomem) -> <<"not enough memory">>;

# 服务器选择
内网收发包 和有关
```
使用 阿里云 8核16G C5主机（30万PPS），23万左右TCP的时候，“cloudmonitor插件”不能够正常工作
使用 阿里云 8核16G ecs.sn1ne.2xlarge主机（100万PPS），97万左右TCP的时候，“cloudmonitor插件”不能够正常工作

阿里云计算网络增强型sn1ne属于网络方面性能比较强悍一些的主机。具体体现在网络宽带能力和收发包能力强于计算型 C5主机，计算网络增强型传输速度更快。

阿里云ECS云服务器内网收发包是指内部网络每秒发包数量，单位是PPS（Packets Per Second，每秒发包数量），内网收发包是衡量云服务器内部网络性能的一项指标。

```
