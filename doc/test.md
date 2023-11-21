
## test_unit

## 压力测试
```

打开文件数 for mac
sudo launchctl limit maxfiles
sudo launchctl limit maxfiles 2097152 2097152
sudo ulimit -n 2097152

sysctl net.inet.ip.portrange.first net.inet.ip.portrange.last

## 及高范围
net.inet.ip.portrange.hifirst: 49152
net.inet.ip.portrange.hilast: 65535

sysctl -w net.inet.ip.portrange.first=1025
sysctl -w net.inet.ip.portrange.last=655350
sysctl -w net.inet.ip.tcp_rmem=655350


HAProxy + Docker * N + K8S + mnesia 集群
erlang:system_info(port_limit).

locust -f src/imboy.py --no-web -c 20000 -r 1000 -t 600s --logfile=logs/imboy-no-web.log
length(chat_store_repo:lookall()).

参考：

http://m.udpwork.com/item/11782.html
https://cloud.tencent.com/developer/article/1422476
https://www.yuanmomo.net/2019/07/26/mac-max-connections-config/
https://colobu.com/2014/09/18/linux-tcpip-tuning/

https://www.cnblogs.com/duanxz/p/4464178.html 单服务器最大tcp连接数及调优汇总

https://blog.51cto.com/yaocoder/1312821

http://hk.uwenku.com/question/p-tgiqupmb-oc.html

https://knowledge.zhaoweiguo.com/8tools/mqtts/emqtts/emqtt_tune.html

https://studygolang.com/articles/2416

https://www.iteye.com/blog/mryufeng-475003  erlang 节点间通讯的通道微调

http://www.wangxingrong.com.cn/archives/tag/百万并发连接服务器

https://qunfei.wordpress.com/2016/09/20/from-c10k-to-c100k-problem-push-over-1000000-messages-to-web-clients-on-1-machine-simultaneously/

https://stackoverflow.com/questions/32711242/erlang-simultaneously-connect-1m-clients

https://colobu.com/2015/05/22/implement-C1000K-servers-by-spray-netty-undertow-and-node-js

https://blog.csdn.net/zcc_0015/article/details/26407683 Linux下基于Erlang的高并发TCP连接压力实验

https://github.com/smallnest/C1000K-Servers

100万并发连接服务器笔记之Erlang完成1M并发连接目标 https://blog.csdn.net/shallowgrave/article/details/19990345?utm_medium=distribute.pc_relevant.none-task-blog-BlogCommendFromBaidu-5.nonecase&depth_1-utm_source=distribute.pc_relevant.none-task-blog-BlogCommendFromBaidu-5.nonecase



```

docker run -it --rm --name imboy-1 -p 9801:9800 -v "$PWD":/usr/src/imboy -w /usr/src/imboy erlang

// 后台运行
docker-compose up -d
docker-compose -f docker-local.yml up -d


下面的命令增加了19个IP地址，其中一个给服务器用
sudo ifconfig lo0 alias 127.0.0.10
sudo ifconfig lo0 alias 127.0.0.11
length(chat_store_repo:lookall()).

sudo ifconfig lo0 -alias 127.0.0.10
sudo ifconfig lo0 -alias 127.0.0.11

 Erlang虚拟机默认的端口上限为65536, erlang17通过erl +Q 1000000可以修改端口上限为1000000,利用erlang:system_info(port_limit)进行查询，系统可以打开的最大文件描述符可以通过erlang:system_info(check_io)中的max_fds进行查看，查看系统当前port数量可以用erlang:length(erlang:ports())得到

erlang:system_info(port_limit)
erlang:system_info(check_io)
erlang:length(erlang:ports()).

Pid = spawn(fun() -> etop:start([{output, text}, {interval, 1}, {lines, 20}, {sort, memory}]) end).

```
查看TCP 数量
netstat -n | awk '/^tcp/ {++S[$NF]} END {for(a in S) print a, S[a]}'
ESTABLISHED 28705

free -h
              total        used        free      shared  buff/cache   available
Mem:           3.7G        2.8G        774M        452K        140M        726M
Swap:            0B          0B          0B

查看 pid
 pmap -d 6380
