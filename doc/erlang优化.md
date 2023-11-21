
https://blog.suexcxine.win/2020/04/18/erlang_vm_flags/

```
+K true
开启epoll调度，在linux中开启epoll，会大大增加调度的效率

+A 1024
异步线程池，为某些port调用服务

+P 2048000
最大进程数

+Q 2048000
最大port数

+sbt db
绑定调度器，绑定后调度器的任务队列不会在各个CPU线程之间跃迁，结合sub使用，可以让CPU负载均衡的同时也避免了大量的跃迁发生。

注意：一个linux系统中，最好只有一个evm开启此选项，若同时有多个erlang虚拟机在系统中运行，还是关闭为好


+sub true
开启CPU负载均衡，false的时候是采用的CPU密集调度策略，优先在某个CPU线程上运行任务，直到该CPU负载较高为止。

+swct eager
此选项设置为eager后，CPU将更频繁的被唤醒，可以增加CPU利用率

+spp true
开启并行port并行调度队列，当开启后会大大增加系统吞吐量，如果关闭，则会牺牲吞吐量换取更低的延迟。

+zdbbl 65536
分布式erlang的端口buffer大小，当buffer满的时候，向分布式的远程端口发送消息会阻塞

+sbwt
https://stressgrid.com/blog/beam_cpu_usage/

+stbt Rabbit MQ default
https://www.rabbitmq.com/runtime.html#scheduler-bind-type

someone says using +stbt ts reduced context switching for 4 times
https://github.com/rabbitmq/rabbitmq-server/issues/612

Whatsapp use tnnps
https://www.youtube.com/watch?v=tW49z8HqsNw&feature=youtu.be&t=11m2s


+zdbbl

Rabbit MQ explain
https://www.rabbitmq.com/runtime.html#distribution-buffer
```

### +sbwt、+sbwtdcpu、 和+sbwtdio
https://stressgrid.com/blog/beam_cpu_usage/
```
在这种情况下，其他（非 BEAM）内核线程可能会收到不公平的低 CPU 时间片。此外，当在云中的突发性能实例上运行BEAM时，繁忙的等待可能会导致花费不必要的CPU积分。

为了缓解这个问题，BEAM 虚拟机有一组选项来调节 VM 完成的繁忙等待量。这些选项是+sbwt、+sbwtdcpu、 和+sbwtdio。当设置为none它们时，它们分别禁用主调度程序、脏 CPU 调度程序和脏 IO 调度程序中的忙等待。
```
