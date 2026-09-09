#!/bin/bash
# 收尾：halt 冒烟节点并确认端口/epmd 释放（用户 9800/9801 节点不受影响）。
erl -noshell -name r9halt@127.0.0.1 -setcookie imboy_smoke_ck -eval '
net_adm:ping('"'"'imboy_smoke@127.0.0.1'"'"'), rpc:call('"'"'imboy_smoke@127.0.0.1'"'"', erlang, halt, []), halt(0).'
sleep 2
epmd -names | grep imboy_smoke && echo "WARN: imboy_smoke 仍在 epmd" || echo "EPMD_CLEAN"
lsof -i :9811 >/dev/null 2>&1 && echo "WARN: 9811 仍监听" || echo "PORT_9811_CLEAN"
