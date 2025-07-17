#!/bin/bash
# 一键批量添加虚拟IP，适合Linux，需root权限
# 用法: ./script/add_virtual_ips.sh eth0 10.0.0.10 10.0.0.109

DEV=$1
START_IP=$2
END_IP=$3

IFS='.' read -r i1 i2 i3 i4 <<< "$START_IP"
IFS='.' read -r e1 e2 e3 e4 <<< "$END_IP"

IP_LIST=()
for c in $(seq $i3 $e3); do
  if [ $c -eq $i3 ]; then
    d_start=$i4
  else
    d_start=0
  fi
  if [ $c -eq $e3 ]; then
    d_end=$e4
  else
    d_end=255
  fi
  for d in $(seq $d_start $d_end); do
    IP_LIST+=("$i1.$i2.$c.$d")
  done
done

for ip in "${IP_LIST[@]}"; do
  sudo ip addr add $ip/24 dev $DEV
done
