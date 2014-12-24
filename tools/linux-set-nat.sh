#!/usr/bin/env bash

# Enable IP forwarding
echo 1 > /proc/sys/net/ipv4/ip_forward

# Configure iptables to enable NAT for eth0
iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE
