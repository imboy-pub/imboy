PROJECT = imapi
PROJECT_DESCRIPTION = 基于cowboy的一款即时聊天软件 api 模块
PROJECT_VERSION = 0.1.17
# Whitespace to be used when creating files from templates.
SP = 4

# Make sure we know where the applications are located.
ROOT_DIR ?= ../..
APPS_DIR ?= ..
DEPS_DIR ?= ../../deps


include $(ROOT_DIR)/include/deps.mk

DEPS ?=  ranch cowlib cowboy

include $(ROOT_DIR)/erlang.mk
