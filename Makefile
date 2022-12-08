PROJECT = imboy
PROJECT_DESCRIPTION = 基于cowboy的一款即时聊天软件
PROJECT_VERSION = 0.1.0

#LOCAL_DEPS 本地依赖比较容易理解，就是otp内部项目的依赖
LOCAL_DEPS = ssl mnesia
# erlang.mk会保证 DEPS依赖的包能运行在shell、run、tests命令的时候
DEPS = goldrush lager poolboy mysql jsone ranch cowlib cowboy jsx jwerl hashids depcache recon observer_cli gen_smtp syn sync qdate

# 如果依赖包不用在erlang运行的时候跑的话，那就把它设置为BUILD_DEPS就行了，这样就只有构建的时候会用到
BUILD_DEPS = elvis_mk bbmustache relx

DEP_PLUGINS = cowboy elvis_mk

# 专为测试用的TEST_DEPS,只有当测试的时候才会运行
# TEST_DEPS = sync

SP = 4
include include/deps.mk

# http://erlang.org/doc/apps/edoc/chapter.html#Introduction
DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}

ifeq ($(IMBOYENV),prod)
	RELX_CONFIG = $(CURDIR)/relx.prod.config
else ifeq ($(IMBOYENV),test)
	RELX_CONFIG = $(CURDIR)/relx.test.config
else ifeq ($(IMBOYENV),dev)
	RELX_CONFIG = $(CURDIR)/relx.dev.config
else ifeq ($(IMBOYENV),local)
	RELX_CONFIG = $(CURDIR)/relx.local.config
	ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform, debug_info}'
else
	RELX_CONFIG = $(CURDIR)/relx.config
endif
dep_cowboy_commit = 2.9.0
dep_lager_commit = 3.9.2

# 生成文档的时候会被用到的依赖项
# DOC_DEPS =
# 用户执行make shell命令的时候会用到的依赖
# SHELL_DEPS =

include erlang.mk
include include/tpl.mk
include include/cli.mk

APP_VERSION = $(shell cat $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/version)

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
