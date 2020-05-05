PROJECT = imboy
PROJECT_DESCRIPTION = 即时聊天软件
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = ssl mnesia
DEPS = sync lager poolboy mysql jsx cowboy
dep_cowboy_commit = 2.7.0
dep_lager_commit = 3.6.8

dep_sync = git https://github.com/rustyio/sync.git master
dep_poolboy = git https://github.com/devinus/poolboy 1.5.2
dep_mysql = git https://github.com/mysql-otp/mysql-otp 1.6.0

DEP_PLUGINS = cowboy

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
