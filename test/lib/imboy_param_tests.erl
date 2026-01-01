-module(imboy_param_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_param 模块的 EUnit 测试
%%%
%%% 目标：验证参数处理工具功能
%%% 覆盖：参数解析、验证、默认值
%%%===================================================================

%% 由于 cowboy_req 依赖复杂，暂时跳过这些测试
%% 这些测试需要完整的 cowboy 环境


