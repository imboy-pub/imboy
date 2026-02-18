%%%-------------------------------------------------------------------
%%% @doc
%%% 群成员角色常量定义
%%%
%%% 角色定义：
%%% - ROLE_UNDEFINED    = 0 : 未定义/非成员
%%% - ROLE_MEMBER       = 1 : 普通成员
%%% - ROLE_GUEST        = 2 : 嘉宾
%%% - ROLE_ADMIN        = 3 : 管理员
%%% - ROLE_OWNER        = 4 : 群主
%%% - ROLE_VICE_OWNER   = 5 : 副群主（新增）
%%%
%%% 权限矩阵：
%%% | 角色      | 踢人 | 禁言 | 公告 | 解散群 | 转让群 | 修改群信息 |
%%% |-----------|------|------|------|--------|--------|------------|
%%% | 群主 (4)  | ✓    | ✓    | ✓    | ✓      | ✓      | ✓          |
%%% | 副群主 (5)| ✓    | ✓    | ✓    | ✗      | ✗      | ✓          |
%%% | 管理员 (3)| ✓    | ✓    | ✓    | ✗      | ✗      | ✗          |
%%% | 嘉宾 (2)  | ✗    | ✗    | ✗    | ✗      | ✗      | ✗          |
%%% | 成员 (1)  | ✗    | ✗    | ✗    | ✗      | ✗      | ✗          |
%%% @end
%%%-------------------------------------------------------------------

-define(ROLE_UNDEFINED, 0).     % 未定义
-define(ROLE_MEMBER, 1).        % 普通成员
-define(ROLE_GUEST, 2).         % 嘉宾
-define(ROLE_ADMIN, 3).         % 管理员
-define(ROLE_OWNER, 4).         % 群主
-define(ROLE_VICE_OWNER, 5).    % 副群主

%% 角色名称映射（用于显示）
-define(ROLE_NAMES, #{
    ?ROLE_MEMBER     => <<"普通成员"/utf8>>,
    ?ROLE_GUEST      => <<"嘉宾"/utf8>>,
    ?ROLE_ADMIN      => <<"管理员"/utf8>>,
    ?ROLE_OWNER      => <<"群主"/utf8>>,
    ?ROLE_VICE_OWNER => <<"副群主"/utf8>>
}).

%% 检查是否为管理员角色（管理员、群主、副群主）
-define(IS_ADMIN_ROLE(Role), (Role >= ?ROLE_ADMIN andalso Role =< ?ROLE_VICE_OWNER)).

%% 检查是否为高级管理员角色（群主、副群主）
-define(IS_SENIOR_ADMIN_ROLE(Role), (Role =:= ?ROLE_OWNER orelse Role =:= ?ROLE_VICE_OWNER)).

%% 检查是否为群主或副群主
-define(IS_OWNER_OR_VICE_OWNER(Role), (Role =:= ?ROLE_OWNER orelse Role =:= ?ROLE_VICE_OWNER)).

%% 检查是否为群主
-define(IS_OWNER_ROLE(Role), (Role =:= ?ROLE_OWNER)).
