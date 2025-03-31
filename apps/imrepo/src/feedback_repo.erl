-module (feedback_repo).
%%%
% feedback 相关操作都放到该模块，存储库模块
% feedback related operations are put in this module, repository module
%%%

-export ([tablename/0]).

-export ([add/11]).
-export([delete/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"feedback">>).


%%% 新增用户反馈
-spec add(integer(), binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, list(), list()} | {error, any()}.
% feedback_repo:add(Uid, Did, COS, COSV, AppVsn, ContactDetail, Body, Attach, FeedbackMd5)
add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach, FeedbackMd5) ->
    imboy_db:insert_into(tablename(), #{
        <<"user_id">> => Uid,  % 用户ID (整型)
        <<"device_id">> => Did,  % 设备ID (字符串)
        <<"client_operating_system">> => COS,  % 客户端操作系统 (字符串)
        <<"client_operating_system_vsn">> => COSV,  % 系统版本 (字符串)
        <<"app_vsn">> => AppVsn,  % 应用版本 (字符串)
        <<"type">> => Type,  % 反馈类型 (字符串)
        <<"rating">> => Rating,  % 评分 (原始数值，需保留类型)
        <<"contact_detail">> => ContactDetail,  % 联系方式 (字符串)
        <<"body">> => Body,  % 反馈内容 (字符串)
        <<"attach">> => Attach,  % 附件信息 (JSON字符串)
        <<"feedback_md5">> => FeedbackMd5,  % MD5校验值 (字符串)
        <<"status">> => 1,  % 状态 (整型 1-有效)
        <<"created_at">> => imboy_dt:now()  % 创建时间 (使用原生时间函数)
    }),
    ok.

-spec delete(integer(), binary()) -> ok.
delete(Uid, FeedbackId) ->
    Tb = tablename(),
    % 状态: -1 删除  0 禁用  1 启用 (待回复）  2 已回复  3 已完结（不允许回复了）
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE status = 1 AND user_id = $1 AND id = $2">>,
    imboy_db:execute(Sql, [Uid, FeedbackId]),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
