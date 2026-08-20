# ===================================================================
# Imboy Project Code Templates for erlang.mk
# Imboy 项目 erlang.mk 代码模板
# ===================================================================
# Templates for the 4-layer architecture: Handler -> Logic -> DS -> Repo.
# 对应四层架构 Handler -> Logic -> DS -> Repo 的代码模板。
#
# Usage (files land in the layer directory via tplp_ mappings below):
# 用法（通过下方 tplp_ 映射，生成文件直接落到对应层目录）：
#   make new t=imboy.rest_handler n=user_handler   -> src/api/user_handler.erl
#   make new t=imboy.logic n=user_logic            -> src/logic/user_logic.erl
#   make new t=imboy.repository n=user_repo        -> src/repo/user_repo.erl
#   make new t=imboy.ds n=user_ds                  -> src/ds/user_ds.erl
#
# Conventions enforced by these templates:
# 模板固化的项目约定：
#   - includes match real modules: log.hrl / common.hrl / error_code.hrl
#   - include 与真实模块一致：log.hrl / common.hrl / error_code.hrl
#   - logging via project macros (?ERROR_LOG/?WARN_LOG/...), not ?LOG_*
#   - 日志使用项目宏（?ERROR_LOG/?WARN_LOG/...），不用 kernel 的 ?LOG_*
#   - elib_pg: SELECT -> query/2, INSERT/UPDATE/DELETE -> execute/2
#     (RETURNING returns rows, so it goes through query/2)
#   - elib_pg：SELECT 用 query/2，INSERT/UPDATE/DELETE 用 execute/2
#     （带 RETURNING 的有返回行，走 query/2）
#   - handler replies via elib_response:handle_logic_result/2;
#     logic returns {ok, map() | list()} | {error, binary()}
#   - handler 统一用 elib_response:handle_logic_result/2 回复；
#     logic 返回 {ok, map() | list()} | {error, binary()}
#   - try/catch only at the HTTP boundary (handler), not in logic/ds
#   - try/catch 只放在 HTTP 边界（handler），logic/ds 不吞异常
# ===================================================================

# Output paths (erlang.mk tplp_ mechanism; template_name is replaced by n)
# 生成文件落点（erlang.mk 的 tplp_ 机制，template_name 会被替换为 n 参数）
tplp_imboy.rest_handler = src/api/template_name.erl
tplp_imboy.logic = src/logic/template_name.erl
tplp_imboy.repository = src/repo/template_name.erl
tplp_imboy.ds = src/ds/template_name.erl

# Cowboy REST Handler Template
# Cowboy REST 控制器模板
# Usage: make new t=imboy.rest_handler n=user_handler
define tpl_imboy.rest_handler
-module($(notdir $(n))).
%%%
% $(subst _handler,,$(notdir $(n))) 控制器模块
% $(subst _handler,,$(notdir $(n))) controller module
%%%

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API / 接口
%% ===================================================================

init(Req0, State0) ->
    Action = maps:get(action, State0, undefined),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

handle_action(demo_action, Req, State) -> demo_action(Req, State);
handle_action(Action, Req, _State) ->
    ?WARN_LOG("~p unknown action: ~p", [?MODULE, Action]),
    elib_response:error(Req, <<"unknown action">>, ?ERR_NOT_FOUND).

%% ===================================================================
%% Internal Function Definitions / 内部函数
%% ===================================================================

demo_action(Req0, State) ->
    try
        CurrentUid = maps:get(current_uid, State, 0),
        Params = elib_req:post_params(Req0),
        Val1 = maps:get(<<"val1">>, Params, <<>>),
        Val2 = maps:get(<<"val2">>, Params, <<>>),
        Result = $(subst _handler,,$(notdir $(n)))_logic:demo(CurrentUid, Val1, Val2),
        elib_response:handle_logic_result(Req0, Result)
    catch
        Class:Reason:Stacktrace ->
            ?ERROR_LOG("~p demo_action crashed: ~p:~p~n~p",
                [?MODULE, Class, Reason, Stacktrace]),
            elib_response:error(Req0, <<"internal server error">>, ?ERR_INTERNAL_SERVER_ERROR)
    end.
endef

# Business Logic Module Template
# 业务逻辑模块模板
# Usage: make new t=imboy.logic n=user_logic
define tpl_imboy.logic
-module($(notdir $(n))).
%%%
% $(subst _logic,,$(notdir $(n))) 业务逻辑模块
% $(subst _logic,,$(notdir $(n))) business logic module
%%%

-export([demo/3]).
-export([validate_params/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include("common.hrl").

%% ===================================================================
%% API / 接口
%% ===================================================================

%%% Returns {ok, Data} | {error, BinaryMsg} — handlers pass the result
%%% straight to elib_response:handle_logic_result/2.
%%% 返回 {ok, Data} | {error, BinaryMsg}，handler 直接把结果交给
%%% elib_response:handle_logic_result/2 回复。
-spec demo(Uid::integer(), Val1::binary(), Val2::binary()) ->
    {ok, [map()]} | {error, binary()}.
demo(Uid, Val1, Val2) when is_integer(Uid), is_binary(Val1), is_binary(Val2) ->
    case validate_params(Val1, Val2) of
        ok ->
            case $(subst _logic,,$(notdir $(n)))_repo:demo(Uid, Val1, Val2) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    ?ERROR_LOG("~p repo demo failed, uid=~p reason=~p", [?MODULE, Uid, Reason]),
                    {error, <<"operation failed">>}
            end;
        {error, Msg} ->
            {error, Msg}
    end;
demo(_Uid, _Val1, _Val2) ->
    {error, <<"invalid parameters">>}.

%% ===================================================================
%% Internal Function Definitions / 内部函数
%% ===================================================================

%%% Validate input parameters
%%% 校验输入参数
-spec validate_params(Val1::binary(), Val2::binary()) -> ok | {error, binary()}.
validate_params(Val1, _Val2) when byte_size(Val1) =:= 0 ->
    {error, <<"val1 is empty">>};
validate_params(_Val1, Val2) when byte_size(Val2) =:= 0 ->
    {error, <<"val2 is empty">>};
validate_params(Val1, _Val2) when byte_size(Val1) > 255 ->
    {error, <<"val1 is too long">>};
validate_params(_Val1, Val2) when byte_size(Val2) > 255 ->
    {error, <<"val2 is too long">>};
validate_params(_Val1, _Val2) ->
    ok.

%% ===================================================================
%% EUnit tests / EUnit 测试
%% ===================================================================

-ifdef(EUNIT).

validate_params_test() ->
    ?assertEqual(ok, validate_params(<<"test1">>, <<"test2">>)),
    ?assertEqual({error, <<"val1 is empty">>}, validate_params(<<>>, <<"test">>)),
    ?assertEqual({error, <<"val2 is empty">>}, validate_params(<<"test">>, <<>>)).

demo_test() ->
    ?assertEqual({error, <<"invalid parameters">>}, demo("not_integer", <<"val1">>, <<"val2">>)),
    ?assertEqual({error, <<"invalid parameters">>}, demo(123, "not_binary", <<"val2">>)).

-endif.
endef

# Repository Module Template
# 存储库模块模板
# Usage: make new t=imboy.repository n=user_repo
define tpl_imboy.repository
-module($(notdir $(n))).
%%%
% $(subst _repo,,$(notdir $(n))) 相关操作都放到该模块，存储库模块
% $(subst _repo,,$(notdir $(n))) related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([demo/3]).
-export([create/2]).
-export([find_by_id/1]).
-export([update/3]).
-export([delete/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include("common.hrl").

%% ===================================================================
%% API / 接口
%% ===================================================================

%%% Get table name
%%% 获取表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"$(subst _repo,,$(notdir $(n)))">>).

%%% Demo query — SELECT goes through elib_pg:query/2 and returns {ok, [map()]}.
%%% Replace the demo columns (val1/val2) with your actual table schema.
%%% 示例查询：SELECT 走 elib_pg:query/2，返回 {ok, [map()]}。
%%% 列名（val1/val2 等）需按实际表结构调整。
-spec demo(Uid::integer(), Val1::binary(), Val2::binary()) ->
    {ok, [map()]} | {error, term()}.
demo(Uid, Val1, Val2) when is_integer(Uid), is_binary(Val1), is_binary(Val2) ->
    Sql = <<"SELECT id, created_at FROM ", (tablename())/binary,
        " WHERE id = $1 AND val1 = $2 AND val2 = $3">>,
    elib_pg:query(Sql, [Uid, Val1, Val2]);
demo(_Uid, _Val1, _Val2) ->
    {error, invalid_parameters}.

%%% Create a new record — RETURNING yields rows, so query/2 is correct here.
%%% 创建记录：带 RETURNING 有返回行，因此这里用 query/2 是对的。
-spec create(Data::map(), Uid::integer()) -> {ok, integer()} | {error, term()}.
create(Data, Uid) when is_map(Data), is_integer(Uid) ->
    Sql = <<"INSERT INTO ", (tablename())/binary,
        " (data, created_by, created_at) VALUES ($1, $2, NOW()) RETURNING id">>,
    case elib_pg:query(Sql, [jsone:encode(Data), Uid]) of
        {ok, [#{<<"id">> := Id}]} ->
            {ok, Id};
        {error, Reason} ->
            ?ERROR_LOG("~p create failed: ~p", [?MODULE, Reason]),
            {error, {db_error, Reason}}
    end;
create(_Data, _Uid) ->
    {error, invalid_parameters}.

%%% Find record by ID
%%% 按 ID 查找记录
-spec find_by_id(Id::integer()) -> {ok, map()} | {error, not_found | term()}.
find_by_id(Id) when is_integer(Id) ->
    Sql = <<"SELECT id, data, created_by, created_at FROM ", (tablename())/binary,
        " WHERE id = $1">>,
    case elib_pg:query(Sql, [Id]) of
        {ok, []} ->
            {error, not_found};
        {ok, [#{<<"id">> := RecordId, <<"data">> := Data,
                <<"created_by">> := CreatedBy, <<"created_at">> := CreatedAt}]} ->
            {ok, #{
                id => RecordId,
                data => jsone:decode(Data),
                created_by => CreatedBy,
                created_at => CreatedAt
            }};
        {error, Reason} ->
            ?ERROR_LOG("~p find_by_id ~p failed: ~p", [?MODULE, Id, Reason]),
            {error, {db_error, Reason}}
    end;
find_by_id(_Id) ->
    {error, invalid_parameters}.

%%% Update record — no returned rows, so execute/2 returning {ok, Count}.
%%% 更新记录：无返回行，用 execute/2，返回 {ok, Count}。
-spec update(Id::integer(), Data::map(), Uid::integer()) -> ok | {error, term()}.
update(Id, Data, Uid) when is_integer(Id), is_map(Data), is_integer(Uid) ->
    Sql = <<"UPDATE ", (tablename())/binary,
        " SET data = $1, updated_by = $2, updated_at = NOW() WHERE id = $3">>,
    case elib_pg:execute(Sql, [jsone:encode(Data), Uid, Id]) of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            {error, not_found};
        {error, Reason} ->
            ?ERROR_LOG("~p update ~p failed: ~p", [?MODULE, Id, Reason]),
            {error, {db_error, Reason}}
    end;
update(_Id, _Data, _Uid) ->
    {error, invalid_parameters}.

%%% Delete record
%%% 删除记录
-spec delete(Id::integer()) -> ok | {error, term()}.
delete(Id) when is_integer(Id) ->
    Sql = <<"DELETE FROM ", (tablename())/binary, " WHERE id = $1">>,
    case elib_pg:execute(Sql, [Id]) of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            {error, not_found};
        {error, Reason} ->
            ?ERROR_LOG("~p delete ~p failed: ~p", [?MODULE, Id, Reason]),
            {error, {db_error, Reason}}
    end;
delete(_Id) ->
    {error, invalid_parameters}.

%% ===================================================================
%% EUnit tests / EUnit 测试
%% ===================================================================

-ifdef(EUNIT).

tablename_test() ->
    TableName = tablename(),
    ?assert(is_binary(TableName)),
    ?assert(byte_size(TableName) > 0).

validation_test() ->
    ?assertEqual({error, invalid_parameters}, demo("not_integer", <<"val1">>, <<"val2">>)),
    ?assertEqual({error, invalid_parameters}, create("not_map", 123)),
    ?assertEqual({error, invalid_parameters}, find_by_id("not_integer")).

-endif.
endef

# Domain Service Module Template
# 领域服务模块模板
# Usage: make new t=imboy.ds n=user_ds
define tpl_imboy.ds
-module($(notdir $(n))).
%%%
% $(subst _ds,,$(notdir $(n))) 领域服务模块
% $(subst _ds,,$(notdir $(n))) domain service module
%%%

-export([demo/3]).
-export([process_business_rule/2]).
-export([validate_domain_constraints/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include("common.hrl").

%% ===================================================================
%% API / 接口
%% ===================================================================

%%% Domain rules live here as pure functions; keep them side-effect free
%%% so they stay trivially testable. Repo access goes through logic or,
%%% for simple cases, directly as below.
%%% 领域规则以纯函数形式放在本层，保持无副作用、可直接单测。
%%% Repo 访问一般经 logic 层，简单场景可如本例直接调用。
-spec demo(Uid::integer(), Val1::binary(), Val2::binary()) ->
    {ok, [map()]} | {error, term()}.
demo(Uid, Val1, Val2) when is_integer(Uid), is_binary(Val1), is_binary(Val2) ->
    case validate_domain_constraints(#{uid => Uid, val1 => Val1, val2 => Val2}) of
        ok ->
            case process_business_rule(Val1, Val2) of
                {ok, _ProcessedData} ->
                    $(subst _ds,,$(notdir $(n)))_repo:demo(Uid, Val1, Val2);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
demo(_Uid, _Val1, _Val2) ->
    {error, invalid_parameters}.

%% ===================================================================
%% Internal Function Definitions / 内部函数
%% ===================================================================

%%% Example pure domain rule: Val1 and Val2 cannot be the same.
%%% 示例纯领域规则：Val1 与 Val2 不允许相同。
-spec process_business_rule(Val1::binary(), Val2::binary()) ->
    {ok, map()} | {error, term()}.
process_business_rule(Val1, Val2) ->
    case Val1 =:= Val2 of
        true ->
            {error, values_cannot_be_same};
        false ->
            {ok, #{combined => <<Val1/binary, "_", Val2/binary>>}}
    end.

%%% Validate domain-specific constraints
%%% 校验领域约束
-spec validate_domain_constraints(Data::map()) -> ok | {error, term()}.
validate_domain_constraints(#{uid := Uid, val1 := Val1, val2 := Val2}) ->
    Validations = [
        {uid_positive, Uid > 0},
        {val1_not_empty, byte_size(Val1) > 0},
        {val2_not_empty, byte_size(Val2) > 0},
        {val1_max_length, byte_size(Val1) =< 100},
        {val2_max_length, byte_size(Val2) =< 100}
    ],
    case lists:filter(fun({_Rule, Result}) -> not Result end, Validations) of
        [] ->
            ok;
        [{FailedRule, _} | _] ->
            {error, {domain_constraint_failed, FailedRule}}
    end;
validate_domain_constraints(_Data) ->
    {error, invalid_data_structure}.

%% ===================================================================
%% EUnit tests / EUnit 测试
%% ===================================================================

-ifdef(EUNIT).

process_business_rule_test() ->
    ?assertEqual({error, values_cannot_be_same}, process_business_rule(<<"test">>, <<"test">>)),
    ?assertMatch({ok, _}, process_business_rule(<<"test1">>, <<"test2">>)).

validate_domain_constraints_test() ->
    ValidData = #{uid => 123, val1 => <<"test1">>, val2 => <<"test2">>},
    ?assertEqual(ok, validate_domain_constraints(ValidData)),

    InvalidData1 = #{uid => -1, val1 => <<"test1">>, val2 => <<"test2">>},
    ?assertMatch({error, {domain_constraint_failed, uid_positive}},
                 validate_domain_constraints(InvalidData1)),

    InvalidData2 = #{uid => 123, val1 => <<>>, val2 => <<"test2">>},
    ?assertMatch({error, {domain_constraint_failed, val1_not_empty}},
                 validate_domain_constraints(InvalidData2)).

demo_test() ->
    ?assertEqual({error, invalid_parameters}, demo("not_integer", <<"val1">>, <<"val2">>)),
    ?assertEqual({error, invalid_parameters}, demo(123, "not_binary", <<"val2">>)).

-endif.
endef
