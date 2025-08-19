# ===================================================================
# Imboy Project Code Templates for erlang.mk
# ===================================================================
# This file contains optimized Erlang code templates for the Imboy project.
# These templates follow erlang.mk conventions and best practices.
#
# Available Templates:
#   - tpl_imboy.rest_handler: Cowboy REST API handler
#   - tpl_imboy.logic:        Business logic module
#   - tpl_imboy.repository:   Data access layer module
#   - tpl_imboy.ds:          Domain service module
#
# Usage Examples:
#   make new t=imboy.rest_handler n=apps/imapi/src/user_handler
#   make new t=imboy.logic n=apps/imapi/src/user_logic
#   make new t=imboy.repository n=apps/imrepo/src/user_repo
#   make new t=imboy.ds n=apps/imds/src/user_ds
#
# Features:
#   - Proper error handling with try-catch blocks
#   - Type specifications (-spec) for all exported functions
#   - Input validation with guard clauses
#   - Comprehensive logging using imlib log macros
#   - EUnit test templates
#   - Consistent code formatting and documentation
# ===================================================================

# Cowboy REST Handler Template
# Usage: make new t=imboy.rest_handler n=apps/imapi/src/user_handler
define tpl_imboy.rest_handler
-module($(notdir $(n))).
%%%
% $(subst _handler,,$(notdir $(n))) 控制器模块
% $(subst _handler,,$(notdir $(n))) controller module
%%%
-behaviour(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% Cowboy REST API
%% ===================================================================

init(Req0, State0) ->
    Action = maps:get(action, State0, undefined),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    
    Req1 = case Action of
        demo_action ->
            demo_action(Method, Req0, State);
        undefined ->
            ?LOG_WARNING("No action specified in state: ~p", [State0]),
            cowboy_req:reply(400, #{}, <<"Bad Request">>, Req0);
        _ ->
            ?LOG_WARNING("Unknown action: ~p", [Action]),
            cowboy_req:reply(404, #{}, <<"Not Found">>, Req0)
    end,
    {ok, Req1, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

demo_action(<<"GET">>, Req0, _State) ->
    Body = jsone:encode(#{status => ok, message => <<"Hello World">>}),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json; charset=utf-8">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, Body, Req0).

demo_action(<<"POST">>, Req0, State) ->
    try
        CurrentUid = maps:get(current_uid, State, 0),
        PostVals = imboy_req:post_params(Req0),
        Val1 = proplists:get_value(<<"val1">>, PostVals, <<>>),
        Val2 = proplists:get_value(<<"val2">>, PostVals, <<>>),
        
        Result = $(subst _handler,,$(notdir $(n)))_logic:demo(CurrentUid, Val1, Val2),
        imboy_response:success(Req0, Result, <<"Operation successful">>)
    catch
        Error:Reason:Stacktrace ->
            ?LOG_ERROR("Error in demo_action: ~p:~p~n~p", [Error, Reason, Stacktrace]),
            imboy_response:error(Req0, 500, <<"Internal Server Error">>)
    end;

demo_action(Method, Req0, _State) ->
    ?LOG_WARNING("Unsupported method: ~p", [Method]),
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

handle_json(Req, State) ->
    Body = jsone:encode(#{status => ok, data => #{}}),
    {Body, Req, State}.

%% ===================================================================
%% EUnit tests
%% ===================================================================

-ifdef(EUNIT).

init_test() ->
    State = #{action => demo_action, current_uid => 123},
    Req = #{method => <<"GET">>},
    ?assertMatch({ok, _, _}, init(Req, State)).

-endif.
endef

# Business Logic Module Template
# Usage: make new t=imboy.logic n=apps/imapi/src/user_logic
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
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% Demo method with validation and error handling
-spec demo(Uid::integer(), Val1::binary(), Val2::binary()) -> 
    {ok, term()} | {error, term()}.
demo(Uid, Val1, Val2) when is_integer(Uid), is_binary(Val1), is_binary(Val2) ->
    try
        case validate_params(Val1, Val2) of
            ok ->
                Result = $(subst _logic,,$(notdir $(n)))_repo:demo(Uid, Val1, Val2),
                case Result of
                    {ok, Data} ->
                        ?LOG_INFO("Demo operation successful for uid: ~p", [Uid]),
                        {ok, Data};
                    {error, Reason} ->
                        ?LOG_ERROR("Demo operation failed for uid: ~p, reason: ~p", [Uid, Reason]),
                        {error, Reason}
                end;
            {error, ValidationError} ->
                ?LOG_WARNING("Validation failed for uid: ~p, error: ~p", [Uid, ValidationError]),
                {error, ValidationError}
        end
    catch
        Error:Reason:Stacktrace ->
            ?LOG_ERROR("Exception in demo/3: ~p:~p~n~p", [Error, Reason, Stacktrace]),
            {error, internal_error}
    end;
demo(Uid, Val1, Val2) ->
    ?LOG_ERROR("Invalid parameters: Uid=~p, Val1=~p, Val2=~p", [Uid, Val1, Val2]),
    {error, invalid_parameters}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%%% Validate input parameters
-spec validate_params(Val1::binary(), Val2::binary()) -> ok | {error, term()}.
validate_params(Val1, Val2) when byte_size(Val1) =:= 0 ->
    {error, val1_empty};
validate_params(Val1, Val2) when byte_size(Val2) =:= 0 ->
    {error, val2_empty};
validate_params(Val1, Val2) when byte_size(Val1) > 255 ->
    {error, val1_too_long};
validate_params(Val1, Val2) when byte_size(Val2) > 255 ->
    {error, val2_too_long};
validate_params(_Val1, _Val2) ->
    ok.

%% ===================================================================
%% EUnit tests
%% ===================================================================

-ifdef(EUNIT).

validate_params_test() ->
    ?assertEqual(ok, validate_params(<<"test1">>, <<"test2">>)),
    ?assertEqual({error, val1_empty}, validate_params(<<>>, <<"test">>)),
    ?assertEqual({error, val2_empty}, validate_params(<<"test">>, <<>>)).

demo_test() ->
    % Mock test - in real implementation, you'd mock the repo call
    ?assertEqual({error, invalid_parameters}, demo("not_integer", <<"val1">>, <<"val2">>)),
    ?assertEqual({error, invalid_parameters}, demo(123, "not_binary", <<"val2">>)).

-endif.
endef

# Repository Module Template
# Usage: make new t=imboy.repository n=apps/imrepo/src/user_repo
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
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% Get table name
-spec tablename() -> binary().
tablename() ->
    imboy_db:public_tablename(<<"$(subst _repo,,$(notdir $(n)))">>).

%%% Demo method with improved error handling
-spec demo(Uid::integer(), Val1::binary(), Val2::binary()) ->
    {ok, list()} | {error, term()}.
demo(Uid, Val1, Val2) when is_integer(Uid), is_binary(Val1), is_binary(Val2) ->
    Sql = <<"SELECT id, created_at FROM ", (tablename())/binary, " WHERE id = $1 AND status = $2">>,
    case imboy_db:query(Sql, [Uid, Val1]) of
        {ok, Columns, Rows} ->
            ?LOG_DEBUG("Query successful: ~p rows returned", [length(Rows)]),
            {ok, Rows};
        {error, Reason} ->
            ?LOG_ERROR("Database query failed: ~p", [Reason]),
            {error, db_error}
    end;
demo(Uid, Val1, Val2) ->
    ?LOG_ERROR("Invalid parameters in demo/3: Uid=~p, Val1=~p, Val2=~p", [Uid, Val1, Val2]),
    {error, invalid_parameters}.

%%% Create a new record
-spec create(Data::map(), Uid::integer()) -> {ok, integer()} | {error, term()}.
create(Data, Uid) when is_map(Data), is_integer(Uid) ->
    Sql = <<"INSERT INTO ", (tablename())/binary, " (data, created_by, created_at) VALUES ($1, $2, NOW()) RETURNING id">>,
    case imboy_db:query(Sql, [jsone:encode(Data), Uid]) of
        {ok, _Columns, [{Id}]} ->
            ?LOG_INFO("Record created with id: ~p", [Id]),
            {ok, Id};
        {error, Reason} ->
            ?LOG_ERROR("Failed to create record: ~p", [Reason]),
            {error, create_failed}
    end;
create(Data, Uid) ->
    ?LOG_ERROR("Invalid parameters in create/2: Data=~p, Uid=~p", [Data, Uid]),
    {error, invalid_parameters}.

%%% Find record by ID
-spec find_by_id(Id::integer()) -> {ok, map()} | {error, not_found | term()}.
find_by_id(Id) when is_integer(Id) ->
    Sql = <<"SELECT id, data, created_by, created_at FROM ", (tablename())/binary, " WHERE id = $1">>,
    case imboy_db:query(Sql, [Id]) of
        {ok, _Columns, []} ->
            {error, not_found};
        {ok, _Columns, [{RecordId, Data, CreatedBy, CreatedAt}]} ->
            Record = #{
                id => RecordId,
                data => jsone:decode(Data),
                created_by => CreatedBy,
                created_at => CreatedAt
            },
            {ok, Record};
        {error, Reason} ->
            ?LOG_ERROR("Failed to find record by id ~p: ~p", [Id, Reason]),
            {error, db_error}
    end;
find_by_id(Id) ->
    ?LOG_ERROR("Invalid id parameter: ~p", [Id]),
    {error, invalid_parameters}.

%%% Update record
-spec update(Id::integer(), Data::map(), Uid::integer()) -> ok | {error, term()}.
update(Id, Data, Uid) when is_integer(Id), is_map(Data), is_integer(Uid) ->
    Sql = <<"UPDATE ", (tablename())/binary, " SET data = $1, updated_by = $2, updated_at = NOW() WHERE id = $3">>,
    case imboy_db:query(Sql, [jsone:encode(Data), Uid, Id]) of
        {ok, 1} ->
            ?LOG_INFO("Record ~p updated successfully", [Id]),
            ok;
        {ok, 0} ->
            {error, not_found};
        {error, Reason} ->
            ?LOG_ERROR("Failed to update record ~p: ~p", [Id, Reason]),
            {error, update_failed}
    end;
update(Id, Data, Uid) ->
    ?LOG_ERROR("Invalid parameters in update/3: Id=~p, Data=~p, Uid=~p", [Id, Data, Uid]),
    {error, invalid_parameters}.

%%% Delete record
-spec delete(Id::integer()) -> ok | {error, term()}.
delete(Id) when is_integer(Id) ->
    Sql = <<"DELETE FROM ", (tablename())/binary, " WHERE id = $1">>,
    case imboy_db:query(Sql, [Id]) of
        {ok, 1} ->
            ?LOG_INFO("Record ~p deleted successfully", [Id]),
            ok;
        {ok, 0} ->
            {error, not_found};
        {error, Reason} ->
            ?LOG_ERROR("Failed to delete record ~p: ~p", [Id, Reason]),
            {error, delete_failed}
    end;
delete(Id) ->
    ?LOG_ERROR("Invalid id parameter: ~p", [Id]),
    {error, invalid_parameters}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% Add internal helper functions here

%% ===================================================================
%% EUnit tests
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
# Usage: make new t=imboy.ds n=apps/imds/src/user_ds
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
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% Demo method with domain logic
-spec demo(Uid::integer(), Val1::binary(), Val2::binary()) -> 
    {ok, term()} | {error, term()}.
demo(Uid, Val1, Val2) when is_integer(Uid), is_binary(Val1), is_binary(Val2) ->
    try
        % Validate domain constraints
        case validate_domain_constraints(#{uid => Uid, val1 => Val1, val2 => Val2}) of
            ok ->
                % Apply business rules
                case process_business_rule(Val1, Val2) of
                    {ok, ProcessedData} ->
                        % Call repository layer
                        Result = $(subst _ds,,$(notdir $(n)))_repo:demo(Uid, Val1, Val2),
                        case Result of
                            {ok, Data} ->
                                ?LOG_INFO("Domain service operation successful for uid: ~p", [Uid]),
                                {ok, Data};
                            {error, Reason} ->
                                ?LOG_ERROR("Repository operation failed: ~p", [Reason]),
                                {error, Reason}
                        end;
                    {error, BusinessError} ->
                        ?LOG_WARNING("Business rule validation failed: ~p", [BusinessError]),
                        {error, BusinessError}
                end;
            {error, DomainError} ->
                ?LOG_WARNING("Domain constraint validation failed: ~p", [DomainError]),
                {error, DomainError}
        end
    catch
        Error:Reason:Stacktrace ->
            ?LOG_ERROR("Exception in domain service demo/3: ~p:~p~n~p", [Error, Reason, Stacktrace]),
            {error, internal_error}
    end;
demo(Uid, Val1, Val2) ->
    ?LOG_ERROR("Invalid parameters in demo/3: Uid=~p, Val1=~p, Val2=~p", [Uid, Val1, Val2]),
    {error, invalid_parameters}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%%% Process business rules specific to this domain
-spec process_business_rule(Val1::binary(), Val2::binary()) -> 
    {ok, term()} | {error, term()}.
process_business_rule(Val1, Val2) ->
    % Example business rule: Val1 and Val2 cannot be the same
    case Val1 =:= Val2 of
        true ->
            {error, values_cannot_be_same};
        false ->
            % Example: combine values with business logic
            ProcessedData = #{combined => <<Val1/binary, "_", Val2/binary>>},
            {ok, ProcessedData}
    end.

%%% Validate domain-specific constraints
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
%% EUnit tests
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
