# IMBoy 四层模板 / Four-layer templates: Handler -> Logic -> DS -> Repo.
# make new t=imboy.rest_handler n=demo_handler -> src/api/demo_handler.erl
# make new t=imboy.logic n=demo_logic          -> src/logic/demo_logic.erl
# make new t=imboy.ds n=demo_ds                -> src/ds/demo_ds.erl
# make new t=imboy.repository n=demo_repo      -> src/repo/demo_repo.erl
# 同一功能使用相同名称前缀；路由与表结构需单独配置。
# Use one prefix per feature; configure routes and schema separately.
# SQL 的 $$$$ 经 erlang.mk 的 eval/render 两次展开后保留为 $。
# SQL needs $$$$ to survive erlang.mk's eval/render expansion.

tplp_imboy.rest_handler = src/api/template_name.erl
tplp_imboy.logic = src/logic/template_name.erl
tplp_imboy.repository = src/repo/template_name.erl
tplp_imboy.ds = src/ds/template_name.erl

define tpl_imboy.rest_handler
-module($(notdir $(n))).
-behaviour(cowboy_handler).

-export([init/2]).

-include("log.hrl").
-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, undefined),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom(), cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(demo_action, Req, #{current_uid := Uid}) when
    is_integer(Uid), Uid > 0
->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            demo_action(Req, Uid);
        _ ->
            Req1 = cowboy_req:set_resp_header(<<"allow">>, <<"POST">>, Req),
            elib_response:error_with_status(
                Req1, 405, <<"method not allowed">>, ?ERR_METHOD_NOT_ALLOWED
            )
    end;
handle_action(demo_action, Req, _State) ->
    elib_response:error_with_status(Req, 401, <<"unauthorized">>, ?ERR_UNAUTHORIZED);
handle_action(_Action, Req, _State) ->
    elib_response:error_with_status(Req, 404, <<"unknown action">>, ?ERR_NOT_FOUND).

-spec demo_action(cowboy_req:req(), pos_integer()) -> cowboy_req:req().
demo_action(Req0, Uid) ->
    {ok, Params, Req1} = elib_req:body(Req0, []),
    try
        Val1 = maps:get(<<"val1">>, Params, <<>>),
        Val2 = maps:get(<<"val2">>, Params, <<>>),
        Result = $(patsubst %_handler,%,$(notdir $(n)))_logic:demo(Uid, Val1, Val2),
        elib_response:handle_logic_result(Req1, Result)
    catch
        Class:Reason:Stacktrace ->
            ?ERROR_LOG("demo_action crashed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            elib_response:error_with_status(
                Req1, 500, <<"internal server error">>, ?ERR_INTERNAL_SERVER_ERROR
            )
    end.
endef

define tpl_imboy.logic
-module($(notdir $(n))).

-export([demo/3]).

-include("log.hrl").
-include("error_code.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec demo(pos_integer(), binary(), binary()) ->
    {ok, [map()]} | {error, binary(), integer()}.
demo(Uid, Val1, Val2) ->
    case $(patsubst %_logic,%,$(notdir $(n)))_ds:demo(Uid, Val1, Val2) of
        {ok, Data} ->
            {ok, Data};
        {error, invalid_parameters} ->
            {error, <<"参数无效"/utf8>>, ?ERR_BAD_REQUEST};
        {error, Reason} ->
            ?ERROR_LOG("demo failed, uid=~p reason=~p", [Uid, Reason]),
            {error, <<"操作失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR}
    end.

-ifdef(TEST).
demo_invalid_params_test() ->
    ?assertMatch({error, _, ?ERR_BAD_REQUEST}, demo(0, <<"first">>, <<"second">>)),
    ?assertMatch({error, _, ?ERR_BAD_REQUEST}, demo(1, [], <<"second">>)).
-endif.
endef

define tpl_imboy.ds
-module($(notdir $(n))).

-export([demo/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec demo(pos_integer(), binary(), binary()) -> {ok, [map()]} | {error, term()}.
demo(Uid, Val1, Val2) when
    is_integer(Uid),
    Uid > 0,
    is_binary(Val1),
    byte_size(Val1) > 0,
    byte_size(Val1) =< 255,
    is_binary(Val2),
    byte_size(Val2) > 0,
    byte_size(Val2) =< 255
->
    $(patsubst %_ds,%,$(notdir $(n)))_repo:demo(Uid, Val1, Val2);
demo(_Uid, _Val1, _Val2) ->
    {error, invalid_parameters}.

-ifdef(TEST).
demo_invalid_params_test() ->
    ?assertEqual({error, invalid_parameters}, demo(-1, <<"a">>, <<"b">>)),
    ?assertEqual({error, invalid_parameters}, demo(1, <<"a">>, false)),
    ?assertEqual({error, invalid_parameters}, demo(1, <<>>, <<"b">>)),
    ?assertEqual({error, invalid_parameters}, demo(1, binary:copy(<<"a">>, 256), <<"b">>)).
-endif.
endef

define tpl_imboy.repository
-module($(notdir $(n))).

-export([tablename/0, demo/3, create/2, find_by_id/2, update/3, delete/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% 示例表使用 TSID 主键、JSONB data 与审计字段，需配套迁移。
%% The sample table needs a TSID key, JSONB data and audit columns in a migration.
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename_quoted(<<"$(patsubst %_repo,%,$(notdir $(n)))">>).

-spec demo(pos_integer(), binary(), binary()) -> {ok, [map()]} | {error, term()}.
demo(Uid, Val1, Val2) when is_integer(Uid), Uid > 0, is_binary(Val1), is_binary(Val2) ->
    Sql =
        <<"SELECT id, created_at FROM ", (tablename())/binary,
            " WHERE created_by = $$$$1 AND data->>'val1' = $$$$2 AND data->>'val2' = $$$$3",
            " ORDER BY id DESC LIMIT 20">>,
    elib_pg:query(Sql, [Uid, Val1, Val2]);
demo(_Uid, _Val1, _Val2) ->
    {error, invalid_parameters}.

-spec create(map(), pos_integer()) -> {ok, pos_integer()} | {error, term()}.
create(Data, Uid) when is_map(Data), is_integer(Uid), Uid > 0 ->
    Id = elib_tsid:generate($(patsubst %_repo,%,$(notdir $(n)))),
    Sql =
        <<"INSERT INTO ", (tablename())/binary,
            " (id, data, created_by, created_at) VALUES ($$$$1, $$$$2, $$$$3, NOW()) RETURNING id">>,
    case elib_pg:query(Sql, [Id, jsone:encode(Data), Uid]) of
        {ok, [#{<<"id">> := Id}]} -> {ok, Id};
        {error, _Reason} = Error -> Error
    end;
create(_Data, _Uid) ->
    {error, invalid_parameters}.

-spec find_by_id(pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
find_by_id(Id, Uid) when is_integer(Id), Id > 0, is_integer(Uid), Uid > 0 ->
    Sql =
        <<"SELECT id, data, created_by, created_at FROM ", (tablename())/binary,
            " WHERE id = $$$$1 AND created_by = $$$$2">>,
    case elib_pg:query(Sql, [Id, Uid]) of
        {ok, []} ->
            {error, not_found};
        {ok, [#{<<"data">> := Data} = Row]} ->
            {ok, Row#{<<"data">> := decode_data(Data)}};
        {error, _Reason} = Error ->
            Error
    end;
find_by_id(_Id, _Uid) ->
    {error, invalid_parameters}.

-spec update(pos_integer(), map(), pos_integer()) -> ok | {error, term()}.
update(Id, Data, Uid) when is_integer(Id), Id > 0, is_map(Data), is_integer(Uid), Uid > 0 ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET data = $$$$1, updated_by = $$$$2, updated_at = NOW() WHERE id = $$$$3 AND created_by = $$$$2">>,
    case elib_pg:execute(Sql, [jsone:encode(Data), Uid, Id]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, _Reason} = Error -> Error
    end;
update(_Id, _Data, _Uid) ->
    {error, invalid_parameters}.

-spec delete(pos_integer(), pos_integer()) -> ok | {error, term()}.
delete(Id, Uid) when is_integer(Id), Id > 0, is_integer(Uid), Uid > 0 ->
    Sql = <<"DELETE FROM ", (tablename())/binary, " WHERE id = $$$$1 AND created_by = $$$$2">>,
    case elib_pg:execute(Sql, [Id, Uid]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, _Reason} = Error -> Error
    end;
delete(_Id, _Uid) ->
    {error, invalid_parameters}.

-spec decode_data(binary() | map()) -> map().
decode_data(Data) when is_binary(Data) -> jsone:decode(Data);
decode_data(Data) when is_map(Data) -> Data.

-ifdef(TEST).
validation_test() ->
    ?assertEqual({error, invalid_parameters}, demo(0, <<"a">>, <<"b">>)),
    ?assertEqual({error, invalid_parameters}, create([], 1)),
    ?assertEqual({error, invalid_parameters}, find_by_id(0, 1)),
    ?assertEqual({error, invalid_parameters}, update(1, #{}, 0)),
    ?assertEqual({error, invalid_parameters}, delete(-1, 1)).
-endif.
endef
