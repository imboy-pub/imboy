-module(user_collect_handler).

%% Thin HTTP adapter for the social_graph collect boundary.

%%%
% collect 控制器模块
% collect controller module
%%%
-behaviour(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").
-include("error_code.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            page ->
                page(Req0, State);
            add ->
                add(Req0, State);
            remove ->
                remove(Req0, State);
            change ->
                change(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 收藏分页列表
%% 获取用户的收藏列表（分页）
%%
%% @param Req0 Cowboy请求对象，包含分页和筛选参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含收藏列表的响应
%% @end
-spec page(cowboy_req:req(), map()) -> cowboy_req:req().
page(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    {ok, Kind} = elib_param:int(kind, Req0, 0),
    Qs4 = cowboy_req:parse_qs(Req0),
    OrderBy = proplists:get_value(<<"order">>, Qs4, <<>>),
    Kwd = proplists:get_value(<<"kwd">>, Qs4, <<>>),
    Tag = proplists:get_value(<<"tag">>, Qs4, <<>>),

    Where0 = #{user_id => CurrentUid, status => 1},
    Where1 =
        case Kind of
            0 -> Where0;
            _ -> Where0#{kind => Kind}
        end,
    Where2 =
        case byte_size(Tag) > 0 of
            true ->
                Where1#{tag => {op, <<"LIKE">>, <<"%", Tag/binary, ",%">>}};
            false ->
                Where1
        end,
    WhereMap =
        case byte_size(Kwd) > 0 of
            true ->
                Like = <<"%", Kwd/binary, "%">>,
                Where2#{
                    <<"__or">> => [
                        #{source => {op, <<"LIKE">>, Like}},
                        #{remark => {op, <<"LIKE">>, Like}},
                        #{info => {op, <<"LIKE">>, Like}}
                    ]
                };
            false ->
                Where2
        end,

    % Determine order
    Order =
        case OrderBy of
            <<"recent_use">> ->
                <<"updated_at desc, id desc">>;
            _ ->
                <<"id desc">>
        end,

    % Build column list
    % A-05：info 取原始列，解密在应用层做（历史实现把主密钥内联进 SQL 列表达式）
    % 迁移 227：sealed 行（key_version 非空）info 为客户端密文 base64，逐行
    % 分流——legacy 行走原解密+JSON 解码链，sealed 行 info 原样返回。
    Column =
        <<"kind, kind_id, source, created_at, updated_at, tag, info, ", "generation, key_version">>,

    {ok, Payload} = user_collect_logic:page(Column, WhereMap, Order, Page, Size),
    List0 = maps:get(list, Payload, []),
    List2 = lists:map(fun decode_collect_info/1, List0),
    Payload2 = maps:put(list, List2, Payload),
    elib_response:success(Req0, Payload2).

%% @doc info 列按行分流解码：legacy 行（key_version NULL）= 服务端持钥
%% 密文 → 解密 + JSON 解码（A-05 原链）；sealed 行 = 客户端密文 base64，
%% 原样返回（含 generation/key_version 供客户端同步与 CAS）。
decode_collect_info(Item) when is_map(Item) ->
    case maps:get(<<"key_version">>, Item, null) of
        null ->
            case elib_hasher:decode_list_field([Item], <<"info">>) of
                [Legacy | _] ->
                    case elib_response:json_decode_list_field([Legacy], <<"info">>) of
                        [Decoded | _] -> Decoded;
                        [] -> Legacy
                    end;
                %% 解码链异常（历史坏行）：原样返回，页面层按空信息渲染
                %% ——与旧行为（decode_list_field 过滤）等价，不 500。
                [] ->
                    Item
            end;
        _Kv ->
            Item
    end;
decode_collect_info(Item) ->
    Item.

%% @doc 添加收藏
%% 添加新的收藏项
%%
%% @param Req0 Cowboy请求对象，包含收藏信息
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec add(cowboy_req:req(), map()) -> cowboy_req:req().
add(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case maps:get(<<"info_sealed">>, PostVals, undefined) of
        undefined ->
            add_legacy(Req0, CurrentUid, PostVals);
        InfoSealed ->
            % E2EE 收藏 v1（迁移 227 / PLAN 2026-10-24-e2ee-collect §7）
            Kind = ec_cnv:to_integer(maps:get(<<"kind">>, PostVals, 0)),
            KindId = maps:get(<<"kind_id">>, PostVals, <<"">>),
            KeyVersion = ec_cnv:to_integer(maps:get(<<"key_version">>, PostVals, 0)),
            Hashes = maps:get(<<"attach_file_hash256">>, PostVals, []),
            collect_v1_response(
                Req0,
                user_collect_logic:add_v1(
                    CurrentUid, Kind, KindId, InfoSealed, KeyVersion, Hashes
                )
            )
    end.

add_legacy(Req0, CurrentUid, PostVals) ->
    % Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息  7 个人名片
    Kind = maps:get(<<"kind">>, PostVals, <<"">>),
    KindId = maps:get(<<"kind_id">>, PostVals, <<"">>),
    Source = maps:get(<<"source">>, PostVals, <<"">>),
    Remark = maps:get(<<"remark">>, PostVals, <<"">>),
    Info = maps:get(<<"info">>, PostVals, #{}),
    case user_collect_logic:add(CurrentUid, Kind, KindId, Info, Source, Remark) of
        {ok, _Msg} ->
            elib_response:success(Req0);
        {error, Code} when is_integer(Code) ->
            elib_response:error(Req0, imboy_error:error_msg(Code), Code);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% v1 统一响应：{ok, Payload} 成功；{error, Code} 带错误码（客户端据此走
%% 升级提示/密钥同步/冲突重读路径）。
collect_v1_response(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
collect_v1_response(Req0, {error, Code}) when is_integer(Code) ->
    elib_response:error(Req0, imboy_error:error_msg(Code), Code).

%% @doc 删除收藏
%% 删除指定的收藏项
%%
%% @param Req0 Cowboy请求对象，包含收藏项ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec remove(cowboy_req:req(), map()) -> cowboy_req:req().
remove(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    KindId = maps:get(<<"kind_id">>, PostVals, ""),
    % Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
    _ = user_collect_logic:remove(CurrentUid, KindId),
    elib_response:success(Req0, #{}, "success.").

%% @doc 修改收藏
%% 修改收藏项的信息
%%
%% @param Req0 Cowboy请求对象，包含收藏项ID和修改内容
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec change(cowboy_req:req(), map()) -> cowboy_req:req().
change(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Action = maps:get(<<"action">>, PostVals, <<>>),
    KindId = maps:get(<<"kind_id">>, PostVals, <<>>),
    case {Action, maps:get(<<"info_sealed">>, PostVals, undefined)} of
        {<<"update">>, InfoSealed} when is_binary(InfoSealed) ->
            % E2EE 收藏 v1 编辑（CAS，§7.3）
            ExpectedGen = ec_cnv:to_integer(maps:get(<<"expected_generation">>, PostVals, 0)),
            NewGen = ec_cnv:to_integer(maps:get(<<"generation">>, PostVals, 0)),
            KeyVersion = ec_cnv:to_integer(maps:get(<<"key_version">>, PostVals, 0)),
            Hashes = maps:get(<<"attach_file_hash256">>, PostVals, []),
            collect_v1_response(
                Req0,
                user_collect_logic:update_v1(
                    CurrentUid,
                    KindId,
                    ExpectedGen,
                    NewGen,
                    InfoSealed,
                    KeyVersion,
                    Hashes
                )
            );
        _ ->
            change_legacy(Req0, CurrentUid, Action, KindId, PostVals)
    end.

change_legacy(Req0, CurrentUid, Action, KindId, PostVals) ->
    case user_collect_logic:change(CurrentUid, Action, KindId, PostVals) of
        ok ->
            elib_response:success(Req0, #{}, "success.");
        {error, Code} when is_integer(Code) ->
            elib_response:error(Req0, imboy_error:error_msg(Code), Code)
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
