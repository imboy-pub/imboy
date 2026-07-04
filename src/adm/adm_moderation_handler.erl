-module(adm_moderation_handler).

%%%
% 内容审核管理控制器（敏感词黑名单 + 消息人工复审队列）
% Content moderation controller (sensitive word blacklist + manual review queue)
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").
-include("log.hrl").

%% 读权限 / 写权限（复用治理中心既有权限，见 adm_index_handler:role_acl/1）
-define(PERM_READ, <<"reports:read">>).
-define(PERM_WRITE, <<"reports:handle">>).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            sensitive_words -> sensitive_words(Method, Req0, State);
            sensitive_words_import -> sensitive_words_import(Method, Req0, State);
            sensitive_word_delete -> sensitive_word_delete(Method, Req0, State);
            review_queue -> review_queue(Method, Req0, State);
            review_moderate -> review_moderate(Method, Req0, State);
            _ -> cowboy_req:reply(404, #{}, <<"Not Found">>, Req0)
        end,
    {ok, Req1, State}.

%% ===================================================================
%% 敏感词
%% ===================================================================

%% GET 列表 / POST 新增
-spec sensitive_words(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
sensitive_words(<<"GET">>, Req0, State) ->
    with_perm(State, ?PERM_READ, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        {ok, Category} = elib_param:binary(<<"category">>, Req0, undefined),
        {ok, Keyword} = elib_param:binary(<<"keyword">>, Req0, undefined),
        Filters = drop_undefined(#{category => Category, keyword => Keyword}),
        case adm_moderation_logic:list_sensitive_words(Page, Size, Filters) of
            {ok, P} -> elib_response:success(Req0, P);
            {error, _} -> elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
sensitive_words(<<"POST">>, Req0, State) ->
    with_perm(State, ?PERM_WRITE, Req0, fun() ->
        Vals = elib_param:post(Req0),
        Word = maps:get(<<"word">>, Vals, <<>>),
        Category = maps:get(<<"category">>, Vals, <<"custom">>),
        Severity = maps:get(<<"severity">>, Vals, <<"medium">>),
        case adm_moderation_logic:add_sensitive_word(Word, Category, Severity) of
            {ok, Result} -> elib_response:success(Req0, Result, <<"添加成功"/utf8>>);
            {error, Reason} -> elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
        end
    end);
sensitive_words(_, Req0, _State) ->
    method_not_allowed(Req0).

%% POST 批量导入
-spec sensitive_words_import(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
sensitive_words_import(<<"POST">>, Req0, State) ->
    with_perm(State, ?PERM_WRITE, Req0, fun() ->
        Vals = elib_param:post(Req0),
        Words = maps:get(<<"words">>, Vals, []),
        WordList =
            case is_list(Words) of
                true -> Words;
                false -> []
            end,
        {ok, Result} = adm_moderation_logic:import_sensitive_words(WordList),
        elib_response:success(Req0, Result, <<"导入完成"/utf8>>)
    end);
sensitive_words_import(_, Req0, _State) ->
    method_not_allowed(Req0).

%% DELETE /moderation/sensitive-words/:id
-spec sensitive_word_delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
sensitive_word_delete(<<"DELETE">>, Req0, State) ->
    with_perm(State, ?PERM_WRITE, Req0, fun() ->
        Id = binding_int(id, Req0),
        case adm_moderation_logic:delete_sensitive_word(Id) of
            {ok, _} -> elib_response:success(Req0, #{}, <<"删除成功"/utf8>>);
            {error, Reason} -> elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
        end
    end);
sensitive_word_delete(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% 复审队列
%% ===================================================================

%% GET 列表
-spec review_queue(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
review_queue(<<"GET">>, Req0, State) ->
    with_perm(State, ?PERM_READ, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        {ok, Status} = elib_param:binary(<<"status">>, Req0, undefined),
        {ok, Keyword} = elib_param:binary(<<"keyword">>, Req0, undefined),
        {ok, Start} = elib_param:binary(<<"start">>, Req0, undefined),
        {ok, End} = elib_param:binary(<<"end">>, Req0, undefined),
        Filters = drop_undefined(#{
            status => normalize_status(Status),
            keyword => Keyword,
            start => Start,
            'end' => End
        }),
        case adm_moderation_logic:list_review_queue(Page, Size, Filters) of
            {ok, P} -> elib_response:success(Req0, P);
            {error, _} -> elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
review_queue(_, Req0, _State) ->
    method_not_allowed(Req0).

%% POST /moderation/review-queue/:id/moderate
-spec review_moderate(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
review_moderate(<<"POST">>, Req0, State) ->
    with_perm(State, ?PERM_WRITE, Req0, fun() ->
        Id = binding_int(id, Req0),
        Vals = elib_param:post(Req0),
        Action = maps:get(<<"action">>, Vals, <<>>),
        Reason =
            case maps:get(<<"reason">>, Vals, undefined) of
                R when is_binary(R), byte_size(R) > 0 -> R;
                _ -> undefined
            end,
        ReviewerId = maps:get(adm_user_id, State, 0),
        case adm_moderation_logic:moderate(Id, Action, Reason, ReviewerId) of
            ok -> elib_response:success(Req0, #{}, <<"操作成功"/utf8>>);
            {error, Msg} -> elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
        end
    end);
review_moderate(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% Internal helpers
%% ===================================================================

%% @doc 权限守卫，通过则执行 Fun，否则返回 403 响应
-spec with_perm(map(), binary(), cowboy_req:req(), fun(() -> cowboy_req:req())) ->
    cowboy_req:req().
with_perm(State, Permission, Req0, Fun) ->
    case adm_acl:ensure_permission(State, Permission, Req0) of
        ok -> Fun();
        {error, Req1} -> Req1
    end.

-spec method_not_allowed(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed(Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc status=all（或空）视为不筛选
-spec normalize_status(term()) -> binary() | undefined.
normalize_status(<<"all">>) -> undefined;
normalize_status(<<>>) -> undefined;
normalize_status(S) when is_binary(S) -> S;
normalize_status(_) -> undefined.

-spec binding_int(atom(), cowboy_req:req()) -> integer().
binding_int(Key, Req) ->
    case cowboy_req:binding(Key, Req, <<>>) of
        Bin when is_binary(Bin), byte_size(Bin) > 0 ->
            try
                erlang:binary_to_integer(Bin)
            catch
                _:_ -> 0
            end;
        _ ->
            0
    end.

-spec drop_undefined(map()) -> map().
drop_undefined(Map) ->
    maps:filter(fun(_K, V) -> V =/= undefined end, Map).
