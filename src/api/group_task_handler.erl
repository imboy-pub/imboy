-module(group_task_handler).
%% Thin HTTP adapter for the group_collab task boundary.
%% Keep transport concerns here and delegate task rules to group_task_logic.

-dialyzer({nowarn_function, [create/2, update/2, assign/2, submit/2, review/2, list/2, detail/2, my_tasks/2, pending_review/2]}).

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化群作业处理器
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case imboy_plugin_registry:required_feature(api, group_task_handler, Action) of
            undefined ->
                handle_action(Action, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        handle_action(Action, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% @doc Action 分发处理
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create, Req, State) -> create(Req, State);
handle_action(update, Req, State) -> update(Req, State);
handle_action(assign, Req, State) -> assign(Req, State);
handle_action(submit, Req, State) -> submit(Req, State);
handle_action(review, Req, State) -> review(Req, State);
handle_action(list, Req, State) -> list(Req, State);
handle_action(detail, Req, State) -> detail(Req, State);
handle_action(my_tasks, Req, State) -> my_tasks(Req, State);
handle_action(pending_review, Req, State) -> pending_review(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 创建作业
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"group_id">>, PostVals, <<>>),
    Gid2 = ec_cnv:to_integer(Gid),
    Title = maps:get(<<"title">>, PostVals, <<>>),
    UserIds = maps:get(<<"user_ids">>, PostVals, []),

    case {Gid2, Title} of
        {0, _} ->
            elib_response:error(Req0, <<"群组ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        {_, <<>>} ->
            elib_response:error(Req0, <<"作业标题必填"/utf8>>, ?ERR_TASK_TITLE_REQUIRED);
        _ ->
            % 构建作业数据
            Data = #{
                description => maps:get(<<"description">>, PostVals, <<>>),
                deadline => maps:get(<<"deadline">>, PostVals, undefined),
                attachment => maps:get(<<"attachment">>, PostVals, <<>>)
            },
            case group_task_logic:create(Gid2, CurrentUid, Title, Data) of
                {ok, TaskId} ->
                    maybe_assign_task(TaskId, UserIds),
                    TaskId2 = TaskId,
                    elib_response:success(Req0, #{<<"task_id">> => TaskId2}, <<"作业创建成功"/utf8>>);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 更新作业
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    TaskId = maps:get(<<"task_id">>, PostVals, <<>>),
    TaskId2 = normalize_id(TaskId),
    Status = normalize_task_status(maps:get(<<"status">>, PostVals, undefined)),

    case {TaskId2, Status} of
        {0, _} ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        {_, invalid} ->
            elib_response:error(Req0, <<"状态参数非法"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            % 构建更新数据
            Data = #{
                title => maps:get(<<"title">>, PostVals, undefined),
                description => maps:get(<<"description">>, PostVals, undefined),
                deadline => maps:get(<<"deadline">>, PostVals, undefined),
                attachment => maps:get(<<"attachment">>, PostVals, undefined),
                status => Status
            },
            % 移除undefined的值
            Data2 = maps:filter(fun(_K, V) -> V =/= undefined end, Data),
            case group_task_logic:update(TaskId2, CurrentUid, Data2) of
                ok ->
                    elib_response:success(Req0, #{<<"task_id">> => TaskId}, <<"作业更新成功"/utf8>>);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 分配作业
-spec assign(cowboy_req:req(), map()) -> cowboy_req:req().
assign(Req0, State) ->
    _CurrentUid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    TaskId = maps:get(<<"task_id">>, PostVals, <<>>),
    TaskId2 = normalize_id(TaskId),
    UserIds = maps:get(<<"user_ids">>, PostVals, []),

    case TaskId2 of
        0 ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            % 解码用户ID列表
            UserIds2 = [ec_cnv:to_integer(Uid) || Uid <- UserIds],
            case group_task_logic:assign(TaskId2, UserIds2) of
                ok ->
                    elib_response:success(Req0, #{<<"task_id">> => TaskId}, <<"作业分配成功"/utf8>>);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 提交作业
-spec submit(cowboy_req:req(), map()) -> cowboy_req:req().
submit(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    TaskId = maps:get(<<"task_id">>, PostVals, <<>>),
    TaskUid = normalize_task_uid(TaskId),
    Content = maps:get(<<"content">>, PostVals, <<>>),
    Attachment0 = case maps:get(<<"attachment">>, PostVals, undefined) of
        undefined ->
            maps:get(<<"attachments">>, PostVals, <<>>);
        Value ->
            Value
    end,
    Attachment = normalize_attachment(Attachment0),

    case TaskUid of
        undefined ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        TaskUid2 ->
            Data = #{
                content => Content,
                attachment => Attachment
            },
            case group_task_logic:submit(TaskUid2, CurrentUid, Data) of
                ok ->
                    elib_response:success(Req0, #{<<"task_id">> => TaskUid2}, <<"作业提交成功"/utf8>>);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 批改作业
-spec review(cowboy_req:req(), map()) -> cowboy_req:req().
review(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    AssignmentId = maps:get(<<"assignment_id">>, PostVals, <<>>),
    AssignmentId2 = normalize_id(AssignmentId),
    Score = maps:get(<<"score">>, PostVals, undefined),
    Comment = maps:get(<<"comment">>, PostVals, <<>>),

    case AssignmentId2 of
        0 ->
            elib_response:error(Req0, <<"作业分配ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            Data = #{
                score => Score,
                comment => Comment
            },
            case group_task_logic:review(AssignmentId2, CurrentUid, Data) of
                ok ->
                    elib_response:success(Req0, #{<<"assignment_id">> => AssignmentId}, <<"作业批改成功"/utf8>>);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 查询作业列表
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"group_id">>, Qs, <<>>),
    Gid2 = ec_cnv:to_integer(Gid),
    Status = normalize_optional_status(proplists:get_value(<<"status">>, Qs, undefined)),
    AssigneeId = normalize_assignee_id(proplists:get_value(<<"assignee_id">>, Qs, undefined), CurrentUid),
    {Page, Size} = elib_param:page(Req0),

    case {Gid2, Status, AssigneeId} of
        {0, _, _} ->
            elib_response:error(Req0, <<"群组ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        {_, invalid, _} ->
            elib_response:error(Req0, <<"状态参数非法"/utf8>>, ?ERR_BAD_REQUEST);
        {_, _, invalid} ->
            elib_response:error(Req0, <<"执行人ID非法"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_task_logic:list(Gid2, Status, AssigneeId, Page, Size) of
                {ok, Tasks} ->
                    % 转换数据格式
                    Tasks2 = [task_transfer(Task) || Task <- Tasks],
                    elib_response:success(Req0, #{<<"list">> => Tasks2, <<"page">> => Page, <<"size">> => Size});
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 查询作业详情
-spec detail(cowboy_req:req(), map()) -> cowboy_req:req().
detail(Req0, State) ->
    _CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    TaskId = proplists:get_value(<<"task_id">>, Qs, <<>>),
    TaskId2 = normalize_id(TaskId),

    case TaskId2 of
        0 ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_task_logic:detail(TaskId2) of
                {ok, Task} ->
                    Task2 = task_transfer(Task),
                    elib_response:success(Req0, Task2);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 查询我的作业
-spec my_tasks(cowboy_req:req(), map()) -> cowboy_req:req().
my_tasks(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Status = normalize_optional_status(proplists:get_value(<<"status">>, Qs, undefined)),
    {Page, Size} = elib_param:page(Req0),

    case Status of
        invalid ->
            elib_response:error(Req0, <<"状态参数非法"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_task_logic:my_tasks(CurrentUid, Status, Page, Size) of
                {ok, Assignments} ->
                    % 转换数据格式
                    Assignments2 = [assignment_transfer(Assignment) || Assignment <- Assignments],
                    elib_response:success(Req0, #{<<"list">> => Assignments2, <<"page">> => Page, <<"size">> => Size});
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 查询待批改作业
-spec pending_review(cowboy_req:req(), map()) -> cowboy_req:req().
pending_review(Req0, State) ->
    _CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    TaskId = proplists:get_value(<<"task_id">>, Qs, <<>>),
    TaskUid = normalize_task_uid(TaskId),
    {Page, Size} = elib_param:page(Req0),

    case TaskUid of
        undefined ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        TaskUid2 ->
            case group_task_logic:pending_review(TaskUid2, Page, Size) of
                {ok, Assignments} ->
                    % 转换数据格式
                    Assignments2 = [assignment_transfer(Assignment) || Assignment <- Assignments],
                    elib_response:success(Req0, #{<<"list">> => Assignments2, <<"page">> => Page, <<"size">> => Size});
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 转换作业数据格式
-spec task_transfer(map()) -> map().
task_transfer(Task) ->
    Task.

%% @doc 转换作业分配数据格式
-spec assignment_transfer(map()) -> map().
assignment_transfer(Assignment) ->
    Assignment.

%% @doc 兼容 task_id:
%% - 原生 task_uid（二进制字符串）
%% - 数字主键（int）
-spec normalize_task_uid(term()) -> binary() | undefined.
normalize_task_uid(undefined) ->
    undefined;
normalize_task_uid(<<>>) ->
    undefined;
normalize_task_uid(TaskId) when is_integer(TaskId), TaskId > 0 ->
    task_uid_by_pk(TaskId);
normalize_task_uid(TaskId) when is_list(TaskId) ->
    normalize_task_uid(ec_cnv:to_binary(TaskId));
normalize_task_uid(TaskId) when is_binary(TaskId) ->
    case TaskId of
        <<>> ->
            undefined;
        _ ->
            case elib_type:is_numeric(TaskId) of
                true ->
                    task_uid_by_pk(ec_cnv:to_integer(TaskId));
                false ->
                    case group_task_repo:find_by_task_id(TaskId) of
                        {ok, _Task} ->
                            TaskId;
                        _ ->
                            case ec_cnv:to_integer(TaskId) of
                                0 ->
                                    undefined;
                                Id ->
                                    task_uid_by_pk(Id)
                            end
                    end
            end
    end;
normalize_task_uid(_TaskId) ->
    undefined.

-spec task_uid_by_pk(integer()) -> binary() | undefined.
task_uid_by_pk(Id) when is_integer(Id), Id > 0 ->
    case group_task_repo:find_by_id(Id) of
        {ok, #{<<"task_id">> := TaskUid}} when is_binary(TaskUid), TaskUid =/= <<>> ->
            TaskUid;
        _ ->
            undefined
    end;
task_uid_by_pk(_) ->
    undefined.

%% @doc 兼容主键参数:
%% - int
%% - 数字字符串
-spec normalize_id(term()) -> integer().
normalize_id(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_id(Value) when is_list(Value) ->
    normalize_id(ec_cnv:to_binary(Value));
normalize_id(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    ec_cnv:to_integer(Value)
            end
    end;
normalize_id(_Value) ->
    0.

-spec normalize_optional_status(term()) -> integer() | undefined | invalid.
normalize_optional_status(undefined) ->
    undefined;
normalize_optional_status(<<>>) ->
    undefined;
normalize_optional_status(Value) when is_integer(Value) ->
    case Value >= 0 andalso Value =< 3 of
        true -> Value;
        false -> invalid
    end;
normalize_optional_status(Value) when is_list(Value) ->
    normalize_optional_status(ec_cnv:to_binary(Value));
normalize_optional_status(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            undefined;
        _ ->
            case elib_type:is_numeric(Value) of
                true -> normalize_optional_status(ec_cnv:to_integer(Value));
                false -> invalid
            end
    end;
normalize_optional_status(_Value) ->
    invalid.

-spec normalize_task_status(term()) -> integer() | undefined | invalid.
normalize_task_status(undefined) ->
    undefined;
normalize_task_status(<<>>) ->
    undefined;
normalize_task_status(Value) ->
    case normalize_optional_status(Value) of
        0 ->
            1;
        S when is_integer(S), S >= 1, S =< 3 ->
            S;
        undefined ->
            undefined;
        _ ->
            invalid
    end.

-spec normalize_assignee_id(term(), integer()) -> integer() | undefined | invalid.
normalize_assignee_id(undefined, CurrentUid) when is_integer(CurrentUid), CurrentUid > 0 ->
    CurrentUid;
normalize_assignee_id(<<>>, CurrentUid) when is_integer(CurrentUid), CurrentUid > 0 ->
    CurrentUid;
normalize_assignee_id(Value, CurrentUid) when is_list(Value) ->
    normalize_assignee_id(ec_cnv:to_binary(Value), CurrentUid);
normalize_assignee_id(<<"all">>, _CurrentUid) ->
    undefined;
normalize_assignee_id(<<"ALL">>, _CurrentUid) ->
    undefined;
normalize_assignee_id(Value, _CurrentUid) ->
    case normalize_id(Value) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> invalid
    end.

-spec normalize_attachment(term()) -> binary().
normalize_attachment(undefined) ->
    <<>>;
normalize_attachment(<<>>) ->
    <<>>;
normalize_attachment(Value) when is_binary(Value) ->
    Value;
normalize_attachment(Value) when is_list(Value); is_map(Value) ->
    jsone:encode(Value);
normalize_attachment(Value) ->
    ec_cnv:to_binary(Value).

-spec maybe_assign_task(integer(), list()) -> ok.
maybe_assign_task(TaskId, UserIds) when is_integer(TaskId), TaskId > 0, is_list(UserIds) ->
    UserIds2 = lists:usort([Id || Id <- [normalize_id(Uid) || Uid <- UserIds], Id > 0]),
    case UserIds2 of
        [] ->
            ok;
        _ ->
            case group_task_logic:assign(TaskId, UserIds2) of
                ok ->
                    ok;
                {error, Msg, Code} ->
                    ?WARN_LOG("group_task create assign failed: ~p ~p", [Msg, Code]),
                    ok
            end
    end;
maybe_assign_task(_TaskId, _UserIds) ->
    ok.
