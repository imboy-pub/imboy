-module(group_task_handler).

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
    Req1 = handle_action(Action, Req0, State),
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
    Gid2 = elib_hashids:decode(Gid),
    Title = maps:get(<<"title">>, PostVals, <<>>),

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
                    TaskId2 = elib_hashids:encode(TaskId),
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
    TaskId2 = elib_hashids:decode(TaskId),

    case TaskId2 of
        0 ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            % 构建更新数据
            Data = #{
                title => maps:get(<<"title">>, PostVals, undefined),
                description => maps:get(<<"description">>, PostVals, undefined),
                deadline => maps:get(<<"deadline">>, PostVals, undefined),
                attachment => maps:get(<<"attachment">>, PostVals, undefined)
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
    TaskId2 = elib_hashids:decode(TaskId),
    UserIds = maps:get(<<"user_ids">>, PostVals, []),

    case TaskId2 of
        0 ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            % 解码用户ID列表
            UserIds2 = [elib_hashids:decode(Uid) || Uid <- UserIds],
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
    Content = maps:get(<<"content">>, PostVals, <<>>),
    Attachment = maps:get(<<"attachment">>, PostVals, <<>>),

    case TaskId of
        <<>> ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            Data = #{
                content => Content,
                attachment => Attachment
            },
            case group_task_logic:submit(TaskId, CurrentUid, Data) of
                ok ->
                    elib_response:success(Req0, #{<<"task_id">> => TaskId}, <<"作业提交成功"/utf8>>);
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
    AssignmentId2 = elib_hashids:decode(AssignmentId),
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
    _CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"group_id">>, Qs, <<>>),
    Gid2 = elib_hashids:decode(Gid),
    {Page, Size} = elib_param:page(Req0),

    case Gid2 of
        0 ->
            elib_response:error(Req0, <<"群组ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_task_logic:list(Gid2, Page, Size) of
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
    TaskId2 = elib_hashids:decode(TaskId),

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
    {Page, Size} = elib_param:page(Req0),

    case group_task_logic:my_tasks(CurrentUid, Page, Size) of
        {ok, Assignments} ->
            % 转换数据格式
            Assignments2 = [assignment_transfer(Assignment) || Assignment <- Assignments],
            elib_response:success(Req0, #{<<"list">> => Assignments2, <<"page">> => Page, <<"size">> => Size});
        {error, Msg, Code} ->
            elib_response:error(Req0, Msg, Code)
    end.

%% @doc 查询待批改作业
-spec pending_review(cowboy_req:req(), map()) -> cowboy_req:req().
pending_review(Req0, State) ->
    _CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    TaskId = proplists:get_value(<<"task_id">>, Qs, <<>>),
    {Page, Size} = elib_param:page(Req0),

    case TaskId of
        <<>> ->
            elib_response:error(Req0, <<"作业ID必填"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_task_logic:pending_review(TaskId, Page, Size) of
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
    TaskId = maps:get(<<"task_id">>, Task, <<>>),
    Task#{<<"task_id">> => TaskId}.

%% @doc 转换作业分配数据格式
-spec assignment_transfer(map()) -> map().
assignment_transfer(Assignment) ->
    Assignment.
