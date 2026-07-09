-module(imboy_mcp_tools).

%%%
% MCP tool wrapper（Phase 3 T3.4）
%
% 把 imboy 现有 logic/ds 层薄封装成 MCP tool，供外部 AI（Claude/Cursor）经
% POST /api/v1/mcp 的 tools/call 调用。本模块只做参数规整 + 越权门控 + 调 logic，
% 零新业务（分层：MCP tool → logic/ds，绝不直连 repo）。
%
% 越权红线：调用者身份一律以 Ctx.auth_info（mcp_handler 从 JWT 注入的 uid）为准，
% **绝不信任** Args 里客户端自报的 uid/gid；先用 auth_info 做 owner/member 判定再调 logic。
%
% 首批 6 个 tool（均不触碰消息投递通道）：
%   get_user_profile   → user_logic:find_by_id/1（仅本人或好友可查）
%   get_contacts       → friend_ds:page_by_uid/3（只查调用者自己的好友）
%   search_messages    → fts_logic:search_msg/5（uid 强制为调用者，权限过滤内建）
%   list_group_members → group_member_ds:list_members/1（仅群成员可列）
%   list_conversations → conversation_logic:list/2（只取调用者自己的会话）
%   create_group       → group_logic:add/4（建群者=调用者，配额 count_by_owner 内建）
%
% 发消息类（send_message → msg_c2c_logic）触及消息投递通道，留待 review 后单独接入。
%
% 常驻 worker：挂 imboy_sup（transient），启动后等 registry 就绪即注册全部 tool；
% reg_all/0 幂等（reg 覆盖同名），registry 若异常重启则监听 DOWN 后自动重注册。
%%%

-behaviour(gen_server).

%% 启动 + 注册 API
-export([start_link/0, reg_all/0]).
%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).
%% tool handlers（arity 2：第二参 Ctx 携带 auth_info）
-export([
    get_user_profile/2,
    get_contacts/2,
    search_messages/2,
    list_group_members/2,
    list_conversations/2,
    create_group/2
]).

%% registry 就绪等待超时；重试间隔
-define(WAIT_TIMEOUT, 5000).
-define(RETRY_MS, 1000).
%% 分页上限（防客户端拉全表）
-define(MAX_PAGE_SIZE, 100).
-define(DEF_PAGE_SIZE, 50).
%% 会话列表拉取上限 / 默认
-define(MAX_CONV_LIMIT, 500).
-define(DEF_CONV_LIMIT, 100).

%%====================================================================
%% 启动 / 注册
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 幂等注册全部 tool（reg 覆盖同名）。registry 须已就绪。
-spec reg_all() -> ok.
reg_all() ->
    ok = reg(
        <<"get_user_profile">>,
        get_user_profile,
        <<"查询用户资料（仅本人或好友，返回昵称/头像等）"/utf8>>,
        profile_schema()
    ),
    ok = reg(
        <<"get_contacts">>,
        get_contacts,
        <<"分页获取调用者自己的好友列表"/utf8>>,
        contacts_schema()
    ),
    ok = reg(
        <<"search_messages">>,
        search_messages,
        <<"在调用者可见范围内全文搜索消息（C2C/C2G）"/utf8>>,
        search_schema()
    ),
    ok = reg(
        <<"list_group_members">>,
        list_group_members,
        <<"列出群成员（仅群成员可调用）"/utf8>>,
        members_schema()
    ),
    ok = reg(
        <<"list_conversations">>,
        list_conversations,
        <<"获取调用者自己的会话列表（C2C + C2G）"/utf8>>,
        conversations_schema()
    ),
    ok = reg(
        <<"create_group">>,
        create_group,
        <<"创建群（建群者为调用者，受其建群配额限制）"/utf8>>,
        create_group_schema()
    ),
    ok.

reg(Name, Fun, Desc, Schema) ->
    barrel_mcp_registry:reg(tool, Name, ?MODULE, Fun, #{
        description => Desc,
        input_schema => Schema
    }).

%%====================================================================
%% tool handlers
%%====================================================================

%% @doc 查用户资料：仅允许查本人或好友。
get_user_profile(Args, Ctx) ->
    with_caller(Ctx, fun(Caller) ->
        Target = to_int(maps:get(<<"uid">>, Args, Caller)),
        case Target =:= Caller orelse friend_ds:is_friend(Caller, Target) of
            true -> {structured, user_logic:find_by_id(Target)};
            false -> tool_error(<<"无权查看该用户资料"/utf8>>)
        end
    end).

%% @doc 好友列表：只查调用者自己（忽略 Args 自报 uid）。
get_contacts(Args, Ctx) ->
    with_caller(Ctx, fun(Caller) ->
        {Page, Size} = page_size(Args),
        Offset = (Page - 1) * Size,
        List = friend_ds:page_by_uid(Caller, Size, Offset),
        {structured, #{<<"list">> => List, <<"page">> => Page, <<"size">> => Size}}
    end).

%% @doc 全文搜索：uid 强制为调用者，logic 内建权限过滤。
search_messages(Args, Ctx) ->
    with_caller(Ctx, fun(Caller) ->
        Kw = maps:get(<<"keyword">>, Args, <<>>),
        case is_binary(Kw) andalso byte_size(Kw) > 0 of
            false ->
                tool_error(<<"keyword 不能为空"/utf8>>);
            true ->
                {Page, Size} = page_size(Args),
                Type = maps:get(<<"type">>, Args, <<"C2C">>),
                {structured, fts_logic:search_msg(Caller, Page, Size, Kw, Type)}
        end
    end).

%% @doc 列群成员：仅群成员可调用。
list_group_members(Args, Ctx) ->
    with_caller(Ctx, fun(Caller) ->
        Gid = to_int(maps:get(<<"group_id">>, Args, 0)),
        case group_member_ds:is_member(Gid, Caller) of
            true ->
                case group_member_ds:list_members(Gid) of
                    {ok, Members} -> {structured, #{<<"list">> => Members}};
                    {error, _} -> tool_error(<<"获取群成员失败"/utf8>>)
                end;
            false ->
                tool_error(<<"你不是该群成员"/utf8>>)
        end
    end).

%% @doc 会话列表：只取调用者自己的（忽略 Args 自报 uid）。
list_conversations(Args, Ctx) ->
    with_caller(Ctx, fun(Caller) ->
        Limit = min(?MAX_CONV_LIMIT, max(1, to_int(maps:get(<<"limit">>, Args, ?DEF_CONV_LIMIT)))),
        {ok, List} = conversation_logic:list(Caller, #{limit => Limit}),
        {structured, #{<<"list">> => List}}
    end).

%% @doc 建群：建群者一律为调用者，配额经 count_by_owner 内建。
create_group(Args, Ctx) ->
    with_caller(Ctx, fun(Caller) ->
        Type = to_int(maps:get(<<"type">>, Args, 2)),
        MemberUids =
            case maps:get(<<"member_uids">>, Args, []) of
                L when is_list(L) -> L;
                _ -> []
            end,
        Count = group_logic:count_by_owner(Caller),
        case group_logic:add(Count, Caller, Type, MemberUids) of
            {ok, Gid} -> {structured, #{<<"group_id">> => Gid}};
            {error, Reason} -> tool_error(Reason)
        end
    end).

%%====================================================================
%% tool 内部辅助
%%====================================================================

%% 未认证（uid=0）统一拒绝；否则把调用者 uid 交给 Fun。
with_caller(Ctx, Fun) ->
    case maps:get(auth_info, Ctx, 0) of
        Caller when is_integer(Caller), Caller > 0 -> Fun(Caller);
        _ -> tool_error(<<"未认证或身份无效"/utf8>>)
    end.

tool_error(Msg) ->
    {tool_error, [#{<<"type">> => <<"text">>, <<"text">> => Msg}]}.

to_int(V) when is_integer(V) -> V;
to_int(V) -> elib_cnv:safe_to_integer(V).

%% 规整分页：page>=1，1=<size=<MAX_PAGE_SIZE。
page_size(Args) ->
    Page = max(1, to_int(maps:get(<<"page">>, Args, 1))),
    Size0 = to_int(maps:get(<<"size">>, Args, ?DEF_PAGE_SIZE)),
    Size = min(?MAX_PAGE_SIZE, max(1, Size0)),
    {Page, Size}.

%%====================================================================
%% input_schema（binary-key JSON Schema，直接进 tools/list）
%%====================================================================

profile_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"uid">> => #{
                <<"type">> => <<"integer">>,
                <<"description">> => <<"目标用户 uid，缺省为本人"/utf8>>
            }
        }
    }.

contacts_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => page_props()
    }.

search_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => maps:merge(
            #{
                <<"keyword">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"搜索关键词"/utf8>>
                },
                <<"type">> => #{
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"C2C">>, <<"C2G">>],
                    <<"description">> => <<"会话类型，默认 C2C"/utf8>>
                }
            },
            page_props()
        ),
        <<"required">> => [<<"keyword">>]
    }.

members_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"group_id">> => #{
                <<"type">> => <<"integer">>,
                <<"description">> => <<"群 ID"/utf8>>
            }
        },
        <<"required">> => [<<"group_id">>]
    }.

page_props() ->
    #{
        <<"page">> => #{<<"type">> => <<"integer">>, <<"description">> => <<"页码，从 1 起"/utf8>>},
        <<"size">> => #{<<"type">> => <<"integer">>, <<"description">> => <<"每页数量，上限 100"/utf8>>}
    }.

conversations_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"limit">> => #{
                <<"type">> => <<"integer">>,
                <<"description">> => <<"最多返回条数，默认 100，上限 500"/utf8>>
            }
        }
    }.

create_group_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"type">> => #{
                <<"type">> => <<"integer">>,
                <<"enum">> => [1, 2],
                <<"description">> => <<"群类型：1 公开，2 私有（默认）"/utf8>>
            },
            <<"member_uids">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => #{<<"type">> => <<"integer">>},
                <<"description">> => <<"初始成员 uid 列表（不含建群者）"/utf8>>
            }
        }
    }.

%%====================================================================
%% gen_server：常驻注册 + registry 重启后重注册
%%====================================================================

init([]) ->
    %% 不在 init 阻塞 sup 启动，延到 continue 里等 registry 就绪。
    {ok, #{mon => undefined}, {continue, register}}.

handle_continue(register, State) ->
    {noreply, do_register(State)}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% registry 异常退出 → 待其重启就绪后重注册（reg_all 幂等）。
handle_info({'DOWN', Ref, process, _Pid, _Reason}, #{mon := Ref} = State) ->
    {noreply, do_register(State)};
handle_info(retry_register, State) ->
    {noreply, do_register(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%% 等 registry 就绪 → reg_all → monitor 其 pid；未就绪则定时重试。
do_register(State) ->
    case barrel_mcp_registry:wait_for_ready(?WAIT_TIMEOUT) of
        ok ->
            ok = reg_all(),
            case whereis(barrel_mcp_registry) of
                undefined ->
                    erlang:send_after(?RETRY_MS, self(), retry_register),
                    State#{mon => undefined};
                Pid ->
                    State#{mon => erlang:monitor(process, Pid)}
            end;
        {error, _R} ->
            erlang:send_after(?RETRY_MS, self(), retry_register),
            State#{mon => undefined}
    end.
