%%%-------------------------------------------------------------------
%%% @doc MCP protocol implementation over JSON-RPC 2.0.
%%%
%%% Handles encoding/decoding and routing of MCP methods.
%%% @end
%%%-------------------------------------------------------------------
-module(barrel_mcp_protocol).

-include("barrel_mcp.hrl").

%% API
-export([
    decode/1,
    encode/1,
    handle/1,
    handle/2,
    error_response/3,
    error_response/4,
    success_response/2,
    success_response/3,
    notification_response/0
]).

%% JSON-RPC envelope helpers (shared by client + server)
-export([
    encode_request/3,
    encode_notification/2,
    encode_response/2,
    encode_error/3,
    decode_envelope/1,
    format_tool_result_external/1,
    drive_async_plan/2,
    drive_async_plan/3
]).

%%====================================================================
%% API
%%====================================================================

%% @doc Decode a JSON-RPC request body. The spec includes `list()'
%% in the success type so the HTTP transport can detect (and reject)
%% JSON-RPC batches.
-spec decode(binary()) -> {ok, map() | list()} | {error, term()}.
decode(Binary) ->
    try
        {ok, json:decode(Binary)}
    catch
        _:_ ->
            {error, parse_error}
    end.

%% @doc Encode a JSON-RPC response.
-spec encode(map()) -> binary().
encode(Response) ->
    iolist_to_binary(json:encode(Response)).

%% @doc Handle a JSON-RPC request with default state.
-spec handle(map() | list()) -> map() | no_response | {async, map()}.
handle(Request) ->
    handle(Request, #{}).

%% @doc Handle a JSON-RPC request with state.
%%
%% Returns one of:
%% <ul>
%%   <li>`map()' — a JSON-RPC response envelope ready to encode.</li>
%%   <li>`no_response' — for inbound notifications.</li>
%%   <li>`{async, AsyncPlan}' — for `tools/call'. The transport
%%       spawns the worker via `(maps:get(spawn, AsyncPlan))(Ctx)'
%%       and waits on its mailbox for a `tool_result' / `tool_error' /
%%       `tool_failed' / `tool_validation_failed' / `cancelled'
%%       message.</li>
%% </ul>
%%
%% MCP forbids JSON-RPC batches (a top-level JSON array) — they are
%% rejected here with `Invalid Request' so non-HTTP callers see the
%% same error as the HTTP transport.
-spec handle(map() | list(), map()) -> map() | no_response | {async, map()}.
handle(L, _State) when is_list(L) ->
    error_response(
        null,
        ?JSONRPC_INVALID_REQUEST,
        <<"Batch requests are not supported">>
    );
handle(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Request, State) ->
    Params = maps:get(<<"params">>, Request, #{}),
    case maps:find(<<"id">>, Request) of
        error ->
            %% No id: this is a notification — no response.
            handle_notification(Method, Params, State),
            no_response;
        {ok, Id} when is_binary(Id); is_integer(Id) ->
            handle_request(Method, Params, Id, State);
        {ok, _BadId} ->
            %% MCP requires id to be a string or integer (and not
            %% null). Anything else is an Invalid Request.
            error_response(
                null,
                ?JSONRPC_INVALID_REQUEST,
                <<"Invalid Request: id must be a string or integer">>
            )
    end;
handle(#{<<"id">> := Id}, _State) when is_binary(Id); is_integer(Id) ->
    error_response(Id, ?JSONRPC_INVALID_REQUEST, <<"Invalid Request">>);
handle(_, _State) ->
    error_response(null, ?JSONRPC_INVALID_REQUEST, <<"Invalid Request">>).

%% @doc Create an error response.
-spec error_response(term(), integer(), binary()) -> map().
error_response(Id, Code, Message) ->
    error_response(Id, Code, Message, #{}).

%% Optional `_meta' map on the error envelope. Empty map is
%% omitted from the wire; the spec's `_meta' is the
%% extensibility hook that lets server / client extensions thread
%% state without changing the JSON-RPC surface.
-spec error_response(term(), integer(), binary(), map()) -> map().
error_response(Id, Code, Message, Meta) ->
    Err = #{
        <<"code">> => Code,
        <<"message">> => Message
    },
    Env = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => Err
    },
    add_meta(Env, Meta).

%% @doc Return a marker for no response (notifications).
-spec notification_response() -> no_response.
notification_response() ->
    no_response.

%%====================================================================
%% Request Handlers
%%====================================================================

handle_request(<<"initialize">>, Params, Id, State) ->
    ServerName = application:get_env(barrel_mcp, server_name, <<"barrel">>),
    ServerVersion = application:get_env(barrel_mcp, server_version, <<"1.0.0">>),
    NegotiatedVersion = negotiate_protocol_version(
        maps:get(<<"protocolVersion">>, Params, undefined)
    ),
    %% Persist client capabilities (notably `sampling') so the server can
    %% later issue server-to-client requests via barrel_mcp_session.
    %% Also persist the negotiated protocol_version on the session.
    _ =
        case maps:find(session_id, State) of
            {ok, SessionId} when is_binary(SessionId) ->
                ClientCaps = maps:get(<<"capabilities">>, Params, #{}),
                _ = barrel_mcp_session:set_client_capabilities(SessionId, ClientCaps),
                _ = barrel_mcp_session:set_protocol_version(SessionId, NegotiatedVersion),
                ok;
            _ ->
                ok
        end,
    BaseCaps = #{
        <<"tools">> => #{<<"listChanged">> => true},
        <<"resources">> => #{
            <<"subscribe">> => true,
            <<"listChanged">> => true
        },
        <<"prompts">> => #{<<"listChanged">> => true},
        <<"logging">> => #{},
        %% Per the MCP tasks SEP (and as enforced by the
        %% reference Python SDK), each operation key is an
        %% object whose presence advertises support; only
        %% `listChanged' is a bare boolean.
        <<"tasks">> => #{
            <<"list">> => #{},
            <<"get">> => #{},
            <<"cancel">> => #{},
            <<"result">> => #{},
            <<"listChanged">> => true
        }
    },
    Caps = maybe_advertise_completions(BaseCaps),
    success_response(Id, #{
        <<"protocolVersion">> => NegotiatedVersion,
        <<"capabilities">> => Caps,
        <<"serverInfo">> => #{
            <<"name">> => ServerName,
            <<"version">> => ServerVersion
        }
    });
handle_request(<<"ping">>, _Params, Id, _State) ->
    success_response(Id, #{});
%% Tools
handle_request(<<"tools/list">>, Params, Id, _State) ->
    Cursor = maps:get(<<"cursor">>, Params, undefined),
    {Page, Next} = paginate(
        barrel_mcp_registry:all(tool),
        Cursor,
        fun({N, _}) -> N end
    ),
    Tools = lists:map(
        fun({Name, Handler}) ->
            Base = #{
                <<"name">> => Name,
                <<"description">> => maps:get(description, Handler, <<>>),
                <<"inputSchema">> => maps:get(input_schema, Handler, #{<<"type">> => <<"object">>})
            },
            with_optional_fields(Base, Handler, [
                {<<"outputSchema">>, output_schema},
                {<<"title">>, title},
                {<<"icons">>, icons},
                {<<"annotations">>, annotations}
            ])
        end,
        Page
    ),
    success_response(Id, with_next_cursor(#{<<"tools">> => Tools}, Next));
handle_request(<<"tools/call">>, Params, Id, _State) ->
    Name = maps:get(<<"name">>, Params, <<>>),
    Args = maps:get(<<"arguments">>, Params, #{}),
    %% Tool dispatch is asynchronous. The transport drives the
    %% lifecycle: it builds `Ctx', invokes the spawn closure, records
    %% the in-flight entry, and waits on its mailbox for one of
    %% `{tool_result, _, _}', `{tool_error, _, _}',
    %% `{tool_failed, _, _}', `{tool_validation_failed, _, _}', or
    %% `{cancelled, _}' (sent by `barrel_mcp_session:cancel_in_flight/2').
    Meta = maps:get(<<"_meta">>, Params, #{}),
    Plan = #{
        request_id => Id,
        meta => Meta,
        spawn => fun(Ctx) ->
            case barrel_mcp_registry:run_tool(Name, Args, Ctx) of
                {ok, Pid} ->
                    Pid;
                {error, _} = Err ->
                    %% Surface as if the worker reported it: the
                    %% transport then maps the error.
                    ReplyTo = maps:get(reply_to, Ctx),
                    RequestId = maps:get(request_id, Ctx),
                    ReplyTo ! {tool_failed, RequestId, Err},
                    %% Return a transient pid so the in-flight
                    %% record has something monitorable.
                    spawn(fun() -> ok end)
            end
        end
    },
    {async, Plan};
%% Resources
handle_request(<<"resources/list">>, Params, Id, _State) ->
    Cursor = maps:get(<<"cursor">>, Params, undefined),
    {Page, Next} = paginate(
        barrel_mcp_registry:all(resource),
        Cursor,
        fun({N, _}) -> N end
    ),
    Resources = lists:map(
        fun({_Name, Handler}) ->
            Base = #{
                <<"uri">> => maps:get(uri, Handler, <<>>),
                <<"name">> => maps:get(name, Handler, <<>>),
                <<"description">> => maps:get(description, Handler, <<>>),
                <<"mimeType">> => maps:get(mime_type, Handler, <<"text/plain">>)
            },
            with_optional_fields(Base, Handler, [
                {<<"title">>, title},
                {<<"icons">>, icons},
                {<<"annotations">>, annotations}
            ])
        end,
        Page
    ),
    success_response(
        Id,
        with_next_cursor(
            #{<<"resources">> => Resources},
            Next
        )
    );
handle_request(<<"resources/read">>, Params, Id, _State) ->
    Uri = maps:get(<<"uri">>, Params, <<>>),
    %% Exact-URI lookup first.
    Resources = barrel_mcp_registry:all(resource),
    case lists:keyfind(Uri, 1, [{maps:get(uri, H, <<>>), N, H} || {N, H} <- Resources]) of
        {Uri, Name, _Handler} ->
            run_resource_read(resource, Name, Params, Uri, Id);
        false ->
            %% Fall back to RFC 6570 template matching against
            %% registered `resource_template' entries.
            case match_resource_template(Uri) of
                {ok, TplName, Vars} ->
                    Args = maps:merge(Params, Vars),
                    run_resource_read(resource_template, TplName, Args, Uri, Id);
                nomatch ->
                    error_response(
                        Id,
                        ?JSONRPC_METHOD_NOT_FOUND,
                        <<"Resource not found">>
                    )
            end
    end;
handle_request(<<"resources/templates/list">>, Params, Id, _State) ->
    Cursor = maps:get(<<"cursor">>, Params, undefined),
    {Page, Next} = paginate(
        barrel_mcp_registry:all(resource_template),
        Cursor,
        fun({N, _}) -> N end
    ),
    Templates = lists:map(
        fun({_Name, Handler}) ->
            Base = #{
                <<"uriTemplate">> => maps:get(uri_template, Handler, <<>>),
                <<"name">> => maps:get(name, Handler, <<>>),
                <<"description">> => maps:get(description, Handler, <<>>),
                <<"mimeType">> => maps:get(mime_type, Handler, <<"text/plain">>)
            },
            Compact = maps:filter(fun(_K, V) -> V =/= <<>> end, Base),
            with_optional_fields(Compact, Handler, [
                {<<"title">>, title},
                {<<"icons">>, icons},
                {<<"annotations">>, annotations}
            ])
        end,
        Page
    ),
    success_response(
        Id,
        with_next_cursor(
            #{<<"resourceTemplates">> => Templates},
            Next
        )
    );
handle_request(<<"resources/subscribe">>, Params, Id, State) ->
    Uri = maps:get(<<"uri">>, Params, <<>>),
    case maps:find(session_id, State) of
        {ok, SessionId} when is_binary(SessionId), Uri =/= <<>> ->
            barrel_mcp_session:subscribe_resource(SessionId, Uri),
            success_response(Id, #{});
        _ ->
            error_response(
                Id,
                ?JSONRPC_INVALID_PARAMS,
                <<"Subscribe requires a session and a uri">>
            )
    end;
handle_request(<<"resources/unsubscribe">>, Params, Id, State) ->
    Uri = maps:get(<<"uri">>, Params, <<>>),
    case maps:find(session_id, State) of
        {ok, SessionId} when is_binary(SessionId), Uri =/= <<>> ->
            barrel_mcp_session:unsubscribe_resource(SessionId, Uri),
            success_response(Id, #{});
        _ ->
            error_response(
                Id,
                ?JSONRPC_INVALID_PARAMS,
                <<"Unsubscribe requires a session and a uri">>
            )
    end;
%% Prompts
handle_request(<<"prompts/list">>, Params, Id, _State) ->
    Cursor = maps:get(<<"cursor">>, Params, undefined),
    {Page, Next} = paginate(
        barrel_mcp_registry:all(prompt),
        Cursor,
        fun({N, _}) -> N end
    ),
    Prompts = lists:map(
        fun({Name, Handler}) ->
            Base = #{
                <<"name">> => Name,
                <<"description">> => maps:get(description, Handler, <<>>),
                <<"arguments">> => lists:map(
                    fun(Arg) ->
                        #{
                            <<"name">> => maps:get(name, Arg, <<>>),
                            <<"description">> => maps:get(description, Arg, <<>>),
                            <<"required">> => maps:get(required, Arg, false)
                        }
                    end,
                    maps:get(arguments, Handler, [])
                )
            },
            with_optional_fields(Base, Handler, [
                {<<"title">>, title},
                {<<"icons">>, icons},
                {<<"annotations">>, annotations}
            ])
        end,
        Page
    ),
    success_response(
        Id,
        with_next_cursor(
            #{<<"prompts">> => Prompts},
            Next
        )
    );
handle_request(<<"prompts/get">>, Params, Id, _State) ->
    Name = maps:get(<<"name">>, Params, <<>>),
    Args = maps:get(<<"arguments">>, Params, #{}),
    case barrel_mcp_registry:run(prompt, Name, Args) of
        {ok, Result} ->
            success_response(Id, #{
                <<"description">> => maps:get(description, Result, <<>>),
                <<"messages">> => maps:get(messages, Result, [])
            });
        {error, {not_found, _, _}} ->
            error_response(Id, ?JSONRPC_METHOD_NOT_FOUND, <<"Prompt not found">>);
        {error, Crash} ->
            log_handler_crash(prompt, Name, Id, Crash),
            error_response(Id, ?MCP_PROMPT_ERROR, <<"Internal prompt error">>)
    end;
%% Tasks
handle_request(<<"tasks/list">>, Params, Id, State) ->
    SessionId = maps:get(session_id, State, undefined),
    Cursor = maps:get(<<"cursor">>, Params, undefined),
    {ok, AllTasks} = barrel_mcp_tasks:list(SessionId, #{}),
    {Page, Next} = paginate(
        AllTasks,
        Cursor,
        fun(T) -> maps:get(<<"taskId">>, T) end
    ),
    success_response(Id, with_next_cursor(#{<<"tasks">> => Page}, Next));
handle_request(<<"tasks/get">>, Params, Id, State) ->
    SessionId = maps:get(session_id, State, undefined),
    TaskId = maps:get(<<"taskId">>, Params, <<>>),
    case barrel_mcp_tasks:get(SessionId, TaskId) of
        {ok, Task} -> success_response(Id, Task);
        {error, not_found} -> error_response(Id, ?JSONRPC_INVALID_PARAMS, <<"Task not found">>)
    end;
handle_request(<<"tasks/cancel">>, Params, Id, State) ->
    SessionId = maps:get(session_id, State, undefined),
    TaskId = maps:get(<<"taskId">>, Params, <<>>),
    case barrel_mcp_tasks:cancel(SessionId, TaskId) of
        ok ->
            %% Spec / reference SDK expect the cancelled Task back,
            %% not an empty object.
            case barrel_mcp_tasks:get(SessionId, TaskId) of
                {ok, Task} -> success_response(Id, Task);
                {error, not_found} -> success_response(Id, #{})
            end;
        {error, not_found} ->
            error_response(Id, ?JSONRPC_INVALID_PARAMS, <<"Task not found">>)
    end;
handle_request(<<"tasks/result">>, Params, Id, State) ->
    SessionId = maps:get(session_id, State, undefined),
    TaskId = maps:get(<<"taskId">>, Params, <<>>),
    case barrel_mcp_tasks:get(SessionId, TaskId) of
        {ok, #{<<"status">> := <<"completed">>} = T} ->
            Result = maps:get(<<"result">>, T, #{}),
            success_response(Id, Result);
        {ok, #{<<"status">> := <<"failed">>} = T} ->
            Err = maps:get(<<"error">>, T, <<"Task failed">>),
            error_response(Id, ?MCP_TOOL_ERROR, Err);
        {ok, #{<<"status">> := <<"cancelled">>}} ->
            error_response(
                Id,
                ?JSONRPC_INVALID_PARAMS,
                <<"Task cancelled">>
            );
        {ok, #{<<"status">> := _}} ->
            error_response(
                Id,
                ?JSONRPC_INVALID_PARAMS,
                <<"Task not yet complete">>
            );
        {error, not_found} ->
            error_response(Id, ?JSONRPC_INVALID_PARAMS, <<"Task not found">>)
    end;
%% Completions
handle_request(<<"completion/complete">>, Params, Id, _State) ->
    Ref = maps:get(<<"ref">>, Params, #{}),
    Argument = maps:get(<<"argument">>, Params, #{}),
    ArgName = maps:get(<<"name">>, Argument, <<>>),
    Value = maps:get(<<"value">>, Argument, <<>>),
    case completion_lookup_key(Ref, ArgName) of
        undefined ->
            success_response(Id, #{<<"completion">> => empty_completion()});
        Key ->
            case barrel_mcp_registry:run_completion(Key, Value, #{}) of
                {ok, {ok, Values}} ->
                    success_response(Id, #{
                        <<"completion">> =>
                            completion_payload(Values, false)
                    });
                {ok, {ok, Values, #{has_more := HasMore}}} ->
                    success_response(Id, #{
                        <<"completion">> =>
                            completion_payload(Values, HasMore)
                    });
                {error, {not_found, _, _}} ->
                    success_response(Id, #{<<"completion">> => empty_completion()});
                {error, Crash} ->
                    log_handler_crash(completion, Key, Id, Crash),
                    error_response(
                        Id,
                        ?JSONRPC_INTERNAL_ERROR,
                        <<"Internal completion error">>
                    )
            end
    end;
%% Logging
handle_request(<<"logging/setLevel">>, Params, Id, State) ->
    Level = maps:get(<<"level">>, Params, undefined),
    case {Level, maps:find(session_id, State)} of
        {undefined, _} ->
            error_response(
                Id,
                ?JSONRPC_INVALID_PARAMS,
                <<"Missing required parameter: level">>
            );
        {_, error} ->
            %% Stdio / no session — accept but no per-session storage.
            case barrel_mcp_session:log_level_priority(Level) of
                error ->
                    error_response(
                        Id,
                        ?JSONRPC_INVALID_PARAMS,
                        <<"Invalid log level">>
                    );
                _ ->
                    success_response(Id, #{})
            end;
        {_, {ok, undefined}} ->
            case barrel_mcp_session:log_level_priority(Level) of
                error ->
                    error_response(
                        Id,
                        ?JSONRPC_INVALID_PARAMS,
                        <<"Invalid log level">>
                    );
                _ ->
                    success_response(Id, #{})
            end;
        {_, {ok, SessionId}} ->
            case barrel_mcp_session:set_log_level(SessionId, Level) of
                ok ->
                    success_response(Id, #{});
                {error, invalid_level} ->
                    error_response(
                        Id,
                        ?JSONRPC_INVALID_PARAMS,
                        <<"Invalid log level">>
                    );
                {error, not_found} ->
                    success_response(Id, #{})
            end
    end;
%% Unknown method
handle_request(Method, _Params, Id, _State) ->
    error_response(
        Id,
        ?JSONRPC_METHOD_NOT_FOUND,
        <<"Method not found: ", Method/binary>>
    ).

%%====================================================================
%% Notification Handlers
%%====================================================================

%% Spec name (2025-03-26+).
handle_notification(<<"notifications/initialized">>, _Params, _State) ->
    ok;
%% Legacy bare name kept for one release; older clients still send this.
handle_notification(<<"initialized">>, _Params, _State) ->
    ok;
handle_notification(<<"notifications/cancelled">>, Params, State) ->
    case maps:find(session_id, State) of
        {ok, SessionId} when is_binary(SessionId) ->
            case maps:find(<<"requestId">>, Params) of
                {ok, RequestId} ->
                    barrel_mcp_session:cancel_in_flight(SessionId, RequestId);
                error ->
                    ok
            end;
        _ ->
            ok
    end;
handle_notification(<<"notifications/progress">>, _Params, _State) ->
    %% The server doesn't currently emit anything special on inbound
    %% client-side progress notifications (used for client→server
    %% requests, which we don't have). Acknowledge silently.
    ok;
handle_notification(<<"notifications/roots/list_changed">>, Params, State) ->
    case application:get_env(barrel_mcp, roots_changed_handler) of
        {ok, {Mod, Fun}} ->
            try
                Mod:Fun(Params, State)
            catch
                _:_ -> ok
            end;
        _ ->
            ok
    end;
handle_notification(_, _Params, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

success_response(Id, Result) ->
    success_response(Id, Result, #{}).

success_response(Id, Result, Meta) ->
    Env = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    add_meta(Env, Meta).

%% Add `_meta' to a JSON-RPC envelope only when the supplied map
%% is non-empty. Keeps wire payloads compact.
add_meta(Env, Meta) when is_map(Meta), map_size(Meta) > 0 ->
    Env#{<<"_meta">> => Meta};
add_meta(Env, _) ->
    Env.

%% @doc Format a tool handler's plain return value into the MCP
%% content-block list shape. Public so transports driving async
%% tool calls (HTTP / stdio) can produce identical envelopes.
-spec format_tool_result_external(term()) -> [map()].
format_tool_result_external(Result) ->
    format_tool_result(Result).

%% @doc Drive an `{async, AsyncPlan}' from `handle/2' to completion
%% on the calling process and return a JSON-RPC response map.
%%
%% Used by transports that don't have their own request/wait
%% machinery (stdio, legacy HTTP). The Streamable HTTP transport
%% drives async plans itself because it needs to record per-session
%% in-flight entries for cancellation routing.
-spec drive_async_plan(map(), timeout()) -> map().
drive_async_plan(Plan, Timeout) ->
    drive_async_plan(Plan, Timeout, undefined).

%% @doc As {@link drive_async_plan/2}, but threads the authenticated
%% principal (`AuthInfo', the auth provider's `authenticate/2' map) into
%% the tool `Ctx' under `auth_info'. Transports that authenticate before
%% driving the plan (the simple HTTP transport) pass it here; callers
%% with no auth provider use the `/2' form and `auth_info' is `undefined'.
-spec drive_async_plan(map(), timeout(), term()) -> map().
drive_async_plan(Plan, Timeout, AuthInfo) ->
    Self = self(),
    RequestId = maps:get(request_id, Plan),
    Spawn = maps:get(spawn, Plan),
    Meta = maps:get(meta, Plan, #{}),
    Ctx = #{
        request_id => RequestId,
        session_id => undefined,
        progress_token => undefined,
        meta => Meta,
        emit_progress => fun(_, _, _) -> ok end,
        reply_to => Self,
        auth_info => AuthInfo
    },
    _Pid = Spawn(Ctx),
    receive
        {tool_result, RequestId, Result} ->
            success_response(
                RequestId,
                #{<<"content">> => format_tool_result_external(Result)}
            );
        {tool_result_meta, RequestId, Result, RespMeta} ->
            success_response(
                RequestId,
                #{<<"content">> => format_tool_result_external(Result)},
                RespMeta
            );
        {tool_structured, RequestId, Data, Content} ->
            success_response(
                RequestId,
                #{
                    <<"content">> => Content,
                    <<"structuredContent">> => Data
                }
            );
        {tool_structured_meta, RequestId, Data, Content, RespMeta} ->
            success_response(
                RequestId,
                #{
                    <<"content">> => Content,
                    <<"structuredContent">> => Data
                },
                RespMeta
            );
        {tool_error, RequestId, Content} ->
            success_response(
                RequestId,
                #{
                    <<"content">> => Content,
                    <<"isError">> => true
                }
            );
        {tool_error_meta, RequestId, Content, RespMeta} ->
            success_response(
                RequestId,
                #{
                    <<"content">> => Content,
                    <<"isError">> => true
                },
                RespMeta
            );
        {tool_validation_failed, RequestId, Errors} ->
            Msg = iolist_to_binary(
                io_lib:format(
                    "Invalid tool input: ~p", [Errors]
                )
            ),
            success_response(
                RequestId,
                #{
                    <<"content">> =>
                        [#{<<"type">> => <<"text">>, <<"text">> => Msg}],
                    <<"isError">> => true
                }
            );
        {tool_failed, RequestId, _Reason} ->
            %% Crash details are logged server-side by the registry; do
            %% not echo `Reason' back to the wire (it can carry module
            %% paths, file paths, or secret-bearing exception terms).
            error_response(
                RequestId,
                ?MCP_TOOL_ERROR,
                <<"Internal tool error">>
            )
    after Timeout ->
        error_response(RequestId, ?MCP_TOOL_ERROR, <<"Tool timed out">>)
    end.

format_tool_result(Result) when is_binary(Result) ->
    [#{<<"type">> => <<"text">>, <<"text">> => Result}];
format_tool_result(Result) when is_map(Result) ->
    case maps:get(<<"type">>, Result, undefined) of
        undefined ->
            [#{<<"type">> => <<"text">>, <<"text">> => iolist_to_binary(json:encode(Result))}];
        _ ->
            [Result]
    end;
format_tool_result(Result) when is_list(Result) ->
    Result;
format_tool_result(Result) ->
    [#{<<"type">> => <<"text">>, <<"text">> => io_lib:format("~p", [Result])}].

%% Run a resource handler (exact match or template) and shape
%% the response.
run_resource_read(Type, Name, Args, Uri, Id) ->
    case barrel_mcp_registry:run(Type, Name, Args) of
        {ok, Result} ->
            Content = format_resource_result(Uri, Result),
            success_response(Id, #{<<"contents">> => Content});
        {error, Crash} ->
            log_handler_crash(resource, Name, Id, Crash),
            error_response(Id, ?MCP_RESOURCE_ERROR, <<"Internal resource error">>)
    end.

%% Walk the registered resource_template entries and return the
%% first whose `uri_template' matches `Uri'.
match_resource_template(Uri) ->
    Templates = barrel_mcp_registry:all(resource_template),
    do_match_template(Uri, Templates).

do_match_template(_Uri, []) ->
    nomatch;
do_match_template(Uri, [{Name, Handler} | Rest]) ->
    Tpl = maps:get(uri_template, Handler, <<>>),
    case Tpl of
        <<>> ->
            do_match_template(Uri, Rest);
        _ ->
            case barrel_mcp_uri_template:match(Uri, Tpl) of
                {ok, Vars} -> {ok, Name, Vars};
                nomatch -> do_match_template(Uri, Rest)
            end
    end.

format_resource_result(Uri, Result) when is_list(Result) ->
    [add_resource_uri(Uri, B) || B <- Result];
format_resource_result(Uri, Result) when is_binary(Result) ->
    [#{<<"uri">> => Uri, <<"text">> => Result}];
format_resource_result(Uri, #{text := Text} = M) ->
    Block = #{<<"uri">> => Uri, <<"text">> => Text},
    [decorate_block(Block, M)];
format_resource_result(Uri, #{blob := Blob, mimeType := MimeType} = M) ->
    Block = #{
        <<"uri">> => Uri,
        <<"blob">> => base64:encode(Blob),
        <<"mimeType">> => MimeType
    },
    [decorate_block(Block, M)];
format_resource_result(Uri, Result) when is_map(Result) ->
    [#{<<"uri">> => Uri, <<"text">> => iolist_to_binary(json:encode(Result))}];
format_resource_result(Uri, Result) ->
    [#{<<"uri">> => Uri, <<"text">> => io_lib:format("~p", [Result])}].

%% Pass `annotations' / `mimeType' through onto an already-built block.
decorate_block(Block, M) ->
    Block1 =
        case maps:find(mimeType, M) of
            {ok, Mime} -> Block#{<<"mimeType">> => Mime};
            error -> Block
        end,
    case maps:find(annotations, M) of
        {ok, Ann} -> Block1#{<<"annotations">> => Ann};
        error -> Block1
    end.

%% Inject `uri' into a pre-built content block (binary-keyed map).
add_resource_uri(Uri, Block) when is_map(Block) ->
    case maps:is_key(<<"uri">>, Block) of
        true -> Block;
        false -> Block#{<<"uri">> => Uri}
    end.

%% Log a crashed resource/prompt/completion handler server-side and
%% never surface the exception term to the client: a caught `Reason'
%% can carry internal paths, argument values or secret-bearing terms.
%% The wire layer returns a generic message; operators cross-reference
%% via the request id. Mirrors the tool path in barrel_mcp_registry.
log_handler_crash(Kind, Name, Id, Crash) ->
    logger:error(
        "~p handler crashed: ~p (request_id=~p, name=~p)",
        [Kind, Crash, Id, Name]
    ).

maybe_advertise_completions(Caps) ->
    case barrel_mcp_registry:all(completion) of
        [] -> Caps;
        _ -> Caps#{<<"completions">> => #{}}
    end.

completion_lookup_key(
    #{<<"type">> := <<"ref/prompt">>, <<"name">> := Name},
    ArgName
) when is_binary(Name) ->
    <<"prompt:", Name/binary, ":", ArgName/binary>>;
completion_lookup_key(
    #{<<"type">> := <<"ref/resource">>, <<"uri">> := Uri},
    ArgName
) when is_binary(Uri) ->
    <<"resource_template:", Uri/binary, ":", ArgName/binary>>;
completion_lookup_key(_, _) ->
    undefined.

empty_completion() ->
    #{<<"values">> => [], <<"hasMore">> => false}.

completion_payload(Values, HasMore) when is_list(Values) ->
    #{
        <<"values">> => Values,
        <<"hasMore">> => HasMore =:= true,
        <<"total">> => length(Values)
    }.

%% Add optional fields from a Handler map to a wire envelope. Each
%% pair `{WireKey, HandlerKey}' becomes `WireKey => Value' in the
%% envelope only when the value is present and not the empty
%% binary; this keeps wire payloads compact and back-compat.
with_optional_fields(Envelope, Handler, Fields) ->
    lists:foldl(
        fun({WireKey, HandlerKey}, Acc) ->
            case maps:get(HandlerKey, Handler, undefined) of
                undefined -> Acc;
                <<>> -> Acc;
                V -> Acc#{WireKey => V}
            end
        end,
        Envelope,
        Fields
    ).

%%====================================================================
%% Cursor pagination for `*/list' handlers
%%====================================================================

-define(PAGE_SIZE, 50).

%% Sort `Items' by `KeyFn(Item)', drop everything up to and
%% including `Cursor', take up to `?PAGE_SIZE'. Returns
%% `{Page, NextCursor}' where `NextCursor' is the last key of the
%% page when more items remain, or `undefined' otherwise.
%% `Cursor' is the opaque last-seen key from a prior response.
paginate(Items, Cursor, KeyFn) ->
    Sorted = lists:sort(fun(A, B) -> KeyFn(A) =< KeyFn(B) end, Items),
    AfterCursor = drop_until_after(Sorted, Cursor, KeyFn),
    case
        lists:split(
            min(?PAGE_SIZE, length(AfterCursor)),
            AfterCursor
        )
    of
        {Page, []} ->
            {Page, undefined};
        {Page, _Rest} ->
            Last = lists:last(Page),
            {Page, KeyFn(Last)}
    end.

drop_until_after(Items, undefined, _) ->
    Items;
drop_until_after(Items, Cursor, KeyFn) ->
    lists:dropwhile(fun(I) -> KeyFn(I) =< Cursor end, Items).

with_next_cursor(Resp, undefined) -> Resp;
with_next_cursor(Resp, Cursor) -> Resp#{<<"nextCursor">> => Cursor}.

%% Pick the protocol version to advertise in the `initialize'
%% response. If the client's requested version is one we speak, echo
%% it; otherwise return our preferred version and let the client
%% decide.
negotiate_protocol_version(undefined) ->
    ?MCP_PROTOCOL_VERSION;
negotiate_protocol_version(Requested) when is_binary(Requested) ->
    case lists:member(Requested, ?MCP_SUPPORTED_VERSIONS) of
        true -> Requested;
        false -> ?MCP_PROTOCOL_VERSION
    end.

%%====================================================================
%% JSON-RPC envelope helpers
%%====================================================================

%% @doc Build a JSON-RPC request envelope.
-spec encode_request(term(), binary(), map()) -> map().
encode_request(Id, Method, Params) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method,
        <<"params">> => Params
    }.

%% @doc Build a JSON-RPC notification envelope (no id).
-spec encode_notification(binary(), map()) -> map().
encode_notification(Method, Params) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    }.

%% @doc Build a JSON-RPC success response.
-spec encode_response(term(), term()) -> map().
encode_response(Id, Result) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    }.

%% @doc Build a JSON-RPC error response. Alias of `error_response/3'.
-spec encode_error(term(), integer(), binary()) -> map().
encode_error(Id, Code, Message) ->
    error_response(Id, Code, Message).

%% @doc Classify a decoded JSON-RPC envelope.
%%
%% Returns the kind so client and server agree on routing without each
%% having to peek at the same keys.
-spec decode_envelope(map()) ->
    {request, Id :: term(), Method :: binary(), Params :: map()}
    | {notification, Method :: binary(), Params :: map()}
    | {response, Id :: term(), Result :: term()}
    | {error, Id :: term(), Code :: integer(), Message :: binary(), Data :: term()}
    | {invalid, term()}.
decode_envelope(L) when is_list(L) ->
    {invalid, batch_unsupported};
decode_envelope(#{<<"jsonrpc">> := <<"2.0">>} = Msg) ->
    case
        {
            maps:find(<<"method">>, Msg),
            maps:find(<<"id">>, Msg),
            maps:find(<<"result">>, Msg),
            maps:find(<<"error">>, Msg)
        }
    of
        {{ok, Method}, {ok, Id}, error, error} when
            is_binary(Id) orelse is_integer(Id)
        ->
            {request, Id, Method, maps:get(<<"params">>, Msg, #{})};
        {{ok, _Method}, {ok, _BadId}, error, error} ->
            {invalid, bad_id};
        {{ok, Method}, error, error, error} ->
            {notification, Method, maps:get(<<"params">>, Msg, #{})};
        {error, {ok, Id}, {ok, Result}, error} when
            is_binary(Id) orelse is_integer(Id)
        ->
            {response, Id, Result};
        {error, {ok, _BadId}, {ok, _Result}, error} ->
            {invalid, bad_id};
        {error, {ok, Id}, error, {ok, Err}} when
            is_binary(Id) orelse is_integer(Id)
        ->
            Code = maps:get(<<"code">>, Err, ?JSONRPC_INTERNAL_ERROR),
            Message = maps:get(<<"message">>, Err, <<>>),
            Data = maps:get(<<"data">>, Err, undefined),
            {error, Id, Code, Message, Data};
        {error, {ok, _BadId}, error, {ok, _Err}} ->
            {invalid, bad_id};
        _ ->
            {invalid, malformed}
    end;
decode_envelope(Other) ->
    {invalid, Other}.
