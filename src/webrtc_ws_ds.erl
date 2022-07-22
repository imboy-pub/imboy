-module(webrtc_ws_ds).

-export([json_encode/1,
         json_decode/1,
         text_event/1,
         text_event/2]).

json_decode(Data) ->
  jsone:decode(Data, [{object_format, map},{keys, atom}]).

json_encode(Data) ->
  jsone:encode(Data, [native_utf8]).

text_event(Event) ->
  {text, json_encode(#{event => Event})}.

text_event(Event, Data) ->
  {text, json_encode(#{event => Event, data => Data})}.
