-module(emqttd_plugin_jsonlog).
-export([load/1, unload/0]).

-include("../../../include/emqttd.hrl").

%% Hooks functions
-export([on_message_publish/2]).

%% Called when the plugin application start
load(Env) ->
    emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]).

%% transform message and return
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};
on_message_publish(Message, _Env) ->
    Args = {Message#mqtt_message.from,
            Message#mqtt_message.timestamp,
            Message#mqtt_message.topic,
            Message#mqtt_message.payload},
    ok = write_log(Args),
    ok = emqttd_plugin_jsonlog_worker:write_log(Args),
    {ok, Message}.

%% Called when the plugin application stop
unload() ->
    emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2).

write_log({_ClientId, _Timestamp, Topic, _Payload} = Tuple) ->
    MatchTopics = application:get_env(?MODULE, topics, "#"),
    case is_topic(MatchTopics, Topic) of
        true ->
            Json = encode_json(Tuple),
            lager:log(topic_lager_event, info, [], "~ts", [Json]);
        false -> ok
    end.

is_topic("#", _) -> true;
is_topic([_|_] = MatchTopics, Topic) ->
    ListT = [begin 
        [H|_] = binary:split(T, <<"#">>),
        H
    end || T <- MatchTopics],
    _ = binary:match(Topic, ListT) =/= nomatch;
is_topic(MatchTopics, Topic) when is_binary(MatchTopics) ->
    [MatchT|_] = binary:split(MatchTopics, <<"#">>),
    _ = binary:match(Topic, MatchT) =/= nomatch.

encode_json({ClientId, Timestamp, Topic, Payload}) ->
    PayloadBase64 = base64:encode(Payload),
    DateTime = format_timestamp(Timestamp),
    DataFormat = [{<<"clientid">>, ClientId},
                  {<<"timestamp">>, DateTime},
                  {<<"topic">>, Topic},
                  {<<"payload">>, PayloadBase64}],
    _ = jsx:encode(DataFormat).

format_timestamp(Timestamp) ->
    DateTime = calendar:now_to_datetime(Timestamp),
    {{Y, M, D}, {H, Mn, S}} = DateTime,
    T = lists:flatten(io_lib:format("~p-~p-~p ~p:~p:~p", [Y, M, D, H, Mn, S])),
    _ = list_to_binary(T).
