-module(emqttd_plugin_jsonlog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:stop(lager),
    Path = application:get_env(emqttd_plugin_jsonlog, location_path, "log"),
    File = application:get_env(emqttd_plugin_jsonlog, file_name, "json.log"),
    Size = application:get_env(emqttd_plugin_jsonlog, rotation_size, 10000),
    Count = application:get_env(emqttd_plugin_jsonlog, count_file, 1),
    Sinks = [{topic_lager_event, [
                {handlers, [
                    {lager_file_backend, [
                        {file, File},
                        {size, Size}, 
                        {formatter_config, [
                            message, "\n"
                        ]},
                        {count, Count},
                        {level, info}]}
                ]},
                {async_threshold, 500},
                {async_threshold_window, 50}]
    }],
    ok = application:set_env(lager, log_root, Path),
    ok = application:set_env(lager, extra_sinks, Sinks),
    application:start(lager),
    {ok, Sup} = emqttd_plugin_jsonlog_sup:start_link(),
    _ = emqttd_plugin_jsonlog:load(application:get_all_env()),
    {ok, Sup}.

stop(_State) ->
    ok.


    
