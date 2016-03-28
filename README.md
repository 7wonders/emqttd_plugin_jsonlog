## Overview

A simple plugin for logging mqtt data as json.


## Build Plugin

Build the plugin in emqttd project. Checkout the plugin to 'emqttd/plugins/' folder:

If the submodules exist:

```
git submodule update --remote plugins/emqttd_plugin_jsonlog
```

Orelse:

```
git submodule add https://github.com/RDXT/emqttd_plugin_jsonlog.git plugins/emqttd_plugin_jsonlog

make && make dist
```


## Configure Plugin

File: etc/plugin.config

```erlang
[
  %% you can have many files based on different topics if you want
  {emqttd_plugin_jsonlog, [
    %% file location "/tmp/log" or "./"
    {location_path, "log"},
    {file_name, "json.log"},
    %% size before file is rotated
    {rotation_size, 1073741824},
    {count_file, 1},
    %% "#" or <<"topic1/#">> or [<<"flow/#">>, <<"car/#">>]
    {topics, "#"}
  ]}
].
```


## Load Plugin

```
./bin/emqttd_ctl plugins load emqttd_plugin_jsonlog
```