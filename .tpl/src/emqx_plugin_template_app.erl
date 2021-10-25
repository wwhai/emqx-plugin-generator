-module(emqx_{{.PluginName}}_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_{{.PluginName}}_sup:start_link(),
    emqx_{{.PluginName}}:load(application:get_all_env()),
    {ok, Sup}.

stop(_State) ->
    emqx_{{.PluginName}}:unload().

