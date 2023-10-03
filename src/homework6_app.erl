-module(homework6_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    homework6_sup:start_link(),
    homework6:start_link().

stop(_State) ->
    ok.

%% internal functions
