-module(ewalker_app).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start() ->
    application:start(ewalker).

start(_StartType, _StartArgs) ->
    ewalker_sup:start_link().

stop(_State) ->
    case whereis ( console ) of
        undefined ->
            ok;
        Pid ->
            Pid ! byebye
    end,
    ok.
