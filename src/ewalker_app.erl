-module(ewalker_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
  % In case we have an api key for hoptoad, use it to start the erlhoptoad application.
  Hoptoad = case application:get_env(ewalker, hoptoad_apikey) of
     {ok, undefined} -> undefined_in_appfile;
     {ok, ApiKey} ->
        application:load(erlhoptoad),
        application:load(ibrowse),

        application:set_env(erlhoptoad, apikey, ApiKey),

        ok = application:start(ibrowse),
        ok = application:start(erlhoptoad)
     ;undefined -> undefined
  end,

  % io:format("~w~n", [Hoptoad]),


  ewalker_sup:start_link().

stop(_State) ->
    case whereis ( console ) of
        undefined ->
            ok;
        Pid ->
            Pid ! byebye
    end,
    ok.
