-module(ewalker_watcher).

-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define (TIMEOUT, 1000*60*30). % 30 mins
 
%% ===================================================================================== %%
start_link(Path) ->
    gen_server:start_link(?MODULE, [Path], []).

%% ===================================================================================== %%

init([RootPath]) ->
    io:format("watcher init: " ++ RootPath ++ "~n"),
    {ok, RootPath, 5000}.

%% ===================================================================================== %%

handle_cast(_Message, State) ->
    {noreply, State}.

%% ===================================================================================== %%

handle_call(_Request, _From, State) ->
    {reply, [], State}.
    
%% ===================================================================================== %%

handle_info(timeout, RootPath) ->
    walk(RootPath),
    {noreply, RootPath, ?TIMEOUT};
    
handle_info(_Message, State) ->
    {noreply, State}.
    
%% ===================================================================================== %%

terminate(_Reason, _State) ->
	ok.
	
%% ===================================================================================== %%

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
        
%% ===================================================================================== %%
walk(Root) ->
    io:format("Visiting " ++ Root ++ "~n"),
    case walk([Root], []) of
        {ok, []} ->
            {ok, []};
            
        {ok, Files} ->
            ok = ewalker_writer:seen(Root, Files),
            {ok, Files};
        {error, Message} ->
            erlang:error(Message)
    end.
    
walk([], ResultFiles) ->
    {ok, ResultFiles};
    
walk([ToVisit|Rest], ResultFiles) ->
    Files = filelib:wildcard(ToVisit ++ "/*"),
    walk(Rest ++ lists:filter(fun(X) -> filelib:is_dir(X) end, Files), ResultFiles ++ Files).
    
