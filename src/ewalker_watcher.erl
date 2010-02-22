%%
%% The watcher is gen_server opening a port to get notified about filesystem events.
%% If such an event occure, or a ?timeout happens, the filesystem is walked and
%% all found files are sent to the ewalker_writer. 
%%
-module(ewalker_watcher).

-behaviour(gen_server).
-export([start_link/1, walk/2]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define (TIMEOUT, 1000*60*30). % 30 mins
 
-record(state, {path, port}). 
%% ===================================================================================== %%
start_link(Path) ->
    gen_server:start_link(?MODULE, [Path], []).

%% ===================================================================================== %%

init([RootPath]) ->
    error_logger:info_msg("Watcher init: ~p~n", [RootPath]),
    
    %Port = open_port({spawn_executable, os:find_executable("ruby")}, [
    %    {args, ["priv/watcher-mac.rb", Path]},
    %    stream,
    %    {line, 200}
    %    %,binary
    %]),
    Port = false,
    State = #state{path=RootPath, port=Port},
    {ok, State, 5000}.

%% ===================================================================================== %%

handle_cast(_Message, State) ->
    {noreply, State}.

%% ===================================================================================== %%

handle_call(_Request, _From, State) ->
    {reply, [], State}.
    
%% ===================================================================================== %%

handle_info(timeout, State) ->
    RootPath = State#state.path,
    
    ewalker_writer:begin_walk(RootPath),
    walk(RootPath, fun(File) ->
        ewalker_writer:visit(RootPath, File)
    end),
    ewalker_writer:end_walk(RootPath),
    
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

walk([], _Fun) ->
    ok;
    
% Call the Fun for every file in the given list of folders
walk([Path|Stack], Fun) when is_list(Path) and is_list(Stack) ->
    ShouldVisit = Fun(Path),
    
    case filelib:is_dir(Path) andalso not(ShouldVisit =:= false) of
        true ->
            Files = filelib:wildcard(Path ++ "/*"),
            NewStack = Stack ++ Files,
            walk(NewStack, Fun);
        false ->
            walk(Stack, Fun)
    end;

% initial Fallback case (Called with just a string)
walk(Path, Fun) ->
    walk([Path], Fun).
    