-module(ewalker_writer).

-behaviour(gen_server).
-export([start_link/3, seen/2]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%%
% The writer collects the data provided by the watcher and writes it occasionally (after a defined timeout)
% to disc.
%
-define(TIMEOUT, 5000).
-record(state, {atom, links, filename}).
%
%% ===================================================================================== %%
start_link(Name, Links, Filename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Name, Links, Filename], []).

seen(Root, Files) ->
    gen_server:cast(?MODULE, {seen, Root, Files}).
    
%% ===================================================================================== %%

init([Name, Links, Filename]) ->
    io:format("writer init~n"),
    {ok, #state{atom=atom:new(Name), links=Links, filename=Filename}}.

%% ===================================================================================== %%

handle_cast({seen, _Root, Files}, State) ->
    
    % Filter to only show files (filtered are mac application folders and hidden files)
    FilteredFiles = lists:filter(
        fun(File) ->
            Eles = string:tokens(File, "/"),
            
            case lists:any(
                fun(X) ->
                    ".app" =:= string:right(X, 4) orelse
                    "." =:= string:left(X,1) 
                end,
                Eles
            ) of
                true -> false;
                _ -> filelib:is_regular(File)
            end
        end,
        Files
    ),
    NewAtom = lists:foldl(
        fun(File, Atom) ->
            atom:add_file(
                File,
                replace_links(File, State#state.links),
                Atom
            )
        end,
        State#state.atom,
        FilteredFiles
    ),
    {noreply, State#state{atom=NewAtom}, ?TIMEOUT}. % raise a 'timeout' after some time

%% ===================================================================================== %%

handle_call(_Request, _From, State) ->
    {reply, [], State}.
    
%% ===================================================================================== %%

handle_info(timeout, State) ->
    ok = dump(State#state.atom, State#state.filename),
    {noreply, State#state{atom=atom:clear_items(State#state.atom)}};
    
handle_info(Message, State) ->
    io:format(Message),
    {noreply, State}.
    
%% ===================================================================================== %%

terminate(_Reason, _State) ->
	ok.
	
%% ===================================================================================== %%

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
        
        
%% ===================================================================================== %%
replace_links(Path, []) ->
    Path;
replace_links(Path, [{Url, Folder}|Mappings]) ->
    PathPrefix = string:left(Path, length(Folder)),
    
    case PathPrefix =:= Folder of
        true ->
            Url ++ string:sub_string(Path, length(Folder)+1);
        false ->
            replace_links(Path, Mappings)
    end.
    
    
dump(Atom, Filename) ->
    {ok, Content} = atom:dump(Atom),
    
    {ok, Fp} = file:open(Filename, write),
    file:write(Fp, Content),
    file:close(Fp),
    
    io:format("ATOM file written.~n"),
    ok.