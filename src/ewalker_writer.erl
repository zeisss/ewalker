-module(ewalker_writer).

-behaviour(gen_server).
-export([start_link/3, begin_walk/1, end_walk/1, visit/2]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%%
% The writer collects the data provided by the watcher and writes it occasionally (after a defined timeout)
% to disc.
%
-define(TIMEOUT, 5000).
-record(state, {atom, files, links, filename}).
%
%% ===================================================================================== %%
start_link(Name, Links, Filename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Name, Links, Filename], []).

begin_walk(Root) ->
    gen_server:cast(?MODULE, {begin_walk, Root}).

visit(Root, File) ->
    gen_server:cast(?MODULE, {visit, Root, File}).
    
end_walk(Root) ->
    gen_server:cast(?MODULE, {end_walk, Root}).


%% ===================================================================================== %%

init([Name, Links, Filename]) ->
    error_logger:info_report("writer init"),
    {ok, #state{atom=atom:new(Name), files=[], links=Links, filename=Filename}}.

%% ===================================================================================== %%
handle_cast({begin_walk, NewRoot}, State) ->
    % Filter out all previous entries for the given path
    NewFiles = lists:filter (
        fun(_Entry = {Root, _File}) ->
            not(Root =:= NewRoot)
        end,
        State#state.files
    ),
    {noreply, State#state{files=NewFiles}};
    
% init a timeout
handle_cast({end_walk, _NewRoot}, State) ->
    {noreply, State, ?TIMEOUT};
    
handle_cast({visit, Root, File}, State) ->
    NewFiles = case accept_file(File) of
        true ->
            State#state.files ++ [{Root, File}];
        _ ->
            State#state.files
    end,
    {noreply, State#state{files=NewFiles}, ?TIMEOUT}.
    
%% ===================================================================================== %%

handle_call(_Request, _From, State) ->
    {reply, [], State}.
    
%% ===================================================================================== %%

handle_info(timeout, State) ->
    Atom = lists:foldl(
        fun({_RootPath, File}, Atom) ->
            atom:add_file(
                File,
                replace_links(File, State#state.links),
                Atom            
            )
        end,
        State#state.atom,
        State#state.files
    ),
    
    ok = dump(Atom, State#state.filename),
    {noreply, State}.
    
%% ===================================================================================== %%

terminate(_Reason, _State) ->
	ok.
	
%% ===================================================================================== %%

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
        
        
%% ===================================================================================== %%
% Returns false for files, that should not be saved (e.g. Thumbs.db)
%
accept_file(File) ->
    Eles = string:tokens(File, "/"),
            
    case lists:any(
        fun(X) ->
            ".app" =:= string:right(X, 4) orelse
            "." =:= string:left(X,1)  orelse
            "Thumbs.db" =:= X
        end,
        Eles
    ) of
        true -> false;
        _ -> filelib:is_regular(File)
    end.
    
%%
% Returns a URI version of the given path
%
% replace_links(Path, Mappings) -> Url
% Path = Url = string()
% Mappings = [{Url, Path}]
%
%
replace_links(Path, _Mappings = []) ->
    Path;
    
replace_links(Path, [{Url, Folder}|Mappings]) ->
    PathPrefix = string:left(Path, length(Folder)),
    
    case PathPrefix =:= Folder of
        true ->
            Url ++ string:sub_string(Path, length(Folder)+1);
        false ->
            replace_links(Path, Mappings)
    end.
    
    
%%
% Writes the given Atom object to the given file.
% 
dump(Atom, Filename) ->
    {ok, Content} = atom:dump(Atom),
    
    {ok, Fp} = file:open(Filename, write),
    file:write(Fp, Content),
    file:close(Fp),
    
    error_logger:info_report("ATOM file written."),
    ok.
