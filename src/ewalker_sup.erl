
-module(ewalker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(CHILD2(Id, I, Type, Args), {Id, {I, start_link, Args}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    case application:get_env(ewalker, folders) of
        undefined -> {error, no_folders};
        {ok, Folders} ->
            start_link(Folders)
    end.
    
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%
% Args must be list of paths to visit
init(Args) ->
    error_logger:info_report(application:get_all_env(ewalker)),
    
    {ok, Mappings} = application:get_env(ewalker, mapping),
    
    {ok, Filename} = application:get_env(ewalker, output),
    
    {ok, Name} = application:get_env(ewalker, name),
    
    
    % Map each path to a childspec definition   
    Childs = lists:map(
        fun(X) ->
            Id = list_to_atom("ewalker_watcher_" ++ integer_to_list(erlang:phash2(X))),
            ?CHILD2(Id, ewalker_watcher, worker, [X])
        end,
        Args
    ),
    {ok,
      {
        {one_for_one, 5, 10},
        Childs ++[?CHILD(ewalker_writer, worker, [Name, Mappings, Filename])]
      }
    }.

