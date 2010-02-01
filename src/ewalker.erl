-module(ewalker).

-export([main/1, handle_args/1]).

main(Args) ->
  application:load(sasl),
  application:load(ewalker),
  
  ok = handle_args(Args),
  ok = application:start(sasl),
  ok = application:start(ewalker),
  
  % Wait for a message, that surely never arives.
  register(console, self()),
  
  receive
    _ -> ok    
  end.
  

handle_args([]) ->
    ok;
handle_args([Arg|Rest]) ->
    case Arg of
        "debug" ->
            toolbar:start();
        _ ->
            % If the Arg is a folder, add it to the watch list!
            case filelib:is_dir(Arg) of
                true ->
                    OldFolders = case application:get_env(ewalker, folders) of
                        undefined -> [];
                        {ok, Folders} -> Folders
                    end,
                    NewArgs = [Arg] ++ OldFolders,
                    ok = application:set_env(ewalker, folders, NewArgs),
                    {ok, NewArgs} = application:get_env(ewalker, folders);
                _ ->
                    io:format("Undefined arg: ~s~n", [Arg])
            end
    end,
    handle_args(Rest).
    