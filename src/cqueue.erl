-module(cqueue).
-export([
        new/0,
        new/1,
        deposit/1,
        withdraw/1
    ]).
-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    io:format("~p\t~p~n", [PrivDir, erlang:load_nif(filename:join(PrivDir, ?MODULE), 0)]).

new() ->
    ?nif_stub.

new(_Name) ->
    ?nif_stub.

deposit(_Ref) ->
    ?nif_stub.

withdraw(_Ref) ->
    ?nif_stub.

register_tid(_SchedulerID) ->
    ?nif_stub.
