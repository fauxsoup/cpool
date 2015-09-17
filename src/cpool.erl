-module(cpool).
-export([new/1, join/1, depart/1, next/1, send/2]).
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
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

new(_) ->
    ?nif_stub.

join(CPool) ->
    join(CPool, self()).

join(_, _) ->
    ?nif_stub.

depart(_) ->
    ?nif_stub.

next(_) ->
    ?nif_stub.

send(Pool, Msg) ->
    case cpool:next(Pool) of
        empty -> error(empty_pool);
        Pid -> Pid ! Msg
    end.
