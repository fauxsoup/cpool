-module(cqueue).
-export([
        new/0,
        deposit/2,
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
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0),
    Schedulers = erlang:system_info(schedulers),
    lists:foreach(fun(Scheduler) ->
                spawn_opt(fun() -> register_tid(Scheduler) end, [{scheduler, Scheduler}])
        end, lists:seq(1, Schedulers)).

new() ->
    ?nif_stub.

deposit(Ref, Term) ->
    ?nif_stub.

withdraw(Ref) ->
    ?nif_stub.

register_tid(_SchedulerID) ->
    ?nif_stub.
