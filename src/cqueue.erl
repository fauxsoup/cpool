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
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

new() ->
    new(erlang:system_info(schedulers)).

new(_Schedulers) ->
    ?nif_stub.

deposit(Ref, Term) ->
    deposit(Ref, Term, erlang:system_info(scheduler_id)).

deposit(_Ref, _Term, _SchedulerID) ->
    ?nif_stub.

withdraw(Ref) ->
    withdraw(Ref, erlang:system_info(scheduler_id)).

withdraw(_Ref, _SchedulerID) ->
    ?nif_stub.
