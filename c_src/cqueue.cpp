#include "erl_nif.h"
#include "CQueue.hpp"
#include "stdio.h"

ErlNifRWLock* lookup_lock;
unsigned int schedulers;
ErlNifTid* scheduler_ids;

extern "C" {
    static ErlNifResourceType* cqueue_RESOURCE = NULL;

    typedef struct {
        CQueue* queue;
    } cqueue_handle;

    // Prototypes
    static ERL_NIF_TERM cqueue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cqueue_deposit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cqueue_withdraw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cqueue_register(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    int get_list_id(ErlNifTid self);

    static ErlNifFunc nif_funcs[] = {
        {"new", 0, cqueue_new},
        {"deposit", 2, cqueue_deposit},
        {"withdraw", 1, cqueue_withdraw},
        {"register_tid", 1, cqueue_register}
    };

    static ERL_NIF_TERM cqueue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        cqueue_handle* handle = (cqueue_handle*)enif_alloc_resource(cqueue_RESOURCE, sizeof(cqueue_handle));
        handle->queue = new CQueue(schedulers);

        ERL_NIF_TERM result = enif_make_resource(env, handle);
        enif_release_resource(handle);

        return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
    }

    static ERL_NIF_TERM cqueue_deposit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        cqueue_handle* handle = NULL;
        int list_id = get_list_id(enif_thread_self());

        if (list_id == -1) return enif_make_badarg(env);

        if (enif_get_resource(env, argv[0], cqueue_RESOURCE, (void**)&handle) == 0) {
            return enif_make_badarg(env);
        }

        handle->queue->Deposit(argv[1], list_id);

        return enif_make_atom(env, "ok");
    }

    static ERL_NIF_TERM cqueue_withdraw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ERL_NIF_TERM result;
        cqueue_handle* handle = NULL;
        int list_id = get_list_id(enif_thread_self());

        if (list_id == -1) return enif_make_badarg(env);

        if (enif_get_resource(env, argv[0], cqueue_RESOURCE, (void**)&handle) == 0) {
            return enif_make_badarg(env);
        }

        result = handle->queue->Withdraw(env, list_id);

        return result;
    }

    static ERL_NIF_TERM cqueue_register(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ErlNifTid thread_id = enif_thread_self();
        unsigned int i = 0;

        enif_rwlock_rwlock(lookup_lock);
        for (i = 0; i < schedulers; ++i) {
            if (scheduler_ids[i] == thread_id) {
                printf("Scheduler (%p) already registered.\r\n", thread_id);
                break;
            } else if (scheduler_ids[i] == NULL) {
                printf("Registering scheduler (%p) with index %d\r\n", thread_id, i);
                scheduler_ids[i] = thread_id;
                break;
            }
        }
        enif_rwlock_rwunlock(lookup_lock);

        if (i == schedulers) {
            return enif_make_badarg(env);
        } else {
            return enif_make_atom(env, "ok");
        }
    }

    int get_list_id(ErlNifTid self) {
        unsigned int i = 0;
        int result = -1;

        enif_rwlock_rlock(lookup_lock);
        for (i = 0; i < schedulers; ++i) {
            if (scheduler_ids[i] == self) {
                result = i;
                break;
            }
        }
        enif_rwlock_runlock(lookup_lock);

        return result;
    }

    static void cqueue_resource_cleanup(ErlNifEnv* env, void* arg) {
        /* Delete any dynamically allocated memory stored in cqueue_handle */
        /* cqueue_handle* handle = (cqueue_handle*)arg; */
        delete ((cqueue_handle*)arg)->queue;
    }

    static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
        ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
        ErlNifResourceType* rt = enif_open_resource_type(env, NULL, "cqueue_resource", &cqueue_resource_cleanup, flags, NULL);
        if (rt == NULL) return -1;

        cqueue_RESOURCE = rt;

        ErlNifSysInfo sys_info;
        enif_system_info(&sys_info, sizeof(ErlNifSysInfo));
        schedulers = sys_info.scheduler_threads;
        scheduler_ids = new ErlNifTid[schedulers];


        for (unsigned int i = 0; i < schedulers; ++i) {
            scheduler_ids[i] = NULL;
        }

        lookup_lock = enif_rwlock_create("cqueue_lookup_lock");

        return 0;
    }

    static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_data, ERL_NIF_TERM load_info) {
        ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
        ErlNifResourceType* rt = enif_open_resource_type(env, NULL, "cqueue_resource", &cqueue_resource_cleanup, flags, NULL);
        if (rt == NULL) return -1;

        cqueue_RESOURCE = rt;

        return 0;
    }

    static void on_unload(ErlNifEnv* env, void* priv_data) {
        printf("Unloading.\r\n");
        delete scheduler_ids;
        enif_rwlock_destroy(lookup_lock);
    }

    ERL_NIF_INIT(cqueue, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload);
}
