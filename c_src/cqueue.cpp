#include "erl_nif.h"
#include "CQueue.hpp"

extern "C" {
    static ErlNifResourceType* cqueue_RESOURCE = NULL;

    typedef struct {
        CQueue* queue;
    } cqueue_handle;

    // Prototypes
    static ERL_NIF_TERM cqueue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cqueue_deposit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cqueue_withdraw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    static ErlNifFunc nif_funcs[] = {
        {"new", 1, cqueue_new},
        {"deposit", 3, cqueue_deposit},
        {"withdraw", 2, cqueue_withdraw}
    };

    static ERL_NIF_TERM cqueue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        unsigned int schedulers = 0;

        if (enif_get_uint(env, argv[0], &schedulers) == 0) {
            return enif_make_badarg(env);
        }

        cqueue_handle* handle = (cqueue_handle*)enif_alloc_resource(cqueue_RESOURCE, sizeof(cqueue_handle));
        handle->queue = new CQueue(schedulers);

        ERL_NIF_TERM result = enif_make_resource(env, handle);
        enif_release_resource(handle);

        return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
    }

    static ERL_NIF_TERM cqueue_deposit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        cqueue_handle* handle = NULL;
        unsigned int scheduler_id = 0;

        if (enif_get_resource(env, argv[0], cqueue_RESOURCE, (void**)&handle) == 0) {
            return enif_make_badarg(env);
        }

        if (enif_get_uint(env, argv[2], &scheduler_id) == 0) {
            return enif_make_badarg(env);
        }

        --scheduler_id;

        handle->queue->Deposit(argv[1], scheduler_id);

        return enif_make_atom(env, "ok");
    }

    static ERL_NIF_TERM cqueue_withdraw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ERL_NIF_TERM result;
        cqueue_handle* handle = NULL;
        unsigned int scheduler_id = 0;

        if (enif_get_resource(env, argv[0], cqueue_RESOURCE, (void**)&handle) == 0) {
            return enif_make_badarg(env);
        }

        if (enif_get_uint(env, argv[1], &scheduler_id) == 0) {
            return enif_make_badarg(env);
        }

        --scheduler_id;

        result = handle->queue->Withdraw(env, scheduler_id);

        return result;
    }

    static void cqueue_resource_cleanup(ErlNifEnv* env, void* arg) {
        /* Delete any dynamically allocated memory stored in cqueue_handle */
        /* cqueue_handle* handle = (cqueue_handle*)arg; */
        delete ((cqueue_handle*)arg)->queue;
    }

    static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
        ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
        ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                "cqueue_resource",
                &cqueue_resource_cleanup,
                flags, NULL);
        if (rt == NULL)
            return -1;

        cqueue_RESOURCE = rt;

        return 0;
    }

    ERL_NIF_INIT(cqueue, nif_funcs, &on_load, NULL, NULL, NULL);
}
