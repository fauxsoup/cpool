#include "erl_nif.h"
#include "CPool.hpp"
#include "stdio.h"

extern "C" {
    static ErlNifResourceType* cpool_node_RESOURCE = NULL;
    static ErlNifResourceType* cpool_RESOURCE = NULL;

    typedef struct {
        CPool *pool;
    } cpool_handle;
    typedef struct {
        CPoolNode *node;
    } cpool_node_handle;

    // Prototypes
    static ERL_NIF_TERM cpool_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cpool_join(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cpool_depart(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cpool_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    static ErlNifFunc nif_funcs[] = {
        {"new", 1, cpool_new},
        {"join", 2, cpool_join},
        {"depart", 2, cpool_depart},
        {"next", 2, cpool_next}
    };

    static ERL_NIF_TERM cpool_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ERL_NIF_TERM result;
        unsigned long schedulers = 0;

        if (enif_get_ulong(env, argv[0], &schedulers) == 0) {
            return enif_make_badarg(env);
        }

        cpool_handle* handle = (cpool_handle*)enif_alloc_resource(cpool_RESOURCE, sizeof(cpool_handle));
        handle->pool = new CPool(schedulers);

        result = enif_make_resource(env, handle);
        enif_release_resource(handle);
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
    }


    static ERL_NIF_TERM cpool_join(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        cpool_handle* handle = NULL;
        cpool_node_handle* node;
        ERL_NIF_TERM result;

        if (enif_get_resource(env, argv[0], cpool_RESOURCE, (void**)&handle) == 0) {
            return enif_make_badarg(env);
        }

        node = (cpool_node_handle*)enif_alloc_resource(cpool_node_RESOURCE, sizeof(cpool_node_handle));
        node->node = handle->pool->Join(argv[1]);
        result  = enif_make_resource(env, (void*)node);
        enif_release_resource((void*)node);

        return result;
    }

    static ERL_NIF_TERM cpool_depart(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ERL_NIF_TERM result;
        cpool_handle* handle_pool = NULL;
        cpool_node_handle* handle_node = NULL;

        if (enif_get_resource(env, argv[0], cpool_RESOURCE, (void**)&handle_pool) == 0) {
            return enif_make_badarg(env);
        }

        if (enif_get_resource(env, argv[1], cpool_node_RESOURCE, (void**)&handle_node) == 0) {
            return enif_make_badarg(env);
        }

        result = handle_pool->pool->Depart(env, handle_node->node);

        return result;
    }

    static ERL_NIF_TERM cpool_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ERL_NIF_TERM result;
        cpool_handle* handle = NULL;
        unsigned long iterator;

        if (enif_get_resource(env, argv[0], cpool_RESOURCE, (void**)&handle) == 0) {
            return enif_make_badarg(env);
        }

        if (enif_get_ulong(env, argv[1], &iterator) == 0) {
            return enif_make_badarg(env);
        }

        // Reduce iterator to account for zero indice
        --iterator;
        result = handle->pool->Next(env, iterator);

        return result;
    }

    static void cpool_resource_cleanup(ErlNifEnv* env, void* arg) {
        cpool_handle* handle = (cpool_handle*)arg;
        delete handle->pool;
        handle->pool = NULL;
    }

    static void cpool_node_resource_cleanup(ErlNifEnv* env, void* arg) {
        ((cpool_node_handle*)arg)->node = NULL;
    }

    static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
        ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
        ErlNifResourceType* cpool = enif_open_resource_type(env, NULL, "cpool_resource", &cpool_resource_cleanup, flags, NULL);
        if (cpool == NULL) return -1;
        ErlNifResourceType* cpool_node = enif_open_resource_type(env, NULL, "cpool_node_resource", &cpool_node_resource_cleanup, flags,  NULL);
        if (cpool_node == NULL) return -1;

        cpool_RESOURCE = cpool;
        cpool_node_RESOURCE = cpool_node;

        return 0;
    }

    static int on_upgrade(ErlNifEnv* env, void** old_priv_data, void** priv_data, ERL_NIF_TERM load_info) {
        ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
        ErlNifResourceType* cpool = enif_open_resource_type(env, NULL, "cpool_resource", &cpool_resource_cleanup, flags, NULL);
        if (cpool == NULL) return -1;
        ErlNifResourceType* cpool_node = enif_open_resource_type(env, NULL, "cpool_node_resource", &cpool_node_resource_cleanup, flags,  NULL);
        if (cpool_node == NULL) return -1;

        cpool_RESOURCE = cpool;
        cpool_node_RESOURCE = cpool_node;

        return 0;
    }

    ERL_NIF_INIT(cpool, nif_funcs, &on_load, NULL, &on_upgrade, NULL);
}
