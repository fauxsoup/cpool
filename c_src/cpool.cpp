#include "erl_nif.h"
#include "CPool.hpp"
#include "Registry.hpp"
#include "stdio.h"

extern "C" {
    static Registry<CPool> registry;

    // Prototypes
    static ERL_NIF_TERM cpool_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cpool_join(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cpool_depart(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cpool_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    static ErlNifFunc nif_funcs[] = {
        {"new", 1, cpool_new},
        {"join", 1, cpool_join},
        {"depart", 1, cpool_depart},
        {"next", 1, cpool_next}
    };

    std::string get_name(ErlNifEnv* env, const ERL_NIF_TERM &term) {
        std::string s_name;
        char* name;
        unsigned int length;

        enif_get_atom_length(env, term, &length, ERL_NIF_LATIN1);
        name = (char*)enif_alloc((length + 1) * sizeof(char));
        enif_get_atom(env, term, name, length + 1, ERL_NIF_LATIN1);

        s_name = name;

        enif_free(name);

        return s_name;
    }

    static ERL_NIF_TERM cpool_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        std::string name(get_name(env, argv[0]));
        CPool* pool = new CPool();

        if (registry.Register(name, pool)) {
            return enif_make_atom(env, "ok");
        } else {
            delete pool;
            return enif_make_badarg(env);
        }
    }


    static ERL_NIF_TERM cpool_join(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        std::string name(get_name(env, argv[0]));
        ErlNifPid self;

        enif_self(env, &self);
        bool result = registry.Transaction<bool>(name, [&](CPool* pool) {
                if (pool != nullptr) {
                    pool->Join(env, self);
                    return true;
                }
                return false;
            });

        if (result) {
            return enif_make_atom(env, "ok");
        } else {
            return enif_make_badarg(env);
        }
    }

    static ERL_NIF_TERM cpool_depart(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        bool result;
        std::string name(get_name(env, argv[0]));
        ErlNifPid self;

        enif_self(env, &self);
        result = registry.Transaction<bool>(name, [&](CPool* pool) {
                    if (pool != nullptr) {
                        pool->Depart(env, self);
                        return true;
                    }
                    return false;
                });

        if (result) {
            return enif_make_atom(env, "ok");
        } else {
            return enif_make_badarg(env);
        }
    }

    static ERL_NIF_TERM cpool_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ERL_NIF_TERM result;
        std::string name(get_name(env, argv[0]));

        result = registry.Transaction<ERL_NIF_TERM>(name, [&](CPool* pool) {
                if (pool != nullptr) {
                    return pool->Next(env);
                }
                return enif_make_badarg(env);
            });

        return result;
    }

    ERL_NIF_INIT(cpool, nif_funcs, NULL, NULL, NULL, NULL);
}
