#include "erl_nif.h"
#include "CQueue.hpp"
#include "Registry.hpp"
#include "stdio.h"
#include <cstring>
#include <string>

extern "C" {
    static Registry<CQueue> registry;
    unsigned int schedulers;
    ErlNifTid* scheduler_ids;

    // Prototypes
    static ERL_NIF_TERM cqueue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cqueue_deposit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cqueue_withdraw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM cqueue_register(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    int get_list_id(ErlNifTid self);

    static ErlNifFunc nif_funcs[] = {
        {"new", 1, cqueue_new},
        {"deposit", 1, cqueue_deposit},
        {"withdraw", 1, cqueue_withdraw}
    };

    std::string get_name(ErlNifEnv* env, ERL_NIF_TERM term) {
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

    static ERL_NIF_TERM cqueue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        CQueue* queue = new CQueue();
        std::string name(get_name(env, argv[0]));
        ERL_NIF_TERM result;

        if (registry.Register(name, queue)) {
            result = enif_make_atom(env, "ok");
        } else {
            delete queue;
            result = enif_make_badarg(env);
        }

        return result;
    }

    static ERL_NIF_TERM cqueue_deposit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        std::string name(get_name(env, argv[0]));
        ErlNifPid pid;

        enif_self(env, &pid);
        registry.Transaction<bool>(name, [&](CQueue* queue) {
                if (queue != nullptr) {
                    queue->Deposit(pid);
                    return true;
                }
                return false;
            });

        return enif_make_atom(env, "ok");
    }

    static ERL_NIF_TERM cqueue_withdraw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ERL_NIF_TERM result;
        std::string name(get_name(env, argv[0]));


        result = registry.Transaction<ERL_NIF_TERM>(name, [&](CQueue* queue) {
                if (queue != nullptr) {
                    return queue->Withdraw(env);
                }
                return enif_make_badarg(env);
            });

        return result;
    }

    ERL_NIF_INIT(cqueue, nif_funcs, NULL, NULL, NULL, NULL);
}
