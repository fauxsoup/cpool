#include "CPool.hpp"
#include "stdio.h"

CPool::CPool() {
    iterator_lock = enif_rwlock_create("cpool_iterator_lock");
}

CPool::~CPool() {
    enif_rwlock_destroy(iterator_lock);
}

ERL_NIF_TERM CPool::Join(ErlNifEnv* env, const ErlNifPid &pid) {
    PidList::iterator it;
    ERL_NIF_TERM pid_term_a = enif_make_pid(env, &pid);
    bool created = false;

    enif_rwlock_rwlock(iterator_lock);
    for (it = pids.begin(); it != pids.end(); ++it) {
        ERL_NIF_TERM pid_term_b = enif_make_pid(env, &(*it));
        if (enif_compare(pid_term_a, pid_term_b) == 0) {
            break;
        }
    }
    if (it == pids.end()) {
        pids.emplace_back(pid);
        created = true;
    }
    enif_rwlock_rwunlock(iterator_lock);

    if (created) {
        return enif_make_atom(env, "ok");
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "already_present"));
    }
}

ERL_NIF_TERM CPool::Depart(ErlNifEnv *env, const ErlNifPid &pid) {
    ERL_NIF_TERM pid_term_a = enif_make_pid(env, &pid);
    PidList::iterator it;
    bool removed = false;

    enif_rwlock_rwlock(iterator_lock);
    for (it = pids.begin(); it != pids.end(); ++it) {
        ERL_NIF_TERM pid_term_b = enif_make_pid(env, &(*it));
        if (enif_compare(pid_term_a, pid_term_b) == 0) {
            pids.erase(it);
            removed = true;
            break;
        }
    }
    enif_rwlock_rwunlock(iterator_lock);

    if (removed) {
        return enif_make_atom(env, "ok");
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "pid_not_found"));
    }
}

ERL_NIF_TERM CPool::Next(ErlNifEnv* env) {
    thread_local static unsigned long iterator = 0;
    ERL_NIF_TERM result;
    enif_rwlock_rlock(iterator_lock);
    if (pids.empty()) {
        result = enif_make_atom(env, "empty");
    } else {
        result = enif_make_pid(env, &(pids[iterator++ % pids.size()]));
    }
    enif_rwlock_runlock(iterator_lock);
    return result;
}
