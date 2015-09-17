#ifndef CPOOL_H
#define CPOOL_H 1

#include "erl_nif.h"
#include <vector>

class CPool;
class CPoolNode;

class CPool {
    private:
        typedef std::vector<ErlNifPid> PidList;

    public:
        CPool();
        ~CPool();

        ERL_NIF_TERM Join(ErlNifEnv* env, const ErlNifPid &pid);
        ERL_NIF_TERM Depart(ErlNifEnv* env, const ErlNifPid &pid);
        ERL_NIF_TERM Next(ErlNifEnv* env);

    private:
        ErlNifRWLock *iterator_lock;
        PidList pids;
};

#endif
