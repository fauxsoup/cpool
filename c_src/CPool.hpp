#ifndef CPOOL_H
#define CPOOL_H 1

#include "erl_nif.h"

class CPool;
class CPoolNode;

class CPool {
    public:
        CPool(unsigned int schedulers);
        ~CPool();

        CPoolNode* Join(const ERL_NIF_TERM &pid);
        ERL_NIF_TERM Depart(ErlNifEnv* env, CPoolNode* node);
        ERL_NIF_TERM Next(ErlNifEnv* env, const unsigned int &iterator_id);

    private:
        void _clear_iterators(CPoolNode* node);
        bool _drop(CPoolNode* node);

        ErlNifRWLock *iterator_lock;
        CPoolNode *head;
        CPoolNode *tail;

        // One iterator per scheduler
        unsigned int iterators;
        CPoolNode **iterator;
};

class CPoolNode {
    friend class CPool;
    public:
        CPoolNode(const ERL_NIF_TERM &value);
        ~CPoolNode();

    protected:
        ErlNifEnv *env;
        ERL_NIF_TERM pid;

        CPoolNode *next;
        CPoolNode *prev;
};

#endif
