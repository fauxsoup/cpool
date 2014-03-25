#ifndef CPOOLNODE_HPP
#define CPOOLNODE_HPP 1

#include "erl_nif.h"

class CPoolNode {
    public:
        CPoolNode(const ERL_NIF_TERM &value);
        ~CPoolNode();

        ERL_NIF_TERM pid;
        CPoolNode *next;
        CPoolNode *prev;

    protected:
        ErlNifEnv *env;
};

#endif
