#ifndef CPOOLNODE_HPP
#define CPOOLNODE_HPP 1

#include "erl_nif.h"

class CPoolNode {
    public:
        CPoolNode(const ErlNifPid &value);
        ~CPoolNode();

        ErlNifPid pid;
        CPoolNode *next;
        CPoolNode *prev;
};

#endif
