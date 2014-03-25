#ifndef CQUEUE_HPP
#define CQUEUE_HPP 1

#include "CPoolNode.hpp"
#include "erl_nif.h"

class CQueue {
    public:
        CQueue(unsigned int schedulers);
        ~CQueue();

        void Deposit(const ERL_NIF_TERM &value, unsigned int list_id);
        ERL_NIF_TERM Withdraw(ErlNifEnv* env, unsigned int list_id);

    private:
        unsigned int lists;
        CPoolNode **heads;
        CPoolNode **tails;
};

#endif
