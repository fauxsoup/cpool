#ifndef CQUEUE_HPP
#define CQUEUE_HPP 1

#include "erl_nif.h"
#include <deque>

class CQueue {
    private:
        typedef std::deque<ErlNifPid> PidList;
        PidList& GetQueue();
    public:
        void Deposit(const ErlNifPid &pid);
        ERL_NIF_TERM Withdraw(ErlNifEnv* env);
};

#endif
