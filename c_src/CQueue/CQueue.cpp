#include "CQueue.hpp"
#include "stdio.h"

CQueue::PidList& CQueue::GetQueue() {
    thread_local static PidList queue;
    return queue;
}

void CQueue::Deposit(const ErlNifPid &pid) {
    GetQueue().push_back(pid);
}

ERL_NIF_TERM CQueue::Withdraw(ErlNifEnv *env) {
    ERL_NIF_TERM result;
    if (GetQueue().empty()) {
        result = enif_make_atom(env, "empty");
    } else {
        result = enif_make_pid(env, &(GetQueue().front()));
        GetQueue().pop_front();
    }

    return result;
}
