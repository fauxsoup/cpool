#include "CQueue.hpp"

CQueue::CQueue(unsigned int schedulers) {
    unsigned int i = 0;
    lists = schedulers;
    heads = new CPoolNode*[lists];
    tails = new CPoolNode*[lists];

    for (; i < lists; ++i) {
        heads[i] = NULL;
        tails[i] = NULL;
    }
}

CQueue::~CQueue() {
    unsigned int i = 0;
    CPoolNode* it = NULL;
    CPoolNode* tmp = NULL;
    for(; i < lists; ++i) {
        it = heads[i];
        while (it != NULL) {
            tmp = it;
            it = it->next;
            delete tmp;
        }
    }
    delete heads;
    delete tails;
}

void CQueue::Deposit(const ERL_NIF_TERM &value, unsigned int list_id) {
    CPoolNode *node = new CPoolNode(value);
    if (heads[list_id] == NULL) {
        heads[list_id] = node;
        tails[list_id] = node;
    } else {
        tails[list_id]->next = node;
        tails[list_id] = node;
    }
}

ERL_NIF_TERM CQueue::Withdraw(ErlNifEnv *env, unsigned int list_id) {
    ERL_NIF_TERM result;
    CPoolNode *node = heads[list_id];
    bool okay = false;

    if (node == NULL) {
        result = enif_make_atom(env, "empty");
    } else if (node == tails[list_id]) {
        result = enif_make_copy(env, heads[list_id]->pid);
        heads[list_id] = NULL;
        tails[list_id] = NULL;
        delete node;
        okay = true;
    } else {
        result = enif_make_copy(env, heads[list_id]->pid);
        heads[list_id] = node->next;
        delete node;
        okay = true;
    }

    if (okay) {
        result = enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
    }

    return result;
}
