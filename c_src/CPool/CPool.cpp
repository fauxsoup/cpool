#include "CPool.hpp"
#include "stdio.h"

CPool::CPool(unsigned int schedulers) {
    unsigned int i = 0;

    iterator_lock = enif_rwlock_create("cpool_iterator_lock");
    head = NULL;
    tail = NULL;

    iterators = schedulers;
    iterator = new CPoolNode*[iterators];
    for (i = 0; i < iterators; ++i) {
        iterator[i] = head;
    }
}

CPool::~CPool() {
    CPoolNode *i = head;

    while (i != NULL) {
        _drop(i);
        i = head;
    }

    enif_rwlock_destroy(iterator_lock);
    delete iterator;
}

CPoolNode* CPool::Join(const ERL_NIF_TERM &pid) {
    CPoolNode* node = new CPoolNode(pid);
    enif_rwlock_rwlock(iterator_lock);
    if (head != NULL) {
        tail->next = node;
        node->prev = tail;
        node->next = head;
        head->prev = node;
        tail = node;
    } else {
        head = node;
        tail = node;
        node->prev = node;
        node->next = node;
    }
    enif_rwlock_rwunlock(iterator_lock);

    return node;
}

ERL_NIF_TERM CPool::Depart(ErlNifEnv *env, CPoolNode* node) {
    ERL_NIF_TERM result;
    bool status;
    enif_rwlock_rwlock(iterator_lock);
    status = _drop(node);
    enif_rwlock_rwunlock(iterator_lock);

    if (status) {
        result = enif_make_atom(env, "ok");
    } else {
        result = enif_make_badarg(env);
    }

    return result;
}

bool CPool::_drop(CPoolNode* node) {
    bool status = false;
    if (node == head && node == tail) {
        head = NULL;
        tail = NULL;
        node->next = NULL;
        node->prev = NULL;
        _clear_iterators(node);
        delete node;
        status = true;
    } else if (node == head) {
        head = head->next;
        tail->next = head;
        _clear_iterators(node);
        delete node;
        status = true;
    } else if (node == tail) {
        head->prev = tail->prev;
        tail = tail->prev;
        tail->next = head;
        _clear_iterators(node);
        delete node;
        status = true;
    } else {
        // Let's make sure this node is still valid
        CPoolNode* i = head;
        do {
            if (i == node) { break; }
            i = i->next;
        } while (i != head);
        if (i == node) {
            // Iterator is in this pool
            node->prev->next = node->next;
            node->next->prev = node->prev;
            _clear_iterators(node);
            delete node;
            status = true;
        } else {
            status = false;
        }
    }

    return status;
}

ERL_NIF_TERM CPool::Next(ErlNifEnv* env, const unsigned int &iterator_id) {
    ERL_NIF_TERM result;
    if (iterator_id >= iterators) {
        return enif_make_badarg(env);
    }
    enif_rwlock_rlock(iterator_lock);
    if (iterator[iterator_id] != NULL) {
        result = enif_make_copy(env, iterator[iterator_id]->pid);
        iterator[iterator_id] = iterator[iterator_id]->next;
    } else if (head != NULL) {
        result = enif_make_copy(env, head->pid);
        iterator[iterator_id] = head->next;
    } else {
        result = enif_make_atom(env, "empty");
    }
    enif_rwlock_runlock(iterator_lock);
    return result;
}

void CPool::_clear_iterators(CPoolNode* node) {
    unsigned int i = 0;
    for (; i < iterators; ++i) {
        if (iterator[i] == node) {
            iterator[i] = node->next;
        }
    }
}
