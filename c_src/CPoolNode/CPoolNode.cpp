#include "CPoolNode.hpp"

CPoolNode::CPoolNode(const ERL_NIF_TERM &value) {
    env = enif_alloc_env();
    pid = enif_make_copy(env, value);
    next = NULL;
    prev = NULL;
}

CPoolNode::~CPoolNode() {
    enif_free_env(env);
}
