#include "CPoolNode.hpp"

CPoolNode::CPoolNode(const ErlNifPid &value) {
    pid     = value;
    next    = NULL;
    prev    = NULL;
}

CPoolNode::~CPoolNode() {
}
