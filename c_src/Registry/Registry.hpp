#ifndef REGISTRY_HPP
#define REGISTRY_HPP

#include <unordered_map>
#include <string>
#include <functional>

template <typename T>
class Registry {
    protected:
        typedef std::unordered_map<std::string, T*> Map;
        typedef std::pair<std::string, T*> MapEntry;

    public:
        Registry(){
            lock = enif_rwlock_create("CPoolRegistryLock");
        }

        ~Registry(){
            typename Map::iterator it;
            enif_rwlock_destroy(lock);

            for (it = registry.begin(); it != registry.end(); ++it) {
                delete it->second;
                it->second = nullptr;
            }
        }

        bool Register(const std::string &name, T* subject) {
            std::pair<typename Map::iterator, bool> result;

            enif_rwlock_rwlock(lock);
            result = registry.emplace(name, subject);
            enif_rwlock_rwunlock(lock);

            return result.second;
        }

        template <typename R>
        R Transaction(std::string name, std::function<R(T*)> op) {
            R result;
            typename Map::iterator it;

            enif_rwlock_rlock(lock);
            it = registry.find(name);
            if (it != registry.end()) {
                result = op(it->second);
            } else {
                result = op(nullptr);
            }
            enif_rwlock_runlock(lock);

            return result;
        }

        void Release(std::string name) {
            typename Map::iterator it;
            enif_rwlock_rwlock(lock);
            it = registry.find(name);
            if (it != registry.end()) {
                delete it->second;
                registry.erase(it);
            }
            enif_rwlock_rwunlock(lock);
        }

    protected:
        Map registry;
        ErlNifRWLock* lock;
};

#endif
