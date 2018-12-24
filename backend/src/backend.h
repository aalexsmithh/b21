#ifndef BACKEND_H
#define BACKEND_H

#include "logger.h"
#include "common.h"

namespace backend
{

class backend_t
{
public:
    backend_t();
    ~backend_t();

    static backend_t& get_instance()
    {
        static backend_t backend;
        return backend;
    }

    void run();
};

}

#endif
