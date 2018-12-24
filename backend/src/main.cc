#ifndef __linux__
#error "This software is built to run on a linux operating system."
#endif 

#include <iostream>
#include <exception>

#include "logger.h"
#include "common.h"
#include "backend.h"

int main(int, char**)
{
    try
    {
        backend::backend_t::get_instance().run();
    }
    catch(const std::exception& e)
    {
#if 0
        backend::global_logging_mutex.lock();
        std::cerr << e.what() << std::endl;
        backend::global_logging_mutex.unlock();
#endif
        return -1;
    }
    return 0;
}
