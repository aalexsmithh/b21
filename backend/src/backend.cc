#include "backend.h"

namespace backend
{

backend_t::backend_t()
{
    BACKEND_LOG_NOTE("Backend instance started");
}

backend_t::~backend_t()
{
    BACKEND_LOG_NOTE("Backend instance ended");
}

void backend_t::run()
{
    BACKEND_THROW("Test");
}

}
