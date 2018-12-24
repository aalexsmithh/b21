#ifndef BACKEND_COMMON_H 
#define BACKEND_COMMON_H

#include <mutex>

#include <cstdint>
#include <cassert>

#include "logger.h"

namespace backend
{

typedef char c8;
typedef bool b8;

typedef std::uint8_t   u8;
typedef std::uint16_t u16;
typedef std::uint32_t u32;
typedef std::uint64_t u64;

typedef std::int8_t   s8;
typedef std::int16_t s16;
typedef std::int32_t s32;
typedef std::int64_t s64;

typedef float  f32;
typedef double f64;

extern std::mutex global_logging_mutex;

}

#ifdef __BACKEND_DEBUG__
#define BACKEND_LOG(msg, log_type) backend::log_stub(backend::global_default_log_path, (msg), (log_type), __FILE__, __LINE__);
#define BACKEND_LOG_NOTE(msg) BACKEND_LOG((msg), backend::log_type_t::NOTE)
#define BACKEND_LOG_WARNING(msg) BACKEND_LOG((msg), backend::log_type_t::WARNING)
#define BACKEND_LOG_ERROR(msg) BACKEND_LOG((msg), backend::log_type_t::ERROR)
#define BACKEND_ASSERT(x) std::assert((x));
#elif __BACKEND_RELEASE__
#define BACKEND_LOG(msg, log_type) 
#define BACKEND_LOG_NOTE(msg)
#define BACKEND_LOG_WARNING(msg)
#define BACKEND_LOG_ERROR(msg)
#define BACKEND_ASSERT(x) 
#else
#error "Either __BACKEND_DEBUG__ or __BACKEND_RELEASE__ must be defined."
#endif

#define BACKEND_THROW(msg) BACKEND_LOG_ERROR((msg)); throw backend::better_exception_t{(msg), __FILE__, __LINE__};

#endif
