#ifndef BACKEND_LOGGER_H
#define BACKEND_LOGGER_H

#include <iostream>
#include <fstream>
#include <exception>
#include <string>
#include <chrono>
#include <mutex>

namespace backend
{

const char* const global_default_log_path = "./backend_log.txt";

enum class log_type_t : std::uint32_t
{
    NOTE    = 1 << 0,
    WARNING = 1 << 1,
    ERROR   = 1 << 2,
};

std::string get_current_time_str();
void log_stub(const std::string& log_path, const std::string& msg, log_type_t log_type, const char* file, unsigned line);

class better_exception_t : public std::exception
{
public:
    better_exception_t(const std::string& msg, const char* file, unsigned line);
    virtual ~better_exception_t() throw() override;
    virtual const char* what() const throw() override;

private:
    std::string _msg;
    std::string _fmt_msg;
};

extern std::mutex global_logging_mutex;

}

#endif 
