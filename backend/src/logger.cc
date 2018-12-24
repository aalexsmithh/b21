#include "logger.h"

namespace backend
{

better_exception_t::better_exception_t(const std::string& msg, const char* file, unsigned line)
    :std::exception{}, _msg{msg}, _fmt_msg{""}
{
    std::string time_str = get_current_time_str();
    _fmt_msg.append(time_str);
    _fmt_msg.append(" ");
    _fmt_msg.append(std::string{file});
    _fmt_msg.append("(");
    _fmt_msg.append(std::to_string(line));
    _fmt_msg.append(") ERROR: ");
    _fmt_msg.append(msg);
}

better_exception_t::~better_exception_t() throw()
{
}

const char* better_exception_t::what() const throw()
{
    return _fmt_msg.c_str();
}

std::string get_current_time_str()
{
    std::time_t now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
    char time_str[256];
    std::strftime(time_str, sizeof(time_str), "%F %T", std::localtime(&now));
    return std::string{time_str};
}

void log_stub(const std::string& log_path, const std::string& msg, log_type_t log_type, const char* file, unsigned line)
{
    std::string fmt_msg;
    std::string time_str = get_current_time_str();
    fmt_msg.append(time_str);
    fmt_msg.append(" ");
    fmt_msg.append(std::string{file});
    fmt_msg.append("(");
    fmt_msg.append(std::to_string(line));
    fmt_msg.append(") ");

    std::string title;
    switch(log_type)
    {
        case log_type_t::NOTE:    title = "NOTE";    break;
        case log_type_t::WARNING: title = "WARNING"; break;
        case log_type_t::ERROR:   title = "ERROR";   break;
    }

    fmt_msg.append(title);
    fmt_msg.append(": ");
    fmt_msg.append(msg);
    fmt_msg.append("\n");

    global_logging_mutex.lock();
    std::ofstream log_file{log_path, std::ios::binary | std::ios::app};
    log_file.write(fmt_msg.c_str(), fmt_msg.size());
    log_file.close();
    std::cerr << fmt_msg << std::flush;
    global_logging_mutex.unlock();
}

}
