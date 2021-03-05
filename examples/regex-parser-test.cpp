#include "../constexpr-parser.hpp"
#include <iostream>

template<typename DFA>
struct rp
{
    template<size_t N>
    constexpr rp(const char(&r)[N])
    {
        auto p = create_regex_parser(sm);
        p.write_diag_str(diag_stream);
        no_stream error_stream;
        auto s = p.parse(cstring_buffer(r), error_stream, trace_stream);
        valid = s.has_value();
    }
    
    DFA sm;
    
    cstream<200000> diag_stream;
    cstream<20000> trace_stream;
    bool valid = true;
};

constexpr rp<dfa<100, 10>> ob("[1-9][0-9]*");

int main()
{
    std::cout << ob.diag_stream.str();
    std::cout << ob.trace_stream.str();    
    return 0;
}

