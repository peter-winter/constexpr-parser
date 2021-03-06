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
        parse_options opts;
        opts.verbose = true;
        auto s = p.parse(opts, cstring_buffer(r), error_stream);
        valid = s.has_value();
    }
    
    DFA sm;
    
    cstream<200000> diag_stream;
    cstream<20000> error_stream;
    bool valid = true;
};

constexpr rp<dfa<100, 10>> ob("[A-Z%a-z]");

int main()
{
    std::cout << "\nRegex verbose parse: \n\n";
    std::cout << ob.error_stream.str();
    return 0;
}

