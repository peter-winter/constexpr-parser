#include <iostream>
#include "../constexpr-parser.hpp"

template<typename DFA>
struct rp
{
    template<size_t N>
    rp(const char(&r)[N])
    {
        static auto p = create_regex_parser(sm);
        std::cout << sizeof(p) << std::endl;
        p.write_diag_str(diag_stream);
        parse_options opts;
        opts.verbose = true;
        auto s = p.parse(opts, cstring_buffer(r), error_stream);
        valid = s.has_value();
        sm.write_diag_str(sm_diag_stream);
    }
    
    DFA sm;
    
    cstream<200000> diag_stream;
    cstream<20000> error_stream;
    cstream<200000> sm_diag_stream;
    bool valid = true;
};

rp<dfa<100>> ob("[A-Z]");

int main()
{
    //std::cout << ob.diag_stream.str();
    std::cout << "\nRegex verbose parse: \n\n";
    std::cout << ob.error_stream.str() << "\n";
    std::cout << ob.sm_diag_stream.str();
    return 0;
}

