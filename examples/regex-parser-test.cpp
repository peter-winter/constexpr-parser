#include <iostream>
#include "../constexpr-parser.hpp"
/*
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
        str_table<2> tn{"t1", "t2"};
        sm.write_diag_str(sm_diag_stream, tn);
    }
    
    DFA sm;
    
    cstream<200000> diag_stream;
    cstream<20000> error_stream;
    cstream<200000> sm_diag_stream;
    bool valid = true;
};

constexpr rp<dfa<100>> ob("[1-9][0-9]*");

int main()
{
    //std::cout << ob.diag_stream.str();
    std::cout << "\nRegex verbose parse: \n\n";
    std::cout << ob.error_stream.str() << "\n";
    std::cout << ob.sm_diag_stream.str();
    return 0;
}

*/

constexpr nterm<int> expr("expr");

constexpr term o_plus('+', 1);

constexpr parser p(
    expr,
    terms(o_plus),
    nterms(expr),
    rules(
        expr('+', '+') >= [](skip, skip){return 1;}
    ),
    no_context{},
    deduce_max_states{}
);

int main()
{
    return 0;
}