#include <iostream>
#include <sstream>

#include "../ctpg.hpp"

using namespace ctpg;


constexpr char pattern[] = "\\x2";
constexpr regex::expr<pattern> r;



int main()
{
    r.write_diag_str(std::cout);
    bool m = r.match("\0", std::cout);
    std::cout << (m ? "Matched" : "Fail") << std::endl;

    /*
    regex::dfa_size_analyzer a;
    auto p = regex::create_regex_parser(a);
    p.write_diag_str(std::cout);
    auto res = p.parse(parse_options{}.set_verbose(), buffers::cstring_buffer(pattern), std::cout);
    std::cout << res.value().n << std::endl;
    regex::dfa<10> sm;
    regex::dfa_builder<10> b(sm);
    auto p1 = regex::create_regex_parser(b);
    auto res1 = p1.parse(parse_options{}.set_verbose(), buffers::cstring_buffer(pattern), std::cout);
    regex::write_dfa_diag_str(sm, std::cout);*/
    return 0;
}
