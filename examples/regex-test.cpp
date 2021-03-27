#include <iostream>
#include <sstream>

#include "../ctpg.hpp"

using namespace ctpg;


constexpr char pattern[] = "-";
constexpr regex::expr<pattern> r;



int main()
{
    r.write_diag_str(std::cout);
    bool m = r.match("-", std::cout);
    std::cout << (m ? "Matched" : "Fail") << std::endl;

    regex::dfa_size_analyzer a;
    regex::create_regex_parser(a).write_diag_str(std::cout);
    return 0;
}
