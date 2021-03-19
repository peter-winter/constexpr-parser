#include <iostream>
#include <sstream>

#include "../ctpg.hpp"

using namespace ctpg;


constexpr char pattern[] = "[1-9]+";
constexpr regex::expr<pattern> r;

constexpr bool m = r.match("123");

int main()
{
    regex::write_dfa_diag_str(r.sm, std::cout);
    std::cout << (m ? "Matched" : "Fail") << std::endl;
    return 0;
}
