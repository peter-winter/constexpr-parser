#include <iostream>
#include <sstream>

#include "../ctpg.hpp"

using namespace ctpg;


constexpr char pattern[] = "[1-9]+";
constexpr regex::expr<pattern> r;

constexpr bool m = r.match("123");

int main()
{
    r.write_diag_str(std::cout);
    std::cout << (m ? "Matched" : "Fail") << std::endl;
    return 0;
}
