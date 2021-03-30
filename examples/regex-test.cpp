#include <iostream>
#include <sstream>

#include "../ctpg.hpp"

using namespace ctpg;

constexpr char pattern[] = "[1-9][0-9]*";
constexpr regex::expr<pattern> r;

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        constexpr bool m = r.match("123");
        std::cout << (m ? "constexpr match" : "constexpr match fail") << std::endl;
        return 0;
    }
    bool m = r.match(regex::match_options{}.set_verbose(), buffers::string_buffer(argv[1]), std::cout);
    std::cout << (m ? "runtime match" : "runtime match fail") << std::endl;
    return 0;
}
