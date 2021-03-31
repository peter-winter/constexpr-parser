#include "../ctpg.hpp"
#include <iostream>
#include <charconv>

using namespace ctpg;
using namespace ctpg::buffers;

constexpr nterm<int> list("list");

constexpr char number_pattern[] = "[1-9][0-9]*";
constexpr regex_term<number_pattern> number("number");

int to_int(const std::string_view& sv)
{
    int i = 0;
    std::from_chars(sv.data(), sv.data() + sv.size(), i);
    return i;
}

constexpr parser p(
    list,
    terms(',', number),
    nterms(list),
    rules(
        list(number) 
            >= [](const auto& sv) { return to_int(sv); },
        list(list, ',', number) 
            >= [](int sum, skip, const auto& sv){ return sum + to_int(sv); }
    )
);

int main(int argc, char* argv[])
{
    if (argc < 2)
        return -1;
    auto res = p.parse(string_buffer(argv[1]), std::cerr);
    bool success = res.has_value();
    if (success)
        std::cout << res.value() << std::endl;
    return success ? 0 : -1;
}