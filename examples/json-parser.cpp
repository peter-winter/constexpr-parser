#include "../ctpg.hpp"

#include <iostream>
#include <variant>

using namespace ctpg;

constexpr double to_js_number(std::string_view sv)
{
    auto it = sv.begin();
    auto end = sv.end();
    
    double sign = 1.0;
    if (*it == '-')
    {
        sign = -1.0;
        it++;
    }
    
    auto get_int = [end](auto& it, char limit1, char limit2, char limit3, char limit4)
    {
        std::uint64_t res = 0;
        while (true)
        {
            if (it == end || *it == limit1 || *it == limit2 || *it == limit3 || *it == limit4)
                break;
            res *= 10;
            res += *it - '0';
            ++it;
        }
        return res;
    };
    std::uint64_t dec = get_int(it, '.', 'E', 'e', '\0');
    if (*it == '.')
        ++it;
    auto it_before_frac = it;
    std::uint64_t frac = get_int(it, 'E', 'e', '\0', '\0');
    std::uint64_t frac_digits = it - it_before_frac;

    std::uint64_t exp = 0;
    bool exp_minus = false;
    if (*it == 'e' || *it == 'E')
    {
        ++it;
        exp_minus = *it++ == '-';
        exp = get_int(it, '\0', '\0', '\0', '\0');
    }

    double frac_d = double(frac);
    for (size_t i = 0; i < frac_digits; ++i)
        frac_d /= 10.0;

    double result = sign * (dec + frac_d);
    for (size_t i = 0; i < exp; ++i)
        result = exp_minus ? result / 10 : result * 10;
    return result;
}

struct string_memory_ref
{
    std::uint32_t start;
    std::uint32_t size;
};

template<typename StringMemory>
string_memory_ref add_js_string(std::string_view sv, StringMemory& string_memory)
{
    auto start = string_memory.size();
    string_memory.insert(string_memory.end(), sv.begin(), sv.end());
    return { std::uint32_t(start), std::uint32_t(sv.size())};
}

constexpr char number_pattern[] = R"_(\-?(0|[1-9][0-9]*)(\.[0-9]*)?((e|E)(\+|\-)[0-9]*)?)_";
constexpr char string_pattern[] = R"_("([^\\"\x00-\x1F]|\\[\\"/bfnrt]|\\u[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f])*")_";

using data_element = std::variant<
    double,
    string_memory_ref
>;

constexpr regex_term<number_pattern> js_number("js_number");
constexpr regex_term<string_pattern> js_string("js_string");

constexpr nterm<data_element> scalar("scalar");

template<typename StringMemory>
constexpr auto create_parser(StringMemory& string_memory)
{
    auto& sm = string_memory;
    return parser(
        scalar,
        terms(js_number, js_string),
        nterms(scalar),
        rules(
            scalar(js_number) >= [](auto sv){ return to_js_number(sv); },
            scalar(js_string) >= [&sm](auto sv){ return add_js_string(sv, sm); }
        )
    );
}

int main(int argc, char* argv[])
{
    if (argc != 2)
        return -1;

    std::vector<char> string_memory;
    auto p = create_parser(string_memory);
    
    p.write_diag_str(std::cout);
    auto res = p.parse(parse_options{}.set_verbose(), buffers::string_buffer(argv[1]), std::cout);
    return 0;
}