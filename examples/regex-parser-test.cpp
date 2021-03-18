#include <iostream>
#include <sstream>

#include "../ctpg.hpp"

using namespace ctpg;
using namespace ctpg::buffers;
using namespace ctpg::lexer;

template<size_t N>
constexpr static size32_t analyze_dfa_size(const char (&pattern)[N])
{
    dfa_size_analyzer a;
    auto p = create_regex_parser(a);
    auto res = p.parse(cstring_buffer(pattern));
    if (!res.has_value())
        throw std::runtime_error("invalid regex");
    return res.value().n;
}

template<auto& Pattern>
struct regex
{
    static const size32_t dfa_size = analyze_dfa_size(Pattern);

    constexpr regex()
    {
        dfa_builder<dfa_size> b(sm);
        auto p = create_regex_parser(b);
        auto s = p.parse(cstring_buffer(Pattern));
        if (!s.has_value())
            throw std::runtime_error("invalid regex");
        b.mark_end_states(s.value(), 0);
    }

    struct simple_state
    {
        struct 
        {
            bool verbose = false;
        } options;
        detail::no_stream error_stream;
        source_point current_sp;
    };

    template<size_t N1>
    constexpr bool match(const char(&str)[N1]) const
    {
        simple_state ss;
        ctpg::buffers::cstring_buffer buf(str);
        auto res = recognize(sm, buf.begin(), buf.end(), ss);
        return res.term_idx == 0 && res.it == buf.end();
    }
    
    ctpg::lexer::dfa<dfa_size> sm;
};

constexpr char pattern[] = "[1-9]+";
constexpr regex<pattern> r;

constexpr bool m = r.match("123");

int main()
{
    ctpg::str_table<1> tn = {""};
    lexer::write_diag_str(r.sm, std::cout, tn);
    std::cout << (m ? "Matched" : "Fail") << std::endl;
    return 0;
}
