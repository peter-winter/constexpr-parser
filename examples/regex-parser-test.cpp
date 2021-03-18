#include <iostream>
#include <sstream>

#include "../ctpg.hpp"

using namespace ctpg;
using namespace ctpg::buffers;
using namespace ctpg::lexer;

template<size_t N>
struct regex
{
    constexpr regex(const char(&str)[N])
    {
        auto p = create_regex_parser(sm);
        auto s = p.parse(cstring_buffer(str));
        if (!s.has_value())
            throw std::runtime_error("invalid regex");
        sm.mark_end_states(s.value(), 0);
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
        auto res = sm.recognize(buf.begin(), buf.end(), ss);
        return res.term_idx == 0 && res.it == buf.end();
    }
    
    ctpg::lexer::dfa<N * 2> sm;
};

template<size_t N>
regex(const char (&)[N]) -> regex<N>;

constexpr regex r("[1-9]+");
constexpr bool m = r.match("123");

int main()
{
    ctpg::str_table<1> tn = {""};
    r.sm.write_diag_str(std::cout, tn);
    std::cout << (m ? "Matched" : "Fail") << std::endl;
    return 0;
}
