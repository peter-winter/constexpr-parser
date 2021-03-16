#include <iostream>
#include <sstream>

#include "../constexpr-parser.hpp"

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
        no_stream error_stream;
    };

    template<size_t N1>
    constexpr bool match(const char(&str)[N1]) const
    {
        simple_state ss;
        cstring_buffer buf(str);
        auto res = sm.recognize(buf.begin(), buf.end(), ss);
        return res.term_idx == 0 && res.it == buf.end();
    }
    
    dfa<N * 2> sm;
};

template<size_t N>
regex(const char (&)[N]) -> regex<N>;

constexpr regex r("[1-9][0-9]*");
constexpr bool m = r.match("123");

int main()
{
    std::cout << m << std::endl;
    return 0;
}
