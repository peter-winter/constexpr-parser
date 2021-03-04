#include "../constexpr-parser.hpp"
#include <iostream>

struct dfa_state
{
    constexpr dfa_state()
    {
        for (auto& t : transitions)
            t = uninitialized;
    }

    bool start_state = false;
    bool end_state = false;
    size_t recognized_idx = 0;
    size_t transitions[distinct_values_count<char>] = {};
};

template<typename DFAStateContainerType, typename CharListType>
struct dfa
{
    using dfa_state_container_type = DFAStateContainerType;
    using char_list_type = CharListType;

    constexpr slice add_primary_single_char(char single)
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_primary_char_list(const char_list_type& l)
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_primary_char_list_exclusive(const char_list_type& l)
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_primary_char_range(char c1, char c2)
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_primary_char_range_exclusive(char c1, char c2)
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_multiplication(slice p)
    {
        size_t old_size = states.size();
        return p;
    }

    constexpr slice add_concat(slice p1, slice p2)
    {
        if (p2.n != 0)
        {

        }
        return slice{ p1.start, p1.n + p2.n };
    }

    constexpr slice add_alt(slice p1, slice p2)
    {
        return slice{ p1.start, p1.n + p2.n };
    }

    constexpr void mark_end_states(slice s, size_t idx)
    {

    }

    constexpr size_t size() const { return states.size(); }

    dfa_state_container_type states;
};

template<typename DFAType, typename BufferType, typename ErrorStream, typename TraceStream, typename... Str, size_t... I>
constexpr auto create_regex_lexer_impl(std::index_sequence<I...>, const Str&... r)
{
    using dfa_type = DFAType;
    using char_list_type = typename DFAType::char_list_type;
    using buffer_type = BufferType;
    using error_stream_type = ErrorStream;
    using trace_stream_type = TraceStream;

    constexpr term regular_char(exclude("\\[]^-.*|()"), 0, associativity::ltor, "regular");
    constexpr nterm<slice> expr("expr");
    constexpr nterm<slice> alt("alt");
    constexpr nterm<slice> concat("concat");
    constexpr nterm<slice> q_expr("q_expr");
    constexpr nterm<slice> primary("primary");
    constexpr nterm<char_list_type> char_list("char_list");
    constexpr nterm<char> single_char("single_char");

    dfa_type sm;

    parser p(
        expr,
        terms(regular_char, "\\", "[", "]", "^", "-", ".", "*", "|", "(", ")"),
        nterms(expr, alt, concat, q_expr, primary, char_list, single_char),
        rules(
            single_char(regular_char) >= [](str_view s) { return s[0]; },
            single_char("\\", "\\") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "[") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "]") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "^") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "-") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", ".") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "|") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "(") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", ")") >= [](skip, str_view s) { return s[0]; },
            char_list() >= []() { return char_list_type{}; },
            char_list(single_char, char_list) >= [&sm](char c, char_list_type&& l) { l.push_back(c); return l; },
            primary(single_char) >= [&sm](char c) { return sm.add_primary_single_char(c); },
            primary("[", char_list, "]") >= [&sm](skip, char_list_type&& l, skip) { return sm.add_primary_char_list(l); },
            primary("[", "^", char_list, "]") >= [&sm](skip, skip, char_list_type&& l, skip) { return sm.add_primary_char_list_exclusive(l); },
            primary("[", single_char, "-", single_char, "]") >= [&sm](skip, char c1, skip, char c2, skip) { return sm.add_primary_char_range(c1, c2); },
            primary("[", "^", single_char, "-", single_char, "]") >= [&sm](skip, skip, char c1, skip, char c2, skip) { return sm.add_primary_char_range_exclusive(c1, c2); },
            primary("(", expr, ")") >= [](skip, slice p, skip) { return p; },
            q_expr(primary) >= [](slice p) { return p; },
            q_expr(primary, "*") >= [&sm](slice p, skip) { return sm.add_multiplication(p); },
            concat() >= []() { return slice{ 0, 0 }; },
            concat(q_expr, concat) >= [&sm](slice p1, slice p2) { return sm.add_concat(p1, p2); },
            alt(concat) >= [](slice p) { return p; },
            alt(alt, "|", alt) >= [&sm](slice p1, skip, slice p2) { return sm.add_alt(p1, p2); },
            expr(alt) >= [](slice p) { return p; }
        ),
        no_context{},
        deduce_max_states{}
    );

    auto parse_f = [&sm, &p](const auto& r, size_t idx)
    { 
        trace_stream_type trace_stream;
        error_stream_type error_stream;
        slice prev{0, sm.size()};
        auto res = p.parse(buffer_type(r), error_stream, trace_stream);
        if (res.has_value())
        {
            sm.mark_end_states(res.value(), idx);
            sm.add_alt(prev, res.value());
        }
        else
        {
            error_stream_type tmp_stream;
            tmp_stream << "Regex " << r << " parse error: " << trace_stream.str();
            throw std::runtime_error(tmp_stream.str());
        }
    };

    (void(parse_f(r, I)), ...);
    
    return sm;
}

template<size_t... N>
constexpr auto create_regex_lexer(const char (&...r)[N])
{
    constexpr size_t total_length = (0 + ... + N);
    return create_regex_lexer_impl<
        dfa<
            cvector<dfa_state, total_length * 2>,
            cvector<char, total_length>
        >,
        cstring_buffer<total_length>,
        typename use_const_message<2000>::type,
        typename use_const_message<20000>::type
    >(std::make_index_sequence<sizeof...(N)>{}, r...);
}

int main()
{
    try
    {
        auto res = create_regex_lexer("a*", "b");
    }
    catch(const std::exception& e)
    {
        std::cerr << e.what() << std::endl;
    }
    return 0;
}

