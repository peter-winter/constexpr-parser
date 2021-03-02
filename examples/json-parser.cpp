#include "../constexpr-parser.hpp"
#include <iostream>

struct row
{
    constexpr row()
    {
        for (auto& t : transitions)
            t = uninitialized;
    }

    bool start_state = false;
    bool end_state = false;
    size_t recognized_idx = 0;
    size_t transitions[distinct_values_count<char>] = {};
};

struct idx_pair
{
    size_t start = 0;
    size_t size = 0;
};

template<size_t MaxStates>
struct table
{
    constexpr idx_pair add_primary_single_char(str_view sv)
    {
        size_t old_size = current_size;
        current_size += 2;
        return idx_pair{ old_size, 2 };
    }

    constexpr idx_pair add_primary_char_list(str_view sv)
    {
        size_t old_size = current_size;
        current_size += 2;
        return idx_pair{ old_size, 2 };
    }

    constexpr idx_pair add_primary_char_list_exclusive(str_view sv)
    {
        size_t old_size = current_size;
        current_size += 2;
        return idx_pair{ old_size, 2 };
    }

    constexpr idx_pair add_primary_char_range(str_view sv1, str_view sv2)
    {
        size_t old_size = current_size;
        current_size += 2;
        return idx_pair{ old_size, 2 };
    }

    constexpr idx_pair add_primary_char_range_exclusive(str_view sv1, str_view sv2)
    {
        size_t old_size = current_size;
        current_size += 2;
        return idx_pair{ old_size, 2 };
    }

    constexpr idx_pair add_multiplication(idx_pair p)
    {
        size_t old_size = current_size;
        return p;
    }

    constexpr idx_pair add_concat(idx_pair p1, idx_pair p2)
    {
        if (p2.size != 0)
        {

        }
        return idx_pair{ p1.start, p1.size + p2.size };
    }

    constexpr idx_pair add_alt(idx_pair p1, idx_pair p2)
    {
        return idx_pair{ p1.start, p1.size + p2.size };
    }

    row rows[MaxStates] = {};
    size_t current_size = 0;
};

template<size_t N>
constexpr auto create_regex_parser()
{
    constexpr term regular_char(exclusive{ "\\[]^-.*|()" }, "regular");
    constexpr nterm<idx_pair> expr("expr");
    constexpr nterm<idx_pair> alt("alt");
    constexpr nterm<idx_pair> concat("concat");
    constexpr nterm<idx_pair> q_expr("q_expr");
    constexpr nterm<idx_pair> primary("primary");
    constexpr nterm<str_view> char_list("char_list");
    constexpr nterm<str_view> single_char("single_char");

    return parser(
        expr,
        make_terms(regular_char, "\\", "[", "]", "^", "-", ".", "*", "|", "(", ")"),
        std::make_tuple(expr, alt, concat, q_expr, primary, char_list, single_char),
        make_rules(
            single_char(regular_char) >= [](skip, str_view sv) { return sv; },
            single_char("\\", "\\") >= [](skip, str_view s1, str_view s2) { return add_str_views(s1, s2); },
            single_char("\\", "[") >= [](skip, str_view s1, str_view s2) { return add_str_views(s1, s2); },
            single_char("\\", "]") >= [](skip, str_view s1, str_view s2) { return add_str_views(s1, s2); },
            single_char("\\", "^") >= [](skip, str_view s1, str_view s2) { return add_str_views(s1, s2); },
            single_char("\\", "-") >= [](skip, str_view s1, str_view s2) { return add_str_views(s1, s2); },
            single_char("\\", ".") >= [](skip, str_view s1, str_view s2) { return add_str_views(s1, s2); },
            single_char("\\", "|") >= [](skip, str_view s1, str_view s2) { return add_str_views(s1, s2); },
            single_char("\\", "(") >= [](skip, str_view s1, str_view s2) { return add_str_views(s1, s2); },
            single_char("\\", ")") >= [](skip, str_view s1, str_view s2) { return add_str_views(s1, s2); },
            char_list() >= [](skip) { return str_view{ nullptr, 0 }; },
            char_list(single_char, char_list) >= [](skip, str_view s1, str_view s2) { return add_str_views(s1, s2); },
            primary(single_char) >= [](table<N>& t, str_view s) { return t.add_primary_single_char(s); },
            primary("[", char_list, "]") >= [](table<N>& t, skip, str_view s, skip) { return t.add_primary_char_list(s); },
            primary("[", "^", char_list, "]") >= [](table<N>& t, skip, skip, str_view s, skip) { return t.add_primary_char_list_exclusive(s); },
            primary("[", single_char, "-", single_char, "]") >= [](table<N>& t, skip, str_view s1, skip, str_view s2, skip) { return t.add_primary_char_range(s1, s2); },
            primary("[", "^", single_char, "-", single_char, "]") >= [](table<N>& t, skip, skip, str_view s1, skip, str_view s2, skip) { return t.add_primary_char_range_exclusive(s1, s2); },
            primary("(", expr, ")") >= [](skip, skip, idx_pair p, skip) { return p; },
            q_expr(primary) >= [](skip, idx_pair p) { return p; },
            q_expr(primary, "*") >= [](table<N>& t, idx_pair p, skip) { return t.add_multiplication(p); },
            concat() >= [](skip) { return idx_pair{ 0, 0 }; },
            concat(q_expr, concat) >= [](table<N>& t, idx_pair p1, idx_pair p2) { return t.add_concat(p1, p2); },
            alt(concat) >= [](skip, idx_pair p) { return p; },
            alt(alt, "|", alt) >= [](table<N>& t, idx_pair p1, skip, idx_pair p2) { return t.add_alt(p1, p2); },
            expr(alt) >= [](skip, idx_pair p) { return p; }
        ),
        use_context<table<N>>{},
        deduce_max_states{}
    );
}

constexpr auto p = create_regex_parser<10>();
constexpr parse_result res(p, cstring_buffer("ab"), use_const_message<2000>{}, use_const_message<2000>{});
constexpr diag_msg msg(p, use_const_message<200000>{});

int main()
{
    std::cout << msg.get_stream().str();
    std::cout << res.get_trace_stream().str();
    return 0;
}

