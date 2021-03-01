#include "constexpr-parser.hpp"
#include <iostream>

struct cc
{
    constexpr void yo() { ++x; }

    int x = 0;
};

struct binary_op
{
    constexpr int operator()(cc& c, int x1, str_view op, int x2) const
    {
        c.yo();
        switch (op[0])
        {
        case '+':
            return x1 + x2;
        case '-':
            return x1 - x2;
        case '*':
            return x1 * x2;
        case '/':
            return x1 / x2;
        case '|':
            return x1 | x2;
        case '&':
            return x1 & x2;
        case '^':
            return x1 ^ x2;
        default:
            return 0;
        }
    }
};

struct unary_op
{
    constexpr int operator()(cc&, str_view op, int x) const
    {
        switch (op[0])
        {
        case '!':
            return ~x;
        case '-':
            return -x;
        default:
            return 0;
        }
    }
};

constexpr nterm<int> expr("expr");

constexpr term o_plus("+", 1);
constexpr term o_minus("-", 1);
constexpr term o_mul("*", 2);
constexpr term o_div("/", 2);
constexpr term o_or("|", 3);
constexpr term o_and("&", 4);
constexpr term o_xor("^", 5);
constexpr term o_neg("~", 6);

constexpr parser p(
    expr,
    make_terms("1", "2", o_plus, o_minus, o_mul, o_div, o_or, o_and, o_xor, o_neg, "(", ")"),
    std::make_tuple(expr),
    make_rules(
        expr(expr, "+", expr) >= binary_op{},
        expr(expr, "-", expr) >= binary_op{},
        expr(expr, "*", expr) >= binary_op{},
        expr(expr, "/", expr) >= binary_op{},
        expr(expr, "|", expr) >= binary_op{},
        expr(expr, "&", expr) >= binary_op{},
        expr(expr, "^", expr) >= binary_op{},
        expr("-", expr)[6] >= unary_op{},
        expr("~", expr) >= unary_op{},
        expr("(", expr, ")") >= [](cc&, auto, int x, auto) { return x; },
        expr("1") >= [](cc&, auto) { return 1; },
        expr("2") >= [](cc&, auto) { return 2; }
    ),
    use_context<cc>{},
    deduce_max_states{}
);

//diag_msg msg(p, use_string_stream{});

constexpr parse_result res(p, cstring_buffer("(1+2)*2"), use_const_message<1000>{}, use_const_message<1000>{});

constexpr auto v = res.get_value();
constexpr auto c = res.get_context();

int main()
{
    //std::cout << msg.get_stream().str();

    std::cout << res.get_error_stream().str();
    std::cout << res.get_trace_stream().str();
    std::cout << v;
    std::cout << c.x;
}

