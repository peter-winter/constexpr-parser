
#include <iostream>
#include "constexpr-parser.hpp"

struct binary_op
{
    constexpr int operator()(int x1, char op, int x2) const
    { 
        switch (op)
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
    constexpr int operator()(char op, int x) const
    {
        switch (op)
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

constexpr term o_plus('+', 1);
constexpr term o_minus("-", 1);
constexpr term o_mul('*', 2);
constexpr term o_div('/', 2);
constexpr term o_or('|', 3);
constexpr term o_and('&', 4);
constexpr term o_xor('^', 5);
constexpr term o_neg('~', 6);

parser p(
    expr,
    make_terms('1', '2', o_plus, o_minus, o_mul, o_div, o_or, o_and, o_xor, o_neg, '(', ')'),
    make_nterms(expr),
    make_rules(
        expr(expr, '+', expr) >= binary_op{},
        expr(expr, '-', expr) >= binary_op{},
        expr(expr, '*', expr) >= binary_op{},
        expr(expr, '/', expr) >= binary_op{},
        expr(expr, '|', expr) >= binary_op{},
        expr(expr, '&', expr) >= binary_op{},
        expr(expr, '^', expr) >= binary_op{},
        expr('-', expr)[6] >= unary_op{},
        expr('~', expr) >= unary_op{},
        expr('(', expr, ')') >= [](char, int x, char) { return x; },
        expr('1') >= [](char) { return 1; },
        expr('2') >= [](char) { return 2; }
    )
);

//diag_msg msg(p, use_string_stream{});

parse_result res(p, cstring_buffer("1+1"), use_message_max_size<1000>{}, use_message_max_size<1000>{});
//parse_result res(p, cstring_buffer("1+1"), use_string_stream{}, use_string_stream{});

int main()
{
    //std::cout << msg.get_stream().str();

    //std::cout << res.get_error_stream().str();
    std::cout << res.get_trace_stream().str();
    //std::cout << res.get_value();
    return 0;
}

