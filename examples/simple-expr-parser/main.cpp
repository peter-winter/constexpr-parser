
#include <iostream>
#include "constexpr-parser.hpp"

constexpr nterm<int> expr("expr");

constexpr term o_plus('+', 1);
constexpr term o_minus("-", 1);
constexpr term o_mul('*', 2);
constexpr term o_div('/', 2);
constexpr term o_or('|', 3);
constexpr term o_and('&', 4);
constexpr term o_xor('^', 5);
constexpr term o_not('!', 6);

constexpr parser p(
    expr,
    make_terms('1', '2', o_plus, o_minus, o_mul, o_div, o_or, o_and, o_xor, o_not, '(', ')'),
    make_nterms(expr),
    make_rules(
        expr(expr, '+', expr),
        expr(expr, '-', expr),
        expr(expr, '*', expr),
        expr(expr, '/', expr),
        expr(expr, '|', expr),
        expr(expr, '&', expr),
        expr(expr, '^', expr),
        expr('-', expr)[6],
        expr('!', expr),
        expr('(', expr, ')'),
        expr('1'),
        expr('2')
    )
);

//diag_msg msg(p, use_string_stream{});

constexpr parse_result res(p, cstring_buffer("1+1"), message_max_size<1000>{}, message_max_size<1000>{});
//parse_result res(p, cstring_buffer("1+1"), use_string_stream{}, use_string_stream{});

int main()
{
    //std::cout << msg.get_stream().str();

    //std::cout << res.get_error_stream().str();
    std::cout << res.get_trace_stream().str();
    //std::cout << res.get_value();
    return 0;
}

