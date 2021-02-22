
#include <iostream>
#include "constexpr-parser.hpp"

constexpr nterm<int> expr("expr");

constexpr term<int> one("1");
constexpr term<int> two("2");
constexpr term o_plus("+", 1);
constexpr term o_minus("-", 1);
constexpr term o_mul("*", 2);
constexpr term o_div("/", 2);
constexpr term o_or("|", 3);
constexpr term o_and("&", 4);
constexpr term o_xor("^", 5);
constexpr term o_not("!", 6);

parser p(
    expr,
    make_terms(one, two, o_plus, o_minus, o_mul, o_div, o_or, o_and, o_xor, o_not, "(", ")"),
    make_nterms(expr),
    make_rules(
        expr(expr, "+", expr),
        expr(expr, "-", expr),
        expr(expr, "*", expr),
        expr(expr, "/", expr),
        expr(expr, "|", expr),
        expr(expr, "&", expr),
        expr(expr, "^", expr),
        expr("-", expr)[6],
        expr("!", expr),
        expr("(", expr, ")"),
        expr("1"),
        expr("2")
    ),
    max_states<100>{}
    //diag_message_size<100000>{}
);

//constexpr const char* msg = p.get_diag_msg();

int main()
{
    //std::cout << msg;
    p.output_diag_msg(std::cout);
    return 0;
}

