#include "constexpr-parser.hpp"
#include <iostream>


constexpr nterm<int> expr("expr");

constexpr term<int> one("1");
constexpr term<int> two("1");

constexpr parser p(
    make_terms("1", "2", "+", "-", "*", "/", "^", "&", "|", "!", "(", ")"),
    make_nterms(expr),
    expr,
    make_rules(
        expr(expr, "+", expr),
        expr(expr, "-", expr),
        expr(expr, "*", expr),
        expr(expr, "/", expr),
        expr(expr, "^", expr),
        expr(expr, "&", expr),
        expr(expr, "|", expr),
        expr("-", expr),
        expr("!", expr),
        expr("(", expr, ")"),
        expr("1"),
        expr("2")
    ),
    max_states<100>{},
    diag_message_size<100000>{}
);

constexpr const char* msg = p.get_diag_msg();

int main()
{
    std::cout << msg;
    return 0;
}

