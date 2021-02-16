#include "constexpr-parser.hpp"

enum class terms { o_plus, o_minus, o_mul, o_div, o_xor, o_and, o_or, o_not, lpar, rpar, one, two, __size__ };
enum class nterms { expr, __size__ };

constexpr nterm<int, nterms> expr(nterms::expr, "expr");

constexpr term<int, terms> one(terms::one, "1");
constexpr term<int, terms> two(terms::two, "2");
constexpr term<no_value, terms> o_plus(terms::o_plus, "+");
constexpr term<no_value, terms> o_minus(terms::o_minus, "-");
constexpr term<no_value, terms> o_mul(terms::o_mul, "*");
constexpr term<no_value, terms> o_div(terms::o_div, "/");
constexpr term<no_value, terms> o_xor(terms::o_xor, "^");
constexpr term<no_value, terms> o_and(terms::o_and, "&");
constexpr term<no_value, terms> o_or(terms::o_or, "|");
constexpr term<no_value, terms> o_not(terms::o_not, "!");
constexpr term<no_value, terms> lpar(terms::lpar, "(");
constexpr term<no_value, terms> rpar(terms::rpar, ")");


constexpr parser p(
    max_states<1000>{},
    make_terms(one, two, o_plus, o_minus, o_mul, o_div, o_xor, o_and, o_or, o_not, lpar, rpar),
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
        expr("!", expr),
        expr("-", expr),
        expr("(", expr, ")"),
        expr("1"),
        expr("2")
    )
);

int main()
{   
    p.output_diag_msg(std::cout);
    return 0;
}

