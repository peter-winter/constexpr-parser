#include "constexpr-parser.hpp"

enum class terms { plus, minus, one, two, __size__ };
enum class nterms { expr, expr2, __size__ };


constexpr nterm<int, nterms> expr(nterms::expr, "expr");
constexpr nterm<int, nterms> expr2(nterms::expr2, "expr2");

constexpr term<int, terms> one(terms::one, "1");
constexpr term<int, terms> two(terms::two, "2");
constexpr term<no_value, terms> plus(terms::plus, "+");
constexpr term<no_value, terms> minus(terms::minus, "-");

constexpr parser p(
    max_states<1000>{},
    make_terms(one, two, plus, minus),
    make_nterms(expr, expr2),
    expr,
    expr(expr, "+", expr),
    expr(expr, "-", expr),
    expr("1"),
    expr2("2"),
    expr(expr2)
);


int main()
{
    p.dump(std::cout, 0);
    return 0;
}

