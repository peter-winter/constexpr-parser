#include "../constexpr-parser.hpp"
#include <iostream>
#include <sstream>

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
        default:
            return 0;
        }
    }
};

constexpr int get_int(const std::string_view& sv)
{
    int sum = 0;
    for (size_t i = 0; i < sv.size(); ++i)
    {
        sum *= 10;
        int digit = sv[i] - '0';
        sum += digit;
    }
    return sum;
}

constexpr nterm<int> expr("expr");

constexpr term o_plus('+', 1);
constexpr term o_minus('-', 1);
constexpr term o_mul('*', 2);
constexpr term o_div('/', 2);
constexpr term number("[1-9][0-9]*", "number");

constexpr parser p(
    expr,
    terms(number, o_plus, o_minus, o_mul, o_div, '(', ')'),
    nterms(expr),
    rules(
        expr(expr, '+', expr) >= binary_op{},
        expr(expr, '-', expr) >= binary_op{},
        expr(expr, '*', expr) >= binary_op{},
        expr(expr, '/', expr) >= binary_op{},
        expr('-', expr)[3] >= [](auto, int x) { return -x; },
        expr('(', expr, ')') >= [](auto, int x, auto) { return x; },
        expr(number) >= [](auto sv) { return get_int(sv); }
    )
);

constexpr auto res_ok = p.parse(cstring_buffer("-120 * 2 / 10"));
constexpr int v = res_ok.value();

constexpr auto res_fail = p.parse(cstring_buffer("--"));
constexpr bool b = res_fail.has_value();

int main()
{
    std::cout << "Success case: " << v << std::endl;
    std::cout << "Fail case: " << b << std::endl;

    std::stringstream ss;
    auto res = p.parse(parse_options{}.set_verbose(true), string_buffer("2 + 2 * 2"), ss);
    int rv = res.value();
    std::cout << "Runtime case: " << rv << std::endl;
    std::cout << "Verbose output: " << std::endl << ss.str();
    return 0;
}

