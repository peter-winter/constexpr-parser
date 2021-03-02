#include "constexpr-parser.hpp"
#include <iostream>


struct binary_op
{
    constexpr int operator()(int x1, str_view op, int x2) const
    {
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

constexpr parser p(
    expr,
    make_terms("1", "2", o_plus, o_minus, o_mul, o_div, "(", ")"),
    std::make_tuple(expr),
    make_rules(
        expr(expr, "+", expr) >= binary_op{},
        expr(expr, "-", expr) >= binary_op{},
        expr(expr, "*", expr) >= binary_op{},
        expr(expr, "/", expr) >= binary_op{},
        expr("-", expr)[3] >= [](auto, int x) { return -x; },
        expr("(", expr, ")") >= [](auto, int x, auto) { return x; },
        expr("1") >= [](auto) { return 1; },
        expr("2") >= [](auto) { return 2; }
    ),
    no_context{},
    deduce_max_states{}
);

constexpr diag_msg diag(p, use_const_message<40000>{});

constexpr parse_result res_ok(p, cstring_buffer("-((1+2)*2)/2"), use_const_message<1000>{}, use_const_message<1000>{});
constexpr parse_result res_fail(p, cstring_buffer("(()"), use_const_message<1000>{}, use_const_message<1000>{});

constexpr auto v = res_ok.get_value();
constexpr const char* error = res_fail.get_error_stream().str();

constexpr const char* diag_str = diag.get_stream().str();

constexpr const char* res_ok_trace = res_ok.get_trace_stream().str();
constexpr const char* res_fail_trace = res_fail.get_trace_stream().str();

int main()
{
    std::cout << res_ok_trace << std::endl;
    std::cout << "Value: " << v << std::endl;
    
    std::cout << res_fail_trace << std::endl;
    std::cout << error << std::endl;

    return 0;
}

