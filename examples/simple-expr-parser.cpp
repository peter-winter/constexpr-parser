#include "../constexpr-parser.hpp"
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

constexpr int get_int(str_view sv)
{
    int sum = 0;
    for (size_t i = 0; i < sv.size; ++i)
    {
        sum *= 10;
        int digit = sv.str[i] - '0';
        sum += digit;
    }
    return sum;
}

constexpr nterm<int> expr("expr");

constexpr term o_plus('+', 1);
constexpr term o_minus('-', 1);
constexpr term o_mul('*', 2);
constexpr term o_div('/', 2);
constexpr term number("[1-9][0-9]*", "number", 0, associativity::ltor);

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
    ),
    no_context{},
    deduce_max_states{}
);

constexpr diag_msg diag(p, use_const_message<40000>{});

constexpr parse_options opts { true };

constexpr parse_result res_ok(p, opts, cstring_buffer("-((1+2)*2)/2"), use_const_message<10000>{});
constexpr parse_result res_fail(p, opts, cstring_buffer("(()"), use_const_message<10000>{});

//constexpr auto v = res_ok.get_value();

constexpr const char* error_ok = res_ok.get_error_stream().str();
constexpr const char* error_fail = res_fail.get_error_stream().str();

int main()
{
    std::cout << diag.get_stream().str() << std::endl;

    std::cout << "Success case" << std::endl;
    std::cout << error_ok << std::endl;
    //std::cout << "Value: " << v << std::endl << std::endl;
    
    std::cout << "Fail case" << std::endl;
    std::cout << error_fail << std::endl;

    return 0;
}

