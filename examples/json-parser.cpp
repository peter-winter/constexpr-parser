#include "../ctpg.hpp"

#include <iostream>
#include <variant>
#include <string>
#include <vector>
#include <map>

using namespace ctpg;
using namespace ctpg::ftors;

class js_value_type;
using js_array_type = std::vector<js_value_type>;
using js_object_type = std::map<std::string, js_value_type>;
using js_object_element_type = std::pair<std::string, js_value_type>;

class js_value_type
{
public:
    js_value_type() = default;
    js_value_type(js_value_type&&) = default;
    js_value_type& operator = (js_value_type&&) = default;

    js_value_type(std::nullptr_t): data(nullptr){}
    js_value_type(bool arg): data(arg){}
    js_value_type(double arg): data(arg){}
    js_value_type(std::string&& arg): data(std::move(arg)){}
    js_value_type(js_array_type&& arg): data(std::move(arg)){}
    js_value_type(js_object_type&& arg): data(std::move(arg)){}

    using type = std::variant<
        std::nullptr_t,
        bool,
        double,
        std::string,
        js_array_type,
        js_object_type
    >;

    std::nullptr_t as_null() const { return std::get<std::nullptr_t>(data); }
    bool as_bool() const { return std::get<bool>(data); }
    double as_number() const { return std::get<double>(data); }
    const std::string& as_string() const { return std::get<std::string>(data); }
    const js_array_type& as_array() const { return std::get<js_array_type>(data); }
    const js_object_type& as_object() const { return std::get<js_object_type>(data); }

private:
    type data;
};

double to_js_number(std::string_view sv)
{
    auto it = sv.begin();
    auto end = sv.end();
    
    double sign = 1.0;
    if (*it == '-')
    {
        sign = -1.0;
        it++;
    }
    
    auto get_int = [end](auto& it, char limit1, char limit2, char limit3, char limit4)
    {
        std::uint64_t res = 0;
        while (true)
        {
            if (it == end || *it == limit1 || *it == limit2 || *it == limit3 || *it == limit4)
                break;
            res *= 10;
            res += *it - '0';
            ++it;
        }
        return res;
    };
    std::uint64_t dec = get_int(it, '.', 'E', 'e', '\0');
    if (*it == '.')
        ++it;
    auto it_before_frac = it;
    std::uint64_t frac = get_int(it, 'E', 'e', '\0', '\0');
    std::uint64_t frac_digits = it - it_before_frac;

    std::uint64_t exp = 0;
    bool exp_minus = false;
    if (*it == 'e' || *it == 'E')
    {
        ++it;
        exp_minus = *it++ == '-';
        exp = get_int(it, '\0', '\0', '\0', '\0');
    }

    double frac_d = double(frac);
    for (size_t i = 0; i < frac_digits; ++i)
        frac_d /= 10.0;

    double result = sign * (dec + frac_d);
    for (size_t i = 0; i < exp; ++i)
        result = exp_minus ? result / 10 : result * 10;
    return result;
}

std::uint32_t hex_digits_to_codepoint(std::string_view sv)
{
    auto f = [](char d) -> std::uint32_t
    { 
        if (d >= 'A' && d <= 'F')
            return std::uint32_t(10 + d - 'A');
        else if (d >= 'a' && d <= 'f') 
            return std::uint32_t(10 + d - 'a');
        else
            return std::uint32_t(d - '0');
    };
    return (f(sv[0]) << 12) | (f(sv[1]) << 8) | (f(sv[2]) << 4 )| f(sv[3]);
}

void invalid_unicode_sequence()
{
    throw std::runtime_error("invalid string: surrogate [\\ud800-\\udbff] must be followed by [\\udc00-\\udfff]");
}

std::string to_js_string(std::string_view sv)
{
    std::string str;
    for (size_t i = 1; i < sv.size() - 1; ++i)
    {
        char c = sv[i];
        if (c == '\\')
        {
            c = sv[++i];
            if (c == 'u')
            {
                ++i;
                std::uint32_t cp = hex_digits_to_codepoint(sv.substr(i));
                i += 4;
                if (cp >= 0xd800 && cp <= 0xdbff)
                {
                    if (sv[i] == '\\' && sv[++i] == 'u')
                    {
                        ++i;
                        std::uint32_t cp2 = hex_digits_to_codepoint(sv.substr(i));
                        i += 4;
                        if (cp2 >= 0xdc00 && cp2 <= 0xdfff)
                        {
                            cp = (cp << 10) + cp2 - 0x35fdc00u;
                        }
                        else
                            invalid_unicode_sequence();
                    }
                    else
                        invalid_unicode_sequence();
                }
                if (cp <= 0x80)
                    str.append(1, char(cp));
                else if (cp <= 0x7ff)
                {
                    str.append(1, char((cp >> 6) | 0xc0));
                    str.append(1, char((cp & 0x3f) | 0x80));
                }
                else if (cp <= 0xffff)
                {
                    str.append(1, char((cp >> 12) | 0xe0));
                    str.append(1, char(((cp >> 6) & 0x3f) | 0x80));
                    str.append(1, char((cp & 0x3f) | 0x80));
                }
                else
                {
                    str.append(1, char((cp >> 18) | 0xf0));
                    str.append(1, char(((cp >> 12) & 0x3f) | 0x80));
                    str.append(1, char(((cp >> 6) & 0x3f) | 0x80));
                    str.append(1, char((cp & 0x3f) | 0x80));
                }
            }
            else
            {
                if (c == 'b')
                    c = '\x08';
                else if (c == 't')
                    c = '\x09';
                else if (c == 'n')
                    c = '\x0a';
                else if (c == 'f')
                    c = '\x0c';
                else if (c == 'r')
                    c = '\x0d';
                str.append(1, c);
            }
        }
        else
            str.append(1, c);
    }
    return str;
}

auto to_array(js_value_type&& v)
{
    js_array_type arr;
    arr.emplace_back(std::move(v));
    return arr;
}

auto&& add_array_element(js_value_type&& v, js_array_type&& arr)
{
    arr.emplace_back(std::move(v));
    return std::move(arr);
}

auto to_object(js_object_element_type&& e)
{
    js_object_type ob;
    ob.emplace(std::move(e));
    return ob;
}

auto to_object_element(std::string_view key, js_value_type&& v)
{
    std::string js_key = to_js_string(key);
    return js_object_element_type(std::move(js_key), std::move(v));
}

auto&& add_object_element(js_object_element_type&& e, js_object_type&& ob)
{
    ob.emplace(std::move(e));
    return std::move(ob);
}

constexpr char number_pattern[] = R"_(\-?(0|[1-9][0-9]*)(\.[0-9]*)?((e|E)(\+|\-)[0-9]*)?)_";
constexpr char string_pattern[] = R"_("([^\\"\x00-\x1F]|\\[\\"/bfnrt]|\\u[0-9A-Fa-f]{4})*")_";
constexpr char true_pattern[] = "true";
constexpr char false_pattern[] = "false";
constexpr char null_pattern[] = "null";

constexpr regex_term<number_pattern> js_number("js_number");
constexpr regex_term<string_pattern> js_string("js_string");
constexpr regex_term<true_pattern> js_true("js_true");
constexpr regex_term<false_pattern> js_false("js_false");
constexpr regex_term<null_pattern> js_null("js_null");

constexpr nterm<js_value_type> js_value("js_value");
constexpr nterm<js_object_type> js_object("js_object");
constexpr nterm<js_object_type> js_object_elements("js_object_elements");
constexpr nterm<js_object_element_type> js_object_element("js_object_element");
constexpr nterm<js_array_type> js_array("js_array");
constexpr nterm<js_array_type> js_array_elements("js_array_elements");


constexpr parser js_parser(
    js_object,
    terms(js_number, js_string, js_true, js_false, js_null, '[', ']', ',', '{', '}', ':'),
    nterms(js_object, js_array, js_value, js_array_elements, js_object_elements, js_object_element),
    rules(
        js_value(js_number) 
            >= [](auto sv){ return to_js_number(sv); },
        js_value(js_string) 
            >= [](auto sv){ return to_js_string(sv); },
        js_value(js_true) 
            >= [](skip){ return true; },
        js_value(js_false) 
            >= [](skip){ return false; },
        js_value(js_null) 
            >= [](skip){ return nullptr; },
        js_value(js_array),
        js_value(js_object),
        js_array('[', js_array_elements, ']') 
            >= _e2,
        js_array('[', ']') 
            >= [](skip, skip) { return js_array_type(); },
        js_object('{', js_object_elements, '}')
            >= _e2,
        js_object('{', '}')
            >= [](skip, skip) { return js_object_type(); },
        js_array_elements(js_value) 
            >= [](auto&& v) { return to_array(std::move(v)); },
        js_array_elements(js_array_elements, ',', js_value) 
            >= [](auto&& arr, skip, auto&& v) { return add_array_element(std::move(v), std::move(arr)); },
        js_object_elements(js_object_element) 
            >= [](auto&& e) { return to_object(std::move(e)); },
        js_object_elements(js_object_elements, ',', js_object_element) 
            >= [](auto&& ob, skip, auto&& e) { return add_object_element(std::move(e), std::move(ob)); },
        js_object_element(js_string, ':', js_value)
            >= [](auto k, skip, auto&& v) { return to_object_element(k, std::move(v)); }
    )
);

int main(int argc, char* argv[])
{
    js_parser.write_diag_str(std::cout);

    if (argc < 2)
        return -1;
    
    auto res = js_parser.parse(
        parse_options{}.set_verbose(), 
        buffers::string_buffer(argv[1]), 
        std::cout);

    return 0;
}
