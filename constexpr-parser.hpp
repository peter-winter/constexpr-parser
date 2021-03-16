#include <utility>
#include <type_traits>
#include <cstdint>
#include <limits>
#include <tuple>
#include <algorithm>
#include <sstream>
#include <vector>
#include <optional>
#include <variant>

using size_t = std::uint64_t;
using size8_t = std::uint8_t;
using size16_t = std::uint16_t;
using size32_t = std::uint32_t;

constexpr size_t uninitialized = size_t(-1);
constexpr size16_t uninitialized16 = size16_t(-1);
constexpr size32_t uninitialized32 = size32_t(-1);

struct slice
{
    size32_t start;
    size32_t n;
};

template<typename T>
struct is_cvector_compatible : std::bool_constant<std::is_default_constructible_v<T> && std::is_trivially_destructible_v<T>>
{};

template<typename T, std::size_t N, typename = void>
struct cvector
{};

template<typename T, std::size_t N>
struct cvector<T, N, std::enable_if_t<is_cvector_compatible<T>::value>>
{
    using size_type = std::size_t;
    
    constexpr cvector() :
        the_data{}, current_size(0)
    {}

    constexpr cvector(T&& arg, size_t count):
        the_data{}, current_size(0)
    {
        for (size_t i = 0; i < count; ++i)
            emplace_back(std::move(arg));
    }

    constexpr cvector(const T& arg, size_t count):
        the_data{}, current_size(0)
    {
        for (size_t i = 0; i < count; ++i)
            push_back(arg);
    }

    template<typename Derived>
    struct iterator_base
    {
        using it_type = Derived;
        constexpr it_type* cast() { return static_cast<it_type*>(this); }
        constexpr const it_type* cast() const { return static_cast<const it_type*>(this); }

        constexpr bool operator == (const it_type& other) const { return cast()->ptr == other.ptr; }
        constexpr bool operator != (const it_type& other) const { return cast()->ptr != other.ptr; }
        constexpr it_type operator - (size_type amount) const { return it_type{ cast()->ptr - amount }; }
        constexpr size_type operator - (const it_type& other) const { return size_type(cast()->ptr - other.ptr); }
        constexpr it_type operator + (size_type amount) const { return it_type{ cast()->ptr + amount }; }
        constexpr it_type operator ++(int) { it_type it{ cast()->ptr }; ++(cast()->ptr); return it; }
        constexpr it_type& operator ++() { ++(cast()->ptr); return *cast(); }
        constexpr bool operator > (const it_type& other) const { return cast()->ptr > other.ptr; }
        constexpr bool operator < (const it_type& other) const { return cast()->ptr < other.ptr; }
    };

    struct iterator : iterator_base<iterator>
    {
        T* ptr;
        constexpr iterator(T* ptr) : ptr(ptr) {}
        constexpr T& operator *() const { return *ptr; }
    };

    struct const_iterator : iterator_base<const_iterator>
    {
        const T* ptr;
        constexpr const_iterator(const T* ptr) : ptr(ptr) {}
        constexpr const T& operator *() const { return *ptr; }
    };

    constexpr const T* data() const { return the_data; }
    constexpr T* data() { return the_data; }
    constexpr size_type size() const { return current_size; }
    constexpr bool empty() const { return current_size == 0; }
    constexpr void reserve(size_type) const {};
    constexpr const T& operator[](size_type idx) const { return the_data[idx]; }
    constexpr T& operator[](size_type idx) { return the_data[idx]; }
    constexpr void push_back(const T& v) { the_data[current_size++] = v; }
    constexpr void emplace_back(T&& v) { the_data[current_size++] = std::move(v); }
    constexpr const T& front() const { return the_data[0]; }
    constexpr T& front() { return the_data[0]; }
    constexpr T& back() { return the_data[current_size - 1]; }
    constexpr const T& back() const { return the_data[current_size - 1]; }
    constexpr const_iterator begin() const { return const_iterator(the_data); }
    constexpr const_iterator end() const { return const_iterator(the_data + current_size); }
    constexpr iterator begin() { return iterator(the_data); }
    constexpr iterator end() { return iterator(the_data + current_size); }
    constexpr void clear() { current_size = 0; }
    constexpr iterator erase(iterator first, iterator last)
    {
        if (!(first < last))
            return end();
        auto from = first < begin() ? begin() : first;
        auto to = last > end() ? end() : last;
        size_type diff = to - from;
        iterator it = to;
        while (!(it == end()))
        {
            *from = std::move(*it);
            ++from; ++it;
        }
        current_size -= diff;

        return end();
    }    

    T the_data[N];
    size_type current_size;
};

struct str_view
{
    constexpr str_view(const char* str, size_t size) :
        str(str), size(size)
    {}

    constexpr char operator[](size_t idx) const { return *(str + idx); }

    constexpr static bool equal(const char* str1, const char* str2)
    {
        if (str1 == str2)
            return true;
        if ((str1 == nullptr) ^ (str2 == nullptr))
            return false;
        while (*str1 == *str2)
        {
            if (*str1 == 0)
                return true;
            str1++; str2++;
        }
        return false;
    }

    const char* str;
    size_t size;
};

template<std::size_t N>
struct cbitset
{
    using size_type = std::size_t;

    using underlying_type = std::uint64_t;

    constexpr cbitset& set(size_type idx)
    {
        check_idx(idx);
        data[idx / underlying_size] |= (underlying_type(1) << (idx % underlying_size));
        return *this;
    }

    constexpr cbitset& set(size_type idx, bool value)
    {
        check_idx(idx);
        data[idx / underlying_size] ^= (-!!value ^ data[idx / underlying_size]) & (underlying_type(1) << (idx % underlying_size));
        return *this;
    }

    constexpr cbitset& reset(size_type idx)
    {
        check_idx(idx);
        data[idx / underlying_size] &= ~(underlying_type(1) << (idx % underlying_size));
        return *this;
    }

    constexpr cbitset& flip(size_type idx)
    {
        check_idx(idx);
        data[idx / underlying_size] ^= (underlying_type(1) << (idx % underlying_size));
        return *this;
    }
    
    constexpr bool test(size_type idx) const
    {
        check_idx(idx);
        return (data[idx / underlying_size] >> (idx % underlying_size)) & underlying_type(1);
    }

    constexpr void check_idx(size_type idx) const
    {
        if (idx >= N)
            throw std::runtime_error("Index access out of range");
    }

    constexpr cbitset& flip()
    {
        for (auto& d : data)
            d = ~d;
        return *this;
    }

    constexpr cbitset& set()
    {
        for (auto& d : data)
            d = underlying_type(-1);
        return *this;
    }

    constexpr cbitset& reset()
    {
        for (auto& d : data)
            d = underlying_type(0);
        return *this;
    }

    constexpr size_type size() const
    {
        return N;
    }

    static const size_type underlying_size = sizeof(underlying_type) * 8;
    static const size_type underlying_count = (N / underlying_size) + ((N % underlying_size) ? 1 : 0);
    underlying_type data[underlying_count] = {};
};

template<typename Container, typename Pred>
constexpr Container& sort(Container& c, Pred p)
{
    bool swap = true;
    while (swap)
    {
        swap = false;
        for (auto i = 0u; i < std::size(c) - 1; i++)
        {
            if (p(c[i + 1], c[i]))
            {
                auto x = c[i];
                c[i] = c[i + 1];
                c[i + 1] = x;
                swap = true;
            }
        }
    }
    return c;
}

template<typename T, size_t... I>
constexpr void copy_array(T *a1, const T* a2, std::index_sequence<I...>)
{
    (void(a1[I] = a2[I]), ...);
}

template<size_t N>
struct cstring_buffer
{
    template<size_t N1>
    constexpr cstring_buffer(const char(&source)[N1])
    {
        copy_array(data, source, std::make_index_sequence<N1>{});
    }

    struct iterator
    {
        const char* ptr;

        constexpr char operator *() const { return *ptr; }
        constexpr iterator& operator ++() { ++ptr; return *this; }
        constexpr iterator operator ++(int) { iterator i(*this); ++ptr; return i; }
        constexpr bool operator == (const iterator& other) const { return ptr == other.ptr; }
        constexpr iterator operator + (size_t size) const { return iterator{ptr + size}; }
    };

    constexpr iterator begin() const { return iterator{ data }; }
    constexpr iterator end() const { return iterator{ data + N - 1 }; }
    constexpr str_view view(const iterator& start, const iterator& end) const { return str_view(start.ptr, end.ptr - start.ptr); }

    char data[N] = { 0 };
};

template<size_t N>
cstring_buffer(const char(&)[N])->cstring_buffer<N>;

struct string_buffer
{
    string_buffer(std::string&& str):
        str(std::move(str))
    {}

    string_buffer(const char* str):
        str(str)
    {}

    auto begin() const { return str.begin(); }
    auto end() const { return str.end(); }
    str_view view(const std::string::iterator& it, size_t size) const
    { 
        return str_view(str.data() + (it - str.begin()), size);
    }

    std::string str;
};

template<typename Buffer>
using iterator_t = typename Buffer::iterator;

struct skip
{
    template<typename T>
    constexpr skip(T&&) {};
};

template<typename T>
struct contains_type
{};

template<typename... T>
struct unique_type_list : contains_type<T>...
{
    using variant_type = std::variant<T...>;

    template<typename New>
    struct insert
    {
        static unique_type_list<T...> foo(contains_type<New>*);
        static unique_type_list<T..., New> foo(...);

        using type = decltype(foo(std::declval<unique_type_list<T...>*>()));
    };

    template<typename New>
    typename insert<New>::type operator + (New);
};

template<typename... T>
struct unique_types_variant
{
    using unique_list = decltype((std::declval<unique_type_list<>>() + ... + std::declval<T>()));
    using type = typename unique_list::variant_type;
};

template<typename... T>
using unique_types_variant_t = typename unique_types_variant<T...>::type;

template<typename... T>
constexpr int sum_size = (0 + ... + sizeof(T));

template<size_t N>
using str_table = const char* [N];

template<typename T>
constexpr size_t distinct_values_count = 1 << (sizeof(T) * 8);

constexpr size_t distinct_chars_count = distinct_values_count<char>;

constexpr size_t char_to_idx(char c)
{
    return size_t(int(c) - std::numeric_limits<char>::min());
}

constexpr char idx_to_char(size_t idx)
{
    return char(int(idx) + std::numeric_limits<char>::min());
}

struct char_names
{
    const static size_t name_size = 5;

    constexpr char_names()
    {
        for (size_t i = 0; i < distinct_chars_count; ++i)
        {
            char d[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
            if (idx_to_char(i) > 32 && idx_to_char(i) < 127)
            {
                arr[i][0] = idx_to_char(i);
                arr[i][1] = 0;
            }
            else
            {
                arr[i][0] = '\\';
                arr[i][1] = 'x';
                arr[i][2] = d[i / 16];
                arr[i][3] = d[i % 16];
                arr[i][4] = 0;
            }
        }
    }

    constexpr const char* name(char c) const { return arr[char_to_idx(c)]; }

    char arr[distinct_chars_count][name_size] = {};
};

constexpr char_names c_names = {};

template<typename Buffer, size_t EmptyRulesCount>
struct parse_table_cursor_stack_type
{
    using type = std::vector<size16_t>;
};

template<size_t N, size_t EmptyRulesCount>
struct parse_table_cursor_stack_type<cstring_buffer<N>, EmptyRulesCount>
{
    using type = cvector<size16_t, N + EmptyRulesCount + 1>;
};

template<typename Buffer, size_t EmptyRulesCount>
using parse_table_cursor_stack_type_t = typename parse_table_cursor_stack_type<Buffer, EmptyRulesCount>::type;

template<typename Buffer, size_t EmptyRulesCount, typename ValueVariantType, typename = void>
struct parser_value_stack_type
{};

template<size_t N, size_t EmptyRulesCount, typename ValueVariantType>
struct parser_value_stack_type<cstring_buffer<N>, EmptyRulesCount, ValueVariantType, std::enable_if_t<!is_cvector_compatible<ValueVariantType>::value>>
{
    using type = std::vector<ValueVariantType>;
};

template<size_t N, size_t EmptyRulesCount, typename ValueVariantType>
struct parser_value_stack_type<cstring_buffer<N>, EmptyRulesCount, ValueVariantType, std::enable_if_t<is_cvector_compatible<ValueVariantType>::value>>
{
    using type = cvector<ValueVariantType, N + EmptyRulesCount + 1>;
};

template<typename Buffer, size_t EmptyRulesCount, typename ValueVariantType>
using parser_value_stack_type_t = typename parser_value_stack_type<Buffer, EmptyRulesCount, ValueVariantType>::type;

template<typename Iterator>
struct recognized_term
{
    Iterator it;
    size16_t term_idx;
};

template<size_t MaxSize>
struct cstream
{
    constexpr cstream& operator << (const char* source)
    {
        const char* s = source;
        while (*s)
        {
            data[current_size++] = *s++;
        }

        return *this;
    }

    constexpr cstream& operator << (str_view v)
    {
        for (size_t i = 0; i < v.size; ++i)
        {
            data[current_size++] = v.str[i];
        }

        return *this;
    }

    constexpr cstream& operator << (char c)
    {
        data[current_size++] = c;
        return *this;
    }

    constexpr cstream& operator << (size16_t x)
    {
        return *this << size_t(x);
    }

    constexpr cstream& operator << (size_t x)
    {
        if (x == 0)
        {
            data[current_size++] = '0';
            return *this;
        }

        char digits[std::numeric_limits<size_t>::digits10] = { 0 };
        int digit_count = 0;
        while (x)
        {
            digits[digit_count++] = x % 10;
            x /= 10;
        }
        for (int i = digit_count - 1; i >= 0; --i)
        {
            data[current_size++] = '0' + digits[i];
        }
        return *this;
    }

    constexpr const char* str() const { return data; }

    char data[MaxSize] = { 0 };
    size_t current_size = 0;
};

struct no_stream
{
    template<typename T>
    constexpr const no_stream& operator <<(T&&) const { return *this; }
};

template<size_t N>
constexpr size_t find_str(const str_table<N>& table, const char* str)
{
    size_t res = 0;
    for (const auto& n : table)
    {
        if (str_view::equal(n, str))
            return res;
        res++;
    }
    cstream<100> s;
    s << "string not found: " << str;
    throw std::runtime_error(s.str());
}

constexpr size_t find_char(char c, const char* str)
{
    size_t i = 0;
    while (*str)
    {
        if (*str == c)
            return i;
        str++; i++;
    }
    return uninitialized;
}

template<size_t First, size_t... Rest>
struct max
{
    static const size_t value = std::max(First, max<Rest...>::value);
};

template<size_t X>
struct max<X>
{
    static const size_t value = X;
};

template<size_t... X>
constexpr size_t max_v = max<X...>::value;

template<size_t... X>
constexpr size_t count_zeros = (0 + ... + (X == 0 ? 1 : 0));

template<typename ValueType>
struct nterm
{
    using value_type = ValueType;

    constexpr nterm(const char* name) :
        name(name)
    {
        if (name[0] == 0)
            throw std::runtime_error("empty name not allowed");
    }

    constexpr const char* get_name() const { return name; }

    template<typename... Args>
    constexpr auto operator()(Args&&... args) const;

    const char* name;
};

template<typename ValueType>
struct fake_root
{
    using value_type = ValueType;

    constexpr auto operator()(const nterm<ValueType>& nt) const;

    constexpr static const char* get_name() { return "##"; };
};

template<typename F, typename L, typename...R>
struct rule
{
    using f_type = F;

    F f;
    L l;
    std::tuple<R...> r;
    static const size_t n = sizeof...(R);
    int precedence;

    constexpr rule(L l, std::tuple<R...> r) :
        f(nullptr), l(l), r(r), precedence(0)
    {}

    template<typename F1>
    constexpr rule(F1&& f, L l, std::tuple<R...> r) :
        f(std::move(f)), l(l), r(r), precedence(0)
    {}

    template<typename F1>
    constexpr rule(F1&& f, L l, std::tuple<R...> r, int precedence) :
        f(std::move(f)), l(l), r(r), precedence(precedence)
    {}

    constexpr auto operator[](int precedence)
    {
        return rule<F, L, R...>(std::move(f), l, r, precedence);
    }

    template<typename F1>
    constexpr auto operator >= (F1&& f)
    {
        return rule<std::decay_t<F1>, L, R...>(std::move(f), l, r, precedence);
    }
};

template<typename L, typename... R>
rule(L l, std::tuple<R...> r) -> rule<std::nullptr_t, L, R...>;

enum class associativity { ltor, rtol };

enum class term_method { exact, exclude, include, regex };

template<size_t N>
struct exclude
{
    template<size_t N1>
    constexpr exclude(const char(&source)[N1])
    {
        copy_array(data, source, std::make_index_sequence<N1>{});
    }

    char data[N] = {};
};

template<size_t N>
exclude(const char(&)[N])->exclude<N>;

template<size_t N>
struct include
{
    template<size_t N1>
    constexpr include(const char(&source)[N1])
    {
        copy_array(data, source, std::make_index_sequence<N1>{});
    }

    char data[N] = {};
};

template<size_t N>
include(const char(&)[N])->include<N>;

using term_value_type = str_view;

template<size_t DataSize>
struct term
{
    using value_type = term_value_type;
    static const size_t data_size = DataSize;

    constexpr term(char c, int precedence = 0, associativity a = associativity::ltor) :
        precedence(precedence), ass(a),  
        method(term_method::exact)
    {
        data[0] = c;
        copy_array(id, c_names.name(c), std::make_index_sequence<char_names::name_size>{});
    }

    template<size_t N>
    constexpr term(const char (&source)[N], const char *custom_name, int precedence = 0, associativity a = associativity::ltor) :
        precedence(precedence), ass(a),  
        method(term_method::regex),
        custom_name(custom_name)
    {
        copy_array(data, source, std::make_index_sequence<N>{});
        id[0] = 'r';
        id[1] = '_';
        copy_array(&id[2], source, std::make_index_sequence<N>{});
    }

    template<size_t N>
    constexpr term(const char (&source)[N], int precedence = 0, associativity a = associativity::ltor) :
        precedence(precedence), ass(a),  
        method(term_method::regex)
    {
        copy_array(data, source, std::make_index_sequence<N>{});
        id[0] = 'r';
        id[1] = '_';
        copy_array(&id[2], source, std::make_index_sequence<N>{});
    }

    template<size_t N>
    constexpr term(exclude<N> ex, const char *custom_name, int precedence = 0, associativity a = associativity::ltor) :
        custom_name(custom_name), precedence(precedence), ass(a), 
        method(term_method::exclude)
    {
        copy_array(data, ex.data, std::make_index_sequence<N>{});
        id[0] = 'e';
        id[1] = '_';
        copy_array(&id[2], ex.data, std::make_index_sequence<N>{});
    }

    template<size_t N>
    constexpr term(include<N> in, const char *custom_name, int precedence = 0, associativity a = associativity::ltor) :
        custom_name(custom_name), precedence(precedence), ass(a), 
        method(term_method::include)
    {
        copy_array(data, in.data, std::make_index_sequence<N>{});
        id[0] = 'i';
        id[1] = '_';
        copy_array(&id[2], in.data, std::make_index_sequence<N>{});
    }
    
    constexpr const char* get_name() const { return custom_name ? custom_name : id; }
    constexpr const char* get_id() const { return id; }

    constexpr bool is_trivial() const 
    { 
        return method != term_method::regex;
    }

    char data[data_size] = {};
    int precedence;
    associativity ass;
    char id[data_size + 10] = {};
    const char* custom_name = nullptr;
    term_method method;
};

term(char c, int = 0, associativity = associativity::ltor) -> term<1>;

template<size_t N>
term(const char (&)[N], int = 0, associativity = associativity::ltor) -> term<N>;

template<size_t N>
term(const char (&)[N], const char *, int = 0, associativity = associativity::ltor) -> term<N>;

template<size_t N>
term(exclude<N>, const char *, int = 0, associativity = associativity::ltor) -> term<N>;

template<size_t N>
term(include<N>, const char *, int = 0, associativity = associativity::ltor) -> term<N>;

struct eof
{
    constexpr static const char* get_name() { return "<eof>"; }
};

template<typename Arg>
constexpr auto make_rule_item(Arg&& arg)
{
    return term(arg);
}

template<typename ValueType>
constexpr auto make_rule_item(const nterm<ValueType>& nt)
{
    return nt;
}

template<typename ValueType>
template<typename... Args>
constexpr auto nterm<ValueType>::operator()(Args&&... args) const
{
    return rule(
        *this, 
        std::make_tuple(make_rule_item(args)...)
    );
}

template<typename ValueType>
constexpr auto fake_root<ValueType>::operator()(const nterm<ValueType>& nt) const
{
    return rule(*this, std::make_tuple(nt));
}

using conflicted_terms = size16_t[4];

constexpr void add_conflicted_term(conflicted_terms& ts, size16_t t)
{
    for (size_t i = 0; i < 4; ++i)
        if (ts[i] == uninitialized16)
        {
            ts[i] = t;
            break;
        }
}

struct dfa_state
{
    constexpr dfa_state()
    {
        for (auto& t : transitions)
            t = uninitialized16;
    }

    size8_t start_state = false;
    size8_t end_state = false;
    size8_t unreachable = false;
    conflicted_terms conflicted_recognition = { uninitialized16, uninitialized16, uninitialized16, uninitialized16 };
    static const size_t transitions_size = distinct_values_count<char>;
    size16_t transitions[transitions_size] = {};
};

struct char_range
{
    char start;
    char end;
};

using char_subset = cbitset<distinct_values_count<char>>;

template<size_t MaxStates>
struct dfa
{
    using dfa_state_container_type = cvector<dfa_state, MaxStates>;
    
    constexpr static char_subset char_subset_from_item(char_range r)
    {
        char_subset res{};
        for (size_t i = char_to_idx(r.start); i <= char_to_idx(r.end); ++i)
        {
            res.set(i);
        }    
        return res;
    }
    
    constexpr static char_subset add_item_to_char_subset(char_subset&& s, char_range r)
    {
        for (size_t i = char_to_idx(r.start); i <= char_to_idx(r.end); ++i)
            s.set(i);
        
        return s;
    }
    
    constexpr slice add_primary_single_char(char c)
    {
        char_subset s;
        s.set(char_to_idx(c));
        return add_primary_char_subset(s);
    }

    constexpr slice add_primary_char_subset(const char_subset& s)
    {
        size_t old_size = states.size();
        states.push_back(dfa_state());
        states.back().start_state = 1;
        for (size_t i = 0; i < std::size(s); ++i)
            if (s.test(i))
            {
                states.back().transitions[i] = size16_t(states.size());
            }
        states.push_back(dfa_state());
        states.back().end_state = 1;
        return slice{ size32_t(old_size), 2 };
    }

    constexpr slice add_primary_char_subset_exclusive(char_subset& s)
    {
        s.flip();
        return add_primary_char_subset(s);
    }

    constexpr slice add_primary_any_char()
    {
        char_subset s;
        s.set();
        return add_primary_char_subset(s);
    }

    constexpr slice add_multiplication(slice s)
    {
        size_t b = s.start;
        states[b].end_state = 1;
        for (size_t i = s.start; i < s.start + s.n; ++i)
        {
            if (states[i].end_state)
                merge(i, b);
        }
        return s;
    }

    constexpr slice add_optional(slice s)
    {
        states[s.start].end_state = 1;
        return s;
    }    

    constexpr slice add_concat(slice s1, slice s2)
    {
        size_t b = s2.start;
        for (size_t i = s1.start; i < s1.start + s1.n; ++i)
        {
            if (states[i].end_state)
                merge(i, b, false, true);
        }
        return slice{ s1.start, s1.n + s2.n };
    }

    constexpr slice add_alt(slice s1, slice s2)
    {
        size_t b1 = s1.start;
        size_t b2 = s2.start;
        merge(b1, b2, true, true);
        return slice{ s1.start, s1.n + s2.n };
    }

    constexpr void merge(size_t to, size_t from, bool alt_end_state = false, bool mark_from_as_unreachable = false)
    {
        if (to == from)
            return;
        dfa_state& s_from = states[from];
        dfa_state& s_to = states[to];
        s_from.start_state = 0;
        if (alt_end_state)
            s_to.end_state = s_to.end_state || s_from.end_state;
        else
            s_to.end_state = s_from.end_state;

        for (size_t i = 0; i < dfa_state::transitions_size; ++i)
        {
            size16_t& tr_from = s_from.transitions[i];
            if (tr_from == uninitialized16)
                continue;
            size16_t& tr_to = s_to.transitions[i];            
            if (tr_to == uninitialized16)
                tr_to = tr_from;
            else
                merge(tr_to, tr_from, alt_end_state, mark_from_as_unreachable);
        }

        auto& cr = s_from.conflicted_recognition;
        for (size_t j = 0; j < 4; ++j)
        {
            size_t term_idx = cr[j];
            if (term_idx != uninitialized16)
                mark_end_state(states[to], term_idx);
            else
                break;
        }

        s_from.unreachable = mark_from_as_unreachable ? 1 : 0;
    }

    constexpr void mark_end_state(dfa_state& s, size_t idx)
    {
        if (!s.end_state)
            return;        
        add_conflicted_term(s.conflicted_recognition, idx);
    }

    constexpr void mark_end_states(slice s, size_t idx)
    {
        for (size_t i = s.start; i < s.start + s.n; ++i)
        {
            mark_end_state(states[i], idx);
        }
    }

    constexpr size_t size() const { return states.size(); }

    template<typename Stream, size_t N>
    constexpr void write_state_diag_str(const dfa_state& st, Stream& s, size16_t idx, const str_table<N>& term_names) const
    {
        s << "STATE " << idx;
        if (st.unreachable)
        {
            s << " (unreachable) \n";
            return;
        }

        if (st.end_state)
            s << " recognized ";
        size16_t term_idx = st.conflicted_recognition[0];
        if (term_idx != uninitialized16)
            s << term_names[term_idx];
        s << "   ";

        auto f_range = [&s](const auto& r, size16_t state_idx)
        {
            if (r.size() > 2)
            {
                s << "[";
                s << c_names.name(r.front());
                s << " - ";
                s << c_names.name(r.back());
                s << "] -> " << state_idx << "  ";
            }
            else
            {
                for (char c : r)
                {
                    s << c_names.name(c);
                    s << " -> " << state_idx << "  ";
                }
            }
        };

        cvector<char, dfa_state::transitions_size> tmp;
        size16_t prev = uninitialized16;
        for (size_t i = 0; i < dfa_state::transitions_size + 1; ++i)
        {       
            size16_t to = (i == dfa_state::transitions_size ? uninitialized16 : st.transitions[i]);
            if (to == prev && to != uninitialized16)
                tmp.push_back(idx_to_char(i));
            else 
            {
                if (prev != uninitialized16)
                {
                    f_range(tmp, prev);
                    tmp.clear();
                }
                if (to != uninitialized16)
                    tmp.push_back(idx_to_char(i));
            }
            prev = to;
        }
        s << "\n";
    }

    template<typename Stream, size_t N>
    constexpr void write_diag_str(Stream& s, const str_table<N>& term_names) const
    {
        for (size_t i = 0; i < states.size(); ++i)
        {
            write_state_diag_str(states[i], s, i, term_names);
        }
    }

    template<typename Iterator, typename ParserState>
    constexpr auto recognize(const Iterator& start, const Iterator& end, ParserState& ps) const
    {
        size16_t state_idx = 0;
        Iterator it = start;
        recognized_term<Iterator> rt{ end, uninitialized16 };
        while (true)
        {
            const dfa_state& state = states[state_idx];
            size16_t rec_idx = state.conflicted_recognition[0];
            if (rec_idx != uninitialized16)
            {
                rt.it = it;
                rt.term_idx = rec_idx;

                if (ps.options.verbose)
                {
                    ps.error_stream << "LEXER: Recognized " << rec_idx << "\n";
                }
            }
            
            if (it == end)
                break;
            
            size16_t tr = state.transitions[char_to_idx(*it)];
            if (tr == uninitialized16)
                break;

            state_idx = tr;

            if (ps.options.verbose)
            {
                ps.error_stream << "LEXER: Current char " << *it << "\n";
                ps.error_stream << "LEXER: New state " << state_idx << "\n";
            }

            ++it;
        }
        return rt;
    }

    dfa_state_container_type states = {};
};

template<typename C>
struct use_context
{
    using type = C;
};

struct no_context
{
    using type = no_context;
};

template<typename DFA, size_t SSize, size_t... N, size_t... I>
constexpr bool create_lexer_impl(DFA& sm, cstream<SSize>& error_stream, std::index_sequence<I...>, const std::tuple<term<N>...>& ts);

struct parse_options
{
    bool verbose = false;
};

constexpr size_t value_stack_initial_capacity = 1 << 10;
constexpr size_t cursor_stack_initial_capacity = 1 << 10;

template<typename CursorStack, typename ValueStack, typename Context, typename ErrorStream>
struct parser_state
{
    constexpr parser_state(
        CursorStack& cursor_stack,
        ValueStack& value_stack,
        Context& context,
        ErrorStream& error_stream,
        parse_options options) :
        cursor_stack(cursor_stack),
        value_stack(value_stack),
        context(context),
        error_stream(error_stream),
        options(options)
    {
        cursor_stack.reserve(cursor_stack_initial_capacity);
        value_stack.reserve(value_stack_initial_capacity);
    }

    CursorStack& cursor_stack;
    ValueStack& value_stack;
    ErrorStream& error_stream;
    Context& context;
    parse_options options;
};

template<typename Root, typename Terms, typename NTerms, typename Rules, typename ContextUsage, typename MaxStatesUsage>
struct parser
{};

template<typename RootValueType, size_t... DataSize, typename... NTermValueType, typename... Rules, typename ContextUsage, typename MaxStatesUsage>
struct parser<
    nterm<RootValueType>,
    std::tuple<term<DataSize>...>,
    std::tuple<nterm<NTermValueType>...>,
    std::tuple<Rules...>,
    ContextUsage,
    MaxStatesUsage
>
{
    static const size_t max_states = MaxStatesUsage::template value<sizeof...(DataSize), Rules::n...>;
    static const size_t max_rule_element_count = max_v<1, Rules::n...>;
    static const size_t term_count = sizeof...(DataSize) + 1;
    static const size16_t eof_idx = sizeof...(DataSize);
    static const size_t nterm_count = sizeof...(NTermValueType) + 1;
    static const size16_t fake_root_idx = sizeof...(NTermValueType);
    static const size_t rule_count = sizeof...(Rules) + 1;
    static const size_t root_rule_idx = sizeof...(Rules);
    static const size_t empty_rules_count = count_zeros<Rules::n...>;
    static const size_t situation_size = max_rule_element_count + 1;
    static const size_t situation_address_space_size = rule_count * situation_size * term_count;    
    static const size_t total_regex_size = (0 + ... + DataSize);

    using term_tuple_type = std::tuple<term<DataSize>...>;
    using nterm_tuple_type = std::tuple<nterm<NTermValueType>...>;
    using rule_tuple_type = std::tuple<Rules...>;
    using root_nterm_type = nterm<RootValueType>;
    using value_variant_type = unique_types_variant_t<char, term_value_type, NTermValueType...>;
    using term_subset = cbitset<term_count>;
    using nterm_subset = cbitset<nterm_count>;
    using right_side_slice_subset = cbitset<situation_size * rule_count>;
    using state = cbitset<situation_address_space_size>;
    using root_value_type = RootValueType;
    using context_type = typename ContextUsage::type;

    struct symbol
    {
        constexpr symbol() :
            term(false), idx(uninitialized16)
        {}

        constexpr symbol(bool term, size16_t idx) :
            term(term), idx(idx)
        {}

        constexpr size16_t get_parse_table_idx() const
        {
            return parser::get_parse_table_idx(term, idx);
        }

        bool term;
        size16_t idx;
    };

    struct situation_address
    {
        size16_t rule_info_idx;
        size16_t after;
        size16_t t;
    };

    struct rule_info
    {
        size16_t l_idx;
        size16_t r_idx;
        size16_t r_elements;
    };

    struct situation_queue_entry
    {
        size16_t state_idx;
        size32_t idx;
    };

    enum class parse_table_entry_kind : size16_t { error, success, shift, reduce, rr_conflict };

    struct parse_table_entry
    {
        parse_table_entry_kind kind;
        size16_t shift;
        size16_t reduce;
        size8_t has_shift;
        size8_t has_reduce;

        constexpr void set_shift(size16_t value) { shift = value; has_shift = 1; }
        constexpr void set_reduce(size16_t value) { reduce = value; has_reduce = 1; }
    };

    constexpr static size16_t get_parse_table_idx(bool term, size16_t idx)
    {
        return term ? nterm_count + idx : idx;
    }

    constexpr parser(
        root_nterm_type root,
        term_tuple_type terms,
        nterm_tuple_type nterms,
        rule_tuple_type&& rules,
        ContextUsage,
        MaxStatesUsage) :
        parser(root, terms, nterms, std::move(rules))
    {}

    constexpr parser(
        root_nterm_type root,
        term_tuple_type terms,
        nterm_tuple_type nterms,
        rule_tuple_type&& rules,
        ContextUsage) :
        parser(root, terms, nterms, std::move(rules))
    {}

    constexpr parser(
        root_nterm_type root,
        term_tuple_type terms,
        nterm_tuple_type nterms,
        rule_tuple_type&& rules):
        term_tuple(terms),
        nterm_tuple(nterms),
        rule_tuple(std::move(rules))
    {
        for (auto& x : trivial_term_table)
            for (size_t i = 0; i < 4; ++i)
                x[i] = uninitialized16;

        analyze_nterms(std::make_index_sequence<std::tuple_size_v<nterm_tuple_type>>{});
        analyze_nterm(fake_root<value_type_t<root_nterm_type>>{});
        analyze_terms(std::make_index_sequence<std::tuple_size_v<term_tuple_type>>{});
        analyze_eof(eof{});
        analyze_rules(std::make_index_sequence<std::tuple_size_v<rule_tuple_type>>{}, root);
        analyze_states();
        
        if (!trivial_lexical_analyzer)
            valid_lexer = create_lexer(lexer_sm, lexer_error_stream, term_tuple);
    }

    constexpr void analyze_eof(eof)
    {
        term_names[eof_idx] = eof::get_name();
        term_ids[eof_idx] = eof::get_name();
        term_precedences[eof_idx] = 0;
        term_associativities[eof_idx] = associativity::ltor;
    }

    template<size_t N>
    constexpr void analyze_trivial_term(const term<N>& t, size16_t idx)
    {
        if (t.method == term_method::exact)
            add_conflicted_term(trivial_term_table[char_to_idx(t.data[0])], idx);
        else if (t.method == term_method::exclude)
        {
            for (size_t i = 0; i < std::size(trivial_term_table); ++i)
            {
                size_t idx_found = find_char(idx_to_char(i), t.data);
                if (idx_found == uninitialized)
                    add_conflicted_term(trivial_term_table[i], idx);
            }
        }
        else if (t.method == term_method::include)
        {
            const char* data = t.data;
            while (*data)
            {
                add_conflicted_term(trivial_term_table[*data], idx);
                ++data;
            }                
        }
    }

    template<size_t N>
    constexpr void analyze_term(const term<N>& t, size16_t idx)
    {
        term_names[idx] = t.get_name();
        term_ids[idx] = t.get_id();
        term_precedences[idx] = t.precedence;
        term_associativities[idx] = t.ass;

        if (trivial_lexical_analyzer && t.is_trivial())
            analyze_trivial_term(t, idx);
        else
            trivial_lexical_analyzer = false;
    }

    template<typename ValueType>
    constexpr void analyze_nterm(const nterm<ValueType>& nt, size16_t idx)
    {
        nterm_names[idx] = nt.get_name();
    }

    template<typename ValueType>
    constexpr void analyze_nterm(fake_root<ValueType>)
    {
        nterm_names[fake_root_idx] = fake_root<ValueType>::get_name();
    }

    template<size_t N>
    constexpr auto make_symbol(const term<N>& t) const
    {
        return symbol{ true, size16_t(find_str(term_ids, t.get_id())) };
    }

    template<typename ValueType>
    constexpr auto make_symbol(const nterm<ValueType>& nt) const
    {
        return symbol{ false, size16_t(find_str(nterm_names, nt.name)) };
    }
    
    template<size_t... I>
    constexpr void analyze_terms(std::index_sequence<I...>)
    {
        (void(analyze_term(std::get<I>(term_tuple), I)), ...);
    }

    template<size_t... I>
    constexpr void analyze_nterms(std::index_sequence<I...>)
    {
        (void(analyze_nterm(std::get<I>(nterm_tuple), I)), ...);
    }

    template<size_t... I>
    constexpr void analyze_rules(std::index_sequence<I...>, const root_nterm_type& root)
    {
        (void(analyze_rule<I>(std::get<I>(rule_tuple), std::make_index_sequence<Rules::n>{})), ...);
        analyze_rule<root_rule_idx>(fake_root<value_type_t<root_nterm_type>>{}(root), std::index_sequence<0>{});
        sort(rule_infos, [](const auto& ri1, const auto& ri2) { return ri1.l_idx < ri2.l_idx; });
        make_nterm_rule_slices();
    }

    constexpr void make_nterm_rule_slices()
    {
        size16_t nt = 0;
        for (size16_t i = 0u; i < rule_count; ++i)
        {
            if (nt != rule_infos[i].l_idx)
            {
                nt = rule_infos[i].l_idx;
                nterm_rule_slices[nt].start = i;
                nterm_rule_slices[nt].n = 1;
            }
            else
                nterm_rule_slices[nt].n++;
        }
    }

    constexpr size16_t calculate_rule_last_term(size16_t rule_idx, size16_t rule_size) const
    {
        for (int i = int(rule_size - 1); i >= 0; --i)
        {
            const auto& s = right_sides[rule_idx][i];
            if (!s.term)
                continue;
            return s.idx;
        }
        return uninitialized16;
    }

    constexpr int calculate_rule_precedence(int precedence, size16_t rule_idx, size16_t rule_size) const
    {
        if (precedence != 0)
            return precedence;
        size16_t last_term_idx = rule_last_terms[rule_idx];
        if (last_term_idx != uninitialized16)
            return term_precedences[last_term_idx];
        return 0;
    }

    template<typename T>
    using value_type_t = typename T::value_type;

    template<size_t Nr, typename F, typename L, typename... R, size_t... I>
    constexpr void analyze_rule(const rule<F, L, R...>& r, std::index_sequence<I...>)
    {
        size16_t l_idx = size16_t(find_str(nterm_names, r.l.get_name()));
        (void(right_sides[Nr][I] = make_symbol(std::get<I>(r.r))), ...);
        constexpr size16_t rule_elements_count = size16_t(sizeof...(R));
        rule_infos[Nr] = { l_idx, size16_t(Nr), rule_elements_count };
        rule_last_terms[Nr] = calculate_rule_last_term(Nr, rule_elements_count);
        rule_precedences[Nr] = calculate_rule_precedence(r.precedence, Nr, rule_elements_count);
        if constexpr (Nr != root_rule_idx)
        {
            value_reductors[Nr] = &reduce_value<Nr, F, value_type_t<L>, value_type_t<R>...>;
        }
    }

    constexpr size32_t make_situation_idx(situation_address a) const
    {
        return a.rule_info_idx * situation_size * term_count + a.after * term_count + a.t;
    }

    constexpr situation_address make_situation_address(size32_t idx) const
    {
        size16_t t = idx % term_count;
        idx /= term_count;
        size16_t after = idx % situation_size;
        size16_t rule_info_idx = idx / situation_size;
        return situation_address{ rule_info_idx, after, t };
    }

    constexpr void add_term_subset(term_subset& dest, const term_subset& source)
    {
        for (size_t i = 0u; i < term_count; ++i)
            dest.set(i, dest.test(i) || source.test(i));
    }

    constexpr const term_subset& make_right_side_slice_first(const rule_info& ri, size_t start)
    {
        size_t right_side_slice_idx = max_rule_element_count * ri.r_idx + start;
        auto& res = right_side_slice_first[right_side_slice_idx];

        if (right_side_slice_first_analyzed.test(right_side_slice_idx))
            return res;
        right_side_slice_first_analyzed.set(right_side_slice_idx);
            
        for (size_t i = start; i < ri.r_elements; ++i)
        {
            const symbol& s = right_sides[ri.r_idx][i];
            if (s.term)
            {
                res.set(s.idx);
                break;
            }
            add_term_subset(res, make_nterm_first(s.idx));
            if (!make_nterm_empty(s.idx))
                break;
        }
        return res;
    }

    constexpr const term_subset& make_nterm_first(size16_t nt)
    {
        if (nterm_first_analyzed.test(nt))
            return nterm_first[nt];
        nterm_first_analyzed.set(nt);

        const slice& s = nterm_rule_slices[nt];
        for (size_t i = 0u; i < s.n; ++i)
        {
            const rule_info& ri = rule_infos[s.start + i];
            add_term_subset(nterm_first[nt], make_right_side_slice_first(ri, 0));
        }
        return nterm_first[nt];
    }

    constexpr bool is_right_side_slice_empty(const rule_info& ri, size_t start)
    {
        for (size_t i = start; i < ri.r_elements; ++i)
        {
            const symbol& s = right_sides[ri.r_idx][i];
            if (s.term)
                return false;
            if (!make_nterm_empty(s.idx))
                return false;
        }
        return true;
    }

    constexpr bool is_right_side_empty(const rule_info& ri)
    {
        return is_right_side_slice_empty(ri, 0);
    }

    constexpr bool make_nterm_empty(size16_t nt)
    {
        if (nterm_empty_analyzed.test(nt))
            return nterm_empty.test(nt);
        nterm_empty_analyzed.set(nt);

        const slice& s = nterm_rule_slices[nt];
        for (size_t i = 0u; i < s.n; ++i)
        {
            if (is_right_side_empty(rule_infos[s.start + i]))
            {
                return (nterm_empty.set(nt), true);
            }
        }
        return (nterm_empty.reset(nt), false);
    }

    constexpr const term_subset& make_situation_first_after(const situation_address& addr, size32_t idx)
    {
        if (situation_first_after_analyzed.test(idx))
            return situation_first_after[idx];
        situation_first_after_analyzed.set(idx);

        const rule_info& ri = rule_infos[addr.rule_info_idx];
        add_term_subset(situation_first_after[idx], make_right_side_slice_first(ri, addr.after + 1));
        if (is_right_side_slice_empty(ri, addr.after + 1))
            situation_first_after[idx].set(addr.t);
        return situation_first_after[idx];
    }

    constexpr void analyze_states()
    {
        situation_address root_situation_address{ root_rule_idx, 0, eof_idx };
        size32_t idx = make_situation_idx(root_situation_address);
        state_count = 1;
        add_situation_to_state(0, idx);

        size32_t n = 0;
        while (n != situation_queue_size)
        {
            analyze_situation(situation_queue[n].state_idx, situation_queue[n].idx);
            n++;
        }
    }

    constexpr void analyze_situation(size16_t state_idx, size32_t idx)
    {
        situation_closure(state_idx, idx);
        situation_transition(state_idx, idx);
    }

    constexpr void add_situation_to_state(size16_t state_idx, size32_t idx)
    {
        state& s = states[state_idx];
        if (!s.test(idx))
        {
            s.set(idx);
            situation_queue[situation_queue_size++] = { state_idx, idx };
        }
    }

    constexpr void situation_closure(size16_t state_idx, size32_t idx)
    {
        situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        if (addr.after >= ri.r_elements)
            return;

        const symbol& s = right_sides[ri.r_idx][addr.after];
        if (!s.term)
        {
            size16_t nt = s.idx;
            const term_subset& first = make_situation_first_after(addr, idx);
            const slice& s = nterm_rule_slices[nt];
            for (auto i = 0u; i < s.n; ++i)
            {
                for (size16_t t = 0; t < term_count; ++t)
                {
                    if (first.test(t))
                    {
                        size32_t new_s_idx = make_situation_idx(situation_address{ size16_t(s.start + i), 0, t });
                        add_situation_to_state(state_idx, new_s_idx);
                    }
                }
            }
        }
    }

    constexpr auto solve_conflict(size16_t rule_info_idx, size16_t term_idx) const
    {
        size16_t rule_idx = rule_infos[rule_info_idx].r_idx;
        int r_p = rule_precedences[rule_idx];
        int t_p = term_precedences[term_idx];
        if (r_p > t_p)
            return parse_table_entry_kind::reduce;

        if (r_p == t_p)
        {
            size16_t last_term_idx = rule_last_terms[rule_idx];
            if (term_associativities[last_term_idx] == associativity::ltor)
                return parse_table_entry_kind::reduce;
        }
        return parse_table_entry_kind::shift;
    }

    constexpr void situation_transition(size16_t state_idx, size32_t idx)
    {
        situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        bool reduction = addr.after >= ri.r_elements;

        const auto& s = right_sides[ri.r_idx][addr.after];
        size16_t symbol_idx = reduction ? get_parse_table_idx(true, addr.t) : s.get_parse_table_idx();
        auto& entry = parse_table[state_idx][symbol_idx];
        if (reduction)
        {
            if (ri.r_idx == root_rule_idx)
            {
                entry.kind = parse_table_entry_kind::success;
            }
            else if (entry.has_shift)
            {
                entry.kind = solve_conflict(addr.rule_info_idx, addr.t);
            }
            else if (entry.has_reduce)
            {
                entry.kind = parse_table_entry_kind::rr_conflict;
            }
            else
            {
                entry.kind = parse_table_entry_kind::reduce;
            }
            entry.set_reduce(size16_t(addr.rule_info_idx));
            return;
        }

        situation_address new_addr = situation_address{ addr.rule_info_idx, size16_t(addr.after + 1), addr.t };
        size32_t new_idx = make_situation_idx(new_addr);

        if (entry.has_shift)
        {
            add_situation_to_state(entry.shift, new_idx);
            return;
        }

        size16_t new_state_idx = uninitialized16;
        for (size16_t i = 0; i < state_count; ++i)
        {
            if (states[i].test(new_idx))
            {
                new_state_idx = i;
                break;
            }
        }
        if (new_state_idx == uninitialized16)
        {
            new_state_idx = state_count;
            ++state_count;
        }

        if (entry.has_reduce)
        {
            entry.kind = solve_conflict(entry.reduce, s.idx);
        }
        else
        {
            entry.kind = parse_table_entry_kind::shift;
        };
        entry.set_shift(new_state_idx);
        add_situation_to_state(new_state_idx, new_idx);
    }

    constexpr const char* get_symbol_name(const symbol& s) const
    {
        return s.term ? term_names[s.idx] : nterm_names[s.idx];
    }

    template<typename Stream>
    constexpr void write_rule_diag_str(Stream& s, size16_t rule_info_idx) const
    {
        const rule_info& ri = rule_infos[rule_info_idx];
        s << nterm_names[ri.l_idx] << " <- ";
        if (ri.r_elements > 0)
            s << get_symbol_name(right_sides[ri.r_idx][0]);
        for (size_t i = 1u; i < ri.r_elements; ++i)
        {
            s << " " << get_symbol_name(right_sides[ri.r_idx][i]);
        }
    }

    template<typename Stream>
    constexpr void write_situation_diag_str(Stream& s, size32_t idx) const
    {
        const situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        s << nterm_names[ri.l_idx] << " <- ";
        for (size_t i = 0u; i < addr.after; ++i)
        {
            s << get_symbol_name(right_sides[ri.r_idx][i]) << " ";
        }
        s << ". ";
        for (size_t i = addr.after; i < ri.r_elements; ++i)
        {
            s << get_symbol_name(right_sides[ri.r_idx][i]) << " ";
        }
        s << "==> " << term_names[addr.t];
    }

    template<typename Stream>
    constexpr void write_state_diag_str(Stream& s, size16_t idx) const
    {
        s << "STATE " << idx << "\n";

        for (size_t i = 0u; i < situation_address_space_size; ++i)
        {
            if (states[idx].test(i))
            {
                write_situation_diag_str(s, i);
                s << "\n";
            }
        }

        s << "\n";

        for (size_t i = 0; i < nterm_count; ++i)
        {
            const auto& entry = parse_table[idx][i];
            if (entry.kind == parse_table_entry_kind::shift)
                s << "On " << nterm_names[i] << " go to " << entry.shift << "\n";
        }
        for (size_t i = nterm_count; i < nterm_count + term_count; ++i)
        {
            const auto& entry = parse_table[idx][i];
            if (entry.kind == parse_table_entry_kind::error)
                continue;

            size_t term_idx = i - nterm_count;
            s << "On " << term_names[term_idx];
            if (entry.kind == parse_table_entry_kind::success)
                s << " success \n";
            else if (entry.kind == parse_table_entry_kind::shift)
                s << " shift to " << entry.shift << "\n";
            else if (entry.kind == parse_table_entry_kind::reduce && entry.has_shift)
                s << " shift to " << entry.shift << " S/R CONFLICT, prefer reduce(" << rule_infos[entry.reduce].r_idx << ") over shift\n";
            else if (entry.kind == parse_table_entry_kind::shift && entry.has_reduce)
                s << " shift to " << entry.shift << " S/R CONFLICT, prefer shift over reduce(" << rule_infos[entry.reduce].r_idx << ")\n";
            else if (entry.kind == parse_table_entry_kind::reduce)
                s << " reduce using (" << rule_infos[entry.reduce].r_idx << ")\n";
            else if (entry.kind == parse_table_entry_kind::rr_conflict)
                s << " R/R CONFLICT - !!! FIX IT !!! \n";
        }
    }

    template<typename Stream>
    constexpr void write_diag_str(Stream& s) const
    {
        s << "PARSER" << "\n" << "\n";

        s << "Parser Object size: " << sizeof(*this) << "\n\n";
        for (size16_t i = 0; i < state_count; ++i)
        {
            write_state_diag_str(s, i);
            s << "\n";
        }

        s << "\n" << "LEXICAL ANALYZER" << "\n" << "\n";
        if (valid_lexer)
        {
            if (trivial_lexical_analyzer)
                s << "Trivial lookup" << "\n";
            else
                lexer_sm.write_diag_str(s, term_names);
        }
        else
            s << lexer_error_stream.str();
    }
    
    template<typename F, typename LValueType, typename... RValueType, size_t... I>
    constexpr static LValueType reduce_value_impl(const F& f, value_variant_type* start, std::index_sequence<I...>, context_type& context)
    {
        if constexpr (std::is_same_v<context_type, no_context>)
        {
            if constexpr (std::is_same_v<F, std::nullptr_t>)
                return LValueType(std::get<RValueType>(std::move(*(start + I)))...);
            else
                return LValueType(f(std::get<RValueType>(std::move(*(start + I)))...));
        }
        else
        {
            if constexpr (std::is_same_v<F, std::nullptr_t>)
                return LValueType(context, std::get<RValueType>(std::move(*(start + I)))...);
            else
                return LValueType(f(context, std::get<RValueType>(std::move(*(start + I)))...));
        }
    }

    template<size_t RuleIdx, typename F, typename LValueType, typename... RValueType>
    constexpr static value_variant_type reduce_value(const rule_tuple_type& rules, value_variant_type* start, context_type& context)
    {
        return value_variant_type(
            reduce_value_impl<F, LValueType, RValueType...>(std::get<RuleIdx>(rules).f, start, std::index_sequence_for<RValueType...>{}, context)
        );
    }

    template<typename ParserState>
    constexpr void shift(ParserState& ps, const str_view& sv, size16_t term_idx, size16_t new_cursor_value) const
    {
        if (ps.options.verbose)
            ps.error_stream << "Shift to " << new_cursor_value << ", term: " << sv << "\n";
        ps.cursor_stack.push_back(new_cursor_value);
        ps.value_stack.emplace_back(sv);
    }

    template<typename ParserState>
    constexpr void reduce(ParserState& ps, size16_t rule_info_idx) const
    {
        const auto& ri = rule_infos[rule_info_idx];
        if (ps.options.verbose)
        {
            ps.error_stream << "Reduced using rule " << ri.r_idx << "  ";
            write_rule_diag_str(ps.error_stream, rule_info_idx);
            ps.error_stream << "\n";
        }

        ps.cursor_stack.erase(ps.cursor_stack.end() - ri.r_elements, ps.cursor_stack.end());
        size16_t new_cursor_value = parse_table[ps.cursor_stack.back()][ri.l_idx].shift;
        
        if (ps.options.verbose)
        {
            ps.error_stream << "Go to " << new_cursor_value << "\n";
        }

        ps.cursor_stack.push_back(new_cursor_value);
        value_variant_type* start = ps.value_stack.data() + ps.value_stack.size() - ri.r_elements;
        value_variant_type lvalue(value_reductors[ri.r_idx](rule_tuple, start, ps.context));
        ps.value_stack.erase(ps.value_stack.end() - ri.r_elements, ps.value_stack.end());
        ps.value_stack.emplace_back(std::move(lvalue));
    }

    template<typename ParserState>
    constexpr void rr_conflict(ParserState& ps, size16_t rule_idx) const
    {
        if (ps.options.verbose)
        {
            ps.error_stream << "R/R conflict encountered \n";
        }
        reduce(ps, rule_idx);
    }


    template<typename ParserState>
    constexpr void syntax_error(ParserState& ps, size16_t term_idx) const
    {
        cstream<100> str;
        str << "Unexpected '" << term_names[term_idx] << "'";
        ps.error_stream << "Syntax error: " << str.str() << "\n";
    }

    template<typename ParserState>
    constexpr root_value_type& success(ParserState& ps) const
    {
        if (ps.options.verbose)
        {
            ps.error_stream << "Success \n";
        }
        return std::get<root_value_type>(ps.value_stack.front());
    }
    
    template<typename Iterator>
    constexpr auto get_next_term_trivial(const Iterator& start, const Iterator& end) const
    {
        return recognized_term<Iterator>{ start + 1, trivial_term_table[char_to_idx(*start)][0] };
    }

    template<typename ParserState, typename Iterator>
    constexpr auto get_next_term_dfa(ParserState& ps, const Iterator& start, const Iterator& end) const
    {
        return lexer_sm.recognize(start, end, ps);
    }

    template<typename ParserState, typename Iterator>
    constexpr auto get_next_term(ParserState& ps, const Iterator& start, const Iterator& end) const
    {
        if (start == end)
        {
            return recognized_term<Iterator>{ end, eof_idx };
        }

        if (trivial_lexical_analyzer)
            return get_next_term_trivial(start, end);
        else
            return get_next_term_dfa(ps, start, end);
    }

    template<typename ParserState>
    constexpr void unexpected_char(ParserState& ps, char c) const
    {
        ps.error_stream << "Unexpected character: " << c << "\n";
    }

    template<typename ParserState>
    constexpr void trace_recognized_term(ParserState& ps, size16_t term_idx) const
    {
        if (ps.options.verbose)
            ps.error_stream << "Recognized " << term_names[term_idx] << " \n";
    }

    template<typename Buffer, typename = std::enable_if_t<std::is_same_v<context_type, no_context>>>
    constexpr std::optional<root_value_type> parse(const Buffer& buffer) const
    {
        no_context c;
        no_stream error_stream;
        return parse_in_context(parse_options{}, buffer, c, error_stream);
    }

    template<typename Buffer, typename ErrorStream, typename = std::enable_if_t<std::is_same_v<context_type, no_context>>>
    constexpr std::optional<root_value_type> parse(const Buffer& buffer, ErrorStream& error_stream) const
    {
        no_context c;
        return parse_in_context(parse_options{}, buffer, c, error_stream);
    }

    template<typename Buffer, typename ErrorStream, typename = std::enable_if_t<std::is_same_v<context_type, no_context>>>
    constexpr std::optional<root_value_type> parse(parse_options options, const Buffer& buffer, ErrorStream& error_stream) const
    {
        no_context c;
        return parse_in_context(options, buffer, c, error_stream);
    }

    template<typename Buffer>
    constexpr std::optional<root_value_type> parse_in_context(const Buffer& buffer, context_type& context) const
    {
        no_stream error_stream;
        return parse_in_context(parse_options{}, buffer, context, error_stream);
    }

    template<typename Buffer>
    constexpr std::optional<root_value_type> parse_in_context(parse_options options, const Buffer& buffer, context_type& context) const
    {
        no_stream error_stream;
        return parse_in_context(options, buffer, context, error_stream);
    }

    template<typename Buffer, typename ErrorStream>
    constexpr std::optional<root_value_type> parse_in_context(parse_options options, const Buffer& buffer, context_type& context, ErrorStream& error_stream) const
    {
        parser_value_stack_type_t<Buffer, empty_rules_count, value_variant_type> value_stack{};
        parse_table_cursor_stack_type_t<Buffer, empty_rules_count> cursor_stack{};
        
        parser_state ps(cursor_stack, value_stack, context, error_stream, options);

        bool error = false;
        iterator_t<Buffer> it = buffer.begin();
        iterator_t<Buffer> term_end = it;
        size16_t term_idx = uninitialized16;
        
        ps.cursor_stack.push_back(0);
        
        std::optional<root_value_type> root_value;

        while (true)
        {
            size16_t cursor = ps.cursor_stack.back();
            if (it == term_end)
            {
                auto res = get_next_term(ps, it, buffer.end());
                term_end = res.it;
                term_idx = res.term_idx;
                
                if (term_idx == uninitialized16)
                {
                    unexpected_char(ps, *it);
                    break;
                }
                else
                {
                    trace_recognized_term(ps, term_idx);
                }
            }
            
            const auto& entry = parse_table[cursor][get_parse_table_idx(true, term_idx)];
            if (entry.kind == parse_table_entry_kind::shift)
            {
                shift(ps, buffer.view(it, term_end), term_idx, entry.shift);
                it = term_end;
            }
            else if (entry.kind == parse_table_entry_kind::reduce)
                reduce(ps, entry.reduce);
            else if (entry.kind == parse_table_entry_kind::rr_conflict)
                rr_conflict(ps, entry.reduce);
            else if (entry.kind == parse_table_entry_kind::success)
            {
                root_value = std::optional(std::move(success(ps)));
                break;
            }
            else
            {
                syntax_error(ps, term_idx);
                break;
            }
        }

        return root_value;
    }

    str_table<term_count> term_names = { };
    str_table<term_count> term_ids = { };
    str_table<nterm_count> nterm_names = { };
    symbol right_sides[rule_count][max_rule_element_count] = { };
    rule_info rule_infos[rule_count] = { };
    slice nterm_rule_slices[nterm_count] = { };
    term_subset situation_first_after[situation_address_space_size] = { };
    state situation_first_after_analyzed = {};
    term_subset right_side_slice_first[situation_size * rule_count] = {};
    right_side_slice_subset right_side_slice_first_analyzed = {};
    nterm_subset nterm_empty = { };
    term_subset nterm_first[nterm_count] = { };
    nterm_subset nterm_empty_analyzed = { };
    nterm_subset nterm_first_analyzed = { };
    state states[max_states] = { };
    parse_table_entry parse_table[max_states][term_count + nterm_count] = {};
    size16_t state_count = 0;
    situation_queue_entry situation_queue[max_states * rule_count + max_states] = { };
    size32_t situation_queue_size = 0;
    int term_precedences[term_count] = { };
    associativity term_associativities[term_count] = { associativity::ltor };
    int rule_precedences[rule_count] = { };
    size16_t rule_last_terms[rule_count] = { };
    conflicted_terms trivial_term_table[distinct_values_count<char>] = { };
    bool trivial_lexical_analyzer = true;
    term_tuple_type term_tuple;
    nterm_tuple_type nterm_tuple;
    rule_tuple_type rule_tuple;
    using value_reductor = value_variant_type(*)(const rule_tuple_type&, value_variant_type*, context_type&);
    value_reductor value_reductors[rule_count] = {};
    using dfa_type = dfa<total_regex_size * 2>;
    dfa_type lexer_sm = {};
    bool valid_lexer = true;
    cstream<20000> lexer_error_stream;
};

template<size_t S>
struct use_max_states
{
    template<size_t, size_t...>
    static const size_t value = S;
};

struct deduce_max_states
{
    template<size_t TermCount, size_t... RuleSizes>
    static const size_t value = ((0 + ... + (RuleSizes + 1)) + 2) * (TermCount + 1);
};

template<typename Root, typename Terms, typename NTerms, typename Rules>
parser(Root, Terms, NTerms, Rules&&) -> parser<Root, Terms, NTerms, Rules, no_context, deduce_max_states>;

template<typename Root, typename Terms, typename NTerms, typename Rules, typename ContextUsage>
parser(Root, Terms, NTerms, Rules&&, ContextUsage) -> parser<Root, Terms, NTerms, Rules, ContextUsage, deduce_max_states>;

template<typename Root, typename Terms, typename NTerms, typename Rules, typename ContextUsage, typename MaxStatesUsage>
parser(Root, Terms, NTerms, Rules&&, ContextUsage, MaxStatesUsage) -> parser<Root, Terms, NTerms, Rules, ContextUsage, MaxStatesUsage>;

template<typename... Terms>
constexpr auto terms(const Terms&... terms)
{
    return std::make_tuple(term(terms)...);
}

template<typename... NTerms>
constexpr auto nterms(NTerms... nterms)
{
    return std::make_tuple(nterms...);
}

template<typename... Rules>
constexpr auto rules(Rules&&... rules)
{
    return std::make_tuple(std::move(rules)...);
}

template<size_t S>
struct use_const_message
{
    using type = cstream<S>;
};

struct use_string_stream
{
    using type = std::stringstream;
};

template<typename StreamType>
struct diag_msg
{
    template<typename Parser, typename StreamUsage>
    constexpr diag_msg(const Parser& p, StreamUsage)
    {
        p.write_diag_str(stream);
    }
        
    constexpr const StreamType& get_stream() const
    {
        return stream;
    }

    StreamType stream;
};

template<typename Parser, typename MessageStreamUsage>
diag_msg(const Parser&, MessageStreamUsage)->diag_msg<typename MessageStreamUsage::type>;

template<typename DFAType>
constexpr auto create_regex_parser(DFAType& sm)
{
    using dfa_type = DFAType;
    
    constexpr term regular_char(exclude("\\[]^-.*?|()"), "regular", 0, associativity::ltor);
    constexpr nterm<slice> expr("expr");
    constexpr nterm<slice> alt("alt");
    constexpr nterm<slice> concat("concat");
    constexpr nterm<slice> q_expr("q_expr");
    constexpr nterm<slice> primary("primary");
    constexpr nterm<char_subset> c_subset("c_subset");
    constexpr nterm<char_range> c_subset_item("c_subset_item");
    constexpr nterm<char_range> c_range("c_range");
    constexpr nterm<char> single_char("single_char");    

    return parser(
        expr,
        terms(regular_char, '\\', '[', ']', '^', '-', '.', '*', '?', '|', '(', ')'),
        nterms(expr, alt, concat, q_expr, primary, c_range, c_subset, c_subset_item, single_char),
        rules(
            single_char(regular_char) >= [](str_view s) { return s[0]; },
            single_char('\\', '\\') >= [](skip, str_view s) { return s[0]; },
            single_char('\\', '[') >= [](skip, str_view s) { return s[0]; },
            single_char('\\', ']') >= [](skip, str_view s) { return s[0]; },
            single_char('\\', '^') >= [](skip, str_view s) { return s[0]; },
            single_char('\\', '-') >= [](skip, str_view s) { return s[0]; },
            single_char('\\', '.') >= [](skip, str_view s) { return s[0]; },
            single_char('\\', '|') >= [](skip, str_view s) { return s[0]; },
            single_char('\\', '(') >= [](skip, str_view s) { return s[0]; },
            single_char('\\', ')') >= [](skip, str_view s) { return s[0]; },
            c_range(single_char, '-', single_char) >= [](char c1, skip, char c2){ return char_range{c1, c2}; },
            c_subset_item(single_char) >= [](char c) { return char_range{ c, c}; },
            c_subset_item(c_range) >= [](char_range r){ return r; },
            c_subset(c_subset_item) >= [](char_range r){ return dfa_type::char_subset_from_item(r); },
            c_subset(c_subset_item, c_subset) >= [](char_range r, char_subset&& s){ return dfa_type::add_item_to_char_subset(std::move(s), r); },
            primary(single_char) >= [&sm](char c) { return sm.add_primary_single_char(c); },
            primary('.') >= [&sm](skip) { return sm.add_primary_any_char(); },
            primary('[', c_subset, ']') >= [&sm](skip, char_subset&& s, skip) { return sm.add_primary_char_subset(s); },
            primary('[', '^', c_subset, ']') >= [&sm](skip, skip, char_subset&& s, skip) { return sm.add_primary_char_subset_exclusive(s); },
            primary('(', expr, ')') >= [](skip, slice p, skip) { return p; },
            q_expr(primary) >= [](slice p) { return p; },
            q_expr(primary, '*') >= [&sm](slice p, skip) { return sm.add_multiplication(p); },
            q_expr(primary, '?') >= [&sm](slice p, skip) { return sm.add_optional(p); },
            concat(q_expr) >= [](slice p) { return p; },
            concat(q_expr, concat) >= [&sm](slice p1, slice p2) { return sm.add_concat(p1, p2); },
            alt(concat) >= [](slice p) { return p; },
            alt(alt, '|', alt) >= [&sm](slice p1, skip, slice p2) { return sm.add_alt(p1, p2); },
            expr(alt) >= [](slice p) { return p; }
        ),
        no_context{},
        deduce_max_states{}
    );
}

template<typename DFA, size_t SSize, size_t... N, size_t... I>
constexpr bool create_lexer_impl(DFA& sm, cstream<SSize>& error_stream, std::index_sequence<I...>, const std::tuple<term<N>...>& term_tuple)
{
    using buffer_type = cstring_buffer<max_v<N...>>;
    
    auto p = create_regex_parser(sm);
    auto single_f = [&p, &sm, &error_stream](const auto& t, size_t idx)
    { 
        slice prev{0, size32_t(sm.size())};

        if (t.is_trivial())
        {
            slice new_sl = sm.add_primary_single_char(t.data[0]);
            sm.mark_end_states(new_sl, idx);
            sm.add_alt(prev, new_sl);
            return true;
        }
        else
        {
            cstream<SSize> stream;
            std::optional<slice> res = p.parse(buffer_type(t.data), stream);
                
            if (res.has_value())
            {
                sm.mark_end_states(res.value(), idx);
                sm.add_alt(prev, res.value());
                return true;
            }
            else
            {   
                error_stream << "Regex " << t.data << " parse error: \n" << stream.str();
                return false;
            }
        }
    };

    return (true && ... && single_f(std::get<I>(term_tuple), I));
}

template<typename DFA, size_t SSize, size_t... N>
constexpr bool create_lexer(DFA& sm, cstream<SSize>& error_stream, const std::tuple<term<N>...>& ts)
{
    return create_lexer_impl(sm, error_stream, std::make_index_sequence<sizeof...(N)>{}, ts);
}

template<typename ValueType, typename ContextType, typename ErrorStreamType>
struct parse_result
{
    ErrorStreamType error_stream;
    std::optional<ValueType> value;
    ContextType context;
    parse_options options;

    template<typename Parser, typename Buffer, typename ErrorStreamUsage>
    constexpr parse_result(const Parser& p, parse_options options, const Buffer& buffer, ErrorStreamUsage)
    {
        value = p.parse_in_context(options, buffer, context, error_stream);
    }

    template<typename Parser, typename Buffer, typename ErrorStreamUsage>
    constexpr parse_result(const Parser& p, const Buffer& buffer, ErrorStreamUsage)
    {
        value = p.parse_in_context(parse_options{}, buffer, context, error_stream);
    }
    
    template<typename Parser, typename Buffer>
    constexpr parse_result(const Parser& p, parse_options options, const Buffer& buffer)
    {
        value = p.parse_in_context(options, buffer, context, error_stream);
    }

    template<typename Parser, typename Buffer>
    constexpr parse_result(const Parser& p, const Buffer& buffer)
    {
        value = p.parse_in_context(parse_options{}, buffer, context, error_stream);
    }

    constexpr const auto& get_error_stream() const { return error_stream; }
    constexpr const auto& get_value() const { return value.value(); }
    constexpr bool get_success() const { return value.has_value(); }
    constexpr const auto& get_context() const { return context; }
};

template<typename Parser, typename Buffer, typename ErrorStreamUsage>
parse_result(const Parser&, parse_options, const Buffer&, ErrorStreamUsage)
->parse_result<typename Parser::root_value_type, typename Parser::context_type, typename ErrorStreamUsage::type>;

template<typename Parser, typename Buffer, typename ErrorStreamUsage>
parse_result(const Parser&, const Buffer&, ErrorStreamUsage)
->parse_result<typename Parser::root_value_type, typename Parser::context_type, typename ErrorStreamUsage::type>;

template<typename Parser, typename Buffer>
parse_result(const Parser&, parse_options, const Buffer&)
->parse_result<typename Parser::root_value_type, typename Parser::context_type, no_stream>;

template<typename Parser, typename Buffer>
parse_result(const Parser&, const Buffer&)
->parse_result<typename Parser::root_value_type, typename Parser::context_type, no_stream>;

