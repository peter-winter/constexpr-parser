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
#include <array>

using size_t = std::uint64_t;
using size8_t = std::uint8_t;
using size16_t = std::uint16_t;
using size32_t = std::uint32_t;

constexpr size_t uninitialized = size_t(-1);
constexpr size16_t uninitialized16 = size16_t(-1);
constexpr size32_t uninitialized32 = size32_t(-1);

struct slice
{
    size_t start;
    size_t n;
};

template<typename T>
struct is_cvector_compatible : std::bool_constant<std::is_default_constructible_v<T>&& std::is_trivially_destructible_v<T>>
{};

template<typename T, size_t N, typename = void>
struct cvector
{};

template<typename T, size_t N>
struct cvector<T, N, std::enable_if_t<is_cvector_compatible<T>::value>>
{
    using size_type = size_t;
    
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
    constexpr iterator erase(iterator first, iterator last)
    {
        if (!(first < last))
            return end();
        auto from = first < begin() ? begin() : first;
        auto to = last > end() ? end() : last;
        current_size -= (to - from);
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

template<typename T, size_t N1, size_t N2, size_t... I>
constexpr void copy_array(T (&a1)[N1], const T (&a2)[N2], std::index_sequence<I...>)
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

        constexpr char operator *() { return *ptr; }
        constexpr iterator& operator ++() { ++ptr; return *this; }
        constexpr iterator operator ++(int) { iterator i(*this); ++ptr; return i; }
        constexpr bool operator == (const iterator& other) { return ptr == other.ptr; }
        constexpr iterator& operator += (size_t size) { ptr += size; return *this; }
    };

    constexpr iterator begin() const { return iterator{ data }; }
    constexpr iterator end() const { return iterator{ data + N - 1 }; }
    constexpr str_view view(const iterator& it, size_t size) const { return str_view(it.ptr, size); }

    char data[N] = { 0 };
};

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

template<size_t N>
cstring_buffer(const char(&)[N])->cstring_buffer<N>;

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
using name_table = const char* [N];

template<typename T>
constexpr size_t distinct_values_count = 1 << (sizeof(T) * 8);

constexpr size_t char_to_idx(char c)
{
    return size_t(int(c) - std::numeric_limits<char>::min());
}

constexpr char index_to_char(size_t idx)
{
    return char(int(idx) + std::numeric_limits<char>::min());
}

template<typename Buffer>
struct parse_table_cursor_stack_type
{
    using type = std::vector<size_t>;
};

template<size_t N>
struct parse_table_cursor_stack_type<cstring_buffer<N>>
{
    using type = cvector<size_t, N * 2>;    // TODO take empty rules into account
};

template<typename Buffer>
using parse_table_cursor_stack_type_t = typename parse_table_cursor_stack_type<Buffer>::type;

template<typename Buffer, typename ValueVariantType, typename = void>
struct parser_value_stack_type
{};

template<size_t N, typename ValueVariantType>
struct parser_value_stack_type<cstring_buffer<N>, ValueVariantType, std::enable_if_t<!is_cvector_compatible<ValueVariantType>::value>>
{
    using type = std::vector<ValueVariantType>;
};

template<size_t N, typename ValueVariantType>
struct parser_value_stack_type<cstring_buffer<N>, ValueVariantType, std::enable_if_t<is_cvector_compatible<ValueVariantType>::value>>
{
    using type = cvector<ValueVariantType, N * 2>;  // TODO take empty rules into account
};

template<typename Buffer, typename ValueVariantType>
using parser_value_stack_type_t = typename parser_value_stack_type<Buffer, ValueVariantType>::type;

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
constexpr size_t find_name(const name_table<N>& table, const char* name)
{
    size_t res = 0;
    for (const auto& n : table)
    {
        if (str_view::equal(n, name))
            return res;
        res++;
    }
    cstream<100> s;
    s << "name not found: " << name;
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

    template<typename... Args>
    constexpr auto operator()(Args&&... args) const;

    constexpr static const char* get_name() { return "##"; };
};

template<typename L, typename...R>
struct move_construct
{
    L operator()(R&&... r) const
    {
        return L(std::move(r)...);
    }
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

template<size_t N>
struct term
{
    using value_type = term_value_type;

    template<size_t N1>
    constexpr term(const char (&source)[N1], int precedence = 0, associativity a = associativity::ltor, const char* name = nullptr) :
        precedence(precedence), ass(a), name(name ? name : source), 
        method(N == 2 && source[1] == 0 ? term_method::exact : term_method::regex)
    {
        copy_array(data, source, std::make_index_sequence<N1>{});
        check();
    }

    template<size_t N1>
    constexpr term(exclude<N1> ex, int precedence = 0, associativity a = associativity::ltor, const char* name = nullptr) :
        precedence(precedence), ass(a), name(name ? name : ex.data), 
        method(term_method::exclude)
    {
        copy_array(data, ex.data, std::make_index_sequence<N1>{});
        check();
    }

    template<size_t N1>
    constexpr term(include<N1> in, int precedence = 0, associativity a = associativity::ltor, const char* name = nullptr) :
        precedence(precedence), ass(a), name(name ? name : in.data), 
        method(term_method::include)
    {
        copy_array(data, in.data, std::make_index_sequence<N1>{});
        check();
    }

    constexpr void check()
    {
        if (data[0] == 0)
            throw std::runtime_error("empty string not allowed");
        if (name[0] == 0)
            throw std::runtime_error("empty name not allowed");
    }
    
    constexpr const char* get_name() const { return name; }

    constexpr bool is_trivial() const 
    { 
        return method != term_method::regex;
    }

    char data[N] = {};
    int precedence;
    associativity ass;
    const char* name;
    term_method method;
};

template<size_t N>
term(const char (&)[N], int = 0, associativity = associativity::ltor, const char* = nullptr) -> term<N>;

template<size_t N>
term(exclude<N>, int = 0, associativity = associativity::ltor, const char* = nullptr) -> term<N>;

template<size_t N>
term(include<N>, int = 0, associativity = associativity::ltor, const char* = nullptr) -> term<N>;

struct eof
{
    constexpr static const char* get_name() { return "<eof>"; }
};

template<typename T>
using value_type_t = typename T::value_type;

template<typename T>
struct symbol_type
{
    using type = T;
};

template<size_t N>
struct symbol_type<char [N]>
{
    using type = term<N>;
};

template<typename T>
using symbol_type_t = typename symbol_type<std::remove_cv_t<std::remove_reference_t<T>>>::type;

template<typename ValueType>
template<typename... Args>
constexpr auto nterm<ValueType>::operator()(Args&&... args) const
{
    using f_type = move_construct<ValueType, value_type_t<symbol_type_t<Args>>...>;
    return rule<f_type, nterm<ValueType>, symbol_type_t<Args>...>(
        f_type{}, 
        *this, 
        std::make_tuple(symbol_type_t<Args>(args)...)
    );
}

template<typename ValueType>
template<typename... Args>
constexpr auto fake_root<ValueType>::operator()(Args&&... args) const
{
    using f_type = move_construct<ValueType, ValueType>;
    return rule<f_type, fake_root<ValueType>, symbol_type_t<Args>...>(
        f_type{}, 
        *this, 
        std::make_tuple(symbol_type_t<Args>(args)...)
    );
}

struct dfa_state
{
    constexpr dfa_state()
    {
        for (auto& t : transitions)
            t = uninitialized;
    }

    bool start_state = false;
    bool end_state = false;
    size_t recognized_idx = uninitialized;
    size_t transitions[distinct_values_count<char>] = {};
};

struct char_range
{
    char start;
    char end;
};

template<size_t MaxStates, size_t MaxListLength>
struct dfa
{
    using dfa_state_container_type = cvector<dfa_state, MaxStates>;
    using char_list_type = cvector<char, MaxListLength>;
    using char_range_list_type = cvector<char_range, MaxListLength>;

    constexpr slice add_primary_single_char(char single)
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_primary_char_list(const char_list_type& l)
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_primary_char_list_exclusive(const char_list_type& l)
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_primary_char_ranges(const char_range_list_type& r)
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_primary_char_ranges_exclusive(const char_range_list_type& r)
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_primary_any_char()
    {
        size_t old_size = states.size();
        //current_size += 2;
        return slice{ old_size, 2 };
    }

    constexpr slice add_multiplication(slice p)
    {
        size_t old_size = states.size();
        return p;
    }

    constexpr slice add_concat(slice p1, slice p2)
    {
        if (p2.n != 0)
        {

        }
        return slice{ p1.start, p1.n + p2.n };
    }

    constexpr slice add_alt(slice p1, slice p2)
    {
        return slice{ p1.start, p1.n + p2.n };
    }

    constexpr void mark_end_states(slice s, size_t idx)
    {

    }

    template<size_t N>
    constexpr size_t match(const char(&r)[N]) const
    {
        return uninitialized;
    }

    constexpr size_t size() const { return states.size(); }

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
constexpr bool create_lexer_impl(DFA& sm, cstream<SSize>& error_stream, bool trace, std::index_sequence<I...>, std::tuple<term<N>...> ts);

template<
    typename RootValueType,
    size_t TermCount, size_t NTermCount, size_t RuleCount,
    size_t MaxRuleElementCount,
    typename ValueVariantType, typename FunctorTupleType,
    size_t TotalRegexSize,
    typename ContextType,
    size_t MaxStates
>
struct parser
{
    static const size_t max_rule_element_count = MaxRuleElementCount;
    static const size_t term_count = TermCount + 1;
    static const size_t eof_idx = TermCount;
    static const size_t nterm_count = NTermCount + 1;
    static const size_t fake_root_idx = NTermCount;
    static const size_t rule_count = RuleCount + 1;
    static const size_t situation_size = max_rule_element_count + 1;
    static const size_t situation_count = rule_count * situation_size * term_count;
    static const size_t root_rule_idx = RuleCount;
    static const size_t total_regex_size = TotalRegexSize;

    using functor_tuple_type = FunctorTupleType;
    using value_variant_type = ValueVariantType;
    using term_subset = bool[term_count];
    using state = bool[situation_count];
    using root_value_type = RootValueType;
    using context_type = ContextType;

    struct symbol
    {
        constexpr symbol() :
            term(false), idx(uninitialized)
        {}

        constexpr symbol(bool term, size_t idx) :
            term(term), idx(idx)
        {}

        constexpr size_t get_parse_table_idx() const
        {
            return parser::get_parse_table_idx(term, idx);
        }

        bool term;
        size_t idx;
    };

    struct situation_address
    {
        size_t rule_info_idx;
        size_t after;
        size_t t;
    };

    struct rule_info
    {
        size_t l_idx;
        size_t r_idx;
        size_t r_elements;
    };

    struct situation_queue_entry
    {
        size_t state_idx;
        size_t idx;
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

    static const size_t value_stack_initial_capacity = 1 << 10;
    static const size_t cursor_stack_initial_capacity = 1 << 10;

    template<typename CursorStack, typename ValueStack, typename ErrorStream, typename TraceStream>
    struct parser_state
    {
        constexpr parser_state(
            CursorStack& cursor_stack,
            ValueStack& value_stack,
            ErrorStream& error_stream,
            TraceStream& trace_stream,
            context_type& context) :
            cursor_stack(cursor_stack),
            value_stack(value_stack),
            error_stream(error_stream),
            trace_stream(trace_stream),
            context(context)
        {
            cursor_stack.reserve(cursor_stack_initial_capacity);
            value_stack.reserve(value_stack_initial_capacity);
        }

        CursorStack& cursor_stack;
        ValueStack& value_stack;
        ErrorStream& error_stream;
        TraceStream& trace_stream;
        context_type& context;
    };

    constexpr static size_t get_parse_table_idx(bool term, size_t idx)
    {
        return term ? nterm_count + idx : idx;
    }

    template<typename Root, typename Terms, typename NTerms, typename Rules, typename ContextUsage, typename MaxStateUsage>
    constexpr parser(
        Root root,
        Terms terms,
        NTerms nterms,
        Rules rules,
        ContextUsage,
        MaxStateUsage) :
        parser(root, terms, nterms, std::move(rules))
    {}

    template<typename Root, typename... Terms, typename... NTerms, typename... Rules>
    constexpr parser(
        Root root,
        std::tuple<Terms...> terms,
        std::tuple<NTerms...> nterms,
        std::tuple<Rules...>&& rules) :
        functors(make_functors_tuple(std::index_sequence_for<Rules...>{}, std::move(rules)))        
    {
        for (size_t& x : trivial_term_table)
            x = uninitialized;

        constexpr auto seq_for_terms = std::index_sequence_for<Terms...>{};
        analyze_nterms(std::index_sequence_for<NTerms...>{}, nterms);
        analyze_nterm(fake_root<RootValueType>{});
        analyze_terms(seq_for_terms, terms);
        analyze_eof(eof{});
        analyze_rules(std::index_sequence_for<Rules...>{}, root, std::move(rules));
        analyze_states();
        
        if (!trivial_lexical_analyzer)
            create_lexer_impl(lexer_sm, lexer_error_stream, true, seq_for_terms, terms);
    }

    template<typename... Rules, size_t... I>
    constexpr auto make_functors_tuple(std::index_sequence<I...>, std::tuple<Rules...>&& rules)
    {
        return std::make_tuple(std::move(std::get<I>(rules).f)...);
    }
    
    constexpr void analyze_eof(eof)
    {
        term_names[eof_idx] = eof::get_name();
        term_precedences[eof_idx] = 0;
        term_associativities[eof_idx] = associativity::ltor;
    }

    template<size_t N>
    constexpr void analyze_trivial_term(term<N> t, size_t idx)
    {
        if (t.method == term_method::exact)
            trivial_term_table[char_to_idx(t.data[0])] = idx;
        else if (t.method == term_method::exclude)
        {
            for (size_t i = 0; i < std::size(trivial_term_table); ++i)
            {
                size_t idx_found = find_char(index_to_char(i), t.data);
                if (idx_found == uninitialized)
                    trivial_term_table[i] = idx;
            }
        }
        else if (t.method == term_method::include)
        {
            const char* data = t.data;
            while (*data)
            {
                trivial_term_table[*data] = idx;
                ++data;
            }                
        }
    }

    template<size_t N>
    constexpr void analyze_term(term<N> t, size_t idx)
    {
        const char* name = t.name;
        int precedence = t.precedence;
        associativity ass = t.ass;

        term_names[idx] = name;
        term_precedences[idx] = precedence;
        term_associativities[idx] = ass;

        if (trivial_lexical_analyzer && t.is_trivial())
            analyze_trivial_term(t, idx);
        else
            trivial_lexical_analyzer = false;
    }

    template<typename ValueType>
    constexpr void analyze_nterm(nterm<ValueType> nt, size_t idx)
    {
        nterm_names[idx] = nt.name;
    }

    template<typename ValueType>
    constexpr void analyze_nterm(fake_root<ValueType>)
    {
        nterm_names[fake_root_idx] = fake_root<ValueType>::get_name();
    }

    template<size_t N>
    constexpr auto make_symbol(term<N> t) const
    {
        return symbol{ true, find_name(term_names, t.name) };
    }

    template<typename ValueType>
    constexpr auto make_symbol(nterm<ValueType> nt) const
    {
        return symbol{ false, find_name(nterm_names, nt.name) };
    }
    
    template<typename... Terms, size_t... I>
    constexpr void analyze_terms(
        std::index_sequence<I...>,
        std::tuple<Terms...> terms)
    {
        (void(analyze_term(std::get<I>(terms), I)), ...);
    }

    template<typename... NTerms, size_t... I>
    constexpr void analyze_nterms(
        std::index_sequence<I...>,
        std::tuple<NTerms...> nterms)
    {
        (void(analyze_nterm(std::get<I>(nterms), I)), ...);
    }

    template<typename... Rules, size_t... I>
    constexpr void analyze_rules(
        std::index_sequence<I...>,
        nterm<RootValueType> root,
        std::tuple<Rules...>&& rules)
    {
        (void(analyze_rule<I>(std::get<I>(std::move(rules)), std::make_index_sequence<Rules::n>{})), ...);
        analyze_rule<root_rule_idx>(fake_root<RootValueType>{}(root), std::index_sequence<0>{});
        sort(rule_infos, [](const auto& ri1, const auto& ri2) { return ri1.l_idx < ri2.l_idx; });
        make_nterm_rule_slices();
    }

    constexpr void make_nterm_rule_slices()
    {
        size_t nt = 0;
        for (size_t i = 0u; i < rule_count; ++i)
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

    constexpr size_t calculate_rule_last_term(size_t rule_idx, size_t rule_size) const
    {
        for (int i = int(rule_size - 1); i >= 0; --i)
        {
            const auto& s = right_sides[rule_idx][i];
            if (!s.term)
                continue;
            return s.idx;
        }
        return uninitialized;
    }

    constexpr int calculate_rule_precedence(int precedence, size_t rule_idx, size_t rule_size) const
    {
        if (precedence != 0)
            return precedence;
        size_t last_term_idx = rule_last_terms[rule_idx];
        if (last_term_idx != uninitialized)
            return term_precedences[last_term_idx];
        return 0;
    }

    template<size_t Nr, typename F, typename L, typename... R, size_t... I>
    constexpr void analyze_rule(rule<F, L, R...>&& r, std::index_sequence<I...>)
    {
        size_t l_idx = find_name(nterm_names, r.l.get_name());
        (void(right_sides[Nr][I] = make_symbol(std::get<I>(r.r))), ...);
        constexpr size_t rule_elements_count = sizeof...(R);
        rule_infos[Nr] = { l_idx, Nr, rule_elements_count };
        rule_last_terms[Nr] = calculate_rule_last_term(Nr, rule_elements_count);
        rule_precedences[Nr] = calculate_rule_precedence(r.precedence, Nr, rule_elements_count);
        if constexpr (Nr != root_rule_idx)
        {
            value_reductors[Nr] = &reduce_value<Nr, F, value_type_t<L>, value_type_t<R>...>;
        }
    }

    constexpr size_t make_situation_idx(situation_address a) const
    {
        return a.rule_info_idx * situation_size * term_count + a.after * term_count + a.t;
    }

    constexpr situation_address make_situation_address(size_t idx) const
    {
        size_t t = idx % term_count;
        idx /= term_count;
        size_t after = idx % situation_size;
        size_t rule_info_idx = idx / situation_size;
        return situation_address{ rule_info_idx, after, t };
    }

    constexpr void add_term_subset(term_subset& dest, const term_subset& source)
    {
        for (size_t i = 0u; i < term_count; ++i)
            dest[i] = dest[i] || source[i];
    }

    constexpr const term_subset& make_right_side_slice_first(const rule_info& ri, size_t start, term_subset& res)
    {
        for (size_t i = start; i < ri.r_elements; ++i)
        {
            const symbol& s = right_sides[ri.r_idx][i];
            if (s.term)
            {
                res[s.idx] = true;
                break;
            }
            add_term_subset(res, make_nterm_first(s.idx));
            if (!make_nterm_empty(s.idx))
                break;
        }
        return res;
    }

    constexpr const term_subset& make_nterm_first(size_t nt)
    {
        if (nterm_first_analyzed[nt])
            return nterm_first[nt];
        nterm_first_analyzed[nt] = true;

        const slice& s = nterm_rule_slices[nt];
        for (size_t i = 0u; i < s.n; ++i)
        {
            const rule_info& ri = rule_infos[s.start + i];
            make_right_side_slice_first(ri, 0, nterm_first[nt]);
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

    constexpr bool make_nterm_empty(size_t nt)
    {
        if (nterm_empty_analyzed[nt])
            return nterm_empty[nt];
        nterm_empty_analyzed[nt] = true;

        const slice& s = nterm_rule_slices[nt];
        for (size_t i = 0u; i < s.n; ++i)
        {
            if (is_right_side_empty(rule_infos[s.start + i]))
            {
                return (nterm_empty[nt] = true);
            }
        }
        return (nterm_empty[nt] = false);
    }

    constexpr const term_subset& make_situation_first_after(const situation_address& addr, size_t idx)
    {
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        make_right_side_slice_first(ri, addr.after + 1, situation_first_after[idx]);
        if (is_right_side_slice_empty(ri, addr.after + 1))
            situation_first_after[idx][addr.t] = true;

        return situation_first_after[idx];
    }

    constexpr void analyze_states()
    {
        situation_address root_situation_address{ root_rule_idx, 0, eof_idx };
        size_t idx = make_situation_idx(root_situation_address);
        state_count = 1;
        add_situation_to_state(0, idx);

        size_t n = 0;
        while (n != situation_queue_size)
        {
            analyze_situation(situation_queue[n].state_idx, situation_queue[n].idx);
            n++;
        }
    }

    constexpr void analyze_situation(size_t state_idx, size_t idx)
    {
        situation_closure(state_idx, idx);
        situation_transition(state_idx, idx);
    }

    constexpr void add_situation_to_state(size_t state_idx, size_t idx)
    {
        bool& b = states[state_idx][idx];
        if (!b)
        {
            b = true;
            situation_queue[situation_queue_size++] = { state_idx, idx };
        }
    }

    constexpr void situation_closure(size_t state_idx, size_t idx)
    {
        situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        if (addr.after >= ri.r_elements)
            return;

        const symbol& s = right_sides[ri.r_idx][addr.after];
        if (!s.term)
        {
            size_t nt = s.idx;
            const term_subset& first = make_situation_first_after(addr, idx);
            const slice& s = nterm_rule_slices[nt];
            for (auto i = 0u; i < s.n; ++i)
            {
                for (size_t t = 0; t < term_count; ++t)
                {
                    if (first[t])
                    {
                        size_t new_s_idx = make_situation_idx(situation_address{ s.start + i, 0, t });
                        add_situation_to_state(state_idx, new_s_idx);
                    }
                }
            }
        }
    }

    constexpr auto solve_conflict(size_t rule_info_idx, size_t term_idx) const
    {
        size_t rule_idx = rule_infos[rule_info_idx].r_idx;
        int r_p = rule_precedences[rule_idx];
        int t_p = term_precedences[term_idx];
        if (r_p > t_p)
            return parse_table_entry_kind::reduce;

        if (r_p == t_p && term_associativities[term_idx] == associativity::ltor)
        {
            size_t last_term_idx = rule_last_terms[rule_idx];
            if (last_term_idx == term_idx)
                return parse_table_entry_kind::reduce;
        }
        return parse_table_entry_kind::shift;
    }

    constexpr void situation_transition(size_t state_idx, size_t idx)
    {
        situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        bool reduction = addr.after >= ri.r_elements;

        const auto& s = right_sides[ri.r_idx][addr.after];
        size_t symbol_idx = reduction ? get_parse_table_idx(true, addr.t) : s.get_parse_table_idx();
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

        situation_address new_addr = situation_address{ addr.rule_info_idx, addr.after + 1, addr.t };
        size_t new_idx = make_situation_idx(new_addr);

        if (entry.has_shift)
        {
            add_situation_to_state(entry.shift, new_idx);
            return;
        }

        size_t new_state_idx = uninitialized;
        for (size_t i = 0; i < state_count; ++i)
        {
            if (states[i][new_idx])
            {
                new_state_idx = i;
                break;
            }
        }
        if (new_state_idx == uninitialized)
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
        entry.set_shift(size16_t(new_state_idx));
        add_situation_to_state(new_state_idx, new_idx);
    }

    constexpr const char* get_symbol_name(const symbol& s) const
    {
        return s.term ? term_names[s.idx] : nterm_names[s.idx];
    }

    template<typename Stream>
    constexpr void write_situation_diag_str(Stream& s, size_t idx) const
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
    constexpr void write_state_diag_str(Stream& s, size_t idx) const
    {
        s << "STATE " << idx << "\n";

        for (size_t i = 0u; i < situation_count; ++i)
        {
            if (states[idx][i])
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
        s << "Parser Object size: " << sizeof(*this) << "\n\n";
        for (size_t i = 0; i < state_count; ++i)
        {
            write_state_diag_str(s, i);
            s << "\n";
        }
    }

    template<typename Stream>
    constexpr void write_unexpected_character_to_stream(Stream& stream, char c) const
    {
        stream << "Unexpected character: " << c << "\n";
    }

    template<typename F, typename LValueType, typename... RValueType, size_t... I>
    constexpr static LValueType reduce_value_impl(const F& f, value_variant_type* start, std::index_sequence<I...>, context_type& context)
    {
        if constexpr (std::is_same_v<context_type, no_context>)
            return LValueType(f(std::get<RValueType>(std::move(*(start + I)))...));
        else
            return LValueType(f(context, std::get<RValueType>(std::move(*(start + I)))...));
    }

    template<size_t RuleIdx, typename F, typename LValueType, typename... RValueType>
    constexpr static value_variant_type reduce_value(const functor_tuple_type& functors, value_variant_type* start, context_type& context)
    {
        return value_variant_type(
            reduce_value_impl<F, LValueType, RValueType...>(std::get<RuleIdx>(functors), start, std::index_sequence_for<RValueType...>{}, context)
        );
    }

    template<typename ParserState>
    constexpr void shift(ParserState& ps, const str_view& sv, size_t term_idx, size_t new_cursor_value) const
    {
        ps.trace_stream << "Shift to " << new_cursor_value << ", term: " << sv << "\n";
        ps.cursor_stack.push_back(new_cursor_value);
        ps.value_stack.emplace_back(sv);
    }

    template<typename ParserState>
    constexpr void reduce(ParserState& ps, size_t rule_info_idx) const
    {
        const auto& ri = rule_infos[rule_info_idx];
        ps.trace_stream << "Reduced using rule " << ri.r_idx << "\n";
        ps.cursor_stack.erase(ps.cursor_stack.end() - ri.r_elements, ps.cursor_stack.end());
        size_t new_cursor_value = parse_table[ps.cursor_stack.back()][ri.l_idx].shift;
        ps.trace_stream << "Go to " << new_cursor_value << "\n";
        ps.cursor_stack.push_back(new_cursor_value);

        value_variant_type* start = ps.value_stack.data() + ps.value_stack.size() - ri.r_elements;
        value_variant_type lvalue(value_reductors[ri.r_idx](functors, start, ps.context));
        ps.value_stack.erase(ps.value_stack.end() - ri.r_elements, ps.value_stack.end());
        ps.value_stack.emplace_back(std::move(lvalue));
    }

    template<typename ParserState>
    constexpr void rr_conflict(ParserState& ps, size_t rule_idx) const
    {
        ps.trace_stream << "R/R conflict encountered \n";
        reduce(ps, rule_idx);
    }

    template<typename Stream>
    constexpr void write_syntax_error_to_stream(Stream& stream, const char* error) const
    {
        stream << "Syntax error: " << error << "\n";
    }

    template<typename ParserState>
    constexpr void syntax_error(ParserState& ps, size_t term_idx) const
    {
        cstream<100> str;
        str << "Unexpected '" << term_names[term_idx] << "'";
        write_syntax_error_to_stream(ps.error_stream, str.str());
        write_syntax_error_to_stream(ps.trace_stream, str.str());
    }

    template<typename ParserState>
    constexpr root_value_type& success(ParserState& ps) const
    {
        ps.trace_stream << "Success \n";
        return std::get<root_value_type>(ps.value_stack.front());
    }

    template<typename It, typename ErrorStream, typename TraceStream>
    constexpr auto get_next_term_trivial(const It& start, const It& end, ErrorStream& error_stream, TraceStream& trace_stream) const
    {
        size_t term_idx = uninitialized;
        size_t term_size = uninitialized;
        It it = start;
        if (it == end)
        {
            term_idx = eof_idx;
            term_size = 0;
        }
        else
        {
            char c = *it;
            term_idx = trivial_term_table[char_to_idx(c)];
            term_size = 1;
        }

        if (term_idx != uninitialized)
        {
            trace_stream << "Recognized " << term_names[term_idx] << " \n";
            it++;
        }
        else
        {
            write_unexpected_character_to_stream(error_stream, *it);
            write_unexpected_character_to_stream(trace_stream, *it);
        }
        return std::make_pair(term_idx, term_size);
    }

    template<typename It, typename ErrorStream, typename TraceStream>
    constexpr auto get_next_term(const It& start, const It& end, ErrorStream& error_stream, TraceStream& trace_stream) const
    {
        if (trivial_lexical_analyzer)
            return get_next_term_trivial(start, end, error_stream, trace_stream);
        else
            throw std::runtime_error("not implemented yet");
    }

    template<typename Buffer, typename ErrorStream, typename TraceStream, typename = std::enable_if_t<std::is_same_v<context_type, no_context>>>
    constexpr std::optional<root_value_type> parse(const Buffer& buffer, ErrorStream& error_stream, TraceStream& trace_stream) const
    {
        no_context c;
        return parse_in_context(buffer, c, error_stream, trace_stream);
    }

    template<typename Buffer, typename ErrorStream, typename = std::enable_if_t<std::is_same_v<context_type, no_context>>>
    constexpr std::optional<root_value_type> parse(const Buffer& buffer, ErrorStream& error_stream) const
    {
        no_context c;
        no_stream trace_stream;
        return parse_in_context(buffer, c, error_stream, trace_stream);
    }

    template<typename Buffer, typename = std::enable_if_t<std::is_same_v<context_type, no_context>>>
    constexpr std::optional<root_value_type> parse(const Buffer& buffer) const
    {
        no_context c;
        no_stream trace_stream;
        no_stream error_stream;
        return parse_in_context(buffer, c, error_stream, trace_stream);
    }

    template<typename Buffer, typename ErrorStream>
    constexpr std::optional<root_value_type> parse_in_context(const Buffer& buffer, context_type& context, ErrorStream& error_stream) const
    {
        no_stream trace_stream;
        return parse_in_context(buffer, context, error_stream, trace_stream);
    }

    template<typename Buffer>
    constexpr std::optional<root_value_type> parse_in_context(const Buffer& buffer, context_type& context) const
    {
        no_stream trace_stream;
        no_stream error_stream;
        return parse_in_context(buffer, context, error_stream, trace_stream);
    }

    template<typename Buffer, typename ErrorStream, typename TraceStream>
    constexpr std::optional<root_value_type> parse_in_context(const Buffer& buffer, context_type& context, ErrorStream& error_stream, TraceStream& trace_stream) const
    {
        parser_value_stack_type_t<Buffer, ValueVariantType> value_stack{};
        parse_table_cursor_stack_type_t<Buffer> cursor_stack{};
        parser_state ps(cursor_stack, value_stack, error_stream, trace_stream, context);

        bool error = false;
        typename Buffer::iterator it = buffer.begin();

        ps.cursor_stack.push_back(0);
        size_t term_idx = uninitialized;
        size_t term_size = uninitialized;
        bool next_term_needed = true;
        std::optional<root_value_type> root_value;

        while (true)
        {
            size_t cursor = ps.cursor_stack.back();
            if (next_term_needed)
            {
                auto res = get_next_term(it, buffer.end(), error_stream, trace_stream);
                term_idx = res.first;
                term_size = res.second;
                next_term_needed = false;
            }
            if (term_idx == uninitialized)
                break;

            const auto& entry = parse_table[cursor][get_parse_table_idx(true, term_idx)];
            if (entry.kind == parse_table_entry_kind::shift)
            {
                shift(ps, buffer.view(it, term_size), term_idx, entry.shift);
                it += term_size;
                next_term_needed = true;
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

    name_table<term_count> term_names = { };
    name_table<nterm_count> nterm_names = { };
    symbol right_sides[rule_count][max_rule_element_count] = { };
    rule_info rule_infos[rule_count] = { };
    slice nterm_rule_slices[nterm_count] = { };
    term_subset situation_first_after[situation_count] = { };
    bool nterm_empty[nterm_count] = { };
    term_subset nterm_first[nterm_count] = { };
    bool nterm_empty_analyzed[nterm_count] = { };
    bool nterm_first_analyzed[nterm_count] = { };
    state states[MaxStates] = { };
    parse_table_entry parse_table[MaxStates][term_count + nterm_count] = {};
    size_t state_count = 0;
    situation_queue_entry situation_queue[MaxStates * situation_count] = { };
    size_t situation_queue_size = 0;
    int term_precedences[term_count] = { };
    associativity term_associativities[term_count] = { associativity::ltor };
    int rule_precedences[rule_count] = { };
    size_t rule_last_terms[rule_count] = { };
    size_t trivial_term_table[distinct_values_count<char>] = { };
    bool trivial_lexical_analyzer = true;
    functor_tuple_type functors;
    using value_reductor = value_variant_type(*)(const functor_tuple_type&, value_variant_type*, context_type&);
    value_reductor value_reductors[rule_count] = {};
    using dfa_type = dfa<total_regex_size * 2, total_regex_size>;
    dfa_type lexer_sm = {};
    bool valid_lexer = true;
    cstream<20000> lexer_error_stream;
};

template<size_t S>
struct use_max_states
{
    template<size_t TermCount, size_t... RuleSizes>
    static const size_t value = S;
};

struct deduce_max_states
{
    template<size_t TermCount, size_t... RuleSizes>
    static const size_t value = ((0 + ... + (RuleSizes + 1)) + 2) * (TermCount + 1);
};

template<typename RootValueType, size_t... N, typename... NTermValueType, typename... Rules, typename ContextUsage, typename MaxStateUsage>
parser(
    nterm<RootValueType>,
    std::tuple<term<N>...>,
    std::tuple<nterm<NTermValueType>...>,
    std::tuple<Rules...>,
    ContextUsage,
    MaxStateUsage
) ->
parser<
    RootValueType,
    sizeof...(N),
    sizeof...(NTermValueType),
    sizeof...(Rules),
    max_v<Rules::n...>,
    unique_types_variant_t<char, term_value_type, NTermValueType...>,
    std::tuple<typename Rules::f_type...>,
    (0 + ... + N),
    typename ContextUsage::type,
    MaxStateUsage::template value<sizeof...(N), Rules::n...>
>;

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
    using char_list_type = typename DFAType::char_list_type;
    using char_range_list_type = typename DFAType::char_range_list_type;
    constexpr term regular_char(exclude("\\[]^-.*|()"), 0, associativity::ltor, "regular");
    constexpr nterm<slice> expr("expr");
    constexpr nterm<slice> alt("alt");
    constexpr nterm<slice> concat("concat");
    constexpr nterm<slice> q_expr("q_expr");
    constexpr nterm<slice> primary("primary");
    constexpr nterm<char_list_type> c_list("c_list");
    constexpr nterm<char_range> c_range("c_range");
    constexpr nterm<char_range_list_type> c_ranges("c_ranges");
    constexpr nterm<char> single_char("single_char");

    return parser(
        expr,
        terms(regular_char, "\\", "[", "]", "^", "-", ".", "*", "|", "(", ")"),
        nterms(expr, alt, concat, q_expr, primary, c_list, c_range, c_ranges, single_char),
        rules(
            single_char(regular_char) >= [](str_view s) { return s[0]; },
            single_char("\\", "\\") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "[") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "]") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "^") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "-") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", ".") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "|") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", "(") >= [](skip, str_view s) { return s[0]; },
            single_char("\\", ")") >= [](skip, str_view s) { return s[0]; },
            c_list(single_char) >= [](char c) { return char_list_type(c, 1); },
            c_list(single_char, c_list) >= [&sm](char c, char_list_type&& l) { l.push_back(c); return l; },
            c_range(single_char, "-", single_char) >= [](char c1, skip, char c2){ return char_range{c1, c2}; },
            c_ranges(c_range) >= [](char_range r) { return char_range_list_type(r, 1); },
            c_ranges(c_range, c_ranges) >= [](char_range r, char_range_list_type&& l) { l.push_back(r); return l; },
            primary(single_char) >= [&sm](char c) { return sm.add_primary_single_char(c); },
            primary(".") >= [&sm](skip) { return sm.add_primary_any_char(); },
            primary("[", c_list, "]") >= [&sm](skip, char_list_type&& l, skip) { return sm.add_primary_char_list(l); },
            primary("[", "^", c_list, "]") >= [&sm](skip, skip, char_list_type&& l, skip) { return sm.add_primary_char_list_exclusive(l); },
            primary("[", c_ranges, "]") >= [&sm](skip, char_range_list_type&& l, skip) { return sm.add_primary_char_ranges(l); },
            primary("[", "^", c_ranges, "]") >= [&sm](skip, skip, char_range_list_type&& l, skip) { return sm.add_primary_char_ranges_exclusive(l); },
            primary("(", expr, ")") >= [](skip, slice p, skip) { return p; },
            q_expr(primary) >= [](slice p) { return p; },
            q_expr(primary, "*") >= [&sm](slice p, skip) { return sm.add_multiplication(p); },
            concat(q_expr) >= [](slice p) { return p; },
            concat(q_expr, concat) >= [&sm](slice p1, slice p2) { return sm.add_concat(p1, p2); },
            alt(concat) >= [](slice p) { return p; },
            alt(alt, "|", alt) >= [&sm](slice p1, skip, slice p2) { return sm.add_alt(p1, p2); },
            expr(alt) >= [](slice p) { return p; }
        ),
        no_context{},
        deduce_max_states{}
    );
}

template<typename DFA, size_t SSize, size_t... N, size_t... I>
constexpr bool create_lexer_impl(DFA& sm, cstream<SSize>& error_stream, bool trace, std::index_sequence<I...>, std::tuple<term<N>...> ts)
{
    using buffer_type = cstring_buffer<max_v<N...>>;
    
    auto p = create_regex_parser(sm);
    auto parse_f = [&p, &sm, &error_stream, trace](const auto& r, size_t idx)
    { 
        cstream<20000> tmp_trace_stream;
        cstream<2000> tmp_error_stream;
        bool valid = true;
        slice prev{0, sm.size()};
        auto res = p.parse(buffer_type(r), tmp_error_stream, tmp_trace_stream);
        if (res.has_value())
        {
            sm.mark_end_states(res.value(), idx);
            sm.add_alt(prev, res.value());
        }
        else
        {
            error_stream << "Regex " << r << " parse error: \n" << tmp_error_stream.str() << "\n";
            if (trace)
                error_stream << "Trace: \n\n: " << tmp_trace_stream.str();

            valid = false;
        }
        return valid;
    };

    return (true && ... && parse_f(std::get<I>(ts).data, I));
}

template<typename DFA, size_t SSize, size_t... N>
constexpr bool create_lexer(DFA& sm, cstream<SSize>& error_stream, bool trace, std::tuple<term<N>...> ts)
{
    return create_lexer_impl(sm, error_stream, trace, std::make_index_sequence<sizeof...(N)>{}, ts);
}

template<typename ValueType, typename ContextType, typename ErrorStreamType, typename TraceStreamType>
struct parse_result
{
    ErrorStreamType error_stream;
    TraceStreamType trace_stream;
    std::optional<ValueType> value;
    ContextType context;

    template<typename Parser, typename Buffer, typename ErrorStreamUsage, typename TraceStreamUsage>
    constexpr parse_result(const Parser& p, const Buffer& buffer, ErrorStreamUsage, TraceStreamUsage) :
        parse_result(p, buffer)
    {}

    template<typename Parser, typename Buffer, typename ErrorStreamUsage>
    constexpr parse_result(const Parser& p, const Buffer& buffer, ErrorStreamUsage) :
        parse_result(p, buffer)
    {}
    
    template<typename Parser, typename Buffer>
    constexpr parse_result(const Parser& p, const Buffer& buffer)
    {
        value = p.parse_in_context(buffer, context, error_stream, trace_stream);
    }

    constexpr const auto& get_error_stream() const { return error_stream; }
    constexpr const auto& get_trace_stream() const { return trace_stream; }
    constexpr const auto& get_value() const { return value.value(); }
    constexpr bool get_success() const { return value.has_value(); }
    constexpr const auto& get_context() const { return context; }
};

template<typename Parser, typename Buffer, typename ErrorStreamUsage, typename TraceStreamUsage>
parse_result(const Parser&, const Buffer&, ErrorStreamUsage, TraceStreamUsage)
->parse_result<typename Parser::root_value_type, typename Parser::context_type, typename ErrorStreamUsage::type, typename TraceStreamUsage::type>;

template<typename Parser, typename Buffer, typename ErrorStreamUsage>
parse_result(const Parser&, const Buffer&, ErrorStreamUsage)
->parse_result<typename Parser::root_value_type, typename Parser::context_type, typename ErrorStreamUsage::type, no_stream>;

template<typename Parser, typename Buffer>
parse_result(const Parser&, const Buffer&)
->parse_result<typename Parser::root_value_type, typename Parser::context_type, no_stream, no_stream>;



