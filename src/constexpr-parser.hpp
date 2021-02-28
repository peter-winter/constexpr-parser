#include <utility>
#include <cstdint>
#include <limits>
#include <tuple>
#include <algorithm>
#include <sstream>
#include <vector>
#include <string_view>
#include <optional>
#include <variant>

using size_t = std::uint64_t;
using size8_t = std::uint8_t;
using size16_t = std::uint16_t;
using size32_t = std::uint32_t;

constexpr size_t uninitialized = size_t(-1);
constexpr size16_t uninitialized16 = size16_t(-1);
constexpr size32_t uninitialized32 = size32_t(-1);

template<size_t N>
struct cstring_buffer
{
    constexpr cstring_buffer(const char(&source)[N])
    {
        init(source, std::make_index_sequence<N>{});
    }

    template<size_t... I>
    constexpr auto init(const char(&source)[N], std::index_sequence<I...>)
    {
        (void(data[I] = source[I]), ...);
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
    constexpr std::string_view view(const iterator& it, size_t size) const { return std::string_view(it.ptr, size); }

    char data[N] = { 0 };
};

template<typename T>
struct is_cvector_compatible : std::bool_constant<std::is_default_constructible_v<T> && std::is_trivially_destructible_v<T>>
{};

template<typename T, size_t N, typename = void>
struct cvector
{};

template<typename T, size_t N>
struct cvector<T, N, std::enable_if_t<is_cvector_compatible<T>::value>>
{
    using size_type = size_t;

    constexpr cvector():
        the_data{}, current_size(0)
    {}

    struct iterator
    {
        T* ptr;
        constexpr bool operator == (const iterator& other) const { return ptr == other.ptr; }
        constexpr T& operator *() const { return *ptr; }
        constexpr iterator operator - (size_type amount) const { return iterator{ ptr - amount }; }
        constexpr size_type operator - (const iterator& other) const { return size_type(ptr - other.ptr); }
        constexpr iterator operator + (size_type amount) const { return iterator{ ptr + amount }; }
        constexpr iterator operator ++(int) { iterator it{ ptr }; ++ptr; return it; }
        constexpr iterator operator ++() { ++ptr; return *this; }
        constexpr bool operator > (const iterator& other) const { return ptr > other.ptr; }
        constexpr bool operator < (const iterator& other) const { return ptr < other.ptr; }
    };

    struct const_iterator
    {
        const T* ptr;
        constexpr bool operator == (const const_iterator& other) const { return ptr == other.ptr; }
        constexpr const T& operator *() const { return *ptr; }
        constexpr const_iterator operator - (size_t amount) const { return const_iterator{ ptr - amount }; }
        constexpr size_t operator - (const const_iterator& other) const { return size_t(ptr - other.ptr); }
        constexpr const_iterator operator + (size_t amount) const { return const_iterator{ ptr - amount }; }
        constexpr const_iterator operator ++(int) { const_iterator it{ ptr }; ++ptr; return it; }
        constexpr const_iterator operator ++() { ++ptr; return *this; }
        constexpr bool operator > (const const_iterator& other) const { return ptr > other.ptr; }
        constexpr bool operator < (const const_iterator& other) const { return ptr < other.ptr; }
    };

    constexpr const T* data() const { return the_data; }
    constexpr T* data() { return the_data; }
    constexpr size_type size() const { return current_size; }
    constexpr void reserve(size_type) const {};
    constexpr void resize(size_type new_size)
    {
        for (; current_size < new_size; ++current_size)
        {
            the_data[current_size] = T();
        }
    };
    constexpr const T& operator[](size_type idx) const { return the_data[idx]; }
    constexpr T& operator[](size_type idx) { return the_data[idx]; }
    constexpr void push_back(const T& v) { the_data[current_size++] = v; }
    constexpr void emplace_back(T&& v) { the_data[current_size++] = std::move(v); }
    constexpr T& back() { return the_data[current_size - 1]; }
    constexpr const T& back() const { return the_data[current_size - 1]; }
    constexpr const_iterator begin() const { return iterator{ the_data }; }
    constexpr const_iterator end() const { return iterator{ the_data + current_size }; }
    constexpr iterator begin() { return iterator{ the_data }; }
    constexpr iterator end() { return iterator{ the_data + current_size }; }
    constexpr iterator erase(iterator first, iterator last)
    {
        if (!(first < last))
            return end();
        auto from = first < begin() ? begin() : first;
        auto to = last > end() ? end() : last;
        current_size -= (to - from);
        return end();
    }
    constexpr const T& front() const { return the_data[0]; }
    constexpr T& front() { return the_data[0]; }

    T the_data[N];
    size_type current_size;
};

template<size_t N>
cstring_buffer(const char(&)[N])->cstring_buffer<N>;

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

template<size_t S>
using use_max_states = std::integral_constant<size_t, S>;

template<size_t TermCount, size_t... RuleSizes>
constexpr size_t deduce_max_states = ((0 + ... + (RuleSizes + 1)) + 2) * (TermCount + 1);

template<size_t S>
using use_message_max_size = std::integral_constant<size_t, S>;

template<typename Buffer>
struct parse_table_cursor_stack_type
{
    using type = std::vector<size_t>;
};

template<size_t N>
struct parse_table_cursor_stack_type<cstring_buffer<N>>
{
    using type = cvector<size_t, N>;
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
    using type = cvector<ValueVariantType, N>;
};

template<typename Buffer, typename ValueVariantType>
using parser_value_stack_type_t = typename parser_value_stack_type<Buffer, ValueVariantType>::type;

template<size_t N>
constexpr size_t find_name(const name_table<N> &table, const char* name)
{
    for (auto i = std::begin(table); i != std::end(table); ++i)
        if (std::string_view(*i).compare(std::string_view(name)) == 0)
            return std::distance(std::begin(table), i);
    return size_t(-1);
}

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

    constexpr cstream& operator << (size_t x)
    {
        if (x == 0u)
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

template<typename T>
struct is_cstream
{
    static const bool value = false;
};

template<size_t S>
struct is_cstream<cstream<S>>
{
    static const bool value = true;
};

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

template<typename... T>
constexpr int size_sum = (0 + ... + sizeof(T));

template<typename ValueType>
struct nterm
{
    using value_type = ValueType;

    constexpr nterm(const char* name):
        name(name)
    {}

    constexpr void set_idx(size_t idx)
    {
        this->idx = idx;
    }

    constexpr const char* get_name() const { return name; }

    template<typename... Args>
    constexpr auto operator()(Args... args) const;

    size_t idx = uninitialized;
    const char* name;
};

template<typename ValueType>
struct fake_root
{
    using value_type = ValueType;

    template<typename... Args>
    constexpr auto operator()(Args... args) const;

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
    constexpr rule(F1&& f, L l, std::tuple<R...> r):
        f(std::move(f)), l(l), r(r), precedence(0)
    {}

    template<typename F1>
    constexpr rule(F1&& f, L l, std::tuple<R...> r, int precedence):
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

template<typename T>
struct value_type
{
    using type = typename T::value_type;
};

template<>
struct value_type<char>
{
    using type = char;
};

template<>
struct value_type<const char*>
{
    using type = std::string_view;
};

template<typename T>
using value_type_t = typename value_type<T>::type;

template<typename ValueType>
template<typename... Args>
constexpr auto nterm<ValueType>::operator()(Args... args) const
{
    using f_type = move_construct<ValueType, value_type_t<Args>...>;
    return rule<f_type, nterm<ValueType>, Args...>(f_type{}, *this, std::make_tuple(args...));
}

template<typename ValueType>
template<typename... Args>
constexpr auto fake_root<ValueType>::operator()(Args... args) const
{
    using f_type = move_construct<ValueType, ValueType>;
    return rule<f_type, fake_root<ValueType>, Args...>{f_type{}, *this, std::make_tuple(args...)};
}

enum class associativity { ltor, rtol };

using char_2 = char[2];

template<typename Seq>
constexpr char_2 char_strings_impl[1];

template<size_t... I>
constexpr char_2 char_strings_impl<std::index_sequence<I...>>[sizeof...(I)] = { {index_to_char(I), '\0'} ... };

constexpr auto& char_strings = char_strings_impl<std::make_index_sequence<distinct_values_count<char>>>;

constexpr const char* char_name(char c)
{
    return char_strings[char_to_idx(c)];
}

struct term_data
{
    constexpr term_data(const char* str):
        str(str)
    {}

    constexpr term_data(char c):
        str(char_name(c))
    {}

    const char* str;
};

template<typename ValueType>
struct term
{
    using value_type = ValueType;
      
    constexpr term(term_data data):
        term(data, 0, associativity::ltor, data.str)
    {}

    constexpr term(term_data data, associativity a):
        term(data, 0, a, data.str)
    {}

    constexpr term(term_data data, int precedence):
        term(data, precedence, associativity::ltor, data.str)
    {}

    constexpr term(term_data data, int precedence, associativity a):
        term(data, precedence, associativity::ltor, data.str)
    {}

    constexpr term(term_data data, const char* name):
        term(data, 0, associativity::ltor, name)
    {}

    constexpr term(term_data data, associativity a, const char* name):
        term(data, 0, a, name)
    {}

    constexpr term(term_data data, int precedence, const char* name):
        term(data, precedence, associativity::ltor, name)
    {}

    constexpr term(term_data data, int precedence, associativity a, const char* name):
        idx(uninitialized), data(data.str), precedence(precedence), ass(a), name(name)
    {}

    constexpr void set_idx(size_t idx)
    {
        this->idx = idx;
    }

    constexpr const char* get_name() const { return name; }

    size_t idx;
    const char* data;
    int precedence;
    associativity ass;
    const char* name;
};

template<typename T, typename... Args>
term(T, Args...)->term<value_type_t<T>>;

struct eof
{
    constexpr static const char* get_name() { return "<eof>"; }
};

struct none_of
{
    size_t idx;
    const char* data;
    const char* name;
    
    constexpr none_of(const char* data, const char* name):
        data(data), name(name), idx(uninitialized)
    {}

    constexpr void set_idx(size_t idx)
    {
        this->idx = idx;
    }

    constexpr const char* get_name() const { return name; }
};

template<
    typename RootValueType, 
    size_t TermCount, size_t NTermCount, size_t RuleCount, 
    size_t MaxRuleElementCount,
    typename ValueVariantType,
    typename FunctorTupleType,
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

    using functor_tuple_type = FunctorTupleType;
    using value_variant_type = ValueVariantType;
    using term_subset = bool[term_count];
    using state = bool[situation_count];
    using root_value_type = RootValueType;

    struct symbol
    {
        constexpr symbol():
            term(false), idx(uninitialized)
        {}

        constexpr symbol(bool term, size_t idx):
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

    struct slice
    {
        size_t start;
        size_t n;
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
            TraceStream& trace_stream):
            cursor_stack(cursor_stack),
            value_stack(value_stack),
            error_stream(error_stream),
            trace_stream(trace_stream)
        {
            cursor_stack.reserve(cursor_stack_initial_capacity);
            value_stack.reserve(value_stack_initial_capacity);
        }

        CursorStack& cursor_stack;
        ValueStack& value_stack;
        ErrorStream& error_stream;
        TraceStream& trace_stream;
    };

    constexpr static size_t get_parse_table_idx(bool term, size_t idx)
    {
        return term ? nterm_count + idx : idx;
    }

    template<typename Root, typename Terms, typename NTerms, typename Rules, typename MaxStatesDef>
    constexpr parser(
        Root root,
        Terms terms,
        NTerms nterms,
        Rules rules,
        MaxStatesDef):
        parser(root, terms, nterms, rules)
    {}

    template<typename Root, typename... Terms, typename NTerms, typename... Rules>
    constexpr parser(
        Root root,
        std::tuple<Terms...> terms,
        NTerms nterms,
        std::tuple<Rules...>&& rules):
        functors(make_functors_tuple(std::index_sequence_for<Rules...>{}, std::move(rules)))
    {
        std::apply([this](auto... nt) { (void(analyze_nterm(nt)), ...); }, nterms);
        analyze_nterm(fake_root<RootValueType>{});

        analyze_terms(std::index_sequence_for<Terms...>{}, terms);
        analyze_eof(eof{});

        analyze_rules(std::index_sequence_for<Rules...>{}, root, std::move(rules));

        analyze_states();
        
        for (size_t x : trivial_term_table) 
            x = uninitialized;
    }

    template<typename... Rules, size_t... I>
    constexpr auto make_functors_tuple(std::index_sequence<I...>, std::tuple<Rules...>&& rules)
    {
        return std::make_tuple(std::move(std::get<I>(rules).f)...);
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

    constexpr void analyze_eof(eof)
    {
        term_names[eof_idx] = eof::get_name();
        term_precedences[eof_idx] = 0;
        term_associativities[eof_idx] = associativity::ltor;
    }

    template<size_t I>
    constexpr void analyze_term(term<std::string_view> t)
    {
        const char* name = t.name;
        int precedence = t.precedence;
        associativity ass = t.ass;
                
        term_names[t.idx] = name;
        term_precedences[t.idx] = precedence;
        term_associativities[t.idx] = ass;
        trivial_lexical_analyzer = false;

        value_shifters[I] = &shift_value<std::string_view>;
    }

    template<size_t I>
    constexpr void analyze_term(term<char> t)
    {
        term_names[t.idx] = t.name;
        term_precedences[t.idx] = t.precedence;
        term_associativities[t.idx] = t.ass;
        trivial_term_table[char_to_idx(t.data[0])] = t.idx;

        value_shifters[I] = &shift_value<char>;
    }

    template<size_t I>
    constexpr void analyze_term(none_of t)
    {
        term_names[t.idx] = t.name;
        term_precedences[t.idx] = 0;
        term_associativities[t.idx] = associativity::ltor;
        const char* data = t.data;
        while (*data != 0)
        {
            trivial_term_table[char_to_idx(*data)] = t.idx;
            ++data;
        }

        value_shifters[I] = &shift_value<char>;
    }

    template<typename ValueType>
    constexpr void analyze_nterm(nterm<ValueType> nt)
    {
        nterm_names[nt.idx] = nt.name;
    }

    template<typename ValueType>
    constexpr void analyze_nterm(fake_root<ValueType>)
    {
        nterm_names[fake_root_idx] = fake_root<ValueType>::get_name();
    }

    template<typename ValueType>
    constexpr auto make_symbol(term<ValueType> t) const
    {
        return symbol{ true, find_name(term_names, t.name) };
    }

    template<typename ValueType>
    constexpr auto make_symbol(nterm<ValueType> nt) const
    {
        return symbol{ false, find_name(nterm_names, nt.name) };
    }

    constexpr auto make_symbol(const char* str) const
    {
        return symbol{ true, find_name(term_names, str) };
    }

    constexpr auto make_symbol(char c) const
    {
        return symbol{ true, find_name(term_names, char_name(c)) };
    }

    template<typename... Terms, size_t... I>
    constexpr void analyze_terms(std::index_sequence<I...>, std::tuple<Terms...> terms)
    {
        (void(analyze_term<I>(std::get<I>(terms))), ...);
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
        s << "| " << term_names[addr.t];
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
        for (size_t i = 0; i < state_count; ++i)
        {
            write_state_diag_str(s, i);
            s << "\n";
        }
    }

    template<typename Stream>
    constexpr void write_lexer_error_to_stream(Stream& stream) const
    {
        stream << "Lexer error: " << "\n";
    }

    template<typename>
    struct tag {};

    constexpr static char shift_value_impl(tag<char>, const std::string_view& sv)
    {
        return sv[0];
    }

    constexpr static std::string_view shift_value_impl(tag<std::string_view>, const std::string_view& sv)
    {
        return sv;
    }

    template<typename ValueType>
    constexpr static value_variant_type shift_value(const std::string_view &sv)
    {
        return shift_value_impl(tag<ValueType>{}, sv);
    }

    template<typename F, typename LValueType, typename... RValueType, size_t... I>
    constexpr static LValueType reduce_value_impl(const F& f, value_variant_type* start, std::index_sequence<I...>)
    {
        return LValueType(f(std::get<RValueType>(std::move(*(start + I)))...));
    }

    template<size_t RuleIdx, typename F, typename LValueType, typename... RValueType>
    constexpr static value_variant_type reduce_value(const functor_tuple_type& functors, value_variant_type* start)
    {
        return value_variant_type(
            reduce_value_impl<F, LValueType, RValueType...>(std::get<RuleIdx>(functors), start, std::index_sequence_for<RValueType...>{})
        );
    }
    
    template<typename It, typename ErrorStream, typename TraceStream>
    constexpr auto get_next_term(const It& start, const It& end, ErrorStream& error_stream, TraceStream& trace_stream) const
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
            char c = *it++;
            term_idx = trivial_term_table[char_to_idx(c)];
            term_size = 1;
        }

        if (term_idx != uninitialized)
        {
            trace_stream << "Recognized " << term_names[term_idx] << " \n";
        }
        return std::make_pair(term_idx, term_size);
    }

    template<typename ParserState>
    constexpr void shift(ParserState& ps, const std::string_view& sv, size_t term_idx, size_t new_cursor_value) const
    {
        ps.trace_stream << "Shift to " << new_cursor_value << "\n";
        ps.cursor_stack.push_back(new_cursor_value);
        ps.value_stack.emplace_back(value_shifters[term_idx](sv));
    }

    template<typename ParserState>
    constexpr void reduce(ParserState& ps, size_t rule_info_idx) const
    {
        ps.trace_stream << "Reduce using rule " << rule_info_idx << "\n";
        const auto& ri = rule_infos[rule_info_idx];
        ps.cursor_stack.erase(ps.cursor_stack.end() - ri.r_elements, ps.cursor_stack.end());
        size_t new_cursor_value = parse_table[ps.cursor_stack.back()][ri.l_idx].shift;
        ps.trace_stream << "Go to " << new_cursor_value << "\n";
        ps.cursor_stack.push_back(new_cursor_value);

        value_variant_type* start = ps.value_stack.data() + ps.value_stack.size() - ri.r_elements;
        value_variant_type lvalue(value_reductors[ri.r_idx](functors, start));
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
    constexpr void write_syntax_error_to_stream(size_t parse_table_cursor, Stream& stream) const
    {
        stream << "Syntax error: " << "\n";
    }

    template<typename ParserState>
    constexpr void syntax_error(ParserState& ps) const
    {
        size_t cursor = ps.cursor_stack.back();
        write_syntax_error_to_stream(cursor, ps.error_stream);
        write_syntax_error_to_stream(cursor, ps.trace_stream);
    }

    template<typename ParserState>
    constexpr decltype(auto) success(ParserState& ps) const
    {
        ps.trace_stream << "Success \n";
        return std::move(std::get<RootValueType>(ps.value_stack.front()));
    }

    template<typename Buffer, typename ErrorStream, typename TraceStream>
    constexpr std::optional<RootValueType> parse(const Buffer& buffer, ErrorStream& error_stream, TraceStream& trace_stream) const
    {
        parser_value_stack_type_t<Buffer, ValueVariantType> value_stack{};
        parse_table_cursor_stack_type_t<Buffer> cursor_stack{};
        parser_state ps(cursor_stack, value_stack, error_stream, trace_stream);

        bool error = false;
        typename Buffer::iterator it = buffer.begin();

        ps.cursor_stack.push_back(0);
        size_t term_idx = uninitialized;
        size_t term_size = uninitialized;
        bool next_term_needed = true;
        std::optional<RootValueType> root_value;

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
                root_value = success(ps);
                break;
            }
            else
            {
                syntax_error(ps);
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
    using value_reductor = value_variant_type (*)(const functor_tuple_type&, value_variant_type*);
    value_reductor value_reductors[rule_count] = {};
    using value_shifter = value_variant_type (*)(const std::string_view&);
    value_shifter value_shifters[term_count] = {};
};


template<typename RootValueType, typename... TermValueType, typename... NTermValueType, typename... Rules, size_t MaxStates>
parser(
    nterm<RootValueType>,
    std::tuple<term<TermValueType>...>,
    std::tuple<nterm<NTermValueType>...>,
    std::tuple<Rules...>,
    use_max_states<MaxStates>
) ->
parser<
    RootValueType, 
    sizeof...(TermValueType), 
    sizeof...(NTermValueType), 
    sizeof...(Rules), 
    max_v<Rules::n...>, 
    unique_types_variant_t<char, TermValueType..., NTermValueType...>,
    std::tuple<typename Rules::f_type...>,
    MaxStates>;

template<typename RootValueType, typename... TermValueType, typename... NTermValueType, typename... Rules>
parser(
    nterm<RootValueType>,
    std::tuple<term<TermValueType>...>,
    std::tuple<nterm<NTermValueType>...>,
    std::tuple<Rules...>
) ->
parser<
    RootValueType, 
    sizeof...(TermValueType), 
    sizeof...(NTermValueType), 
    sizeof...(Rules), 
    max_v<Rules::n...>,
    unique_types_variant_t<char, TermValueType..., NTermValueType...>,
    std::tuple<typename Rules::f_type...>,
    deduce_max_states<sizeof...(TermValueType), Rules::n...>
>;

template<typename... S, size_t... I>
constexpr auto make_symbols_impl(std::index_sequence<I...>, S... s)
{
    (void(s.set_idx(I)), ...);
    return std::make_tuple(s...);
}

template<typename... T>
constexpr auto make_terms(T... ts)
{
    return make_symbols_impl(std::index_sequence_for<T...>{}, term(ts)...);
}

template<typename... NT>
constexpr auto make_nterms(NT... nts)
{
    return make_symbols_impl(std::index_sequence_for<NT...>{}, nts...);
}

template<typename... Rules>
constexpr auto make_rules(Rules&&... rules)
{
    return std::make_tuple(std::move(rules)...);
}

struct use_string_stream
{};

template<typename StreamType>
struct diag_msg
{
    template<typename Parser, size_t size_t>
    constexpr diag_msg(const Parser& p, use_message_max_size<size_t>)
    {
        p.write_diag_str(stream);
    }

    template<typename Parser>
    constexpr diag_msg(const Parser& p, use_string_stream)
    {
        p.write_diag_str(stream);
    }

    const StreamType& get_stream() const
    {
        return stream;
    }

    StreamType stream;
};

template<typename Parser, size_t Size>
diag_msg(const Parser&, use_message_max_size<Size>)->diag_msg<cstream<Size>>;

template<typename Parser>
diag_msg(const Parser&, use_string_stream)->diag_msg<std::stringstream>;

template<typename ValueType, typename ErrorStreamType, typename TraceStreamType>
struct parse_result
{
    ErrorStreamType error_stream;
    TraceStreamType trace_stream;
    std::optional<ValueType> value{};

    template<typename Parser, size_t ErrorStreamSize, size_t TraceStreamSize, typename Buffer>
    constexpr parse_result(const Parser& p, const Buffer& buffer, use_message_max_size<ErrorStreamSize>, use_message_max_size<TraceStreamSize>):
        parse_result(p, buffer)
    {}

    template<typename Parser, size_t ErrorStreamSize, typename Buffer>
    constexpr parse_result(const Parser& p, const Buffer& buffer, use_message_max_size<ErrorStreamSize>):
        parse_result(p, buffer)
    {}

    template<typename Parser, typename Buffer>
    constexpr parse_result(const Parser& p, const Buffer& buffer, use_string_stream, use_string_stream):
        parse_result(p, buffer)
    {}

    template<typename Parser, typename Buffer>
    constexpr parse_result(const Parser& p, const Buffer& buffer, use_string_stream):
        parse_result(p, buffer)
    {}

    template<typename Parser, typename Buffer>
    constexpr parse_result(const Parser& p, const Buffer& buffer)
    {
        value = p.parse(buffer, error_stream, trace_stream);
    }

    constexpr auto success() const { return value.has_object(); }

    constexpr const auto& get_error_stream() const { return error_stream; }
    constexpr const auto& get_trace_stream() const { return trace_stream; }
    constexpr const auto get_value() const { return value.value(); }
};

struct no_stream
{
    template<typename T>
    constexpr const no_stream& operator <<(T&&) const { return *this; }
};

template<typename Parser, typename Buffer, size_t ErrorStreamSize, size_t TraceStreamSize>
parse_result(const Parser&, const Buffer&, use_message_max_size<ErrorStreamSize>, use_message_max_size<TraceStreamSize>)
->parse_result<typename Parser::root_value_type, cstream<ErrorStreamSize>, cstream<TraceStreamSize>>;

template<typename Parser, typename Buffer>
parse_result(const Parser&, const Buffer&, use_string_stream, use_string_stream)
->parse_result<typename Parser::root_value_type, std::stringstream, std::stringstream>;

template<typename Parser, typename Buffer, size_t ErrorStreamSize>
parse_result(const Parser&, const Buffer&, use_message_max_size<ErrorStreamSize>)
->parse_result<typename Parser::root_value_type, cstream<ErrorStreamSize>, no_stream>;

template<typename Parser, typename Buffer>
parse_result(const Parser&, const Buffer&, use_string_stream)
->parse_result<typename Parser::root_value_type, std::stringstream, no_stream>;

template<typename Parser, typename Buffer>
parse_result(const Parser&, const Buffer&)
->parse_result<typename Parser::root_value_type, no_stream, no_stream>;


