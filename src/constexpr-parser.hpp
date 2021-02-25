#include <utility>
#include <cstdint>
#include <limits>
#include <tuple>
#include <algorithm>
#include <sstream>
#include <vector>
#include <string_view>

using size = std::uint64_t;
using size8 = std::uint8_t;
using size16 = std::uint16_t;
using size32 = std::uint32_t;

constexpr size uninitialized = size(-1);
constexpr size16 uninitialized16 = size16(-1);
constexpr size32 uninitialized32 = size32(-1);

template<size N>
using name_table = const char* [N];

template<typename T>
constexpr size distinct_values_count = 1 << (sizeof(T) * 8);

constexpr int min_char = std::numeric_limits<char>::min();

constexpr size char_to_idx(char c)
{
    return -min_char + c;
}

template<size S>
using max_states = std::integral_constant<size, S>;

template<size TermCount, size... RuleSizes>
constexpr size deduce_max_states = ((0 + ... + (RuleSizes + 1)) + 2) * (TermCount + 1);

template<size S>
using message_max_size = std::integral_constant<size, S>;

template<typename Buffer>
struct parse_table_cursor_stack_type
{
    using type = std::vector<size>;
};

template<typename Buffer>
using parse_table_cursor_stack_type_t = typename parse_table_cursor_stack_type<Buffer>::type;

template<typename Buffer, size MaxValueTypeSize>
struct parser_value_stack_type
{
    using type = std::vector<std::byte>;
};

template<typename Buffer, size MaxValueTypeSize>
using parser_value_stack_type_t = typename parser_value_stack_type<Buffer, MaxValueTypeSize>::type;

template<typename T, size N>
struct cvector
{
    constexpr cvector() :
        data{}, current_size(0)
    {}

    struct iterator
    {
        T* ptr;
        constexpr bool operator == (const iterator& other) const { return ptr == other.ptr; }
        constexpr T& operator *() const { return *ptr; }
        constexpr iterator operator - (size amount) const { return iterator{ ptr - amount }; }
        constexpr size operator - (const iterator& other) const { return size(ptr - other.ptr); }
        constexpr iterator operator + (size amount) const { return iterator{ ptr + amount }; }
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
        constexpr const_iterator operator - (size amount) const { return const_iterator{ ptr - amount }; }
        constexpr size operator - (const const_iterator& other) const { return size(ptr - other.ptr); }
        constexpr const_iterator operator + (size amount) const { return const_iterator{ ptr - amount }; }
        constexpr const_iterator operator ++(int) { const_iterator it{ ptr }; ++ptr; return it; }
        constexpr const_iterator operator ++() { ++ptr; return *this; }
        constexpr bool operator > (const const_iterator& other) const { return ptr > other.ptr; }
        constexpr bool operator < (const const_iterator& other) const { return ptr < other.ptr; }
    };

    constexpr void reserve(size) const {};
    constexpr void resize(size) const {};
    constexpr const T& operator[](size idx) const { return data[idx]; }
    constexpr T& operator[](size idx) { return data[idx]; }
    constexpr void push_back(const T& v) { data[current_size++] = v; }
    constexpr void emplace_back(T&& v) { data[current_size++] = std::move(v); }
    constexpr T& back() { return data[current_size - 1]; }
    constexpr const T& back() const { return data[current_size - 1]; }
    constexpr const_iterator begin() const { return iterator{ data }; }
    constexpr const_iterator end() const { return iterator{ data + N }; }
    constexpr iterator begin() { return iterator{ data }; }
    constexpr iterator end() { return iterator{ data + current_size }; }
    constexpr iterator erase(iterator first, iterator last)
    {
        if (!(first < last))
            return end();
        auto from = first < begin() ? begin() : first;
        auto to = last > end() ? end() : last;
        current_size -= (to - from);
        return end();
    }

    T data[N];
    size current_size;
};

template<size N>
constexpr size find_name(const name_table<N> &table, const char* name)
{
    for (auto i = std::begin(table); i != std::end(table); ++i)
        if (std::string_view(*i).compare(std::string_view(name)) == 0)
            return std::distance(std::begin(table), i);
    return size(-1);
}

template<typename T>
struct optional
{
    template<typename T1>
    constexpr optional(T1&& object):
        object(std::move(object)), has_object(true)
    {}

    constexpr optional():
        object(), has_object(false)
    {}

    constexpr const T& get_object() const
    {
        return object;
    }

    bool has_object = false;
    T object;
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

template<size MaxSize>
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

    constexpr cstream& operator << (size x)
    {
        if (x == 0u)
        {
            data[current_size++] = '0';
            return *this;
        }

        char digits[std::numeric_limits<size>::digits10] = { 0 };
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
    size current_size = 0;
};

template<typename T>
struct is_cstream
{
    static const bool value = false;
};

template<size S>
struct is_cstream<cstream<S>>
{
    static const bool value = true;
};

template<bool Reverse, typename Seq, std::size_t Sum, size... X>
struct make_offsets
{};

template<bool Reverse, std::size_t... I, std::size_t Sum, size First, size... Rest>
struct make_offsets<Reverse, std::index_sequence<I...>, Sum, First, Rest...>
{
    using type = std::conditional_t<
        Reverse,
        typename make_offsets<
        Reverse,
        std::index_sequence<Sum + First, I...>, Sum + First, Rest...
        >::type,
        typename make_offsets<
        Reverse,
        std::index_sequence<I..., Sum>, Sum + First, Rest...
        >::type
    >;
};

template<bool Reverse, std::size_t... I, std::size_t Sum>
struct make_offsets<Reverse, std::index_sequence<I...>, Sum>
{
    using type = std::index_sequence<I...>;
};

template<bool Reverse, size...X>
using make_offsets_t = typename make_offsets<Reverse, std::index_sequence<>, 0, X...>::type;

template<size First, size... Rest>
struct max
{
    static const size value = std::max(First, max<Rest...>::value);
};

template<size X>
struct max<X>
{
    static const size value = X;
};

template<size... X>
constexpr size max_v = max<X...>::value;

struct no_value {};

template<typename T>
constexpr size optimized_size = sizeof(T);

template<>
constexpr size optimized_size<no_value> = 0;

template<typename... T>
constexpr size optimized_size_sum = (0 + ... + optimized_size<T>);

template<typename ValueType>
struct nterm
{
    using value_type = ValueType;

    constexpr nterm(const char* name):
        name(name)
    {}

    constexpr void set_idx(size idx)
    {
        this->idx = idx;
    }

    constexpr const char* get_name() const { return name; }

    template<typename... Args>
    constexpr auto operator()(Args... args) const;

    size idx = uninitialized;
    const char* name;
};

template<typename ValueType>
struct fake_root
{
    template<typename... Args>
    constexpr auto operator()(Args... args) const;

    constexpr static const char* get_name() { return "##"; };
};

template<typename L, typename...R>
struct move_construct
{
    L operator()(R&&... r)
    {
        return L(std::move(r));
    }
};

template<typename F, typename L, typename...R>
struct rule
{
    F f;
    L l;
    std::tuple<R...> r;
    static const size n = sizeof...(R);
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

template<int... C>
constexpr char_2 char_strings_impl<std::integer_sequence<int, C...>>[sizeof...(C)] = { {min_char + C, '\0'} ... };

constexpr auto& char_strings = char_strings_impl<std::make_integer_sequence<int, distinct_values_count<char>>>;

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

    constexpr void set_idx(size idx)
    {
        this->idx = idx;
    }

    constexpr const char* get_name() const { return name; }

    size idx;
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
    size idx;
    const char* data;
    const char* name;
    
    constexpr none_of(const char* data, const char* name):
        data(data), name(name), idx(uninitialized)
    {}

    constexpr void set_idx(size idx)
    {
        this->idx = idx;
    }

    constexpr const char* get_name() const { return name; }
};

template<typename RootValueType, size TermCount, size NTermCount, size RuleCount, size RuleSize, size MaxValueTypeSize, size MaxStates>
struct parser
{
    static const size term_count = TermCount + 1;
    static const size eof_idx = TermCount;
    static const size nterm_count = NTermCount + 1;
    static const size fake_root_idx = NTermCount;
    static const size rule_count = RuleCount + 1;
    static const size situation_size = RuleSize + 1;
    static const size situation_count = rule_count * situation_size * term_count;
    static const size root_rule_idx = RuleCount;

    using term_subset = bool[term_count];
    using state = bool[situation_count];

    using root_value_type = RootValueType;

    struct symbol
    {
        constexpr symbol():
            term(false), idx(uninitialized)
        {}

        constexpr symbol(bool term, size idx):
            term(term), idx(idx)
        {}

        constexpr size get_parse_table_idx() const
        {
            return parser::get_parse_table_idx(term, idx);
        }

        bool term;
        size idx;
    };

    struct situation_address
    {
        size rule_info_idx;
        size after;
        size t;
    };

    struct slice
    {
        size start;
        size n;
    };

    struct rule_info
    {
        size l_idx;
        size r_idx;
        size r_elements;
        size l_byte_size;
        size r_byte_size;
    };

    struct situation_queue_entry
    {
        size state_idx;
        size idx;
    };

    enum class parse_table_entry_kind : size16 { error, success, shift, reduce, rr_conflict };

    struct parse_table_entry
    {
        parse_table_entry_kind kind;
        size16 shift;
        size16 reduce;
        size8 has_shift;
        size8 has_reduce;

        constexpr void set_shift(size16 value) { shift = value; has_shift = 1; }
        constexpr void set_reduce(size16 value) { reduce = value; has_reduce = 1; }
    };

    static const size value_stack_initial_capacity = 1 << 10;
    static const size cursor_stack_initial_capacity = 1 << 10;
        
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
            value_stack.reserve(value_stack_initial_capacity * MaxValueTypeSize);
        }

        CursorStack& cursor_stack;
        ValueStack& value_stack;
        ErrorStream& error_stream;
        TraceStream& trace_stream;
    };

    constexpr static size get_parse_table_idx(bool term, size idx)
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

    template<typename Root, typename Terms, typename NTerms, typename... Rules>
    constexpr parser(
        Root root,
        Terms terms,
        NTerms nterms,
        std::tuple<Rules...> rules)
    {
        std::apply([this](auto... t) { (void(analyze_term(t)), ...); }, terms);
        analyze_term(eof{});
        std::apply([this](auto... nt) { (void(analyze_nterm(nt)), ...); }, nterms);
        analyze_nterm(fake_root<RootValueType>{});
        analyze_rules(std::index_sequence_for<Rules...>{}, root, rules);
        analyze_states();
        
        for (size x : trivial_term_table) 
            x = uninitialized;
    }

    constexpr void make_nterm_rule_slices()
    {
        size nt = 0;
        for (size i = 0u; i < rule_count; ++i)
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

    constexpr void analyze_term(eof)
    {
        term_names[eof_idx] = eof::get_name();
        term_precedences[eof_idx] = 0;
        term_associativities[eof_idx] = associativity::ltor;
    }

    constexpr void analyze_term(term<std::string_view> t)
    {
        const char* name = t.name;
        int precedence = t.precedence;
        associativity ass = t.ass;

        if (t.data[0] == 0)
        {
            name = eof::get_name();
            precedence = 0;
            ass = associativity::ltor;
        }        

        term_names[t.idx] = name;
        term_precedences[t.idx] = precedence;
        term_associativities[t.idx] = ass;
        trivial_lexical_analyzer = false;
    }

    constexpr void analyze_term(term<char> t)
    {
        term_names[t.idx] = t.name;
        term_precedences[t.idx] = t.precedence;
        term_associativities[t.idx] = t.ass;
        trivial_term_table[char_to_idx(t.data[0])] = t.idx;
    }

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

    template<typename... Rules, size... I>
    constexpr void analyze_rules(
        std::index_sequence<I...>,
        nterm<RootValueType> root,
        std::tuple<Rules...> rules)
    {
        (void(analyze_rule<I>(std::get<I>(rules), std::make_index_sequence<Rules::n>{})), ...);
        analyze_rule<root_rule_idx>(fake_root<RootValueType>{}(root), std::index_sequence<0>{});
        sort(rule_infos, [](const auto& ri1, const auto& ri2) { return ri1.l_idx < ri2.l_idx; });
        make_nterm_rule_slices();
    }

    constexpr size calculate_rule_last_term(size rule_idx, size rule_size) const
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

    constexpr int calculate_rule_precedence(int precedence, size rule_idx, size rule_size) const
    {
        if (precedence != 0)
            return precedence;
        size last_term_idx = rule_last_terms[rule_idx];
        if (last_term_idx != uninitialized)
            return term_precedences[last_term_idx];
        return 0;
    }

    template<size Nr, typename F, typename L, typename... R, size... I>
    constexpr void analyze_rule(rule<F, L, R...> r, std::index_sequence<I...>)
    {
        size l_idx = find_name(nterm_names, r.l.get_name());
        (void(right_sides[Nr][I] = make_symbol(std::get<I>(r.r))), ...);
        constexpr size rule_size = sizeof...(R);
        rule_infos[Nr] = { l_idx, Nr, rule_size, optimized_size<L>, optimized_size_sum<R...> };
        rule_last_terms[Nr] = calculate_rule_last_term(Nr, rule_size);
        rule_precedences[Nr] = calculate_rule_precedence(r.precedence, Nr, rule_size);
    }

    constexpr size make_situation_idx(situation_address a) const
    {
        return a.rule_info_idx * situation_size * term_count + a.after * term_count + a.t;
    }

    constexpr situation_address make_situation_address(size idx) const
    {
        size t = idx % term_count;
        idx /= term_count;
        size after = idx % situation_size;
        size rule_info_idx = idx / situation_size;
        return situation_address{ rule_info_idx, after, t };
    }

    constexpr void add_term_subset(term_subset& dest, const term_subset& source)
    {
        for (size i = 0u; i < term_count; ++i)
            dest[i] = dest[i] || source[i];
    }

    constexpr const term_subset& make_right_side_slice_first(const rule_info& ri, size start, term_subset& res)
    {
        for (size i = start; i < ri.r_elements; ++i)
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

    constexpr const term_subset& make_nterm_first(size nt)
    {
        if (nterm_first_analyzed[nt])
            return nterm_first[nt];
        nterm_first_analyzed[nt] = true;

        const slice& s = nterm_rule_slices[nt];
        for (size i = 0u; i < s.n; ++i)
        {
            const rule_info& ri = rule_infos[s.start + i];
            make_right_side_slice_first(ri, 0, nterm_first[nt]);
        }
        return nterm_first[nt];
    }

    constexpr bool is_right_side_slice_empty(const rule_info& ri, size start)
    {
        for (size i = start; i < ri.r_elements; ++i)
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

    constexpr bool make_nterm_empty(size nt)
    {
        if (nterm_empty_analyzed[nt])
            return nterm_empty[nt];
        nterm_empty_analyzed[nt] = true;

        const slice& s = nterm_rule_slices[nt];
        for (size i = 0u; i < s.n; ++i)
        {
            if (is_right_side_empty(rule_infos[s.start + i]))
            {
                return (nterm_empty[nt] = true);
            }
        }
        return (nterm_empty[nt] = false);
    }

    constexpr const term_subset& make_situation_first_after(const situation_address& addr, size idx)
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
        size idx = make_situation_idx(root_situation_address);
        state_count = 1;
        add_situation_to_state(0, idx);

        size n = 0;
        while (n != situation_queue_size)
        {
            analyze_situation(situation_queue[n].state_idx, situation_queue[n].idx);
            n++;
        }
    }

    constexpr void analyze_situation(size state_idx, size idx)
    {
        situation_closure(state_idx, idx);
        situation_transition(state_idx, idx);
    }

    constexpr void add_situation_to_state(size state_idx, size idx)
    {
        bool& b = states[state_idx][idx];
        if (!b)
        {
            b = true;
            situation_queue[situation_queue_size++] = { state_idx, idx };
        }
    }

    constexpr void situation_closure(size state_idx, size idx)
    {
        situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        if (addr.after >= ri.r_elements)
            return;

        const symbol& s = right_sides[ri.r_idx][addr.after];
        if (!s.term)
        {
            size nt = s.idx;
            const term_subset& first = make_situation_first_after(addr, idx);
            const slice& s = nterm_rule_slices[nt];
            for (auto i = 0u; i < s.n; ++i)
            {
                for (size t = 0; t < term_count; ++t)
                {
                    if (first[t])
                    {
                        size new_s_idx = make_situation_idx(situation_address{ s.start + i, 0, t });
                        add_situation_to_state(state_idx, new_s_idx);
                    }
                }
            }
        }
    }

    constexpr auto solve_conflict(size rule_info_idx, size term_idx) const
    {
        size rule_idx = rule_infos[rule_info_idx].r_idx;
        int r_p = rule_precedences[rule_idx];
        int t_p = term_precedences[term_idx];
        if (r_p > t_p)
            return parse_table_entry_kind::reduce;

        if (r_p == t_p && term_associativities[term_idx] == associativity::ltor)
        {
            size last_term_idx = rule_last_terms[rule_idx];
            if (last_term_idx == term_idx)
                return parse_table_entry_kind::reduce;
        }
        return parse_table_entry_kind::shift;
    }

    constexpr void situation_transition(size state_idx, size idx)
    {
        situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        bool reduction = addr.after >= ri.r_elements;

        const auto& s = right_sides[ri.r_idx][addr.after];
        size symbol_idx = reduction ? get_parse_table_idx(true, addr.t) : s.get_parse_table_idx();
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
            entry.set_reduce(size16(addr.rule_info_idx));
            return;
        }

        situation_address new_addr = situation_address{ addr.rule_info_idx, addr.after + 1, addr.t };
        size new_idx = make_situation_idx(new_addr);

        if (entry.has_shift)
        {
            add_situation_to_state(entry.shift, new_idx);
            return;
        }

        size new_state_idx = uninitialized;
        for (size i = 0; i < state_count; ++i)
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
        entry.set_shift(size16(new_state_idx));
        add_situation_to_state(new_state_idx, new_idx);
    }

    constexpr const char* get_symbol_name(const symbol& s) const
    {
        return s.term ? term_names[s.idx] : nterm_names[s.idx];
    }

    template<typename Stream>
    constexpr void write_situation_diag_str(Stream& s, size idx) const
    {
        const situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        s << nterm_names[ri.l_idx] << " <- ";
        for (size i = 0u; i < addr.after; ++i)
        {
            s << get_symbol_name(right_sides[ri.r_idx][i]) << " ";
        }
        s << ". ";
        for (size i = addr.after; i < ri.r_elements; ++i)
        {
            s << get_symbol_name(right_sides[ri.r_idx][i]) << " ";
        }
        s << "| " << term_names[addr.t];
    }

    template<typename Stream>
    constexpr void write_state_diag_str(Stream& s, size idx) const
    {
        s << "STATE " << idx << "\n";

        for (size i = 0u; i < situation_count; ++i)
        {
            if (states[idx][i])
            {
                write_situation_diag_str(s, i);
                s << "\n";
            }
        }

        s << "\n";

        for (size i = 0; i < nterm_count; ++i)
        {
            const auto& entry = parse_table[idx][i];
            if (entry.kind == parse_table_entry_kind::shift)
                s << "On " << nterm_names[i] << " go to " << entry.shift << "\n";
        }
        for (size i = nterm_count; i < nterm_count + term_count; ++i)
        {
            const auto& entry = parse_table[idx][i];
            if (entry.kind == parse_table_entry_kind::error)
                continue;

            size term_idx = i - nterm_count;            
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
        for (size i = 0; i < state_count; ++i)
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

    template<typename Buffer, typename ErrorStream, typename TraceStream>
    constexpr size get_next_term(const Buffer& buffer, typename Buffer::iterator& it, ErrorStream& error_stream, TraceStream& trace_stream) const
    {
        size res = uninitialized;
        if (it == buffer.end())
        {
            res = eof_idx;
        }
        else
        {
            char c = *it++;
            res = trivial_term_table[char_to_idx(c)];
        }

        if (res != uninitialized)
        {
            trace_stream << "Recognized " << term_names[res] << " \n";
        }
        return res;
    }

    template<typename ParserState>
    constexpr void shift(ParserState& ps, size new_cursor_value) const
    {
        ps.trace_stream << "Shift to " << new_cursor_value << "\n";
        ps.cursor_stack.push_back(new_cursor_value);
    }

    template<typename ParserState>
    constexpr void reduce(ParserState& ps, size rule_info_idx) const
    {
        ps.trace_stream << "Reduce using rule " << rule_info_idx << "\n";
        const auto& ri = rule_infos[rule_info_idx];
        ps.cursor_stack.erase(ps.cursor_stack.end() - ri.r_elements, ps.cursor_stack.end());
        size new_cursor_value = parse_table[ps.cursor_stack.back()][ri.l_idx].shift;
        ps.trace_stream << "Go to " << new_cursor_value << "\n";
        ps.cursor_stack.push_back(new_cursor_value);
    }

    template<typename ParserState>
    constexpr void rr_conflict(ParserState& ps, size rule_idx) const
    {
        ps.trace_stream << "R/R conflict encountered \n";
        reduce(ps, rule_idx);
    }

    template<typename Stream>
    constexpr void write_syntax_error_to_stream(size parse_table_cursor, Stream& stream) const
    {
        stream << "Syntax error: " << "\n";
    }

    template<typename ParserState>
    constexpr void syntax_error(ParserState& ps) const
    {
        size cursor = ps.cursor_stack.back();
        write_syntax_error_to_stream(cursor, ps.error_stream);
        write_syntax_error_to_stream(cursor, ps.trace_stream);
    }

    template<typename ParserState>
    constexpr void success(ParserState& ps) const
    {
        ps.trace_stream << "Success \n";
    }

    template<typename Buffer, typename ErrorStream, typename TraceStream>
    constexpr auto parse(const Buffer& buffer, ErrorStream& error_stream, TraceStream& trace_stream) const
    {
        parser_value_stack_type_t<Buffer, MaxValueTypeSize> value_stack{};
        parse_table_cursor_stack_type_t<Buffer> cursor_stack{};
        parser_state ps(cursor_stack, value_stack, error_stream, trace_stream);

        bool error = false;
        typename Buffer::iterator it = buffer.begin();

        ps.cursor_stack.push_back(0);
        size term_idx = uninitialized;
        bool next_term_needed = true;

        while (true)
        {
            size cursor = ps.cursor_stack.back();
            if (next_term_needed)
            {
                term_idx = get_next_term(buffer, it, error_stream, trace_stream);
                next_term_needed = false;
            }
            if (term_idx == uninitialized)
            {
                error = true;
                break;
            }

            const auto& entry = parse_table[cursor][get_parse_table_idx(true, term_idx)];
            if (entry.kind == parse_table_entry_kind::shift)
            {
                shift(ps, entry.shift);
                next_term_needed = true;
            }
            else if (entry.kind == parse_table_entry_kind::reduce)
                reduce(ps, entry.reduce);
            else if (entry.kind == parse_table_entry_kind::rr_conflict)
                rr_conflict(ps, entry.reduce);
            else if (entry.kind == parse_table_entry_kind::success)
            {
                success(ps);
                break;
            }
            else
            {
                syntax_error(ps);
                error = true;
                break;
            }
        }
        
        return error ? optional<RootValueType>() : RootValueType{};
    }
    
    name_table<term_count> term_names = { };
    name_table<nterm_count> nterm_names = { };
    symbol right_sides[rule_count][RuleSize] = { };
    rule_info rule_infos[rule_count] = { };
    slice nterm_rule_slices[nterm_count] = { };
    term_subset situation_first_after[situation_count] = { };
    bool nterm_empty[nterm_count] = { };
    term_subset nterm_first[nterm_count] = { };
    bool nterm_empty_analyzed[nterm_count] = { };
    bool nterm_first_analyzed[nterm_count] = { };
    state states[MaxStates] = { };
    parse_table_entry parse_table[MaxStates][term_count + nterm_count] = {};
    size state_count = 0;
    situation_queue_entry situation_queue[MaxStates * situation_count] = { };
    size situation_queue_size = 0;
    int term_precedences[term_count] = { }; 
    associativity term_associativities[term_count] = { associativity::ltor };
    int rule_precedences[rule_count] = { };
    size rule_last_terms[rule_count] = { };
    size trivial_term_table[distinct_values_count<char>] = { };
    bool trivial_lexical_analyzer = true;
};


template<typename RootValueType, typename... TermValueType, typename... NTermValueType, typename... Rules, size MaxStates>
parser(
    nterm<RootValueType>,
    std::tuple<term<TermValueType>...>,
    std::tuple<nterm<NTermValueType>...>,
    std::tuple<Rules...>,
    std::integral_constant<size, MaxStates>
) ->
parser<
    RootValueType, 
    sizeof...(TermValueType), 
    sizeof...(NTermValueType), 
    sizeof...(Rules), 
    max_v<Rules::n...>, 
    max_v<sizeof(TermValueType)..., sizeof(NTermValueType)...>,
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
    max_v<sizeof(TermValueType)..., sizeof(NTermValueType)...>,
    deduce_max_states<sizeof...(TermValueType), Rules::n...>
>;

template<typename... S, size... I>
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
constexpr auto make_rules(Rules... rules)
{
    return std::make_tuple(rules...);
}

struct use_string_stream
{};

template<typename StreamType>
struct diag_msg
{
    template<typename Parser, size Size>
    constexpr diag_msg(const Parser& p, message_max_size<Size>)
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

template<typename Parser, size Size>
diag_msg(const Parser&, message_max_size<Size>)->diag_msg<cstream<Size>>;

template<typename Parser>
diag_msg(const Parser&, use_string_stream)->diag_msg<std::stringstream>;

template<typename ValueType, typename ErrorStreamType, typename TraceStreamType>
struct parse_result
{
    ErrorStreamType error_stream;
    TraceStreamType trace_stream;
    optional<ValueType> value{};

    template<typename Parser, size ErrorStreamSize, size TraceStreamSize, typename Buffer>
    constexpr parse_result(const Parser& p, const Buffer& buffer, message_max_size<ErrorStreamSize>, message_max_size<TraceStreamSize>):
        parse_result(p, buffer)
    {}

    template<typename Parser, size ErrorStreamSize, typename Buffer>
    constexpr parse_result(const Parser& p, const Buffer& buffer, message_max_size<ErrorStreamSize>):
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

    constexpr bool success() const { return value.has_object(); }

    constexpr const auto& get_error_stream() const { return error_stream; }
    constexpr const auto& get_trace_stream() const { return trace_stream; }
    constexpr const auto get_value() const { return value.get_object(); }
};

struct no_stream
{
    template<typename T>
    constexpr const no_stream& operator <<(T&&) const { return *this; }
};

template<typename Parser, typename Buffer, size ErrorStreamSize, size TraceStreamSize>
parse_result(const Parser&, const Buffer&, message_max_size<ErrorStreamSize>, message_max_size<TraceStreamSize>)
->parse_result<typename Parser::root_value_type, cstream<ErrorStreamSize>, cstream<TraceStreamSize>>;

template<typename Parser, typename Buffer>
parse_result(const Parser&, const Buffer&, use_string_stream, use_string_stream)
->parse_result<typename Parser::root_value_type, std::stringstream, std::stringstream>;

template<typename Parser, typename Buffer, size ErrorStreamSize>
parse_result(const Parser&, const Buffer&, message_max_size<ErrorStreamSize>)
->parse_result<typename Parser::root_value_type, cstream<ErrorStreamSize>, no_stream>;

template<typename Parser, typename Buffer>
parse_result(const Parser&, const Buffer&, use_string_stream)
->parse_result<typename Parser::root_value_type, std::stringstream, no_stream>;

template<typename Parser, typename Buffer>
parse_result(const Parser&, const Buffer&)
->parse_result<typename Parser::root_value_type, no_stream, no_stream>;

template<size N>
struct cstring_buffer
{
    constexpr cstring_buffer(const char (&source)[N])
    {
        init(source, std::make_index_sequence<N>{});
    }
    
    template<size... I>
    constexpr auto init(const char(&source)[N], std::index_sequence<I...>)
    {
        (void(data[I] = source[I]),...);
    }

    struct iterator
    {
        const char* ptr;

        constexpr char operator *() { return *ptr; }
        constexpr iterator& operator ++() { ++ptr; return *this; }
        constexpr iterator operator ++(int) { iterator i(*this); ++ptr; return i; }
        constexpr bool operator == (const iterator& other) { return ptr == other.ptr; }
    };

    constexpr iterator begin() const { return iterator{ data }; }
    constexpr iterator end() const { return iterator{ data + N - 1 }; }

    char data[N] = { 0 };
};

template<size N>
cstring_buffer(const char(&)[N])->cstring_buffer<N>;

template<size N>
struct parse_table_cursor_stack_type<cstring_buffer<N>>
{
    using type = cvector<size, N>;
};

template<size N, size MaxValueTypeSize>
struct parser_value_stack_type<cstring_buffer<N>, MaxValueTypeSize>
{
    using type = cvector<std::byte, N * MaxValueTypeSize>;
};
