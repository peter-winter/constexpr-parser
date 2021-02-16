#include <utility>
#include <limits>
#include <tuple>
#include <algorithm>
#include <iostream>

using size = std::uint64_t;

template<size Max>
using max_states = std::integral_constant<size, Max>;

using char_2 = char[2];
constexpr size char_strings_size = std::numeric_limits<char>::max();

template<typename Container, typename V>
constexpr size find(const Container& c, const V& v)
{
    for (auto i = std::begin(c); i != std::end(c); ++i)
        if (*i == v)
            return std::distance(std::begin(c), i);
    return size(-1);
}

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
        char* d = str + current_size;
        while (*s)
        { 
            *d++ = *s++; 
            current_size++;
        }
        
        return *this; 
    }
    
    constexpr cstream& operator << (size x)
    {
        char* d = str + current_size;
        if (x == 0u)
        {
            *d = '0';
            current_size++;
            return *this;
        }

        char digits[std::numeric_limits<size>::digits10] = { 0 };
        char* digit = digits;
        while (x)
        {
            *digit++ = x % 10;
            x /= 10;
        }
        digit = digits;
        while (*digit != 0) digit++;
        digit--;

        while (digit >= digits)
        {
            *d++ = '0' + *digit--;
            current_size++;
        }
        return *this;
    }

    constexpr const char* c_str() const { return str; }

    char str[MaxSize] = { 0 };
    size current_size = 0;
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

template<typename ValueType, typename EnumType>
struct nterm
{
    constexpr nterm(EnumType e, const char* name):
        e(e), name(name)
    {}

    template<typename... Args>
    constexpr auto operator()(Args... args) const;

    EnumType e;
    const char* name;
};

template<typename ValueType, typename EnumType>
constexpr nterm<ValueType, EnumType> fake_root(EnumType::__size__, "##");

template<typename ValueType, typename EnumType, typename...R>
struct rule
{
    nterm<ValueType, EnumType> l;
    std::tuple<R...> r;
    static const size n = sizeof...(R);
};

template<typename ValueType, typename NTermType>
template<typename... Args>
constexpr auto nterm<ValueType, NTermType>::operator()(Args... args) const
{
    return rule<ValueType, NTermType, Args...>{*this, std::make_tuple(args...)};
}

template<typename ValueType, typename EnumType>
struct term
{
    constexpr term(EnumType e, const char* data):
        e(e), data(data), name(data)
    {}

    constexpr term(EnumType e, const char* data, const char* name):
        e(e), data(data), name(name)
    {}

    EnumType e;
    const char* data;
    const char* name;
};

template<typename EnumType>
constexpr term<no_value, EnumType> eof(EnumType::__size__, "$$");

template<size TermCount, size NTermCount, size RuleCount, size RuleSize, size MaxStates>
struct parser
{
    static const size term_count = TermCount + 1;
    static const size eof_idx = TermCount;
    static const size nterm_count = NTermCount + 1;
    static const size root_idx = NTermCount;
    static const size rule_count = RuleCount + 1;
    static const size situation_count = rule_count * RuleSize * term_count;
    static const size root_rule_idx = RuleCount;
    
    using term_subset = bool[term_count];    
    using state = bool[situation_count];
    
    struct symbol
    {
        constexpr symbol():
            term(false), idx(-1)
        {}

        constexpr symbol(bool term, size idx):
            term(term), idx(idx)
        {}

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
        size l;
        size r;
        size n;
    };
    
    template<
        typename... TermValueType, typename TermEnumType, 
        typename... NTermValueType, typename NTermEnumType, 
        typename RootValueType,
        typename... Rules,
        size Max
    >
    constexpr parser(
        std::integral_constant<size, Max>,
        std::tuple<term<TermValueType, TermEnumType>...> terms, 
        std::tuple<nterm<NTermValueType, NTermEnumType>...> nterms, 
        nterm<RootValueType, NTermEnumType> root,
        Rules... rules)
    {
        std::apply([this](auto... t) { (void(analyze_term(t)), ...); }, terms);
        analyze_term(eof<TermEnumType>);
        std::apply([this](auto... nt) { (void(analyze_nterm(nt)), ...); }, nterms);
        analyze_nterm(fake_root<RootValueType, NTermEnumType>);        
        analyze_rules(std::index_sequence_for<Rules...>{}, root, rules...);
        analyze_states();
        dump_state(debug_str, 0);
    }        

    template<typename... Rules, size... I, typename RootValueType, typename NTermEnumType>
    constexpr void analyze_rules(
        std::index_sequence<I...>,
        nterm<RootValueType, NTermEnumType> root,
        Rules... rules)
    {
        (void(analyze_rule<I>(rules, std::make_index_sequence<Rules::n>{})), ...);
        analyze_rule<root_rule_idx>(fake_root<RootValueType, NTermEnumType>(root), std::index_sequence<0>{});
        sort(rule_infos, [](const auto& l1, const auto& l2) { return l1.l < l2.l; });
        make_nterm_rule_slices();
    }
    
    constexpr void make_nterm_rule_slices()
    {
        size nt = 0;
        for (size i = 0u; i < rule_count; ++i)
        {
            if (nt != rule_infos[i].l)
            {
                nt = rule_infos[i].l;
                nterm_rule_slices[nt].start = i;
                nterm_rule_slices[nt].n = 1;
            }
            else
                nterm_rule_slices[nt].n++;
        }
    }

    template<typename ValueType, typename EnumType>
    constexpr void analyze_term(term<ValueType, EnumType> t)
    {
        term_names[static_cast<size>(t.e)] = t.name;
    }

    template<typename ValueType, typename EnumType>
    constexpr void analyze_nterm(nterm<ValueType, EnumType> nt)
    {
        nterm_names[static_cast<size>(nt.e)] = nt.name;
    }
    
    template<typename ValueType, typename EnumType>
    constexpr auto make_symbol(term<ValueType, EnumType> t) const
    {
        return symbol{ true, static_cast<size>(t.e) };
    }

    template<typename ValueType, typename EnumType>
    constexpr auto make_symbol(nterm<ValueType, EnumType> nt) const
    {
        return symbol{ false, static_cast<size>(nt.e) };
    }

    constexpr auto make_symbol(const char* name) const
    {
        return symbol{ true, find(term_names, name) };
    }

    template<size Nr, typename ValueType, typename EnumType, typename... R, size... I>
    constexpr void analyze_rule(rule<ValueType, EnumType, R...> r, std::index_sequence<I...>)
    {
        size l_idx = static_cast<size>(r.l.e);
        (void(right_sides[Nr][I] = make_symbol(std::get<I>(r.r))), ...);
        rule_infos[Nr] = { l_idx, Nr, sizeof...(R) };
    }

    constexpr size make_situation_idx(situation_address a) const
    {
        return rule_infos[a.rule_info_idx].r * RuleSize * term_count + a.after * term_count + a.t;
    }

    constexpr situation_address make_situation_address(size idx) const
    {
        size t = idx % term_count;
        idx /= term_count;
        size after = idx % RuleSize;
        size rule_idx = idx / RuleSize;
        return situation_address{ rule_idx, after, t};
    }

    constexpr void analyze_states()
    {
        situation_address root_situation_address{ root_rule_idx, 0, eof_idx };
        size idx = make_situation_idx(root_situation_address);
        states[0][idx] = true;
        state_count = 1;

        situation_closure(0u, idx);
    }

    constexpr void add_term_subset(term_subset& dest, const term_subset& source)
    {
        for (size i = 0u; i < term_count; ++i)
            dest[i] = source[i];
    }

    constexpr const term_subset& make_right_side_slice_first(const rule_info& ri, size start, term_subset& res)
    {
        for (size i = start; i < ri.n; ++i)
        {
            const symbol& s = right_sides[ri.r][i];
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
        for (size i = start; i < ri.n; ++i)
        {
            const symbol& s = right_sides[ri.r][i];
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

    constexpr void situation_closure(size state_idx, size idx)
    {
        situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        if (addr.after >= ri.n)
            return;

        const symbol& s = right_sides[ri.r][addr.after];
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
                        size new_s_idx = make_situation_idx(situation_address{ rule_infos[s.start + i].r, 0, t });
                        if (!states[state_idx][new_s_idx])
                        {
                            states[state_idx][new_s_idx] = true;
                            situation_closure(state_idx, new_s_idx);
                        }
                    }   
                }
            }
        }
    }

    constexpr const char* get_symbol_name(const symbol& s) const
    {
        return s.term ? term_names[s.idx] : nterm_names[s.idx];
    }

    template<typename Stream>
    constexpr void dump_situation(Stream& s, size idx)
    {
        const situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        s << nterm_names[ri.l] << " <- ";
        for (size i = 0u; i < addr.after; ++i)
        {
            s << get_symbol_name(right_sides[ri.r][i]) << " ";
        }
        s << ". ";
        for (size i = addr.after; i < ri.n; ++i)
        {
            s << get_symbol_name(right_sides[ri.r][i]) << " ";
        }
        s << "| " << term_names[addr.t];
    }

    template<typename Stream>
    constexpr void dump_state(Stream& s, size idx)
    {
        s << "STATE " << idx << "\n";

        for (size i = 0u; i < situation_count; ++i)
        {
            if (states[idx][i])
            {
                dump_situation(s, i);
                s << "\n";
            }
        }
    }

    template<typename Stream>
    constexpr void dump(Stream& s, size idx) const
    {
        s << debug_str.c_str();
    }

    const char* term_names[term_count] = { 0 };
    const char* nterm_names[nterm_count] = { 0 };    
    symbol right_sides[rule_count][RuleSize] = { };  
    rule_info rule_infos[rule_count] = { 0 };
    slice nterm_rule_slices[nterm_count] = { 0 };
    term_subset situation_first_after[situation_count] = { 0 };    
    bool nterm_empty[nterm_count] = { 0 };    
    term_subset nterm_first[nterm_count] = { 0 };
    bool nterm_empty_analyzed[nterm_count] = { 0 };    
    bool nterm_first_analyzed[nterm_count] = { 0 };
    state states[MaxStates] = { 0 };
    size state_count = 0;
    cstream<10000> debug_str;
};

template<
    typename... TermValueType, typename TermEnumType, 
    typename... NTermValueType, typename NTermEnumType, 
    typename RootValueType,
    typename... Rules,
    size MaxStates>
parser(
    std::integral_constant<size, MaxStates>,
    std::tuple<term<TermValueType, TermEnumType>...>, 
    std::tuple<nterm<NTermValueType, NTermEnumType>...>, 
    nterm<RootValueType, NTermEnumType>,
    Rules...) -> 
    parser<
        static_cast<size>(TermEnumType::__size__), 
        static_cast<size>(NTermEnumType::__size__), 
        sizeof...(Rules), 
        max_v<Rules::n...>,
        MaxStates
    >;

template<typename... T>
constexpr auto make_terms(T... ts)
{
    return std::make_tuple(ts...);
}

template<typename... NT>
constexpr auto make_nterms(NT... nts)
{
    return std::make_tuple(nts...);
}

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

