#include <utility>
#include <cstdint>
#include <limits>
#include <tuple>
#include <algorithm>

using size = std::uint64_t;
using size16 = std::uint16_t;

template<size S>
using max_states = std::integral_constant<size, S>;

template<size S>
using diag_message_size = std::integral_constant<size, S>;

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

struct no_diag_str
{};

template<size MaxSize>
struct cstream
{
    constexpr cstream& operator << (const char* source)
    {
        const char* s = source;
        char* d = str + current_size;
        while (*s)
        {
            str[current_size++] = *s++;
        }

        return *this;
    }

    constexpr cstream& operator << (size x)
    {
        if (x == 0u)
        {
            str[current_size++] = '0';
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
            str[current_size++] = '0' + digits[i];
        }
        return *this;
    }

    constexpr const char* c_str() const { return str; }

    char str[MaxSize] = { 0 };
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

template<typename ValueType>
struct nterm
{
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

    size idx = (size)-1;
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
struct rule
{
    L l;
    std::tuple<R...> r;
    static const size n = sizeof...(R);
};

template<typename ValueType>
template<typename... Args>
constexpr auto nterm<ValueType>::operator()(Args... args) const
{
    return rule<nterm<ValueType>, Args...>{*this, std::make_tuple(args...)};
}

template<typename ValueType>
template<typename... Args>
constexpr auto fake_root<ValueType>::operator()(Args... args) const
{
    return rule<fake_root<ValueType>, Args...>{*this, std::make_tuple(args...)};
}

template<typename ValueType = no_value>
struct term
{
    constexpr term(const char* data):
        data(data), name(data)
    {}

    constexpr term(const char* data, const char* name):
        data(data), name(name)
    {}

    constexpr void set_idx(size idx)
    {
        this->idx = idx;
    }

    constexpr const char* get_name() const { return name; }

    size idx = -1;
    const char* data;
    const char* name;
};

struct eof
{
    constexpr static const char* get_name() { return "$$"; }
};

template<size TermCount, size NTermCount, size RuleCount, size RuleSize, size MaxStates, typename DiagStr>
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

    struct symbol
    {
        constexpr symbol() :
            term(false), idx(-1)
        {}

        constexpr symbol(bool term, size idx) :
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

    struct situation_queue_entry
    {
        size state_idx;
        size idx;
    };

    enum class parse_table_entry_kind : size16 { next_state, reduce, error, success };

    struct parse_table_entry
    {
        parse_table_entry_kind kind = { parse_table_entry_kind::error };
        size16 value = size16(-1);
    };

    template<typename... TermValueType, typename... NTermValueType, typename RootValueType, typename... Rules, size MaxStates1, size DiagStrSize1>
    constexpr parser(
        std::tuple<term<TermValueType>...> terms,
        std::tuple<nterm<NTermValueType>...> nterms,
        nterm<RootValueType> root,
        std::tuple<Rules...> rules,
        std::integral_constant<size, MaxStates1> max_states,
        std::integral_constant<size, DiagStrSize1> diag_str_size):
        parser(terms, nterms, root, rules)
    {
        write_diag_str(diag_str);
    }

    template<typename... TermValueType, typename... NTermValueType, typename RootValueType, typename... Rules, size MaxStates1>
    constexpr parser(
        std::tuple<term<TermValueType>...> terms,
        std::tuple<nterm<NTermValueType>...> nterms,
        nterm<RootValueType> root,
        std::tuple<Rules...> rules,
        std::integral_constant<size, MaxStates1> max_states):
        parser(terms, nterms, root, rules)
    {}

    template<typename... TermValueType, typename... NTermValueType, typename RootValueType, typename... Rules>
    constexpr parser(
        std::tuple<term<TermValueType>...> terms,
        std::tuple<nterm<NTermValueType>...> nterms,
        nterm<RootValueType> root,
        std::tuple<Rules...> rules)
    {
        std::apply([this](auto... t) { (void(analyze_term(t)), ...); }, terms);
        analyze_term(eof{});
        std::apply([this](auto... nt) { (void(analyze_nterm(nt)), ...); }, nterms);
        analyze_nterm(fake_root<RootValueType>{});
        analyze_rules(std::index_sequence_for<Rules...>{}, root, rules);
        analyze_states();
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

    constexpr void analyze_term(eof)
    {
        term_names[eof_idx] = eof::get_name();
    }

    template<typename ValueType>
    constexpr void analyze_term(term<ValueType> t)
    {
        term_names[t.idx] = t.name;
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
        return symbol{ true, find(term_names, t.name) };
    }

    template<typename ValueType>
    constexpr auto make_symbol(nterm<ValueType> nt) const
    {
        return symbol{ false, find(nterm_names, nt.name) };
    }

    constexpr auto make_symbol(const char* name) const
    {
        return symbol{ true, find(term_names, name) };
    }

    template<typename... Rules, size... I, typename RootValueType>
    constexpr void analyze_rules(
        std::index_sequence<I...>,
        nterm<RootValueType> root,
        std::tuple<Rules...> rules)
    {
        (void(analyze_rule<I>(std::get<I>(rules), std::make_index_sequence<Rules::n>{})), ...);
        analyze_rule<root_rule_idx>(fake_root<RootValueType>{}(root), std::index_sequence<0>{});
        sort(rule_infos, [](const auto& l1, const auto& l2) { return l1.l < l2.l; });
        make_nterm_rule_slices();
    }

    template<size Nr, typename L, typename... R, size... I>
    constexpr void analyze_rule(rule<L, R...> r, std::index_sequence<I...>)
    {
        size l_idx = find(nterm_names, r.l.get_name());
        (void(right_sides[Nr][I] = make_symbol(std::get<I>(r.r))), ...);
        rule_infos[Nr] = { l_idx, Nr, sizeof...(R) };
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
                        size new_s_idx = make_situation_idx(situation_address{ s.start + i, 0, t });
                        add_situation_to_state(state_idx, new_s_idx);
                    }
                }
            }
        }
    }

    constexpr void situation_transition(size state_idx, size idx)
    {
        situation_address addr = make_situation_address(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        if (addr.after >= ri.n)
        {
            parse_table_entry_kind kind = (ri.r == root_rule_idx ? parse_table_entry_kind::success : parse_table_entry_kind::reduce);
            parse_table[state_idx][nterm_count + addr.t] = parse_table_entry{ kind, size16(ri.r) };
            return;
        }

        situation_address new_addr = situation_address{ addr.rule_info_idx, addr.after + 1, addr.t };
        size new_idx = make_situation_idx(new_addr);
        const symbol& s = right_sides[ri.r][addr.after];
        size symbol_idx = s.term ? nterm_count + s.idx : s.idx;

        if (parse_table[state_idx][symbol_idx].kind == parse_table_entry_kind::next_state)
        {
            size new_state_idx = parse_table[state_idx][symbol_idx].value;
            add_situation_to_state(new_state_idx, new_idx);
            return;
        }

        size new_state_idx = size(-1);
        for (size i = 0; i < state_count; ++i)
        {
            if (states[i][new_idx])
            {
                new_state_idx = i;
                break;
            }
        }
        if (new_state_idx == size(-1))
        {
            new_state_idx = state_count;
            ++state_count;
        }
        parse_table[state_idx][symbol_idx] = parse_table_entry{ parse_table_entry_kind::next_state, size16(new_state_idx) };
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
            if (parse_table[idx][i].kind == parse_table_entry_kind::next_state)
                s << "On " << nterm_names[i] << " -> " << parse_table[idx][i].value << "\n";
        }
        for (size i = nterm_count; i < nterm_count + term_count; ++i)
        {
            if (parse_table[idx][i].kind == parse_table_entry_kind::next_state)
                s << "On " << term_names[i - nterm_count] << " -> " << parse_table[idx][i].value << "\n";
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

    constexpr const char* get_diag_msg() const
    {
        return diag_str.c_str();
    }

    template<typename Stream>
    constexpr void output_diag_msg(Stream& s) const
    {
        write_diag_str(s);
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
    parse_table_entry parse_table[MaxStates][term_count + nterm_count] = {};
    size state_count = 0;
    situation_queue_entry situation_queue[MaxStates * situation_count] = { 0 };
    size situation_queue_size = 0;
    DiagStr diag_str;
};

template<
    typename... TermValueType, typename... NTermValueType, typename RootValueType, typename... Rules, 
    size MaxStates, size DiagStrSize
>
parser(
    std::tuple<term<TermValueType>...>,
    std::tuple<nterm<NTermValueType>...>,
    nterm<RootValueType>,
    std::tuple<Rules...>,
    std::integral_constant<size, MaxStates>,
    std::integral_constant<size, DiagStrSize>
) -> 
parser<sizeof...(TermValueType), sizeof...(NTermValueType), sizeof...(Rules), max_v<Rules::n...>, MaxStates, cstream<DiagStrSize>>;

template<
    typename... TermValueType, typename... NTermValueType, typename RootValueType, typename... Rules,
    size MaxStates
>
parser(
    std::tuple<term<TermValueType>...>,
    std::tuple<nterm<NTermValueType>...>,
    nterm<RootValueType>,
    std::tuple<Rules...>,
    std::integral_constant<size, MaxStates>
) ->
parser<sizeof...(TermValueType), sizeof...(NTermValueType), sizeof...(Rules), max_v<Rules::n...>, MaxStates, no_diag_str>;

template<typename... S, size... I>
constexpr auto make_symbols_impl(std::index_sequence<I...>, S... s)
{
    (void(s.set_idx(I)), ...);
    return std::make_tuple(s...);
}

constexpr auto deduce_term(const char* data)
{
    return term<no_value>(data);
}

template<typename ValueType>
constexpr auto deduce_term(term<ValueType> t)
{
    return t;
}

template<typename... T>
constexpr auto make_terms(T... ts)
{
    return make_symbols_impl(std::index_sequence_for<T...>{}, deduce_term(ts)...);
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