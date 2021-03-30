#include "../ctpg.hpp"

#include <iostream>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

struct word_t
{
    std::string w;
    source_point sp;
};

using text_t = std::vector<word_t>;

auto&& add_word(text_t&& txt, std::string_view sv, source_point sp)
{
    txt.push_back(word_t{std::string(sv), sp});
    return std::move(txt);
}

constexpr char word_pattern[] = "[A-Za-z]+";
constexpr regex_term<word_pattern> word("word");
constexpr nterm<text_t> text("text");

constexpr parser p(
    text,
    terms(word),
    nterms(text),
    rules(
        text() >= create<text_t>{},
        text(text, word) >= [](auto&& txt, const auto& w) { return add_word(std::move(txt), w.get_value(), w.get_sp()); }
    )
);

int main(int argc, char* argv[])
{
    if (argc < 2)
        return -1;
    auto res = p.parse(string_buffer(argv[1]), std::cout);
    if (res.has_value())
    {
        for (const auto& w : res.value())
        {
            std::cout << w.w << " at " << w.sp << std::endl;
        }
    }
    return 0;
}
