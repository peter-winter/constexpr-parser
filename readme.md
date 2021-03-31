# CTPG
## _C++ Compile Time Parser Generator_

C++ _**single header only**_ library which takes a language description as a C++ code and turns it into a LR1 table parser with a deterministic finite automaton lexical analyzer, all in compile time.
What's more, the generated parser is actually itself capable of parsing in compile time.
All it needs is a C++17 compiler!

## Usage
Following code demonstrates a simple parser which takes a comma separated list of integer numbers as argument and prints a sum of them.

#### **`readme_example.cpp`**
```c++
#include "ctpg.hpp"
#include <iostream>
#include <charconv>

using namespace ctpg;
using namespace ctpg::buffers;

constexpr nterm<int> list("list");

constexpr char number_pattern[] = "[1-9][0-9]*";
constexpr regex_term<number_pattern> number("number");

int to_int(const std::string_view& sv)
{
    int i = 0;
    std::from_chars(sv.data(), sv.data() + sv.size(), i);
    return i;
}

constexpr parser p(
    list,
    terms(',', number),
    nterms(list),
    rules(
        list(number) 
            >= [](const auto& sv) { return to_int(sv); },
        list(list, ',', number) 
            >= [](int sum, skip, const auto& sv){ return sum + to_int(sv); }
    )
);

int main(int argc, char* argv[])
{
    if (argc < 2)
        return -1;
    auto res = p.parse(string_buffer(argv[1]), std::cerr);
    bool success = res.has_value();
    if (success)
        std::cout << res.value() << std::endl;
    return success ? 0 : -1;
}
```

Compile and run:

```sh
g++ readme_example.cpp -std=c++17 -o example && example "1, 2, 3"
```

You should see the output : 6. If incorrect text supplied as an argument:

```sh
g++ readme_example.cpp -std=c++17 -o example && example "1, 2, 3x"
```
you should see:
```
[1:8] PARSE: Unexpected character: x
```

### Compile time  parsing

Above code can be easily changed to create an actual constexpr parser.
Change the ```to_int``` function to:

```c++
constexpr int to_int(const std::string_view& sv)
{
    int sum = 0;
    for (auto c : sv) { sum *= 10; sum += c - '0'; }
    return sum;
}
```

The function is now _constexpr_.

Also change the _main_ to:

```c++
int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        constexpr char example_text[] = "1, 2, 3";
        constexpr auto cres = p.parse(cstring_buffer(example_text));
        std::cout << cres.value() << std::endl;
        return 0;
    }
        
    auto res = p.parse(string_buffer(argv[1]), std::cerr);
    bool success = res.has_value();
    if (success)
        std::cout << res.value() << std::endl;
    return success ? 0 : -1;
}
```

Now when no argument specified it prints the compile time result of parsing "1, 2, 3". 

```sh
g++ readme_example.cpp -std=c++17 -o example && example
```
should print the number 6.

If the ```example_text``` variable was an invalid input, the code ```cres.value()```
would throw, because the ```cres``` is of type ```std::optional<int>``` with no value.


Changing the ```parse``` call to:
```
constexpr int cres = p.parse(cstring_buffer(example_text)).value();
```
would cause compilation error, because throwing ```std::bad_optional_access``` is not _constexpr_.
