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
g++ readme_example.cpp -std=c++17 -o example && example "10, 20, 30"
```

You should see the output : 60. If incorrect text supplied as an argument:

```sh
g++ readme_example.cpp -std=c++17 -o example && example "1, 2, 3x"
```
you should see:
```
[1:8] PARSE: Unexpected character: x
```

### Explanation

```c++
//Include the only header
#include "ctpg.hpp"

// These two are for this example purposes
#include <iostream>
#include <charconv>

using namespace ctpg;           // use the top namespace
using namespace ctpg::buffers;  // and the one for buffers

//Define nonterminal symbols used in the grammar, here just one is enough
// - the argument passed to constructor is a name, 
//     name is used in debug features - therefore it should be meaningfull
//     name also serves as an identifier - therefore it should be unique for all nonterminal symbols
// - template parameter is a value type of a nonterminal symbol
//     whenever a nonterminal is reduced by a LR parser, value of this type is put on LR parser stack
constexpr nterm<int> list("list");  


constexpr char number_pattern[] = "[1-9][0-9]*";
// define the terminal numbers, here just one is enough
// - regex patterns should have the static linkage to allow them as a non-type template parameter
//     this is relaxed in c++20 which allows string constants to be passed as template parameters
//     for now CTPG allow only c++17 syntax
// - the argument passed to constructor is a name, 
//    name is used in debug features - therefore it should be meaningfull
//    name also serves as an identifier - therefore it should be unique for all regex terminal symbols
constexpr regex_term<number_pattern> number("number");

// Function converting a value type of a regex terminal (always convertible to std::string_view) to an int
int to_int(const std::string_view& sv)
{
    int i = 0;
    // return value of 'from_chars' can be safely ignored, the input is guaranteed to be valid at this point
    std::from_chars(sv.data(), sv.data() + sv.size(), i);
    return i;
}

// Parser definition
constexpr parser p(
    list,                           // root symbol of the grammar
    terms(',', number),             // all terminal symbols used in the grammar, 
                                    // notice the ',', which is a char terminal
                                    // and can be used without previous definition, unlike regex terminals
    nterms(list),                   // all nonterminal symbols
    rules(                          // rules definition
        list(number)                // single rule, a list can be a single number
            >= [](const auto& n)   // functor to be called when this rule is reduced in LR parsing
            { return to_int(n); }, // arguments passed to a functor are previously reduced symbols from a stack
                                    
        list(list, ',', number)     // another single rule, this time a list is defined using left recurrence
            >= [](int sum, char, const auto& n)    // another functor, this time 3 symbols in a rule
            { return sum + to_int(n); }                                            
    )
);

int main(int argc, char* argv[])
{
    if (argc < 2)
        return -1;
    
    // First argument is a buffer object
    // Second argument is an error stream
    // Parse result is of type std::optional<int>, because root symbol (list) value type is 'int'
    auto res = p.parse(string_buffer(argv[1]), std::cerr);
    
    bool success = res.has_value();
    if (success)
        std::cout << res.value() << std::endl;
    return success ? 0 : -1;
}
```

### Compile time parsing

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

The function is now _constexpr_. The ```<charconv>``` header is now unneccessary.

Also change the _main_ to:

```c++
int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        constexpr char example_text[] = "1, 20, 3";
        
        constexpr auto cres = p.parse(cstring_buffer(example_text)); // notice cstring_buffer and no std::err output
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

Now when no argument specified it prints the compile time result of parsing "1, 20, 3". 

```sh
g++ readme_example.cpp -std=c++17 -o example && example
```
should print the number 24.

#### Invalid input in _constexpr_ parsing
If the ```example_text``` variable was an invalid input, the code ```cres.value()```
would throw, because the ```cres``` is of type ```std::optional<int>``` with no value.


Changing the ```parse``` call to:
```
constexpr int cres = p.parse(cstring_buffer(example_text)).value();
```
would cause compilation error, because throwing ```std::bad_optional_access``` is not _constexpr_.
