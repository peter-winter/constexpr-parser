# CTPG
## _C++ Compile Time Parser Generator_

C++ _**single header**_ library which takes a language description as a C++ code and turns it into a LR1 table parser with a deterministic finite automaton lexical analyzer, all in compile time.
What's more, the generated parser is actually itself capable of parsing in compile time.
All it needs is a C++17 compiler!

### Contents
* [Usage](#usage)
* [Explanation](#explanation)
    * [Header](#header)
    * [Namespaces](#namespaces)
    * [Terminal symbols](#terminal-symbols)
    * [Nonterminal symbols](#nonterminal-symbols)
    * [Parser definition](#parser-definition)
    * [Parse method call](#parse-method-call)
* [Compile Time Parsing](#compile-time-parsing)
* [LR(1) Parser](#lr1-parser)
   * [Algorithm](#algorithm)
   * [Conflicts](#conflicts)
   * [Precedence and associativity](#precedence-and-associativity)
* [Functors - advanced](#functors---advanced)
   * [Limitations](#limitations)
   * [Functor helpers](#functor-helpers)
* [Various features](#various-features)
   * [Verbose output](#verbose-output)
   * [Diagnostics](#diagnostics)
   * [Parse options](#parse-options)
   * [Source tracking](#source-tracking)
   * [Buffers](#buffers)
* [Regular expressions](#regular-expressions)


## Usage
Following code demonstrates a simple parser which takes a comma separated list of integer numbers as argument and prints a sum of them.

**`readme_example.cpp`**
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
            to_int,
        list(list, ',', number) 
            >= [](int sum, char, const auto& n){ return sum + to_int(n); }
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

## Explanation

### Header
```c++
#include "ctpg.hpp"
```

### Namespaces

Namespace ctpg is the top namespace. There are couple of feature namespaces like ```buffers```
```c++
using namespace ctpg;
using namespace ctpg::buffers;
```
### Terminal symbols

Terminal symbols (short: terms) are symbols used in grammar definition that are atomic blocks.
Examples of the terms from a C++ language are: identifier, '+' operator, various keywords etc.

To define a term use the one of ```char_term```, ```string_term``` and ```regex_term``` classes.

Here is the example of a regex_term with a common integer number regex pattern.

```c++
constexpr char number_pattern[] = "[1-9][0-9]*";
constexpr regex_term<number_pattern> number("number");
```
The constructor argument ```("number")``` indicates a debug name and can be omitted, however it is not advised.
Names are handy to diagnose problems with the grammar. If omitted, the name will be set to the pattern string.

>Note: the pattern needs to have a static linkage to be allowed as a template parameter. This is C++17 limitation, and CTPG does not support C++20 features yet.

**Other types of terms**

```char_term``` is used when we need to match things like a ```+``` or ```,``` operator.
```string_term``` is used when we need to match a whole string, like a language keyword.

### Nonterminal symbols

Nonterminal symbols (short: nonterms) are essentially all non atomic symbols in the grammar. 
In C++ language these are things like: expression, class definition, function declaration etc.

To define a nonterm use the ```nterm``` class.

```c++
constexpr nterm<int> list("list");  
```

The constructor argument ("list") is a debug name as well, like in the case of regex_term.
The difference is in nterms names are neccessary, because they serve as unique identifiers as well.
Therefore it is a requirement that nonterm names are unique.

Template parameter ```<int>``` in this case is a **value type**. More on this concept later.

### Parser definition

The ```parser``` class together with its template deduction guides allows to define parsers using 4 arguments:

* Grammar root - symbol which is a top level nonterm for a grammar.
* List of all terms 
* List of all nonterms
* List of rules

The ```parser``` object should be declared as ```constexpr```, which makes all the neccessary calculations of the LR(1) table parser done in compile time.


Let's break down the arguments.

```c++
constexpr parser p(
    list,
    terms(',', number),         
    nterms(list),  
```
**Grammar root.** 

When the root symbol gets matched the parse is successful.

**Term list.**

List of terms enclosed in a ```terms``` call.

> Note: we can see that there is one extra term, the ```,```.
This one is an implicit ```char_term```. The code implicitly converts the char to the ```char_term``` class.
Therefore ```char_terms``` (as well as ```string_terms```) are allowed not to be defined in advance. Their debug names are assigned to 
the them by default to a char (or a string) they represent.

**Nonterm list.**

List of terms enclosed in a ```nterms``` call.

**Rules**

List of rules enclosed in a ```rules``` call.
Each rule is in the form of:
```nonterm(symbols...) >= functor ```
The ```nonterm``` part is what's called a **left side** of the rule. The symbols are called the **right side**.

The right side can contain any number of ```nterm``` objects as well as terms (```regex_terms```, ```char_terms``` or ```string_terms```).
Terms can be in their implicit form, like ```,``` in the example. Implicit ```string_terms``` are in form of "strings".

```c++
    rules(
        list(number)
            >= to_int
        list(list, ',', number)
            >= [](int sum, char, const auto& n)
            { return sum + to_int(n); }
    )
```

The first rule ```list(number)``` indicates that the ```list``` nonterm can be parsed using a single ```number``` regex term.

The second rule uses what's know as a left recurrence. In other words, a ```list``` can be parsed as a ```list``` followed by a ```,``` and a ```number```.

**Functors**

The functors are any callables that can accept the exact number of arguments as there are symbols on the right side and return a value type of the left side.
Each nth argument needs to accept a value of a **value type** of the nth right side symbol.

So in the case of the first ```to_int``` functor, it is required to accept a value type of ```regex_term``` and return an ```int```.

The second functor is a lambda which accepts 3 arguments: an ```int``` for the ```list```, a ```char``` for the ```,``` and auto for whatever is passed as
a value type for the ```regex_term```.

>Note: Functors are called in a way that allows taking advantage of move semantics, so defining it's arguments as a move reference is encouraged.

**Value types for terms**

Terms unlike nonterms (which have their value types defined as a template parameter to the nterm definition),
have their value types predefined to either a ```term_value<char>``` for a ```char_term```, and a ```term_value<std::string_view>``` 
for both ```regex_term``` and ```string_term```.

The ```term_value``` class template is a simple wrapper that is implicitly convertible to it's template parameter (either a ```char``` or ```std::string_view```).
That's why when providing functors we can simply declare arguments as either a ```char``` or a ```std::string_view```.
Of course an ```auto``` in case of lambda will always do the trick.

The advantage of declaring functor arguments as ```term_value``` is that we can access other features (like source tracking) using the ```term_value``` methods.

### Parse method call

Use ```parse``` method with 2 argumets:
- a buffer
- an error stream

**Buffers**

Use a string_buffer from a ```buffers``` namespace to parse a null terminated string or a ```std::string```.

**Error stream**

Stream reference like ``std::cerr`` or any other ```std::ostream``` can be pased as a stream argument.
This is the place where the ```parse``` method is going to spit out error messages like a syntax error.

```c++
auto res = p.parse(string_buffer(argv[1]), std::cerr);
```

**Parse return value**

The ```parse``` method returns an ```std::optional<T>```, where ```T``` is a value type of the root symbol.
Use the ```.has_value()``` and the ```.value()``` to check and access the result of the parse.

## Compile time parsing

Example code can be easily changed to create an actual constexpr parser.
First, all the functors need to be constexpr. 
To achieve this change the ```to_int``` function to:

```c++
constexpr int to_int(const std::string_view& sv)
{
    int sum = 0;
    for (auto c : sv) { sum *= 10; sum += c - '0'; }
    return sum;
}
```

The function is now _constexpr_. The ```<charconv>``` header is now unneccessary.

>Note: To allow _constexpr_ parsing all of the nonterm value types have to be _literal types_.

Also change the _main_ to use ```cstring_buffer``` and declare a parse result _constexpr_.
The error stream argument is also unavailable in _constexpr_ parsing.

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

Now when no argument is passed to the program, it prints the compile time result of parsing "1, 20, 3". 

```sh
g++ readme_example.cpp -std=c++17 -o example && example
```
should print the number 24.

### Invalid input in _constexpr_ parsing
If the ```example_text``` variable was an invalid input, the code ```cres.value()```
would throw, because the ```cres``` is of type ```std::optional<int>``` with no value.


Changing the ```parse``` call to:
```
constexpr int cres = p.parse(cstring_buffer(example_text)).value();
```
would cause compilation error, because throwing ```std::bad_optional_access``` is not _constexpr_.

## LR(1) parser
   
CTPG uses a LR(1) parser. This is short from left-to-right and 1 lookahead symbol.

### Algorithm

The parser uses a parse table which is somewhat resebling a state machine. 
Here is pseudo code for the algorithm:

```
struct entry
   int next          // valid if shift
   int rule_length   // valid if reduce
   int nterm_nr      // valid if reduce   
   enum kind {success, shift, reduce, error }
   
bool parse(input, sr_table[states_count][terms_count], goto_table[states_count][nterms_count])
   state = 0
   states.push(state)
   needs_term = true;
   
   while (true)
      if (needs_term)
         term_nr = get_next_term(input)
      entry = sr_table[state, term_nr]
      kind = entry.kind
           
      if (kind == success)
         return true
         
      else if (kind == shift)
         needs_term = true;
         state = entry.next
         states.push(state)
         continue
         
      else if (kind == reduce)
         states.pop_n(entry.rule_length)
         state = states.top()
         state = goto_table[state, entry.nterm_nr]
         continue
         
      else
         return false
```

Parser contains a state stack, which grows when the algorithm encounters a _shift_ operation and shrinks on _reduce_ operation.

Aside from a state stack, there is also a value stack for dedicated for parse result calculation. 
Each _shift_ pushes a value to the stack and each _reduce_ calls an appropriate functor with values from a value stack, removing values from a stack and replacing them with a single value associated with a rule's left side.

**Table creation**

This topic is out of scope of this manual. There is plenty of material online on LR parsers. 
Recomended book on the topic: [Compilers: Principles, Techniques and Tools](https://en.wikipedia.org/wiki/Compilers:_Principles,_Techniques,_and_Tools)

### Conflicts

There are situations (parser states) in which when a particualr term is encountered on the input, there is an ambiguity regarding the operation a parser should perform.

In other words a language grammar may be defined in such a way, that both _shift_ and _reduce_ can lead to a successfull parse result, however the result will be different in both cases.

**Example 1**

Consider a classic expression parser (functors omitted for clarity):

```
constexpr parser p(
    expr,
    terms('+', '*', number),
    nterms(expr),
    rules(
        expr(number),
        expr(expr, '+', expr),
        expr(expr, '*', expr)
    )
);

```

Consider ```2 + 2 * 2``` input being parsed and a parser in a state after successfully matching ```2 + 2``` and encountering ```*``` term.

Both _shifting_ a ```*``` term and _reducing_ by the rule expr(expr, '+', expr) would be valid, however would produce different results.
This is a classic operator **precedence** case, and this conflict needs to be resolved somehow. This is where precedence and associativity take place.

### Precedence and associativity

CTPG parsers can resolve such conflict based on precedence and associativity rules defined in a grammar.

Example above can be fixed by explicit term definitions.

Normally, ```char_terms``` can be introduced by implicit definition in the ```terms``` call. However when in need to define a precedence, explicit definition is required.

Simply change the code to:

``` c++
char_term o_plus('+', 1);  // precedence set to 1
char_term o_mul('*', 2);   // precedence set to 2

constexpr parser p(
    expr,
    terms(o_plus, o_mul, number),
    nterms(expr),
    rules(
        expr(number),
        expr(expr, '+', expr),      // note: no need for o_plus and o_mul in the rules, however possible
        expr(expr, '*', expr)
    )
 );
```

The higher the precedence value set, the higher the term precedence. Default term precedence is equal to 0. 

This explicit precedence definition allows a ```*``` operator to have bigger precedence over ```+```.

**Example 2**

``` c++
char_term o_plus('+', 1);  // precedence set to 1
char_term o_minus('-', 1);  // precedence set to 1
char_term o_mul('*', 2);   // precedence set to 2

constexpr parser p(
    expr,
    terms(o_plus, o_minus, o_mul, number),
    nterms(expr),
    rules(
        expr(number),
        expr(expr, '+', expr),
        expr(expr, '-', expr),   // extra rule allowing binary -
        expr(expr, '*', expr),
        expr('-', expr)          // extra rule allowing unary -
    )
 );
```

Binary ```-``` and ```+``` operators have the same precedence in pretty much all languages.
Unary ```-``` however almost always have a bigger precedence than all binary operators. 
We can't achieve this by simply defining ```-``` precedence in ```char_term``` definition.
We need a way to tell that ```expr('-', expr)``` has a bigger precedence then all binary rules.

To achieve this override the precedence in a term by a precedence in a rule changing:

```expr('-', expr)```
to
```expr('-', expr)[3]```

The ```[]``` operator allows exactly this. It explicitly sets the rule precedence so the parser does not have to deduce rule precedence from a term.

So the final code looks like this:

``` c++
char_term o_plus('+', 1);  // precedence set to 1
char_term o_minus('-', 1);  // precedence set to 1
char_term o_mul('*', 2);   // precedence set to 2

constexpr parser p(
    expr,
    terms(o_plus, o_minus, o_mul, number),
    nterms(expr),
    rules(
        expr(number),
        expr(expr, '+', expr),
        expr(expr, '-', expr),   // extra rule allowing binary -
        expr(expr, '*', expr),
        expr('-', expr)[3]       // extra rule allowing unary -, with biggest precedence
    )
 );
```

**Example 3**

Consider the final code and let's say the input is ```2 + 2 + 2```, parser has read ```2 + 2``` and is about to read the second ```+```.
In this case what is the required behaviour? Should the first ```2 + 2``` be reduced or a second ```+``` should be shifted?

This is the classis **associativity** case which can be solved by expicitly defining the term associativity. 

There are 3 types of associativity available: _left to right_, _right_to_left_ and _no_assoc_ as the default.

## Functors - advanced

### Limitations

### Functor helpers

## Various features

### Verbose output

### Diagnostics

### Parse options

### Source tracking

### Buffers

## Regular expressions
