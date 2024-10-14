# python3-ps_tokenizer
Tokenize powershell source scripts with python. Returns a list of PowerShellToken dataclasses, which contain the type, string, start and stop indexes

# PowerShell Source Tokenizer
Tokenize powershell source scripts.
Returns a list of PowerShellToken dataclasses, which contain the type, string, start and stop indexes

```Python3
@dataclass
class PowerShellToken:
    string:str
    type:  str
    start: int 
    end:   int
```

# Usage
```Python3
from ps_tokenizer import tokenize

with open("<some-powershell-script>.ps1", "r") as f:
    src = f.read()
tokens = tokenize(src)

```


# LEXER

The lexical grammar shows how Unicode characters are combined to form line terminators, comments, white space, and tokens


# SYNTACTIC
 The syntactic grammar shows how the tokens resulting from the lexical grammar are combined to form PowerShell scripts


## NonTokens

### Whitespace

Description:
```
White space consists of any sequence of one or more whitespace characters.
Except for the fact that white space may act as a separator for tokens, it is ignored.
```


## Tokens

A token is the smallest lexical element within the PowerShell language.

Tokens can be separated by new-lines, comments, white space, or any combination thereof.


### Keyword
```
keyword: one of
    begin          break          catch       class
    continue       data           define      do
    dynamicparam   else           elseif      end
    exit           filter         finally     for
    foreach        from           function    if
    in             inlinescript   parallel    param
    process        return         switch      throw
    trap           try            until       using
    var            while          workflow
```
Description:

A keyword is a sequence of characters that has a special meaning when used in a context-dependent place. Most often, this is as the first token in a statement; however, there are other locations, as indicated by the grammar. (A token that looks like a keyword, but is not being used in a keyword context, is a command-name or a command-argument.)


### Future Keywords
The keywords ```class```, ```define```, ```from```, ```using```, and ```var``` are reserved for future use.

