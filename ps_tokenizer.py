import unicodedata
import re
from dataclasses import dataclass



SimpleVarPattern = r'\$\$|\$\?|\$\^|\$[a-zA-Z_][\w]*|@[a-zA-Z_][\w]*'
BracedVarPattern = r'\${[a-zA-Z_][\w]*}'
EscapedBracedVarPattern =  r'\${[^}`]*(`.)*[^}`]*}'
VariablePatterns = [SimpleVarPattern, BracedVarPattern, EscapedBracedVarPattern]

KEYWORDS = [
    "begin", "break", "catch", "class", "continue", "data", 
    "define", "do", "dynamicparam", "else", "elseif", "end", 
    "exit", "filter", "finally", "for", "foreach", "from", 
    "function", "if", "in", "inlinescript", "parallel", "param", 
    "process", "return", "switch", "throw", "trap", "try", 
    "until", "using", "var", "while", "workflow"
]

##########################################
# TOKEN CHECKS
##########################################

def is_whitespace(char_sequence: str) -> bool:
    # Check if the sequence starts with a backtick followed by a newline
    if char_sequence == "`\n":
        return True
    for char in char_sequence:
        # Check for Unicode categories Zs (space separator), Zl (line separator), Zp (paragraph separator)
        if unicodedata.category(char) in ['Zs', 'Zl', 'Zp']:
            return True
        # Check for specific whitespace characters
        if char in ['\t', '\v', '\f']:  # U+0009 (tab), U+000B (vertical tab), U+000C (form feed)
            return True

def is_keyword(char_sequence:str):
    return char_sequence in KEYWORDS

def is_variable(char_sequence:str):
    for pat in VariablePatterns:
        m = re.match(pat, char_sequence)
        if m:
            return True

def is_command(char_sequence:str):
    invalid_chars = '{}();,|&$`\n"\' \t'
    expandable_string_literal = r'".*?"'  # Simple quoted string
    verbatim_here_string_literal = r'\'\'\'[\s\S]*?\'\'\''  # Triple quoted string (verbatim)
    variable_patterns = r'\$\$|\$\?|\$\^|\$[a-zA-Z_][\w]*|@[a-zA-Z_][\w]*|\${[a-zA-Z_][\w]*}'
    
    def is_generic_token_char(char: str) -> bool:
        if char in invalid_chars:
            return False
        # Check if the character is a space or whitespace
        if char.isspace():
            return False
        # Otherwise, it's a valid token character
        return True

    def is_generic_token(text: str) -> bool:
        pos = 0
        length = len(text)
        
        # Loop through the text to identify each part of the generic-token
        while pos < length:
            # Check for expandable string literal
            expandable_match = re.match(expandable_string_literal, text[pos:])
            if expandable_match:
                pos += len(expandable_match.group(0))
                continue
            # Check for verbatim here-string literal
            verbatim_match = re.match(verbatim_here_string_literal, text[pos:])
            if verbatim_match:
                pos += len(verbatim_match.group(0))
                continue
            # Check for variable
            variable_match = re.match(variable_patterns, text[pos:])
            if variable_match:
                pos += len(variable_match.group(0))
                continue
            # Check if it's a valid generic-token-char
            if is_generic_token_char(text[pos]):
                pos += 1
                continue
            # If none of the conditions match, it's not a valid generic token
            return False
        
        return True
    
    return is_generic_token(char_sequence)

def is_command_parameter(char_sequence:str):
    invalid_chars = '{}();,|&.[\n '
    
    def is_first_parameter_char(char):
        if char in ['_', '?']:
            return True
        category = unicodedata.category(char)
        return category in ['Lu', 'Ll', 'Lt', 'Lm', 'Lo']
    
    def is_parameter_char(char):
        if char in invalid_chars or char == ':' or char.isspace():
            return False
        return True
    
    if not char_sequence.startswith('-'):
        return False
    
    pos = 1  # Skip the dash
    length = len(char_sequence)
    
    # Check for first-parameter-char
    if pos >= length or not is_first_parameter_char(char_sequence[pos]):
        return False
    pos += 1
    
    # Check for subsequent parameter-chars
    while pos < length and is_parameter_char(char_sequence[pos]):
        pos += 1
    
    # Check if there's an optional colon at the end
    if pos < length and char_sequence[pos] == ':':
        pos += 1
    
    # If we've consumed all the characters, it's valid
    return pos == length

def is_real_literal(chars:str):
    real_literal_pattern = (
    r'^([+-]?\d+\.\d+|^\.\d+|[+-]?\d+)([eE][+-−–—⸺]?\d+)?[dl]?\s*(kb|mb|gb|tb|pb)?$'
    )
    if re.match(real_literal_pattern, chars):
        return True

def is_decimal_literal(chars:str):
    decimal_integer_literal_pattern = r'^[+-]?\d+([lL]?)\s*(kb|mb|gb|tb|pb)?$'
    if re.match(decimal_integer_literal_pattern, chars):
        return True

def is_hexidecimal_literal(chars:str):
    hexadecimal_integer_literal_pattern = r'^0x[0-9a-fA-F]+([lL]?)\s*(kb|mb|gb|tb|pb)?$'
    if re.match(hexadecimal_integer_literal_pattern, chars):
        return True

def is_string_literal(chars:str):
    # Regex patterns for different string literals
    double_quote_pattern = r'["\u201C\u201D\u201E]'
    single_quote_pattern = r"[\'\u2018\u2019\u201A\u201B]"

    expandable_string_literal_pattern = (
        fr'{double_quote_pattern}'         # Opening double-quote
        r'[^$"`]*'                         # Any character except $, ", ` (unescaped)
        r'(?:\{\{[^}}]*\}\}|'              # Braced variables like {{...}} with escaping
        r'\$[^{(\u201C\u201D\u201E`]*|'    # Dollar sign followed by any character except curly braces, special quotes, or backtick
        r'\$\{[^{{(\u201C\u201D\u201E`]*\})*'  # Dollar-braced variables like ${...} with escaped closing }
        fr'{double_quote_pattern}'          # Closing double-quote
    )

    expandable_here_string_literal_pattern = (
        fr'@{double_quote_pattern}\s*\n[^$\n]*\n{double_quote_pattern}@'
    )

    verbatim_string_literal_pattern = (
        fr'{single_quote_pattern}[^\'\u2018\u2019\u201A\u201B]*'
        fr'(?:{single_quote_pattern}{single_quote_pattern})?{single_quote_pattern}'
    )

    verbatim_here_string_literal_pattern = (
        fr'@{single_quote_pattern}\s*\n[^\n]*\n{single_quote_pattern}@'
    )
    
    if re.match(expandable_string_literal_pattern, text):
        return True
    # Check for expandable here string literal
    elif re.match(expandable_here_string_literal_pattern, text):
        return True
    # Check for verbatim string literal
    elif re.match(verbatim_string_literal_pattern, text):
        return True
    # Check for verbatim here string literal
    elif re.match(verbatim_here_string_literal_pattern, text):
        return True

def is_literal(chars:str):
    funcs = [is_decimal_literal, is_hexidecimal_literal, is_real_literal, is_string_literal]
    for f in funcs:
        if f(chars):
            return True
    
def is_type_literal(chars:str):
    
    type_character_pattern = r'[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nd}_]'  # Unicode classes for type characters

    # Pattern for type-identifier and type-characters
    type_identifier_pattern = fr'{type_character_pattern}+'  # At least one type character
    type_name_pattern = fr'({type_identifier_pattern})(\.{type_identifier_pattern})*'  # type-name structure

    # Pattern for array-type-name and generic-type-name
    array_type_name_pattern = fr'{type_name_pattern} \['  # Array type name
    generic_type_name_pattern = fr'{type_name_pattern} \['  # Generic type name
    
    # Check for type-name
    if re.match(type_name_pattern, text):
        return True
    # Check for array-type-name
    elif re.match(array_type_name_pattern, text):
        return True
    # Check for generic-type-name
    elif re.match(generic_type_name_pattern, text):
        return True
    
def is_op_or_punct(chars:str):
    operators_and_punctuators = [
        r'\{', r'\}', r'\[', r'\]', r'\(', r'\)', r'@\(',
        r'@\{', r'\$\(', r';', r'&&', r'\|\|', r'&', r'\|', r',', 
        r'\+\+', r'\.\.', r'::', r'\.', r'!', r'\*', r'/', r'%', 
        r'\+', r'-', r'--', r'-and', r'-band', r'-bnot', r'-bor', 
        r'-bxor', r'-not', r'-or', r'-xor',
        r'=', r'-=', r'\+=', r'\*=', r'/=', r'%=',
        r'>', r'>>', r'2>', r'2>>', r'3>', r'3>>', 
        r'4>', r'4>>', r'5>', r'5>>', r'6>', r'6>>', 
        r'\*>', r'\*\>', r'<',
        r'\*>&1', r'2>&1', r'3>&1', r'4>&1', r'5>&1', r'6>&1',
        r'\*>&2', r'1>&2', r'3>&2', r'4>&2', r'5>&2', r'6>&2',
        r'-as', r'-ccontains', r'-ceq', r'-cge', r'-cgt', 
        r'-cle', r'-clike', r'-clt', r'-cmatch', r'-cne', 
        r'-cnotcontains', r'-cnotlike', r'-cnotmatch', 
        r'-contains', r'-creplace', r'-csplit', r'-eq', 
        r'-ge', r'-gt', r'-icontains', r'-ieq', r'-ige', 
        r'-igt', r'-ile', r'-ilike', r'-ilt', r'-imatch', 
        r'-in', r'-ine', r'-inotcontains', r'-inotlike', 
        r'-inotmatch', r'-ireplace', r'-is', r'-isnot', 
        r'-isplit', r'-join', r'-le', r'-like', r'-lt', 
        r'-match', r'-ne', r'-notcontains', r'-notin', 
        r'-notlike', r'-notmatch', r'-replace', r'-shl', 
        r'-shr', r'-split', r'-f'
    ]
    # Combine all patterns into a single regex pattern
    operator_pattern = r'^(?:' + '|'.join(operators_and_punctuators) + r')$'
    if re.match(operator_pattern, chars):
        return True

def is_integer_literal(chars:str):
    if is_hexidecimal_literal(chars) or is_decimal_literal(chars):
        return True 

def is_newline(chars:str):
    new_line_pattern = r'^(?:\r\n|\r|\n)+$'
    if re.match(new_line_pattern, chars):
        return True

single_line_comment_pattern = r'^#(?:[^\n]*)?$'
requires_comment_pattern = r'^#requires\s+[^\n]+$'
delimited_comment_pattern = r'^<\s*#(?:[^\n]*\#)*\s*>$'

def is_single_line_comment(text: str) -> bool:
    if re.match(single_line_comment_pattern, text):
        return True

def is_requires_comment(text: str) -> bool:
    if re.match(requires_comment_pattern, text):
        return True

def is_delimited_comment(text: str) -> bool:
    if re.match(delimited_comment_pattern, text):
        return True

def is_comment(text: str) -> bool:
    return (is_single_line_comment(text) or
            is_requires_comment(text) or
            is_delimited_comment(text))


#########################################       
 
@dataclass
class PowerShellToken:
    string:str
    type:  str
    start: int 
    end:   int
    
    def __str__(self) -> str:
        return f"<{self.type}@({self.start} {self.end}) {self.string}>"
    
    def __repr__(self) -> str:
        return self.__str__()
    
    
    
class Token:
    KEYWORD = "KEY"                
    VARIABLE = "VAR"               
    COMMAND = "CMD"                
    COMMAND_PARAMETER = "PAR"      
    COMMAND_ARGUMENT_TOKEN = "ARG" 
    INTEGER_LITERAL = "INT"        
    REAL_LITERAL = "LIT"           
    STRING_LITERAL = "STR"         
    TYPE_LITERAL = "TYP"           
    OPERATOR_OR_PUNCTUATOR = "OOP" 
    NEWLINE = "NL"
    COMMENT = "COMMENT"
    UNKNOWN = "???"                
    
    map = (
        (is_newline, NEWLINE),
        (is_keyword, KEYWORD),
        (is_variable, VARIABLE),
        (is_command, COMMAND,) 
        (is_command_parameter, COMMAND_PARAMETER),
        #(is_command_arg, COMMAND_ARGUMENT_TOKEN),
        (is_integer_literal, INTEGER_LITERAL),
        (is_real_literal, REAL_LITERAL),
        (is_string_literal, STRING_LITERAL),
        (is_type_literal, TYPE_LITERAL),
        (is_op_or_punct, OPERATOR_OR_PUNCTUATOR),
        (is_comment, COMMENT)
    )
    
    @classmethod
    def tokenize(chars:str):
        for func, key in Token.map:
            if func(chars):
                return key
            
        return Token.COMMAND_ARGUMENT_TOKEN
    
    @classmethod
    def make(string:str, index:int):
        end = index + len(string)
        return PowerShellToken(string, Token.tokenize(string), index, end)
    
      
class BraceTracker:
    def __init__(self) -> None:
        self.l = "{"
        self.lc = 0
        self.r = "}"
        self.rc = 0
        self.is_inside_brace = False
        self.nest_level = self.lc - self.rc
    
    def nested(self):
        if self.nest_level > 1:
            return True
    
    def visit(self, char:str):
        if char == self.l:
            self.lc+=1
        elif char == self.r:
            self.rc+=1
        if self.lc == self.rc:
            self.is_inside_brace = False
        else:
            self.is_inside_brace = True
        
        self.nest_level = self.lc - self.rc
    
    def inside_brace(self):
        if self.is_inside_brace == True:
            return True
    
    def outside_brace(self):
        if self.is_inside_brace == False:
            return True


class LexBuffer:
    def __init__(self) -> None:
        self._brace_tracker = BraceTracker()
        self.chars = ""
        self.index = -1
        self.tokens = []
    
    def _clear(self):
        self.chars = ""
    
    def _tokenize(self) -> None:
        current = self.chars 
        self._clear()
        self.tokens.append(Token.make(current, self.index))
    
    def _collect(self, char:str) -> None:
        self.chars+=char
    
    def _track(self, char:str) -> None:
        self._brace_tracker.visit(char)
    
    def _consume(self, char:str) -> None:
        self._track(char)
        if is_whitespace(char):
            self._tokenize()
        else:
            self._collect(char)
    
    def _handleMultiChar(self, chars:str) -> None:
        for c in chars:
            self.add(c)
    
    def _handleSingleChar(self, char:str) -> None:
        if self._brace_tracker.inside_brace():
            self._collect(char)
            self._track(char)
        elif self._brace_tracker.outside_brace():
            self._consume(char)
    
    def add(self, char:str):
        self.index+=1
        if len(char) > 1:
            self._handleMultiChar(char)
        elif len(char) == 1:
            self._handleSingleChar(char)


class Tokenizer:
    def __init__(self) -> None:
        self.lbuffer = LexBuffer()
    
    def tokenize(self, source:str) -> list[PowerShellToken]:
        self.lbuffer = LexBuffer()
        for char in source:
            self.lbuffer.add(char)
        return self.lbuffer.tokens


def tokenize(source:str) -> list[PowerShellToken]:
    """
    Takes a string of powershell source script as <source>
    returns a list[PowerShellToken]
    """
    tknzr = Tokenizer()
    return tknzr.tokenize(source)
    
        
