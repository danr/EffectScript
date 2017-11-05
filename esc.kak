# Detection
# ‾‾‾‾‾‾‾‾‾

hook global BufCreate .*[.]esc %{
    set buffer filetype effectscript
}

# Highlighters
# ‾‾‾‾‾‾‾‾‾‾‾‾

add-highlighter -group / regions -default code effectscript \
    double_string '"'  (?<!\\)(\\\\)*"         '' \
    single_string "'"  (?<!\\)(\\\\)*'         '' \
    literal       "`"  (?<!\\)(\\\\)*`         '' \
    comment       //   '$'                     '' \
    comment       /\*  \*/                     ''
    #balancedPar   \b\w+\h*\( \) \(          \

#add-highlighter -group /effectscript/balancedPar regions -default call urf loop \( \) \(

#add-highlighter -group /effectscript/balancedPar/urf/call regex (\w+)\h*\( 1:keyword
#add-highlighter -group /effectscript/balancedPar/urf/loop ref effectscript

add-highlighter -group /effectscript/double_string fill string
add-highlighter -group /effectscript/single_string fill string
add-highlighter -group /effectscript/comment       fill comment
add-highlighter -group /effectscript/literal       fill string

add-highlighter -group /effectscript/code regex "\b(?:-?[0-9]*\.)?[0-9]+\b" 0:value
add-highlighter -group /effectscript/code regex \$\w* 0:variable
add-highlighter -group /effectscript/code regex \b(false|true)\b 0:value
add-highlighter -group /effectscript/code regex \b(boolean|string|number|u?int|integer|nat|[iu]\d+)\b 0:type

add-highlighter -group /effectscript/code regex \b(alias|type|effect|fn|function|switch|case|match|handle|interface|instance|inst|trait|class|typeclass|data|record|variant|where)\b 0:keyword

# rudimentary support to show function name as keyword in
#     if (cond) {
#     while { cond } {
#     list {
# incomplete because cond may not include parens
#add-highlighter -group /effectscript/code regex \b(\w+)\h*[(][^()\n]*[)]\h*[{]\h*\n 1:keyword
#add-highlighter -group /effectscript/code regex \b(\w+)\h*[{] 1:keyword

add-highlighter -group /effectscript/code regex ^\h*\b(\w+)\h+[(] 1:keyword
add-highlighter -group /effectscript/code regex ^\h*\b(\w+)\h+[{] 1:keyword


# Commands
# ‾‾‾‾‾‾‾‾

def -hidden effectscript-filter-around-selections %{
    # remove trailing white spaces
    try %{ exec -draft -itersel <a-x> s \h+$ <ret> d }
}

def -hidden effectscript-indent-on-char %<
    eval -draft -itersel %<
        # align closer token to its opener when alone on a line
        try %/ exec -draft <a-h> <a-k> ^\h+[]}]$ <ret> m s \`|.\' <ret> 1<a-&> /
    >
>

def -hidden effectscript-indent-on-new-line %<
    eval -draft -itersel %<
        # copy // comments prefix and following white spaces
        try %{ exec -draft k <a-x> s ^\h*\K#\h* <ret> y gh j P }
        # preserve previous line indent
        try %{ exec -draft \; K <a-&> }
        # filter previous line
        try %{ exec -draft k : effectscript-filter-around-selections <ret> }
        # indent after lines beginning / ending with opener token
        try %_ exec -draft k <a-x> <a-k> ^\h*[[{]|[[{]$ <ret> j <a-gt> _
    >
>

# Initialization
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾

hook -group effectscript-highlight global WinSetOption filetype=effectscript %{ add-highlighter ref effectscript }

hook global WinSetOption filetype=effectscript %{
    hook window InsertEnd  .* -group effectscript-hooks  effectscript-filter-around-selections
    hook window InsertChar .* -group effectscript-indent effectscript-indent-on-char
    hook window InsertChar \n -group effectscript-indent effectscript-indent-on-new-line
}

hook -group effectscript-highlight global WinSetOption filetype=(?!effectscript).* %{ remove-highlighter effectscript }

hook global WinSetOption filetype=(?!effectscript).* %{
    remove-hooks window effectscript-indent
    remove-hooks window effectscript-hooks
}

