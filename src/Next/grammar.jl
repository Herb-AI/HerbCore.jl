function get_rules end
function get_terminal_symbols end
function get_nonterminal_symbols end

"""
get_rules([f,] grammar::AbstractGrammar)

Get the production rules of `grammar` for which `f` returns `true`.

By default, return all of the rules of the `grammar`.
"""
get_rules(grammar::AbstractGrammar)
get_rules(f, grammar::AbstractGrammar) = filter(f, get_rules(grammar)) 

abstract type AbstractRule end
function get_lhs end
function get_rhs end

"""
    get_lhs(rule::AbstractRule)

Get the left-hand side of the `rule`.
"""
get_lhs(::AbstractRule)

"""
    get_lhs(rule::AbstractRule)

Get the right-hand side of the `rule`.
"""
get_rhs(::AbstractRule)

function Base.show(io::IO, grammar::AbstractGrammar)
    println(io, length(get_rules(grammar)), "-rule ", typeof(grammar))
    for (i, r) in enumerate(get_rules(grammar))
        print(io, " ", i, ": ")
        show(io, r)
        println(io)
    end
    return
end
