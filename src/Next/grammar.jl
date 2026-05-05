abstract type AbstractGrammarNext <: AbstractGrammar end
function get_rules end
function get_terminal_symbols end
function get_nonterminal_symbols end

"""
get_rules([f,] grammar::AbstractGrammar)

Get the production rules of `grammar` for which `f` returns `true`.

By default, return all of the rules of the `grammar`.
"""
get_rules(grammar::AbstractGrammarNext)
get_rules(f, grammar::AbstractGrammarNext) = filter(f, get_rules(grammar)) 

abstract type AbstractProductionRule end
function get_lhs end
function get_rhs end

"""
    get_lhs(rule::AbstractProductionRule)

Get the left-hand side of the `rule`.
"""
get_lhs(::AbstractProductionRule)

"""
    get_lhs(rule::AbstractProductionRule)

Get the right-hand side of the `rule`.
"""
get_rhs(::AbstractProductionRule)

function Base.show(io::IO, grammar::AbstractGrammarNext)
    println(io, length(get_rules(grammar)), "-rule ", typeof(grammar))
    for (i, r) in enumerate(get_rules(grammar))
        print(io, " ", i, ": ")
        show(io, r)
        println(io)
    end
    return
end
