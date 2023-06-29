"""
Represents all grammars.
The library assumes that the grammar structs have at least the following attributes:
rules::Vector{Any}    # list of RHS of rules (subexpressions)
types::Vector{Symbol} # list of LHS of rules (types, all symbols)
isterminal::BitVector # whether rule i is terminal
iseval::BitVector     # whether rule i is an eval rule
bytype::Dict{Symbol,Vector{Int}}   # maps type to all rules of said type
domains::Dict{Symbol,BitVector}    # maps type to a domain bitvector
childtypes::Vector{Vector{Symbol}} # list of types of the children for each rule. Empty if terminal
probabilities::Union{Vector{Real}, Nothing} # List of probabilities for each rule. Nothing if grammar is non-probabilistic
"""
abstract type Grammar end