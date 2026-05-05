struct RuleNode{R} <: AbstractRuleNode
    rules::R
    children::Vector{RuleNode{R}}

    function RuleNode{R}(rules, children=RuleNode{R}[]) where R
        return new{R}(R(rules), children)
    end
end
function RuleNode(rules::R, children=RuleNode{R}[]) where R
    return RuleNode{R}(rules, children)
end


get_rules(rules) = rules
get_rules(rn::RuleNode) = get_rules(rn.rules)
HerbCore.get_children(rn::RuleNode) = rn.children

function Base.iterate(rn::RuleNode, i=1)
    if i == 1
        return (get_rules(rn), i+1)
    elseif i == 2
        return (HerbCore.get_children(rn), i+1)
    else
        return nothing
    end
end

AbstractTrees.children(rn::RuleNode) = rn.children
AbstractTrees.nodevalue(rn::RuleNode) = get_rules(rn)
AbstractTrees.ChildIndexing(::Type{RuleNode}) = AbstractTrees.IndexedChildren()
AbstractTrees.NodeType(::Type{RuleNode}) = AbstractTrees.HasNodeType()

struct GrammarDomain{G<:AbstractGrammar,D}
    grammar::G
    rules::D
end
get_rules(gd::GrammarDomain) = get_rules(gd.rules)
get_grammar(gd::GrammarDomain) = gd.grammar

struct DomainLabel{D,L}
    rules::D
    label::L
end
function DomainLabel{D}(dl::DomainLabel, _::L) where {D,L}
    return DomainLabel{D,L}(D(get_rules(dl)), get_label(dl))
end
function DomainLabel{D}(rules, label::L=:_) where {D,L}
    return DomainLabel{D,L}(D(rules), label)
end
get_rules(ld::DomainLabel) = get_rules(ld.rules)
get_label(ld::DomainLabel) = ld.label

struct DomainCount{D}
    rules::D
    max::Union{Int,Nothing}
    min::Int
    function DomainCount{D}(rules, max=1, min=1) where D
        if !isnothing(max) && max < min
            error(lazy"Maximum count ($max) must be >= the minimum ($min)")
        end
        if min < 0
            error(lazy"Minimum ($min) be non-negative.") 
        end
        return new{D}(D(rules), max, min)
    end
end
function DomainCount(rules::D, max=1, min=1) where D
    return DomainCount{D}(rules, max, min)
end
function DomainCount{D}(dc::DomainCount) where D
    return DomainCount{D}(get_rules(dc), get_max(dc), get_min(dc))
end
get_rules(dc::DomainCount) = get_rules(dc.rules)
get_max(dc::DomainCount) = dc.max
get_min(dc::DomainCount) = dc.min

# struct RuleNode{<:GrammarDomain}{R<:AbstractRuleNode,G<:AbstractGrammar} <: AbstractRuleNode
#     rulenode::R
#     grammar::G
#
#     function RuleNode{<:GrammarDomain}(rulenode::R, grammar::G) where {R,G}
#         !is_tree_valid(rulenode, grammar) || error(lazy"Rulenode ($rulenode) must be valid within the grammar ($grammar) to construct a RuleNode{<:GrammarDomain}")
#         return new{R,G}(rulenode, grammar)
#     end
# end
# get_rulenode(grn::RuleNode{<:GrammarDomain}) = grn.rulenode
# get_grammar(grn::RuleNode{<:GrammarDomain}) = grn.grammar

abstract type NodeProperty end

abstract type Uniformity <: NodeProperty end
struct Uniform <: Uniformity end
struct NonUniform <: Uniformity end

grammar_based_property_note = """
!!! note
    As uniformity depends on the grammar, this trait is, in general, only
    defined for [`RuleNode{<:GrammarDomain}`](@ref)s, because they contain a reference to
    a grammar.
"""

"""
    node_uniformity(node::RuleNode{<:GrammarDomain})

[`Uniform`](@ref) iff the `node`'s children all have the same right-hand sides.

$grammar_based_property_note
"""
node_uniformity(::RuleNode{<:GrammarDomain})::Uniformity = nothing #TODO

"""
    tree_uniformity(node::RuleNode{<:GrammarDomain})

[`Uniform`](@ref) iff `node` and all of its descendents are [`Uniform`](@ref).

Specifically, this is true iff [`node_uniformity`](@ref) is [`Uniform`](@ref)
for `node`, and `tree_uniformity` is [`Uniform`](@ref) for all of its children.

$grammar_based_property_note
"""
tree_uniformity(::RuleNode{<:GrammarDomain})::Uniformity = nothing #TODO

abstract type Terminality <: NodeProperty end
struct Terminal <: Terminality end
struct NonTerminal <: Terminality end

"""
    terminality(node::RuleNode{<:GrammarDomain})

[`Terminal`](@ref) iff all rules in the `node`'s domain are [`Terminal`](@ref).

$grammar_based_property_note
"""
terminality(::RuleNode{<:GrammarDomain})::Terminality = nothing #TODO
isterminal(node::RuleNode{<:GrammarDomain}) = terminality(node) === Terminal()
isnonterminal(node::RuleNode{<:GrammarDomain}) = terminality(node) === NonTerminal()

macro rulenode(node_type, ex)
    _shorthand2rulenode(node_type, ex)
end

function _shorthand2rulenode(node_type, ex::Integer)
    return :($node_type($ex))
end

function _shorthand2rulenode(node_type, ex)::Expr
    ex = postwalk(ex) do x
        if @capture(x, domain_{children__})
            return :(RuleNode{$node_type}($domain, [$(children...)]))
        else
            return x
        end
    end
    ex = postwalk(ex) do x
        if @capture(x, type_(domain_, [children__]))
            children = map(c -> iscall(c, node_type) ? c : :(RuleNode{$node_type}($c)), children)
            return :($type($domain, [$(children...)])) 
        else
            return x
        end
    end
    ex = postwalk(ex) do x
        if @capture(x, (label_:domain_))
            return :(DomainLabel($domain, Symbol($(string(label)))))
        else
            return x
        end
    end
    ex = postwalk(ex) do x
        if @capture(x, (min_ <= domain_ <= max_))
            return :(DomainCount($domain, $max, $min))
        elseif @capture(x, (domain_ <= max_)) 
            return :(DomainCount($domain, $max, 0))
        elseif @capture(x, (min_ <= domain_)) 
            return :(DomainCount($domain, nothing, $min))
        else
            return x
        end
    end
    return ex
end

# struct PatternRuleNode{D} <: AbstractRuleNode
#     domain::D
#     children::Vector{PatternRuleNode{D}}
#     at_most::Int
#     at_least::Int
#
#     function PatternRuleNode(
#         domain::D,
#         children::Vector{PatternRuleNode{D}}=PatternRuleNode{D}[],
#         at_most::Int=1,
#         at_least::Int=0,
#     ) where D
#         if at_least < 0 
#             error(lazy"The lower bound of the pattern ($count_lower_bound) must be non-negative.")
#         elseif at_most < at_least
#             error(lazy"The upper bound of the pattern ($count_upper_bound) must be >= the lower bound ($count_lower_bound")
#         end
#
#         return new{D}(domain, children, at_most, at_least)
#     end
# end
#
# const PRN = PatternRuleNode
#
# HerbCore.get_domain(prn::PatternRuleNode) = prn.domain
# HerbCore.get_children(prn::PatternRuleNode) = prn.children
#
# AbstractTrees.children(prn::PatternRuleNode) = prn.children
# AbstractTrees.nodevalue(prn::PatternRuleNode) = get_domain(prn)
# AbstractTrees.ChildIndexing(::Type{PatternRuleNode}) = AbstractTrees.IndexedChildren()
# AbstractTrees.NodeType(::Type{PatternRuleNode}) = AbstractTrees.HasNodeType()

abstract type HoleHeuristicStyle end
struct LeftMost <: HoleHeuristicStyle end
struct RightMost <: HoleHeuristicStyle end
struct Random <: HoleHeuristicStyle end
struct SmallestDomain <: HoleHeuristicStyle end


function hole_heuristic(::LeftMost, rulenode::AbstractRuleNode, max_depth)
    heuristic_leftmost(rulenode, max_depth)
end

function hole_heuristic(::RightMost, rulenode::AbstractRuleNode, max_depth)
    heuristic_rightmost(rulenode, max_depth)
end

function hole_heuristic(::Random, rulenode::AbstractRuleNode, max_depth)
    heuristic_random(rulenode, max_depth)
end

function hole_heuristic(::SmallestDomain, rulenode::AbstractRuleNode, max_depth)
    heuristic_smallest_domain(rulenode, max_depth)
end
