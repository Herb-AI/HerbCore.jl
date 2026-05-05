@auto_hash_equals struct RuleNode{R} <: AbstractRuleNode
    rules::R
    children::Vector{RuleNode{R}}

    function RuleNode{R}(rules, children=RuleNode{R}[]) where R
        return new{R}(R(rules), children)
    end
end
function RuleNode(rules::R, children=RuleNode{R}[]) where R
    return RuleNode{R}(rules, children)
end
function RuleNode{R}(rn::RuleNode{R}, ::Vector{<:RuleNode}) where R
    return RuleNode{R}(rn.rules, rn.children)
end

get_rules(rules) = rules
get_rules(rn::RuleNode) = get_rules(rn.rules)
HerbCore.get_children(rn::RuleNode) = rn.children

function Base.in(a::RuleNode, b::RuleNode)
    if get_rules(a) in get_rules(b)
        if allequal(length, get_children.((a,b))) && all(in.(get_children(a), get_children(b)))
            return true
        end
    end
    if any(a in cb for cb in HerbCore.get_children(b))
        return true
    else
        return false
    end
end

function Base.intersect(a::R, bs...) where R<:RuleNode
    new_rules = intersect(get_rules(a), get_rules.(bs)...)
    new_children = [intersect(ca, cbs...) for (ca, cbs) in zip(get_children(a), get_children.(bs)) if !isempty(cbs)]
    return R(new_rules, new_children)
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
get_grammar(rn::RuleNode{<:GrammarDomain}) = rn.rules.grammar

struct DomainLabel{D,L}
    rules::D
    label::L
end
function DomainLabel{D,L}(rules) where {D,L}
    return DomainLabel{D,L}(D(get_rules(rules)), :_)
end
function DomainLabel{D,L}(dl::DomainLabel) where {D,L}
    return DomainLabel{D,L}(D(get_rules(dl)), get_label(dl))
end
function DomainLabel{D}(dl::DomainLabel, _::L=:_) where {D,L}
    return DomainLabel{D,L}(D(get_rules(dl)), get_label(dl))
end
function DomainLabel{D}(rules, label::L=:_) where {D,L}
    return DomainLabel{D,L}(D(rules), label)
end
get_rules(ld::DomainLabel) = get_rules(ld.rules)
get_label(ld::DomainLabel) = ld.label
get_label(rn::RuleNode{<:DomainLabel}) = rn.rules.label

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
@unstable get_max(dc::DomainCount) = dc.max
get_min(dc::DomainCount) = dc.min
get_min(rn::RuleNode{<:DomainCount}) = rn.rules.min
@unstable get_max(rn::RuleNode{<:DomainCount}) = rn.rules.max

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
