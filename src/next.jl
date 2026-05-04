module Next
import AbstractTrees
using ..HerbCore: HerbCore, AbstractRuleNode
using DispatchDoctor: @stable
using MacroTools: @capture, postwalk, iscall

@stable begin

struct DomainRuleNode{D} <: AbstractRuleNode
    domain::D
    children::Vector{DomainRuleNode{D}}

    function DomainRuleNode(domain::D, children=DomainRuleNode{D}[]) where D
        return new{D}(domain, children)
    end
    function DomainRuleNode{D}(domain, children=DomainRuleNode{D}[]) where D
        return new{D}(D(domain), children)
    end
end

const DRN = DomainRuleNode

HerbCore.get_domain(drn::DomainRuleNode) = drn.domain
HerbCore.get_children(drn::DomainRuleNode) = drn.children

AbstractTrees.children(drn::DomainRuleNode) = drn.children
AbstractTrees.nodevalue(drn::DomainRuleNode) = get_domain(drn)
AbstractTrees.ChildIndexing(::Type{DomainRuleNode}) = AbstractTrees.IndexedChildren()
AbstractTrees.NodeType(::Type{DomainRuleNode}) = AbstractTrees.HasNodeType()

abstract type GrammarNode end

abstract type NodeProperty end

abstract type Uniformity <: NodeProperty end
struct IsUniform <: Uniform end
struct NonUniform <: Uniform end

"""
    node_uniformity(node::GrammarNode)

True iff the `node` has the same child non-terminals for its entire domain.
"""
isuniform_node(::GrammarNode) = nothing #TODO
"""
    tree_uniformity(node::GrammarNode)

True iff [`isuniform_node`](@ref) is true for `node` and all of its children, recursively.
"""
is_uniform_tree(::GrammarNode) = nothing #TODO

abstract type Terminality <: NodeProperty end
struct Terminal <: Terminal end
struct NonTerminal <: Terminal end

isterminal(::GrammarNode) = nothing #TODO
isnonterminal(node::GrammarNode) = !isterminal(node)

macro rulenode(node_type, ex)
    _shorthand2rulenode(node_type, ex)
end

function _shorthand2rulenode(node_type, ex)::Expr
    ex = postwalk(ex) do x
        if @capture(x, domain_{children__})
            return :($node_type($domain, [$(children...)]))
        else
            return x
        end
    end
    ex = postwalk(ex) do x
        if @capture(x, type_(domain_, [children__]))
            children = map(c -> iscall(c, node_type) ? c : :($node_type($c)), children)
            return :($type($domain, [$(children...)])) 
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

end # @stable
end # module Next
