module Next
import ..HerbCore
import AbstractTrees

struct RuleNode{V} <: HerbCore.AbstractRuleNode
    value::V
    children::Vector{RuleNode{V}}
end
get_children(rn::RuleNode) = rn.children
AbstractTrees.children(rn::RuleNode) = rn.children
AbstractTrees.nodevalue(rn::RuleNode) = rn.value
AbstractTrees.NodeType(::Type{<:RuleNode}) = HasNodeType()
AbstractTrees.nodetype(::Type{<:R}) where R<:RuleNode = R
AbstractTrees.childrentype(::Type{<:R}) where R<:RuleNode = Vector{R}
AbstractTrees.childtype(::Type{<:R}) where R<:RuleNode = R

struct Tree{I,V,C<:AbstractVector{V},E<:AbstractDict{I,<:AbstractVector{I}}}
    nodes::C
    edges::E
end

end
