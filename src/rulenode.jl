"""
Type for representing expression trees.
"""
abstract type AbstractRuleNode end

"""
RuleNode
Type for representing nodes in an expression tree.
"""
mutable struct RuleNode <: AbstractRuleNode
	ind::Int # index in grammar
	_val::Any  #value of _() evals
	children::Vector{AbstractRuleNode}
end

mutable struct Hole <: AbstractRuleNode
	domain::BitVector
end

RuleNode(ind::Int) = RuleNode(ind, nothing, AbstractRuleNode[])
RuleNode(ind::Int, children::Vector{AbstractRuleNode}) = RuleNode(ind, nothing, children)
RuleNode(ind::Int, children::Vector{RuleNode}) = RuleNode(ind, nothing, children)
RuleNode(ind::Int, children::Vector{Hole}) = RuleNode(ind, nothing, children)
RuleNode(ind::Int, _val::Any) = RuleNode(ind, _val, AbstractRuleNode[])
    
Base.:(==)(::RuleNode, ::Hole) = false
Base.:(==)(::Hole, ::RuleNode) = false
Base.:(==)(A::RuleNode, B::RuleNode) = 
	(A.ind == B.ind) && 
	length(A.children) == length(B.children) && #required because zip doesn't check lengths
	all(isequal(a, b) for (a, b) ∈ zip(A.children, B.children))
# We do not know how the holes will be expanded yet, so we cannot assume equality even if the domains are equal.
Base.:(==)(A::Hole, B::Hole) = false

function Base.hash(node::RuleNode, h::UInt=zero(UInt))
	retval = hash(node.ind, h)
	for child in node.children
		retval = hash(child, retval)
	end
	return retval
end

function Base.hash(node::Hole, h::UInt=zero(UInt))
	return hash(node.domain, h)
end

function Base.show(io::IO, node::RuleNode; separator=",", last_child::Bool=false)
	print(io, node.ind)
	if !isempty(node.children)
	    print(io, "{")
	    for (i,c) in enumerate(node.children)
		show(io, c, separator=separator, last_child=(i == length(node.children)))
	    end
	    print(io, "}")
	elseif !last_child
	    print(io, separator)
	end
end

function Base.show(io::IO, node::Hole; separator=",", last_child::Bool=false)
	print(io, "hole[$(node.domain)]")
	if !last_child
		print(io, separator)
	end
end

"""
Return the number of vertices in the tree rooted at root.
Holes don't count.
"""
function Base.length(root::RuleNode)
	retval = 1
	for c in root.children
	    retval += length(c)
	end
	return retval
end

"""
Return the number of vertices in the tree rooted at root.
Holes don't count.
"""
Base.length(::Hole) = 0


Base.isless(rn₁::AbstractRuleNode, rn₂::AbstractRuleNode)::Bool = _rulenode_compare(rn₁, rn₂) == -1


"""
Helper function for `Base.isless(::RuleNode, ::RuleNode)`
Returns -1 if `rn₁ < rn₂`, 0 if `rn₁ == rn₂` and 1 if `rn₁ > rn₂`
"""
function _rulenode_compare(rn₁::RuleNode, rn₂::RuleNode)::Int
	if rn₁.ind == rn₂.ind
		for (c₁, c₂) ∈ zip(rn₁.children, rn₂.children)
			comparison = _rulenode_compare(c₁, c₂)
			if comparison ≠ 0
				return comparison
			end
		end
		return 0
	else
		return rn₁.ind < rn₂.ind ? -1 : 1
	end
end

_rulenode_compare(::Hole, ::RuleNode) = -1
_rulenode_compare(::RuleNode, ::Hole) = 1
_rulenode_compare(::Hole, ::Hole) = 0


"""
Return the depth of the expression tree rooted at root.
Holes don't count.
"""
function depth(root::RuleNode)
	retval = 1
	for c in root.children
	    retval = max(retval, depth(c)+1)
	end
	return retval
end


"""
Return the depth of the expression tree rooted at root.
Holes don't count.
"""
depth(::Hole) = 0


"""
Return the depth of node for an expression tree rooted at root. 
Depth is 1 when root == node.
"""
function node_depth(root::AbstractRuleNode, node::AbstractRuleNode)
	root === node && return 1
	root isa Hole && return 0
	for c in root.children
	    d = node_depth(c, node)
	    d > 0 && (return d+1)
	end
	return 0
end

"""
Returns all rules of a specific type used in a RuleNode.
"""
function rulesoftype(node::RuleNode, ruleset::Set{Int})
	retval = Set()

	if node.ind in ruleset
		union!(retval, [node.ind])
	end

	if isempty(node.children)
		return retval
	else
		for child in node.children
			union!(retval, rulesoftype(child, ruleset))
		end

		return retval
	end
end

"""
Replace a node in expr, specified by path, with new_expr.
Path is a sequence of child indices, starting from the node.
"""
function swap_node(expr::AbstractRuleNode, new_expr::AbstractRuleNode, path::Vector{Int})
	if length(path) == 1
		expr.children[path[begin]] = new_expr
	else
		swap_node(expr.children[path[begin]], new_expr, path[2:end])
	end
end


"""
Replace child i of a node, a part of larger expr, with new_expr.
"""
function swap_node(expr::RuleNode, node::RuleNode, child_index::Int, new_expr::RuleNode)
	if expr == node 
		node.children[child_index] = new_expr
	else
		for child ∈ expr.children
			swap_node(child, node, child_index, new_expr)
		end
	end
end


"""
Extract derivation sequence from path (sequence of child indices).
If the path is deeper than the deepest node, it returns what it has.
"""
function get_rulesequence(node::RuleNode, path::Vector{Int})
	if node.ind == 0 # sign for empty node 
		return Vector{Int}()
	elseif isempty(node.children) # no childnen, nowehere to follow the path; still return the index
		return [node.ind]
	elseif isempty(path)
		return [node.ind]
	elseif isassigned(path, 2)
		# at least two items are left in the path
		# need to access the child with get because it can happen that the child is not yet built
		return append!([node.ind], get_rulesequence(get(node.children, path[begin], RuleNode(0)), path[2:end]))
	else
		# if only one item left in the path
		# need to access the child with get because it can happen that the child is not yet built
		return append!([node.ind], get_rulesequence(get(node.children, path[begin], RuleNode(0)), Vector{Int}()))
	end
end

get_rulesequence(::Hole, ::Vector{Int}) = Vector{Int}()

"""
Extracts rules in the left subtree defined by the path.
"""
function rulesonleft(expr::RuleNode, path::Vector{Int})
	if isempty(expr.children)
		# if the encoutered node is terminal or non-expanded non-terminal, return node id
		Set{Int}(expr.ind)
	elseif isempty(path)
		# if path is empty, collect the entire subtree
		ruleset = Set{Int}(expr.ind)
		for ch in expr.children
			union!(ruleset, rulesonleft(ch, Vector{Int}()))
		end
		return ruleset 
	elseif length(path) == 1
		# if there is only one element left in the path, collect all children except the one indicated in the path
		ruleset = Set{Int}(expr.ind)
		for i in 1:path[begin]-1
			union!(ruleset, rulesonleft(expr.children[i], Vector{Int}()))
		end
		return ruleset 
	else
		# collect all subtrees up to the child indexed in the path
		ruleset = Set{Int}(expr.ind)
		for i in 1:path[begin]-1
			union!(ruleset, rulesonleft(expr.children[i], Vector{Int}()))
		end
		union!(ruleset, rulesonleft(expr.children[path[begin]], path[2:end]))
		return ruleset 
	end
end

rulesonleft(::Hole, ::Vector{Int}) = Set{Int}()


"""
Retrieves a rulenode at the original location by reference. 
"""
function get_node_at_location(root::RuleNode, location::Vector{Int})
    if location == []
        return root
    else
        return get_node_at_location(root.children[location[1]], location[2:end])
    end
end

function get_node_at_location(root::Hole, location::Vector{Int})
    if location == []
        return root
    end
    return nothing
end



"""
Checks if a rulenode tree contains a hole.
"""
contains_hole(rn::RuleNode) = any(contains_hole(c) for c ∈ rn.children)
contains_hole(hole::Hole) = true
