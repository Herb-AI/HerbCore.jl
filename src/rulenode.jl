"""
	AbstractRuleNode

Abstract type for representing expression trees.
Expression trees consist of [`RuleNode`](@ref)s and [`Hole`](@ref)s.

- A [`RuleNode`](@ref) represents a certain production rule in the [`Grammar`](@ref).
- A [`Hole`](@ref) is a placeholder where certain rules in the grammar still can be applied. 
"""
abstract type AbstractRuleNode end


"""
	RuleNode <: AbstractRuleNode

A [`RuleNode`](@ref) represents a node in an expression tree.
Each node corresponds to a certain rule in the [`Grammar`](@ref).
A [`RuleNode`](@ref) consists of:

- `ind`: The index of the rule in the [`Grammar`](@ref) which this node is representing.
- `_val`: Field for storing immediately evaluated values
- `children`: The children of this node in the expression tree

!!! compat
	Evaluate immediately functionality is not yet supported by most of Herb.jl.
"""
mutable struct RuleNode <: AbstractRuleNode
	ind::Int # index in grammar
	_val::Any  #value of _() evals
	children::Vector{AbstractRuleNode}
end


"""
	Hole <: AbstractRuleNode

A [`Hole`](@ref) is a placeholder where certain rules from the grammar can still be applied.
The `domain` of a [`Hole`](@ref) defines which rules can be applied.
The `domain` is a bitvector, where the `i`th bit is set to true if the `i`th rule in the grammar can be applied. 
"""
mutable struct Hole <: AbstractRuleNode
	domain::BitVector
end


"""
	HoleReference

Contains a hole and the path to the hole from the root of the tree.
"""
struct HoleReference
    hole::Hole
    path::Vector{Int}
end


RuleNode(ind::Int) = RuleNode(ind, nothing, AbstractRuleNode[])
"""
	RuleNode(ind::Int, children::Vector{AbstractRuleNode})

Create a [`RuleNode`](@ref) for the [`Grammar`](@ref) rule with index `ind` and `children` as subtrees.
"""
RuleNode(ind::Int, children::Vector{AbstractRuleNode}) = RuleNode(ind, nothing, children)
RuleNode(ind::Int, children::Vector{RuleNode}) = RuleNode(ind, nothing, children)
RuleNode(ind::Int, children::Vector{Hole}) = RuleNode(ind, nothing, children)


"""
	RuleNode(ind::Int, _val::Any)

Create a [`RuleNode`](@ref) for the [`Grammar`](@ref) rule with index `ind`, 
`_val` as immediately evaluated value and no children

!!! warning
	Only use this constructor if you are absolutely certain that a rule is terminal and cannot have children.
	Use [`RuleNode(ind::Int, grammar::Grammar)`] for rules that might have children.
	In general, [`Hole`](@ref)s should be used as a placeholder when the children of a node are not yet known.   

!!! compat
	Evaluate immediately functionality is not yet supported by most of Herb.jl.
"""
RuleNode(ind::Int, _val::Any) = RuleNode(ind, _val, AbstractRuleNode[])
    
Base.:(==)(::RuleNode, ::Hole) = false
Base.:(==)(::Hole, ::RuleNode) = false
Base.:(==)(A::RuleNode, B::RuleNode) = 
	(A.ind == B.ind) && 
	length(A.children) == length(B.children) && #required because zip doesn't check lengths
	all(isequal(a, b) for (a, b) ∈ zip(A.children, B.children))
# We do not know how the holes will be expanded yet, so we cannot assume equality even if the domains are equal.
Base.:(==)(A::Hole, B::Hole) = false

Base.copy(r::RuleNode) = RuleNode(r.ind, r._val, r.children)
Base.copy(h::Hole) = Hole(copy(h.domain))

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
	Base.length(root::RuleNode)

Return the number of nodes in the tree rooted at root.
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
	Base.length(root::RuleNode)

Return the number of nodes in the tree rooted at root.
Holes don't count.
"""
Base.length(::Hole) = 0

"""
	Base.isless(rn₁::AbstractRuleNode, rn₂::AbstractRuleNode)::Bool

Compares two [`RuleNode`](@ref)s. Returns true if the left [`RuleNode`](@ref) is less than the right [`RuleNode`](@ref).
Order is determined from the index of the [`RuleNode`](@ref)s. 
If both [`RuleNode`](@ref)s have the same index, a depth-first search is 
performed in both [`RuleNode`](@ref)s until nodes with a different index
are found.
"""
Base.isless(rn₁::AbstractRuleNode, rn₂::AbstractRuleNode)::Bool = _rulenode_compare(rn₁, rn₂) == -1


function _rulenode_compare(rn₁::RuleNode, rn₂::RuleNode)::Int
	# Helper function for Base.isless
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
	depth(root::RuleNode)::Int

Return the depth of the [`AbstractRuleNode`](@ref) tree rooted at root.
Holes don't count towards the depth.
"""
function depth(root::RuleNode)::Int
	retval = 1
	for c in root.children
	    retval = max(retval, depth(c)+1)
	end
	return retval
end

depth(::Hole) = 0


"""
	node_depth(root::AbstractRuleNode, node::AbstractRuleNode)::Int

Return the depth of `node` for an [`AbstractRuleNode`](@ref) tree rooted at `root`.
Depth is `1` when `root == node`.

!!! warning
	`node` must be a subtree of `root` in order for this function to work.
"""
function node_depth(root::AbstractRuleNode, node::AbstractRuleNode)::Int
	root ≡ node && return 1
	root isa Hole && return 0
	for c in root.children
	    d = node_depth(c, node)
	    d > 0 && (return d+1)
	end
	return 0
end

"""
	rulesoftype(node::RuleNode, ruleset::Set{Int})

Returns every rule in the ruleset that is also used in the [`AbstractRuleNode`](@ref) tree.
"""
function rulesoftype(node::RuleNode, ruleset::Set{Int})
	retval = Set()

	if node.ind ∈ ruleset
		union!(retval, [node.ind])
	end

	if isempty(node.children)
		return retval
	else
		for child ∈ node.children
			union!(retval, rulesoftype(child, ruleset))
		end

		return retval
	end
end

"""
	swap_node(expr::AbstractRuleNode, new_expr::AbstractRuleNode, path::Vector{Int})

Replace a node in `expr`, specified by `path`, with `new_expr`.
Path is a sequence of child indices, starting from the root node.
"""
function swap_node(expr::AbstractRuleNode, new_expr::AbstractRuleNode, path::Vector{Int})
	if length(path) == 1
		expr.children[path[begin]] = new_expr
	else
		swap_node(expr.children[path[begin]], new_expr, path[2:end])
	end
end

"""
	swap_node(expr::RuleNode, node::RuleNode, child_index::Int, new_expr::RuleNode)

Replace child `i` of a node, a part of larger `expr`, with `new_expr`.
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
	get_rulesequence(node::RuleNode, path::Vector{Int})

Extract the derivation sequence from a path (sequence of child indices) and an [`AbstractRuleNode`](@ref).
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
	rulesonleft(expr::RuleNode, path::Vector{Int})::Set{Int}

Finds all rules that are used in the left subtree defined by the path.
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

rulesonleft(h::Hole, loc::Vector{Int}) = Set{Int}(findall(h.domain))


"""
	get_node_at_location(root::RuleNode, location::Vector{Int})

Retrieves a [`RuleNode`](@ref) at the given location by reference. 
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
	contains_hole(rn::RuleNode) = any(contains_hole(c) for c ∈ rn.children)

Checks if an [`AbstractRuleNode`](@ref) tree contains a [`Hole`](@ref).
"""
contains_hole(rn::RuleNode) = any(contains_hole(c) for c ∈ rn.children)
contains_hole(hole::Hole) = true
