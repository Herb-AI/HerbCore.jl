using StyledStrings: @styled_str
using MacroTools: @capture, postwalk, prewalk, isexpr
using AbstractTrees: PreOrderState, TreeCursor

"""
	abstract type AbstractRuleNode end

Abstract type for representing expression trees.
An `AbstractRuleNode` is expected to implement the following functions:

- `isfilled(::AbstractRuleNode)::Bool`. True iff the grammar rule this node holds is not ambiguous, i.e. has domain size 1.
- `isuniform(::AbstractRuleNode)::Bool`. True iff the children of this node are known.
- `get_rule(::AbstractRuleNode)::Int`. Returns the index of the grammar rule it represents.
- `get_children(::AbstractRuleNode)::Vector{AbstractRuleNode}`. Returns the children of this node.

Expression trees consist of [`RuleNode`](@ref)s and [`AbstractHole`](@ref)s.

- A [`RuleNode`](@ref) represents a certain production rule in the [`AbstractGrammar`](@ref).
- A [`AbstractHole`](@ref) is a placeholder where certain rules in the grammar still can be applied. 
"""
abstract type AbstractRuleNode end

Base.getindex(rn::AbstractRuleNode, inds...) = getindex(get_children(rn), inds...)
Base.view(rn::AbstractRuleNode, inds...) = view(get_children(rn), inds...)

# Interface to AbstractTrees.jl
AbstractTrees.children(node::AbstractRuleNode) = get_children(node)
AbstractTrees.nodevalue(node::AbstractRuleNode) = get_rule(node)
AbstractTrees.ChildIndexing(::Type{<:AbstractRuleNode}) = AbstractTrees.IndexedChildren()

# struct ZipNode{Ns<:Tuple} <: AbstractRuleNode
#     nodes::Ns
# end
# ZipNode(nodes...) = ZipNode(nodes)
# AbstractTrees.children(z::ZipNode) = ZipNode.(zip(children.(z.nodes)...))
# function AbstractTrees.nodevalue(z::ZipNode)
#     return nodevalue.(z.nodes)
# end
# Base.zip(nodes::Vararg{<:AbstractRuleNode}; strict = false) = zipnodes(nodes...; strict)
# Base.eltype(::Type{Z}) where {Ns, Z <: ZipNode{Ns}} = fieldtypes(Ns) 
# Base.show(io::IO, z::ZipNode) = Base.show_delim_array(io, z.nodes, "zipnodes(", ',', ')', false)
#
has_definite_children(::Type) = true

struct ZipNode{N<:Tuple} <: AbstractRuleNode
    nodes::N
end
ZipNode(nodes...) = ZipNode(nodes)

"""
    zipnodes(nodes...; strict = false)

Lazily construct a zipped version of the trees rooted at `nodes`.

Essentially, like `zip`, but for trees.

The `AbstractTrees.nodevalue` of each node is a tuple of the node values
of each of the `nodes`. The `AbstractTrees.children` of each node is a list of
tuples of the children of each of the nodes. First children in the first tuple,
second children in the second tuple, etc.

`strict = true` changes the behavior of `AbstractTrees.nodevalue` so that the
`nodevalue` becomes `missing` when the children of each of the nodes are not
compatible. Children are compatible if they have equal lengths, ignoring the
children of nodes that have unknown children (like [`Hole`](@ref)s).

```jldoctest
julia> rn1 = @rulenode 1{2,3};

julia> rn2 = @rulenode 3;

julia> zn = zipnodes(rn1, rn2; strict=false) 
zipnodes(1{2,3}, 3; strict=false)

julia> nodevalue(zn)
missing
```

The value of the node is `missing` because `rn1` has two children while `rn2`
has none, and both are [`RuleNode`](@ref)s which have definite children. This is in contrast to a [`Hole`](@ref), which has no children, but represents a node that *might* have children. If `rn1` above is swapped out with a [`Hole`](@ref), the nodevalue is no longer `missing`.

```jldoctest
julia> rn1 = @rulenode Hole[1, 1, 1]

julia> rn2 = @rulenode 3;

julia> zn = zipnodes(rn1, rn2; strict=false) 
zipnodes(Hole[Bool[1, 1, 1]], 3; strict=false)

julia> nodevalue(zn)
(Bool[1, 1, 1], 3)
```
"""
zipnodes(nodes...) = ZipNode(nodes...)
# Base.eltype(::Type{<:ZipNode{Z}}) where Z = eltype(Z)
Base.zip(nodes::Vararg{<:AbstractRuleNode}) = zipnodes(nodes...)
Base.show(io::IO, z::ZipNode) = Base.show_delim_array(io, z.nodes, "zip(", ',', ')', false)
AbstractTrees.children(z::ZipNode) = ZipNode.(zip(children.(z.nodes)...))

function _children_compatible(z::ZipNode{T}) where T
    # Holes don't have definite children, other nodes do by default
    ch_def = has_definite_children.(fieldtypes(T))
    ch_ls = length.(children.(z.nodes))
    def_and_ls = zip(ch_def, ch_ls)
    length_or_incompat = ((ch_def, ch_l),) -> ch_def ? ch_l : children_incompatible
    
    # All of the lengths of the children of nodes that have definite
    # children
    lengths_def_ch = skipincompatible(
        Iterators.map(length_or_incompat, def_and_ls)
    )
    return allequal(lengths_def_ch)
end

# like missing, but with different behavior on `isdisjoint` and `issubset`
# namely, the incompatibility propagates over set checks
struct ChildrenIncompatible end
const children_incompatible = ChildrenIncompatible()
isincompatible(::Any) = false
isincompatible(::ChildrenIncompatible) = true
Base.isdisjoint(::T, ::T) where T<:ChildrenIncompatible = children_incompatible
Base.issubset(::T, ::T) where T<:ChildrenIncompatible = children_incompatible
Base.to_index(::ChildrenIncompatible) = children_incompatible
Base.:(!)(::ChildrenIncompatible) = children_incompatible
skipincompatible(itr) = Iterators.filter(!isincompatible, itr)

function AbstractTrees.nodevalue(z::ZipNode{T}) where T
    ch_compat = _children_compatible(z)
    if !ch_compat
        return ntuple(Returns(children_incompatible), length(z.nodes))
    end
    return Base.to_index.(nodevalue.(z.nodes))
end

function mapreduce_nodes(f, op, nodes...; kw...)
    return mapreduce(f ∘ nodevalue, op, PreOrderDFS.(nodes)...; kw...)
end
function Base.mapreduce(f, op, nodes::Vararg{<:AbstractRuleNode}; kw...)
    return mapreduce_nodes(f, op, nodes...; kw...)
end

for func in [:any, :all]
    func_nodes = Symbol(func, :_nodes)

    @eval begin
        $func_nodes(f, rn) = $func(f ∘ nodevalue, PreOrderDFS(rn))
        function $func_nodes(f, zn::ZipNode)
            f1 = incompatible_to_false(splat(f)) ∘ nodevalue
            return $func(f1, PreOrderDFS(zn))
        end
        Base.$func(f, rn::AbstractRuleNode) = $func_nodes(f, rn)

        $func_nodes(rn) = $func_nodes(identity, rn)
        Base.$func(rn::AbstractRuleNode) = $func_nodes(rn)
    end
end

function incompatible_to_false(f, x)
    v = f(x)
    return v isa ChildrenIncompatible ? false : v
end
incompatible_to_false(f) = Base.Fix1(incompatible_to_false, f)

function equiv_to_t(t, equiv)
    fixed_zip = Base.Fix1(zip, t)
    fixed_all_equiv = Base.Fix1(all, equiv)
    return fixed_all_equiv ∘ fixed_zip 
end

function intree_rulenodes(t1, t2; equiv)
    f = equiv_to_t(t1, equiv)
    remapped_f = incompatible_to_false(equiv_to_t(t1, equiv))
    dfs = PreOrderDFS(t2)

    return any(remapped_f, dfs)
end

function AbstractTrees.intree(rn1::AbstractRuleNode, rn2::AbstractRuleNode; equiv = ===)
    return intree_rulenodes(rn1, rn2; equiv)
end

"""
	RuleNode <: AbstractRuleNode

A [`RuleNode`](@ref) represents a node in an expression tree.
Each node corresponds to a certain rule in the [`AbstractGrammar`](@ref).
A [`RuleNode`](@ref) consists of:

- `ind`: The index of the rule in the [`AbstractGrammar`](@ref) which this node is representing.
- `_val`: Field for caching evaluations of `RuleNode`s, preventing multiple unnecessary evaluations. The field can be used to store any needed infromation.
- `children`: The children of this node in the expression tree

!!! compat
	Evaluate immediately functionality is not yet supported by most of Herb.jl.
"""
mutable struct RuleNode <: AbstractRuleNode
    ind::Int # index in grammar
    _val::Any  #value of _() evals
    children::Vector{AbstractRuleNode}
end

Base.getindex(rn::AbstractRuleNode, inds...) = getindex(get_children(rn), inds...)
Base.view(rn::AbstractRuleNode, inds...) = view(get_children(rn), inds...)

"""
	update_rule_indices!(node::RuleNode, n_rules::Integer)

Updates the `node` as required when grammar size changes. Errors if the rule index exceeds new `n_rules`.

# Arguments
- `node`: The current `RuleNode` being processed
- `n_rules`: The new number of rules in the grammar
"""
function update_rule_indices!(node::RuleNode, n_rules::Integer)
    if get_rule(node) > n_rules
        error("Rule index $(get_rule(node)) exceeds the number of grammar rules ($n_rules).")
    end
    children = get_children(node)
    for child in children
        update_rule_indices!(child, n_rules)
    end
    return
end

"""
	update_rule_indices!(node::RuleNode, n_rules::Integer, mapping::AbstractDict{<:Integer, <:Integer})

Remap the rule indices of `node` and its children according to the provided `mapping`. Errors if the rule index exceeds new `n_rules`.

# Arguments
- `node`: The current `RuleNode` being processed
- `n_rules`: The new number of rules in the grammar
- `mapping`: A dictionary mapping old rule indices to new rule indices
"""
function update_rule_indices!(
        node::RuleNode,
        n_rules::Integer,
        mapping::AbstractDict{<:Integer, <:Integer}
    )
    rule_ind = get_rule(node)
    if rule_ind > n_rules
        error("Rule index $(rule_ind) exceeds the number of grammar rules ($n_rules).")
    end
    if haskey(mapping, rule_ind)
        node.ind = mapping[rule_ind]
    end
    children = get_children(node)
    for child in children
        update_rule_indices!(child, n_rules, mapping)
    end
    return
end

# Check whether the `node`'s rule index exceeds the number of rules `n_rules.`
function is_domain_valid(node::RuleNode, n_rules::Integer)
    if get_rule(node) > n_rules
        return false
    end
    return all(child -> is_domain_valid(child, n_rules), get_children(node))
end

"""
	AbstractHole <: AbstractRuleNode

A [`AbstractHole`](@ref) is a placeholder where certain rules from the grammar can still be applied.
The `domain` of a [`AbstractHole`](@ref) defines which rules can be applied.
The `domain` is a bitvector, where the `i`th bit is set to true if the `i`th rule in the grammar can be applied.
"""
abstract type AbstractHole <: AbstractRuleNode end

AbstractTrees.nodevalue(h::AbstractHole) = h.domain

"""
	Hole <: AbstractHole

An [`AbstractUniformHole`](@ref) is a placeholder where certain rules from the grammar can still be applied,
but all rules in the domain are required to have the same childtypes.
"""
abstract type AbstractUniformHole <: AbstractHole end

"""
	UniformHole <: AbstractHole

- `domain`: A bitvector, where the `i`th bit is set to true if the `i`th rule in the grammar can be applied. All rules in the domain are required to have the same childtypes.
- `children`: The children of this hole in the expression tree.
"""
mutable struct UniformHole <: AbstractUniformHole
    domain::BitVector
    children::Vector{AbstractRuleNode}
end

UniformHole(domain) = UniformHole(domain, AbstractRuleNode[])
Base.getindex(uh::UniformHole, inds...) = getindex(get_children(uh), inds...)

# Check if `hole`'s domain length matches `n_rules`.
function is_domain_valid(hole::AbstractHole, n_rules::Integer)
    if length(hole.domain) != n_rules
        return false
    end
    return all(child -> is_domain_valid(child, n_rules), get_children(hole))
end

"""
	update_rule_indices!(node::AbstractHole, n_rules::Integer)

Resize the domains of `hole` and its children. Errors if the length of the domain vector exceeds new `n_rules`.

# Arguments
- `hole`: The current `AbstractHole` being processed
- `n_rules`: The new number of rules in the grammar
"""
function update_rule_indices!(hole::AbstractHole, n_rules::Integer)
    if n_rules < length(hole.domain)
        error("Length domain vector $(length(hole.domain)) exceeds the number of grammar rules $(n_rules).")
    end
    append!(hole.domain, falses(n_rules - length(hole.domain)))
    children = get_children(hole)
    for child in children
        update_rule_indices!(child, n_rules)
    end
    return
end

"""
	update_rule_indices!(hole::AbstractHole, n_rules::Integer, mapping::AbstractDict{<:Integer, <:Integer})

Updates the domains of `hole` and its children according to `mapping`.
For `AbstractHole`s, this updates both the size of the domain `BitVector` and the rule indices. 
Errors if the length of the domain vector exceeds new `n_rules`.

# Arguments
- `hole`: The current `AbstractHole` being processed
- `n_rules`: The new number of rules in the grammar
- `mapping`: A dictionary mapping the old rule indices to new ones
"""
function update_rule_indices!(
        hole::AbstractHole,
        n_rules::Integer,
        mapping::AbstractDict{<:Integer, <:Integer}
    )
    update_rule_indices!(hole, n_rules) # resize (also checks n_rules)
    # update domain BV according to mapping
    rule_indices = findall(hole.domain)
    for i in rule_indices
        if haskey(mapping, i)
            hole.domain[i] = 0 # set old index to false
            hole.domain[mapping[i]] = 1 # set new index to true
        end
    end
    children = get_children(hole)
    for child in children
        update_rule_indices!(child, n_rules, mapping)
    end
    return
end

"""
	Hole <: AbstractHole


- `domain`: A bitvector, where the `i`th bit is set to true if the `i`th rule in the grammar can be applied.
"""
mutable struct Hole <: AbstractHole
    domain::BitVector
end
has_definite_children(::Type{<:Hole}) = false

"""
	HoleReference

Contains a hole and the path to the hole from the root of the tree.
"""
struct HoleReference
    hole::AbstractHole
    path::Vector{Int}
end

RuleNode(ind::Int) = RuleNode(ind, nothing, AbstractRuleNode[])
"""
	RuleNode(ind::Int, children::Vector{AbstractRuleNode})

Create a [`RuleNode`](@ref) for the [`AbstractGrammar`](@ref) rule with index `ind` and `children` as subtrees.
"""
RuleNode(ind::Int, children::Vector{<:AbstractRuleNode}) = RuleNode(ind, nothing, children)

function _get_hole_name(holetype)
    if isdefined(@__MODULE__, holetype)
        return holetype
    else
        throw(
            ArgumentError(
                styled"""
                Input to the {code:@rulenode} macro appears to be a hole, but the macro does not support the type: {code:{red:$holetype}}. The macro currently supports concrete subtypes of {code:AbstractHole}s {bold:that are defined in {code:HerbCore.jl}}.
                """,
            ),
        )
    end
end

function _shorthand2rulenode(i::Integer)
    return :(RuleNode($i))
end

function _shorthand2rulenode(ex::Expr)
    # holes with children
    ex = postwalk(ex) do x
        @capture(x, holetype_[Bool[domain__]]{children__}) ||
            @capture(x, holetype_[domain__]{children__}) || return x
        hole_constructor = _get_hole_name(holetype)
        return :($hole_constructor(BitVector([$(domain...)]), {$(children...)}))
    end

    # holes without children
    # need to prewalk here becuase otherwise we try to parse UniformHole[Bool[...]] first as a
    # hole with type Bool, instead of a UniformHole
    ex = prewalk(ex) do x
        @capture(x, holetype_Symbol[Bool[domain__]]) ||
            @capture(x, holetype_Symbol[domain__]) || return x
        hole_constructor = _get_hole_name(holetype)
        return :($hole_constructor(BitVector([$(domain...)])))
    end

    # rulenodes with children
    ex = postwalk(ex) do x
        @capture(x, index_Int{children__}) || return x
        return :(RuleNode($index, {$(children...)}))
    end

    # rulenodes without children
    ex = postwalk(ex) do x
        @capture(x, {children__}) || return x
        children = [
            isexpr(child, Int, Integer) ? :(RuleNode($child)) : child
                for child in children
        ]
        return :([$(children...)])
    end

    return ex
end

"""
	@rulenode 

Construct a [`RuleNode`](@ref) using the shorthand notation [`RuleNode`](@ref)s
and [`AbstractHole`](@ref)s are printed with using [`Base.show`](@ref).

Does not yet support [`AbstractHole`](@ref)s defined outside of [`HerbCore`](@ref).

!!! note "Hole domain representation"
	[`AbstractHole`](@ref)s' domains are printed with a `Bool[...]` surrounding them.
	The macro accepts the domain with or without the `Bool[...]`:
	`UniformHole[Bool[1, 1, 0, 0]]{2,3}` and `UniformHole[1, 1, 0, 0]{2,3}` both work.

# Examples

```jldoctest
julia> @rulenode 1{4{5,6},1{2,3}}
1{4{5,6},1{2,3}}

julia> @rulenode 1
1

julia> @rulenode 1{2, 3}
1{2,3}

julia> @rulenode UniformHole[1, 1, 0, 0]{2,3}
UniformHole[Bool[1, 1, 0, 0]]{2,3}

julia> @rulenode Hole[1, 1, 0, 0]
Hole[Bool[1, 1, 0, 0]]

```
"""
macro rulenode(ex::Union{Integer, Expr})
    return _shorthand2rulenode(ex)
end

"""
	RuleNode(ind::Int, _val::Any)

Create a [`RuleNode`](@ref) for the [`AbstractGrammar`](@ref) rule with index `ind`, 
`_val` as immediately evaluated value and no children

!!! warning
	Only use this constructor if you are absolutely certain that a rule is terminal and cannot have children.
	Use [`RuleNode(ind::Int, grammar::AbstractGrammar)`] for rules that might have children.
	In general, [`AbstractHole`](@ref)s should be used as a placeholder when the children of a node are not yet known.   

!!! compat
	Evaluate immediately functionality is not yet supported by most of Herb.jl.
"""
RuleNode(ind::Int, _val::Any) = RuleNode(ind, _val, AbstractRuleNode[])

Base.:(==)(::RuleNode, ::AbstractHole) = false
Base.:(==)(::AbstractHole, ::RuleNode) = false
function Base.:(==)(A::RuleNode, B::RuleNode)
    return (A.ind == B.ind) && A.children == B.children
        # length(A.children) == length(B.children) && #required because zip doesn't check lengths
        # all(isequal(a, b) for (a, b) in zip(A.children, B.children))
end
function Base.:(==)(a::UniformHole, b::UniformHole)
    return a.domain == b.domain && a.children == b.children ##length(a.children) == length(b.children) &&
        # all(isequal(a, b) for (a, b) in zip(a.children, b.children))
end
function Base.:(==)(a::Hole, b::Hole)
    return a.domain == b.domain
end

Base.copy(r::RuleNode) = RuleNode(r.ind, r._val, r.children)
Base.copy(h::Hole) = Hole(copy(h.domain))
Base.copy(h::UniformHole) = UniformHole(copy(h.domain), h.children)

# can specialize deepcopy because there are no objects within RuleNodes, Holes, and UniformHoles
# that share memory with eachother. The default deepcopy maintains egality (===) of descendant
# objects. Note that if you implement a new type that shares children (effectively a DAG)
# This assumption no longer holds and a different deepcopy is needed (or just use the existing
# definition in Base)

Base.deepcopy(r::RuleNode) = RuleNode(r.ind, r._val, [deepcopy(c) for c in r.children])
Base.deepcopy(h::Hole) = Hole(copy(h.domain)) # since this is a bitvector, regular copy is fine
Base.deepcopy(h::UniformHole) = UniformHole(copy(h.domain), [deepcopy(c) for c in h.children])

function Base.hash(node::RuleNode, h::UInt)
    retval = hash(node.ind, h)
    for child in node.children
        retval = hash(child, retval)
    end
    return retval
end

function Base.hash(node::AbstractHole, h::UInt)
    return hash(node.domain, h)
end

function Base.hash(node::UniformHole, x::UInt)
    x = hash(node.domain)
    for child in get_children(node)
        x = hash(child, x)
    end
    return x
end

function Base.show(io::IO, node::RuleNode; separator = ",")
    print(io, node.ind)
    return if !isempty(children(node))
        Base.show_enclosed_list(io, "{", children(node), separator, "}", 0)
    end
end

function Base.show(io::IO, node::AbstractHole; _...)
    return print(io, "Hole[$(node.domain)]")
end

function Base.show(io::IO, node::UniformHole; separator = ",")
    print(io, "UniformHole[$(node.domain)]")
    return if !isempty(node.children)
        Base.show_enclosed_list(io, "{", children(node), separator, "}", 0)
    end
end

"""
	Base.length(root::RuleNode)

Return the number of nodes in the tree rooted at root.
"""
function Base.length(root::AbstractRuleNode)
    retval = 1
    for c in get_children(root)
        retval += length(c)
    end
    return retval
end
Base.length(::Hole) = 1

"""
	Base.isless(rn₁::AbstractRuleNode, rn₂::AbstractRuleNode)::Bool

Compares two [`RuleNode`](@ref)s. Returns true if the left [`RuleNode`](@ref) is less than the right [`RuleNode`](@ref).
Order is determined from the index of the [`RuleNode`](@ref)s.
If both [`RuleNode`](@ref)s have the same index, a depth-first search is
performed in both [`RuleNode`](@ref)s until nodes with a different index
are found.
"""
Base.isless(
    rn₁::AbstractRuleNode, rn₂::AbstractRuleNode
)::Bool = _rulenode_compare(
    rn₁, rn₂
) == -1

function _rulenode_compare(rn₁::AbstractRuleNode, rn₂::AbstractRuleNode)::Int
    # Helper function for Base.isless
    if !isfilled(rn₁) || !isfilled(rn₂)
        throw(ArgumentError("Unable to compare nodes of types ($(typeof(rn₁)), $(typeof(rn₂)))"))
    end
    if get_rule(rn₁) == get_rule(rn₂)
        for (c₁, c₂) in zip(get_children(rn₁), get_children(rn₂))
            comparison = _rulenode_compare(c₁, c₂)
            if comparison ≠ 0
                return comparison
            end
        end
        return 0
    else
        return get_rule(rn₁) < get_rule(rn₂) ? -1 : 1
    end
end

"""
	depth(root::RuleNode)::Int

Return the depth of the [`AbstractRuleNode`](@ref) tree rooted at root.
Holes do count towards the depth.
"""
function depth(root::AbstractRuleNode)::Int
    retval = 1
    for c in root.children
        retval = max(retval, depth(c) + 1)
    end
    return retval
end

depth(::Hole) = 1

"""
	node_depth(root::AbstractRuleNode, node::AbstractRuleNode)::Int

Return the depth of `node` for an [`AbstractRuleNode`](@ref) tree rooted at `root`.
Depth is `1` when `root == node`.

!!! warning
	`node` must be a subtree of `root` in order for this function to work.
"""
function node_depth(root::AbstractRuleNode, node::AbstractRuleNode)::Int
    root ≡ node && return 1
    root isa Hole && return 1
    for c in root.children
        d = node_depth(c, node)
        d > 0 && (return d + 1)
    end
    return 0
end

"""
	rulesoftype(node::RuleNode, ruleset::Set{Int}[, ignoreNode::AbstractRuleNode])
	rulesoftype(node::RuleNode, rule_index::Int[, ignoreNode::AbstractRuleNode])

Returns every rule in the ruleset that is also used in the [`AbstractRuleNode`](@ref) tree, but not in the `ignoreNode` subtree.

!!! warning
	The `ignoreNode` must be a subtree of `node` for it to have an effect.
"""
function rulesoftype(
        node::RuleNode, ruleset::Set{Int},
        ignoreNode::Union{Nothing, AbstractRuleNode} = nothing
    )::Set{Int}
    retval = Set{Int}()

    if !isnothing(ignoreNode) && node == ignoreNode
        return retval
    end

    if get_rule(node) ∈ ruleset
        union!(retval, [get_rule(node)])
    end

    if isempty(node.children)
        return retval
    else
        for child in node.children
            union!(retval, rulesoftype(child, ruleset, ignoreNode))
        end

        return retval
    end
end
function rulesoftype(
        node::RuleNode, rule_index::Int,
        ignoreNode::Union{Nothing, AbstractRuleNode} = nothing
    )
    return rulesoftype(node, Set{Int}(rule_index), ignoreNode)
end

rulesoftype(::Hole, ::Vararg{Any}) = Set{Int}()

"""
	rulesoftype(node::RuleNode, grammar::AbstractGrammar, ruletype::Symbol[, ignoreNode::AbstractRuleNode])

Returns every rule of nonterminal symbol `ruletype` from the `grammar` that is also used in the [`AbstractRuleNode`](@ref) tree, but not in the `ignoreNode` subtree.

!!! warning
	The `ignoreNode` must be a subtree of `node` for it to have an effect.
"""
function rulesoftype(
        node::RuleNode,
        grammar::AbstractGrammar,
        ruletype::Symbol,
        ignoreNode::Union{Nothing, RuleNode} = nothing
    )
    return rulesoftype(
        node, Set(grammar[ruletype]), ignoreNode
    )
end

"""
	contains_index(rulenode::RuleNode, index::Int)

Returns true if the rulenode contains the index.
"""
function contains_index(rulenode::AbstractRuleNode, index::Int)
    return !isempty(
        rulesoftype(
            rulenode, index
        )
    )
end

"""
	swap_node(expr::AbstractRuleNode, new_expr::AbstractRuleNode, path::Vector{Int})

Replace a node in `expr`, specified by `path`, with `new_expr`.
Path is a sequence of child indices, starting from the root node.
"""
function swap_node(expr::AbstractRuleNode, new_expr::AbstractRuleNode, path::Vector{Int})
    return if length(path) == 1
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
    return if expr == node
        node.children[child_index] = new_expr
    else
        for child in expr.children
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
        return append!(
            [node.ind],
            get_rulesequence(get(node.children, path[begin], RuleNode(0)), path[2:end])
        )
    else
        # if only one item left in the path
        # need to access the child with get because it can happen that the child is not yet built
        return append!(
            [node.ind],
            get_rulesequence(get(node.children, path[begin], RuleNode(0)), Vector{Int}())
        )
    end
end

get_rulesequence(::AbstractHole, ::Vector{Int}) = Vector{Int}()

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
        for i in 1:(path[begin] - 1)
            union!(ruleset, rulesonleft(expr.children[i], Vector{Int}()))
        end
        return ruleset
    else
        # collect all subtrees up to the child indexed in the path
        ruleset = Set{Int}(expr.ind)
        for i in 1:(path[begin] - 1)
            union!(ruleset, rulesonleft(expr.children[i], Vector{Int}()))
        end
        union!(ruleset, rulesonleft(expr.children[path[begin]], path[2:end]))
        return ruleset
    end
end

rulesonleft(h::AbstractHole, loc::Vector{Int}) = Set{Int}(findall(h.domain))

"""
    get_node_at_location(root::AbstractRuleNode, location::AbstractVector{<:Integer})

Traverse the tree rooted at `root` via the child indices given by `location`.

```jldoctest
julia> rn = @rulenode 1{2{3,4},5};

julia> get_node_at_location(rn, [1, 2])
4

julia> get_node_at_location(rn, [2])
5

julia> get_node_at_location(rn, Int[])
1{2{3,4},5}
```
"""
function get_node_at_location(root::AbstractRuleNode, location::AbstractVector{<:Integer})
    return getdescendant(root, location)
end

"""
	get_path(root::AbstractRuleNode, node::AbstractRuleNode)

Returns the path from the `root` to the `targetnode`. Returns nothing if no path exists.
"""
function get_path(
        root::AbstractRuleNode, targetnode::AbstractRuleNode
    )::Union{Vector{Int}, Nothing}
    if root === targetnode
        return Vector{Int}()
    end
    for (i, child) in enumerate(get_children(root))
        path = get_path(child, targetnode)
        if !isnothing(path)
            return prepend!(path, i)
        end
    end
    return nothing
end

"""
	number_of_holes(rn::AbstractRuleNode)::Int

Recursively counts the number of holes in an [`AbstractRuleNode`](@ref)
"""
function number_of_holes(rn::RuleNode)
    return reduce(
        +, [number_of_holes(c) for c in rn.children], init = 0
    )
end
function number_of_holes(rn::UniformHole)
    return 1 + reduce(+, [number_of_holes(c) for c in rn.children], init = 0)
end
number_of_holes(rn::Hole) = 1

"""
	contains_hole(rn::RuleNode) = any(contains_hole(c) for c ∈ rn.children)

Checks if an [`AbstractRuleNode`](@ref) tree contains a [`AbstractHole`](@ref).
"""
contains_hole(rn::RuleNode) = any(contains_hole(c) for c in rn.children)
contains_hole(hole::AbstractHole) = true

"""
	contains_nonuniform_hole(rn::RuleNode)

Checks if an [`AbstractRuleNode`](@ref) tree contains a [`Hole`](@ref).
"""
function contains_nonuniform_hole(rn::AbstractRuleNode)
    return any(
        contains_nonuniform_hole(c)
            for c in rn.children
    )
end
contains_nonuniform_hole(hole::Hole) = true

#Shared reference to an empty vector to reduce memory allocations.
const NOCHILDREN = Vector{AbstractRuleNode}()

"""
	get_children(rn::AbstractRuleNode)

Returns the children of the given [`AbstractRuleNode`](@ref)
"""
get_children(rn::AbstractRuleNode)::Vector{AbstractRuleNode} = rn.children
get_children(::Hole)::Vector{AbstractRuleNode} = NOCHILDREN
get_children(h::UniformHole)::Vector{AbstractRuleNode} = h.children

"""
	isuniform(rn::AbstractRuleNode)

Returns true iff the children of the [`AbstractRuleNode`](@ref) are known.
"""
isuniform(::RuleNode)::Bool = true
isuniform(::Hole)::Bool = false
isuniform(::UniformHole)::Bool = true

"""
	isfilled(node::AbstractRuleNode)::Bool

Returns whether the [`AbstractRuleNode`] holds a single rule. This is always the case for [`RuleNode`](@ref)s.
Holes are considered to be "filled" iff their domain size is exactly 1.
"""
isfilled(rn::RuleNode)::Bool = true
isfilled(hole::UniformHole)::Bool = (sum(hole.domain) == 1)
isfilled(hole::Hole)::Bool = (sum(hole.domain) == 1)

"""
	function hasdynamicvalue(rn::AbstractRuleNode)::Bool

Returns true iff the rule has a `_val` field set up.
"""
hasdynamicvalue(rn::RuleNode)::Bool = !isnothing(rn._val)
hasdynamicvalue(rn::AbstractRuleNode)::Bool = false

"""
	get_rule(rn::AbstractRuleNode)

Returns the index of the rule that this [`AbstractRuleNode`](@ref) represents
"""
get_rule(rn::RuleNode) = rn.ind
function get_rule(hole::AbstractHole)
    @assert isfilled(hole) "$(hole) is not filled, unable to get the rule"
    return findfirst(hole.domain)
end

"""
	have_same_shape(node1::AbstractRuleNode, node2::AbstractRuleNode)

Returns true iff `node1` and `node2` have the same shape
Example:
RuleNode(3, [
	RuleNode(1),
	RuleNode(1)
]) and
RuleNode(9, [
	RuleNode(2),
	Hole(domain)
])
have the same shape: 1 root with 2 children.
"""
function have_same_shape(node1, node2)
    children1 = get_children(node1)
    children2 = get_children(node2)
    if length(children1) != length(children2)
        return false
    end
    if length(children1) > 0
        for (child1, child2) in zip(children1, children2)
            if !have_same_shape(child1, child2)
                return false
            end
        end
    end
    return true
end
