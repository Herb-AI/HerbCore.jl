module HerbCore

using AbstractTrees

include("grammar.jl")
include("rulenode.jl")
include("constraint.jl")

const HOLE_NAMES = Dict()

function __init__()
    HOLE_NAMES[:hole] = Hole
    HOLE_NAMES[:fshole] = UniformHole
end

export
       AbstractRuleNode,
       RuleNode,
       @rulenode,
       AbstractHole,
       AbstractUniformHole,
       UniformHole,
       Hole,
       HoleReference, depth,
       node_depth,
       rulesoftype,
       contains_index,
       swap_node,
       get_rulesequence,
       rulesonleft,
       get_node_at_location,
       get_path,
       number_of_holes,
       contains_hole,
       contains_nonuniform_hole,
       get_children,
       get_rule,
       isuniform,
       isfilled,
       hasdynamicvalue,
       have_same_shape, AbstractConstraint,
       AbstractGrammar,
       print_tree

end # module HerbCore
