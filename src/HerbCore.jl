module HerbCore

include("rulenode.jl")
include("constraint.jl")
include("grammar.jl")

export
       AbstractRuleNode,
       RuleNode,
       AbstractHole,
       AbstractUniformHole,
       UniformHole,
       Hole,
       HoleReference, depth,
       node_depth,
       rulesoftype,
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
       AbstractGrammar

end # module HerbCore
