module HerbCore

include("rulenode.jl")
include("constraint.jl")
include("grammar.jl")

export 
    AbstractRuleNode,
    RuleNode,
    Hole,
    FixedShapedHole,
    VariableShapedHole,
    HoleReference,

    depth,
    node_depth,
    rulesoftype,
    swap_node,
    get_rulesequence,
    rulesonleft,
    get_node_at_location,
    get_node_path,
    contains_hole,
    contains_variable_shaped_hole,

    Constraint,
    Grammar

end # module HerbCore
