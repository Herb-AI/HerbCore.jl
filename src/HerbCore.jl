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
    number_of_holes,
    contains_hole,
    contains_variable_shaped_hole,
    get_children,
    get_rule,
    has_children,
    isfilled,

    Constraint,
    Grammar

end # module HerbCore
