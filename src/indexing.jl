"""
    update_rule_indices!(...)

Updates the rule indices of the given rule node, hole or grammar constraint when the grammar changes. 
"""
function update_rule_indices! end

"""
    is_domain_valid(x, n_rules::Integer)
    is_domain_valid(x, grammar::AbstractGrammar)

Check if the domain for the given object `x` (ex: [`RuleNode`](@ref),
[`Hole`](@ref) or [`AbstractConstraint`](@ref)) is valid given the provided
grammar or number of rules.

If [`isfilled`](@ref)`(x)` and `x` has children, it checks if all children are valid.
"""
function is_domain_valid end

"""
    issame(a, b)

Returns whether the two given objects `a` and `b` (ex: [`RuleNode`](@ref),
[`Hole`](@ref) or [`AbstractConstraint`](@ref)) are the same.
"""
function issame(a, b)
    false
end
