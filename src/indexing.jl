"""
    update_rule_indices!(...)

Updates the rule indices of the given rule node, hole or grammar constraint when the grammar changes. 
"""
function update_rule_indices! end

"""
    is_domain_valid(x, n_rules::Integer)
    is_domain_valid(x, grammar::AbstractGrammar)

Check if the domain for the given object (rule node, hole or constraint) is valid wrt the provided grammar or number of rules.
For objects `<:AbstractRuleNode`, it checks if all children are valid.
"""
function is_domain_valid end
