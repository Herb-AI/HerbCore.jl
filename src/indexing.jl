"""
    update_rule_indices!(...)

Updates the rule indices of the given rule node, hole or grammar constraint when the grammar changes. 
"""
function update_rule_indices! end

"""
    is_domain_valid(...)

Returns whether or not the domain of the given rule node, hole or grammar constraint is valid wrt `grammar``.
"""
function is_domain_valid end