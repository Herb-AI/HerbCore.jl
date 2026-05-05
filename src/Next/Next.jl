module Next
import AbstractTrees
using DispatchDoctor: @stable, @unstable
using ..HerbCore: HerbCore, AbstractRuleNode, AbstractGrammar, get_children
using MacroTools: @capture, postwalk, iscall
using AutoHashEquals: @auto_hash_equals

@stable begin
include("grammar.jl")
include("rulenode.jl")

end # @stable
end # module Next
