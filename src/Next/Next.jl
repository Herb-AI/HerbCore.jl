module Next
import AbstractTrees
using DispatchDoctor: @stable
using ..HerbCore: HerbCore, AbstractRuleNode, AbstractGrammar
using MacroTools: @capture, postwalk, iscall

@stable begin
include("grammar.jl")
include("rulenode.jl")

end # @stable
end # module Next
