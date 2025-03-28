using AbstractTrees: children, nodevalue, treeheight
using Aqua
using HerbCore
using Test

@testset "HerbCore.jl" verbose=true begin
    @testset "Aqua Tests" Aqua.test_all(HerbCore)

    include("test_rulenode.jl")
    include("test_grammar.jl")
end
