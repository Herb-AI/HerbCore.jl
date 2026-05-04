using HerbCore
using HerbCore: Next
using AbstractTrees: PreOrderDFS
using HerbSearch: heuristic_leftmost
using BenchmarkTools

function make_rulenode_bench()
    suite = BenchmarkGroup()
    rn = @rulenode 1{2{3}}
    suite["PreOrderDFS traversal small"] = @benchmarkable collect(preorder) setup=(preorder=PreOrderDFS($rn)) 
    suite["leftmost small"] = @benchmarkable heuristic_leftmost($rn, 10)
    rn = @rulenode 1{2{3},4{5{6{7}}}}
    suite["PreOrderDFS traversal"] = @benchmarkable collect(preorder) setup=(preorder=PreOrderDFS($rn)) 
    suite["leftmost"] = @benchmarkable heuristic_leftmost($rn, 10)

    return suite
end

function make_typestable_bench()
    suite = BenchmarkGroup()
    rn = Next.@rulenode Next.RuleNode 1{2{3}}
    suite["PreOrderDFS traversal small"] = @benchmarkable collect(preorder) setup=(preorder=PreOrderDFS($rn)) 
    suite["leftmost small"] = @benchmarkable heuristic_leftmost($rn, 10)
    rn = Next.@rulenode Next.RuleNode 1{2{3},4{5{6{7}}}} 
    suite["PreOrderDFS traversal"] = @benchmarkable collect(preorder) setup=(preorder=PreOrderDFS($rn)) 
    suite["leftmost"] = @benchmarkable heuristic_leftmost($rn, 10)

    return suite
end

function make_suite()
    suite = BenchmarkGroup()
    suite["RuleNode"] = make_rulenode_bench()
    suite["Next.RuleNode"] = make_typestable_bench()

    return suite 
end

function create_or_load_params(suite)
    params_file = joinpath(@__DIR__, "params.json")
    if isfile(params_file)
        loadparams!(suite, BenchmarkTools.load(params_file)[1])
    else
        tune!(suite)
        BenchmarkTools.save(params_file, params(suite))
    end

    return suite
end

const SUITE = create_or_load_params(make_suite())
