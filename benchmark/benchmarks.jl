using BenchmarkTools: @benchmarkable, BenchmarkGroup
using HerbCore
using HerbCore: all_tree, zipnodes
using AbstractTrees: intree

include("examples.jl")

const SUITE = BenchmarkGroup()

function bench_deepcopy_rulenodes()
    suite = BenchmarkGroup(["rulenodes"])

    for (name, setup_fn) in pairs(all_trees())
        suite[name] = @benchmarkable deepcopy(tree) setup=(tree=$setup_fn())
    end

    return suite
end

function bench_get_node_at_location()
    suite = BenchmarkGroup(["rulenodes"])

    locations = Dict(
        "Big tree" => [Int[], [1, 1, 1, 2]],
        "Small uniform tree" => [Int[], [2, 2], [1, 2]],
        "Tree with hole middle" => [[2, 2]],
        "Tree with hole left side" => [[1, 1]],
        "Small tree" => [Int[], [2], [1, 2]]
    )

    for (name, setup_fn) in pairs(all_trees())
        for l in locations[name]
            suite[name] = @benchmarkable get_node_at_location(tree, $l) setup=(tree=$setup_fn())
        end
    end

    return suite
end

function bench_intree()
    suite = BenchmarkGroup(["rulenodes", "iteration"])
    rn1 = @rulenode 3{2,Hole[0, 1, 1, 1]}
    rn2 = @rulenode 3{2,4}
    rn3 = @rulenode 3{2,3{4,2}}

    suite["overlapping domain"] = @benchmarkable intree(
        $rn1, $rn2;
        equiv = $((rn1, rn2) -> all_tree(!isdisjoint, rn1, rn2))
    )

    return suite
end

function populate!(suite)
    suite["deepcopy"] = bench_deepcopy_rulenodes()
    suite["get_node_at_location"] = bench_get_node_at_location()
    return nothing
end

populate!(SUITE)
