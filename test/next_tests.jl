@testitem "RuleNode" tags = [:next] begin
    using HerbCore.Next: RuleNode, get_rules

    drn = RuleNode(trues(10))

    @test isempty(get_children(drn))
    @test all(get_rules(drn))
    @test length(get_rules(drn)) == 10

    drn_with_children = RuleNode(trues(10), [RuleNode(trues(10))])
    @test !isempty(get_children(drn_with_children))
    @test length(get_children(drn_with_children)) == 1
    @test length(get_rules(drn_with_children)) == 10
end

@testitem "@rulenode" tags = [:next] begin
    using HerbCore.Next: @rulenode, DomainLabel, get_rules, get_label, get_min, get_max
    using AbstractTrees: PreOrderDFS
    
    rn_int = @rulenode Int 1{2,3}
    @test length(get_children(rn_int)) == 2
    @test typeof(get_rules(rn_int)) == Int

    rn_int = @rulenode Int 1{2{3}}
    @test length(get_children(rn_int)) == 1
    @test typeof(get_rules(rn_int)) == Int

    rn_bitset = @rulenode BitSet 1{2,3}
    @test typeof(get_rules(rn_bitset)) == BitSet
    @test all(typeof.(get_rules.(get_children(rn_bitset))) .== BitSet)

    rn_bitset_labels = @rulenode DomainLabel{BitSet,Symbol} 1{X: 2} 
    @test typeof(get_label(rn_bitset_labels)) == Symbol
    @test map(x -> get_label(x), PreOrderDFS(rn_bitset_labels)) == [:_, :X]

    rn_bitset_count = @rulenode DomainCount{BitSet} [1,2]{2 <= [3,4] <= 4} 
    @test get_min(first(get_children(rn_bitset_count))) == 2
    @test get_max(first(get_children(rn_bitset_count))) == 4
    rn_bitset_count_root = @rulenode DomainCount{BitSet} (2 <= [3,4] <= 4){} 
    @test get_min(rn_bitset_count_root) == 2
    @test get_max(rn_bitset_count_root) == 4
    # @test map(x -> get_label(x), PreOrderDFS(rn_bitset_labels)) == [:_, :X]
end
