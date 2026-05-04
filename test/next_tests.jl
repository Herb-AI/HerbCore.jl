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
    using HerbCore.Next: @rulenode, RuleNode, get_rules
    
    drn_int = @rulenode RuleNode 1{2,3}
    @test length(get_children(drn_int)) == 2
    @test typeof(get_rules(drn_int)) == Int

    drn_bitset = @rulenode RuleNode{BitSet} 1{2,3}
    @test typeof(get_rules(drn_bitset)) == BitSet
    @test all(typeof.(get_rules.(get_children(drn_bitset))) .== BitSet)
end
