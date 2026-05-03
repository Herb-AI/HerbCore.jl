@testitem "DomainRuleNode" tags = [:next] begin
    using HerbCore: get_domain
    using HerbCore.Next: DomainRuleNode

    drn = DomainRuleNode(trues(10))

    @test isempty(get_children(drn))
    @test all(get_domain(drn))
    @test length(get_domain(drn)) == 10

    drn_with_children = DomainRuleNode(trues(10), [DomainRuleNode(trues(10))])
    @test !isempty(get_children(drn_with_children))
    @test length(get_children(drn_with_children)) == 1
    @test length(get_domain(drn_with_children)) == 10
end

@testitem "@rulenode" tags = [:next] begin
    using HerbCore: get_domain, get_children
    using HerbCore.Next: @rulenode
    
    drn_int = @rulenode DomainRuleNode 1{2,3}
    @test length(get_children(drn_int)) == 2
    @test typeof(get_domain(drn_int)) == Int

    drn_bitset = @rulenode DomainRuleNode{BitSet} 1{2,3}
    @test typeof(get_domain(drn_bitset)) == BitSet
    @test all(typeof.(get_domain.(get_children(drn_bitset))) .== BitSet)
end
