@testitem "StableDomainRuleNode" begin
    using HerbCore: StableDomainRuleNode, get_domain

    sdrn = StableDomainRuleNode(trues(10))

    @test isempty(get_children(sdrn))
    @test all(get_domain(sdrn))
    @test length(get_domain(sdrn)) == 10

    sdrn_with_children = StableDomainRuleNode(trues(10), [StableDomainRuleNode(trues(10))])
    @test !isempty(get_children(sdrn_with_children))
    @test length(get_children(sdrn_with_children)) == 1
    @test length(get_domain(sdrn_with_children)) == 10
end
