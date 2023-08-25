using HerbCore
using Test

@testset "HerbCore.jl" verbose=true begin
    @testset "RuleNode tests"  begin 
        @testset "Equality tests" begin 
            @test RuleNode(1) == RuleNode(1)

            node = RuleNode(1,[RuleNode(2),RuleNode(3)])
            @test node == node
            @test RuleNode(1,[RuleNode(2),RuleNode(3)]) == RuleNode(1,[RuleNode(2),RuleNode(3)])
            @test RuleNode(1,[RuleNode(2),node]) == RuleNode(1,[RuleNode(2),node])


            @test RuleNode(1) !== RuleNode(2)
            @test RuleNode(1,[RuleNode(2),RuleNode(3)]) !== RuleNode(2,[RuleNode(2),RuleNode(3)])

        end

        @testset "Hash tests" begin 
            node = RuleNode(1,[RuleNode(2),RuleNode(3)])
            @test hash(node) == hash(node)
            @test hash(node) == hash(RuleNode(1,[RuleNode(2),RuleNode(3)]))
            @test hash(RuleNode(1,[RuleNode(2)])) !== hash(RuleNode(1))
        end

        @testset "Depth tests" begin 
            @test depth(RuleNode(1)) == 1
            @test depth(RuleNode(1,[RuleNode(2), RuleNode(3)])) == 2
        end

        @testset "Length tests" begin 
            @test length(RuleNode(1)) == 1
            @test length(RuleNode(1,[RuleNode(2), RuleNode(3)])) == 3
            @test length(RuleNode(1,[RuleNode(2, [RuleNode(3), RuleNode(4)])])) == 4
        end
        @testset "RuleNode compare" begin
            @test HerbCore._rulenode_compare(RuleNode(1), RuleNode(1)) == 0
            @test RuleNode(1) < RuleNode(2)
            @test RuleNode(2) > RuleNode(1) 
            @test RuleNode(1,[RuleNode(2)]) < RuleNode(1,[RuleNode(3)]) 
            @test RuleNode(1,[RuleNode(2)]) < RuleNode(2,[RuleNode(1)]) 
        end

        @testset "Node depth from a tree" begin 
            #=    1      -- depth 1
               2  3  4   -- depth 2
                    5  6 -- depth 3

            =#
            rulenode = RuleNode(1,[RuleNode(2),RuleNode(3),RuleNode(4,[RuleNode(5),RuleNode(6)])])
            @test node_depth(rulenode, rulenode) == 1
            
            @test node_depth(rulenode, rulenode.children[1]) == 2
            @test node_depth(rulenode, rulenode.children[2]) == 2
            @test node_depth(rulenode, rulenode.children[3]) == 2
            
            @test node_depth(rulenode, rulenode.children[3].children[1]) == 3
            @test node_depth(rulenode, rulenode.children[3].children[2]) == 3

            # in case a random node appears the node_depth is 0
            @test node_depth(rulenode, RuleNode(100)) == 0
        end

        @testset "rule sequence" begin 

            #=    1      
               2  3  4   
                    5  6 
                   7    9
                          10 
            =#
            rulenode = 
                RuleNode(1,
                    [
                        RuleNode(2),
                        RuleNode(3),
                        RuleNode(4,
                            [
                                RuleNode(5,
                                    [RuleNode(7)]
                                ),
                                RuleNode(6,
                                    [RuleNode(9,[RuleNode(10)])]
                                )
                            ]
                        )
                    ]
                )
            @test get_rulesequence(rulenode, [3,1,1]) == [1,4,5,7]
            @test get_rulesequence(rulenode, [3,2,1]) == [1,4,6,9]
            @test get_rulesequence(rulenode, [3,2,1,1]) == [1,4,6,9,10]

            # putting out of bounds indices returns the root
            @test get_rulesequence(rulenode, [100,4,1000]) == [1]
        end
    end
end
