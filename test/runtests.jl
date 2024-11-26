using AbstractTrees: children, nodevalue, treeheight
using HerbCore
using HerbGrammar: @csgrammar
using Test

@testset "HerbCore.jl" verbose=true begin
    @testset "AbstractTrees Interface" begin
        @show typeof(nodevalue(RuleNode(1)))
        @test nodevalue(RuleNode(1)) == 1
        @test isempty(children(RuleNode(1)))
        @test length(children(RuleNode(1, [RuleNode(2), RuleNode(2)]))) == 2
        @test treeheight(RuleNode(1)) == 0
        @test treeheight(RuleNode(1, [RuleNode(2), RuleNode(2)])) == 1
    end

    @testset "RuleNode tests" begin
        @testset "Equality tests" begin
            @test RuleNode(1) == RuleNode(1)

            node = RuleNode(1, [RuleNode(2), RuleNode(3)])
            @test node == node
            @test RuleNode(1, [RuleNode(2), RuleNode(3)]) ==
                  RuleNode(1, [RuleNode(2), RuleNode(3)])
            @test RuleNode(1, [RuleNode(2), node]) == RuleNode(1, [RuleNode(2), node])

            @test RuleNode(1) !== RuleNode(2)
            @test RuleNode(1, [RuleNode(2), RuleNode(3)]) !==
                  RuleNode(2, [RuleNode(2), RuleNode(3)])
        end

        @testset "Hash tests" begin
            node = RuleNode(1, [RuleNode(2), RuleNode(3)])
            @test hash(node) == hash(node)
            @test hash(node) == hash(RuleNode(1, [RuleNode(2), RuleNode(3)]))
            @test hash(RuleNode(1, [RuleNode(2)])) !== hash(RuleNode(1))
        end

        @testset "Depth tests" begin
            @test depth(RuleNode(1)) == 1
            @test depth(RuleNode(1, [RuleNode(2), RuleNode(3)])) == 2
        end

        @testset "Length tests" begin
            @test length(RuleNode(1)) == 1
            @test length(RuleNode(1, [RuleNode(2), RuleNode(3)])) == 3
            @test length(RuleNode(1, [RuleNode(2, [RuleNode(3), RuleNode(4)])])) == 4
        end
        @testset "RuleNode compare" begin
            @test HerbCore._rulenode_compare(RuleNode(1), RuleNode(1)) == 0
            @test RuleNode(1) < RuleNode(2)
            @test RuleNode(2) > RuleNode(1)
            @test RuleNode(1, [RuleNode(2)]) < RuleNode(1, [RuleNode(3)])
            @test RuleNode(1, [RuleNode(2)]) < RuleNode(2, [RuleNode(1)])
            @test_throws ArgumentError RuleNode(1)<Hole(BitVector((1, 1)))
            @test_throws ArgumentError Hole(BitVector((1, 1)))<RuleNode(1)
        end

        @testset "Node depth from a tree" begin
            #=    1      -- depth 1
               2  3  4   -- depth 2
                    5  6 -- depth 3

            =#
            rulenode = RuleNode(
                1, [RuleNode(2), RuleNode(3), RuleNode(4, [RuleNode(5), RuleNode(6)])])
            @test node_depth(rulenode, rulenode) == 1

            @test node_depth(rulenode, rulenode.children[1]) == 2
            @test node_depth(rulenode, rulenode.children[2]) == 2
            @test node_depth(rulenode, rulenode.children[3]) == 2

            @test node_depth(rulenode, rulenode.children[3].children[1]) == 3
            @test node_depth(rulenode, rulenode.children[3].children[2]) == 3

            # in case a random node appears the node_depth is 0
            @test node_depth(rulenode, RuleNode(100)) == 0
        end

        @testset "rulesoftype" begin
            #=    1
               2  3  4
                    5  6
                   7    9
                          10
            =#
            rulenode = RuleNode(
                1,
                [
                    RuleNode(2),
                    RuleNode(3),
                    RuleNode(
                        4,
                        [
                            RuleNode(5, [RuleNode(7)]),
                            RuleNode(6, [RuleNode(9, [RuleNode(10)])])
                        ]
                    )
                ]
            )
            @test isempty(rulesoftype(rulenode, Set((11, 12))))
            ruleset = Set((1, 3, 7, 9, 10, 15, 23))
            expected = Set((1, 3, 7, 9, 10))
            @test rulesoftype(rulenode, ruleset) == expected
            @test rulesoftype(rulenode, Set(1)) == Set(1)
            @test isempty(rulesoftype(rulenode, Set{Int}()))
            @test rulesoftype(RuleNode(1), Set((1, 2))) == Set(1)
        end

        @testset "rule sequence" begin

            #=    1      
               2  3  4   
                    5  6 
                   7    9
                          10 
            =#
            rulenode = RuleNode(1,
                [
                    RuleNode(2),
                    RuleNode(3),
                    RuleNode(4,
                        [
                            RuleNode(5,
                                [RuleNode(7)]
                            ),
                            RuleNode(6,
                                [RuleNode(9, [RuleNode(10)])]
                            )
                        ]
                    )
                ]
            )
            @test get_rulesequence(rulenode, [3, 1, 1]) == [1, 4, 5, 7]
            @test get_rulesequence(rulenode, [3, 2, 1]) == [1, 4, 6, 9]
            @test get_rulesequence(rulenode, [3, 2, 1, 1]) == [1, 4, 6, 9, 10]

            # putting out of bounds indices returns the root
            @test get_rulesequence(rulenode, [100, 4, 1000]) == [1]
        end

        @testset "get_node_at_location" begin
            rulenode = UniformHole(BitVector((1, 1, 0, 0)), [RuleNode(3), RuleNode(4)])
            @test get_node_at_location(rulenode, Vector{Int64}()) isa UniformHole
            @test get_node_at_location(rulenode, [1]).ind == 3
            @test get_node_at_location(rulenode, [2]).ind == 4
        end

        @testset "get_path" begin
            n1 = RuleNode(1)
            n2 = RuleNode(2)
            n3 = UniformHole(BitVector((1, 1, 1)), [RuleNode(1), n2])
            n4 = RuleNode(1)
            root = RuleNode(4, [
                RuleNode(4, [
                    n1,
                    RuleNode(1)
                ]),
                n3
            ])
            @test get_path(root, n1) == [1, 1]
            @test get_path(root, n2) == [2, 2]
            @test get_path(root, n3) == [2]
            @test isnothing(get_path(root, n4))
        end

        @testset "Length tests with holes" begin
            domain = BitVector((1, 1))
            @test length(UniformHole(domain, [])) == 1
            @test length(UniformHole(domain, [RuleNode(2)])) == 2
            @test length(RuleNode(1, [RuleNode(2, [Hole(domain), RuleNode(4)])])) == 4
            @test length(UniformHole(domain, [RuleNode(2, [RuleNode(4), RuleNode(4)])])) ==
                  4
        end

        @testset "Depth tests with holes" begin
            domain = BitVector((1, 1))
            @test depth(UniformHole(domain, [])) == 1
            @test depth(UniformHole(domain, [RuleNode(2)])) == 2
            @test depth(RuleNode(1, [RuleNode(2, [Hole(domain), RuleNode(4)])])) == 3
            @test depth(UniformHole(domain, [RuleNode(2, [RuleNode(4), RuleNode(4)])])) == 3
        end

        @testset "number_of_holes" begin
            domain = BitVector((1, 1))
            @test number_of_holes(RuleNode(1)) == 0
            @test number_of_holes(Hole(domain)) == 1
            @test number_of_holes(UniformHole(domain, [RuleNode(1), RuleNode(1)])) == 1
            @test number_of_holes(UniformHole(domain, [Hole(domain), RuleNode(1)])) == 2
            @test number_of_holes(RuleNode(2, [Hole(domain), RuleNode(1)])) == 1
            @test number_of_holes(UniformHole(domain,
                [
                    Hole(domain),
                    UniformHole(domain, [Hole(domain), RuleNode(1)])
                ])) == 4
        end

        @testset "isuniform" begin
            domain = BitVector((1, 1))

            @test isuniform(RuleNode(1, [RuleNode(2)])) == true
            @test isuniform(UniformHole(domain, [RuleNode(2)])) == true

            @test isuniform(RuleNode(1)) == true
            @test isuniform(RuleNode(1, [])) == true
            @test isuniform(UniformHole(domain, [])) == true

            @test isuniform(Hole(domain)) == false
        end

        @testset "isfilled" begin
            domain1 = BitVector((0, 1, 0, 0, 0))
            domain2 = BitVector((0, 1, 0, 1, 0))
            @test isfilled(RuleNode(1, [])) == true
            @test isfilled(RuleNode(1, [RuleNode(2)])) == true
            @test isfilled(RuleNode(1, [Hole(domain1)])) == true
            @test isfilled(RuleNode(1, [Hole(domain2)])) == true

            @test isfilled(UniformHole(domain1, [Hole(domain2)])) == true
            @test isfilled(UniformHole(domain2, [Hole(domain2)])) == false

            @test isfilled(Hole(domain1)) == true
            @test isfilled(Hole(domain2)) == false
        end

        @testset "get_rule" begin
            domain_of_size_1 = BitVector((0, 1, 0, 0, 0))
            @test get_rule(RuleNode(99, [RuleNode(3), RuleNode(4)])) == 99
            @test get_rule(RuleNode(2, [RuleNode(3), RuleNode(4)])) == 2
            @test get_rule(UniformHole(domain_of_size_1, [RuleNode(5), RuleNode(6)])) == 2
            @test get_rule(Hole(domain_of_size_1)) == 2
        end

        @testset "have_same_shape" begin
            domain = BitVector((1, 1, 1, 1, 1, 1, 1, 1, 1))
            @test have_same_shape(RuleNode(1), RuleNode(2))
            @test have_same_shape(RuleNode(1), Hole(domain))
            @test have_same_shape(RuleNode(1), RuleNode(4, [RuleNode(1)])) == false
            @test have_same_shape(RuleNode(4, [RuleNode(1)]), RuleNode(1)) == false

            node1 = RuleNode(3, [
                RuleNode(1),
                RuleNode(1)
            ])
            node2 = RuleNode(9, [
                RuleNode(2),
                Hole(domain)
            ])
            @test have_same_shape(node1, node2)

            node1 = RuleNode(3, [
                RuleNode(1),
                RuleNode(1)
            ])
            node2 = RuleNode(9, [
                RuleNode(2),
                RuleNode(3, [
                    RuleNode(1),
                    RuleNode(1)
                ])
            ])
            @test have_same_shape(node1, node2) == false
        end

        @testset "hasdynamicvalue" begin
            @test hasdynamicvalue(RuleNode(1, "DynamicValue")) == true
            @test hasdynamicvalue(RuleNode(1)) == false
            @test hasdynamicvalue(UniformHole(
                BitVector((1, 0)), [RuleNode(1, "DynamicValue")])) == false
            @test hasdynamicvalue(UniformHole(BitVector((1, 0)), [RuleNode(1)])) == false
            @test hasdynamicvalue(Hole(BitVector((1, 0)))) == false
        end

        @testset "show" begin
            node = RuleNode(1, [RuleNode(1), RuleNode(2), RuleNode(3)])
            io = IOBuffer()
            Base.show(io, node)
            @test String(take!(io)) == "1{1,2,3}"

            # 12{14,2{4{9}},2{4{6}}}
            node = RuleNode(12,
                [
                    RuleNode(14),
                    RuleNode(2, [
                        RuleNode(4, [
                        RuleNode(9)
                    ])
                    ]),
                    RuleNode(2, [
                        RuleNode(4, [
                        RuleNode(6)
                    ])
                    ])
                ]
            )
            io = IOBuffer()
            Base.show(io, node)
            @test String(take!(io)) == "12{14,2{4{9}},2{4{6}}}"
        end
    end

    @testset "UniformHole" begin
        @testset "show" begin
            # fshole[Bool[0, 0, 1]]{14,2{4{9}},2{4{6}}}
            node = UniformHole([0, 0, 1],
                [
                    RuleNode(14),
                    RuleNode(2, [
                        RuleNode(4, [
                        RuleNode(9)
                    ])
                    ]),
                    RuleNode(2, [
                        RuleNode(4, [
                        RuleNode(6)
                    ])
                    ])
                ]
            )
            io = IOBuffer()
            Base.show(io, node)
            @test String(take!(io)) == "fshole[Bool[0, 0, 1]]{14,2{4{9}},2{4{6}}}"
        end
    end

    @testset "Hole" begin
        @testset "show" begin
            # 12{14,2{4{hole[...]}},2{4{6}}}
            node = RuleNode(12,
                [
                    RuleNode(14),
                    RuleNode(2, [
                        RuleNode(4, [
                        Hole(ones(14))
                    ])
                    ]),
                    RuleNode(2, [
                        RuleNode(4, [
                        RuleNode(6)
                    ])
                    ])
                ]
            )
            io = IOBuffer()
            Base.show(io, node)
            @test String(take!(io)) ==
                  "12{14,2{4{hole[Bool[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]]}},2{4{6}}}"
        end
    end

    @testset "Grammar" begin
        g = @csgrammar begin
            A = 1
        end

        io = IOBuffer()
        Base.show(io, g)
        @test String(take!(io)) == "1: A = 1\n"
    end
end
