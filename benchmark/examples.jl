function big_tree()
    return "Big tree" => Returns(@rulenode 60{46{47{44{42{40},44{44{42{40},43{3}},44{43{12},43{13}}}}}}})
end

function small_tree()
    return "Small tree" => Returns(@rulenode 1{2{3,4},5})
end

function tree_with_hole_left_side()
    return "Tree with hole left side" =>  Returns(@rulenode 1{2{Hole[1,1,1,1,1,1,1],4},5{6,7}})
end

function tree_with_hole_middle()
    return "Tree with hole middle" => Returns(@rulenode 1{2{3,4},6{7,Hole[1,1,1,1,1,1,1]},8{9,10}})
end

function small_uniform_tree()
    return "Small uniform tree" => Returns(@rulenode 1{2{3,4},UniformHole[0,1,0,0,1,0,0]{6,7}})
end

function all_trees()
    return Dict(
        big_tree(),
        small_tree(),
        tree_with_hole_left_side(),
        tree_with_hole_middle(),
        small_uniform_tree()
    )
end
