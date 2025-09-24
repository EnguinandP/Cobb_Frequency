(* 

from pbt/pbt_examples/demo_bst.jl

function gen_list(size)
    size == 0 && return DistNil()

    @alea_ite if flip(sigmoid(Var(size)))
        DistNil()
    else
        DistCons(uniform(DistUInt32, 0, 10), gen_list(size-1))
    end
end
 *)