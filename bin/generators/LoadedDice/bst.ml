(* 

from pbt/pbt_examples/demo_natlist.jl

function gen_bst(size, lo, hi)
    @alea_ite if size == 0 || flip(register_weight!("sz$(size)"))
        DistLeaf()
    else
        x = unif(lo, hi)
        DistBranch(x, gen_bst(size-1, lo, x), gen_bst(size-1, x, hi))
    end
end
 *)

