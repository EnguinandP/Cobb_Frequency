import subprocess
import os
import re
from pathlib import Path

tests = [
    "sized_list",
    "even_list",
    "rb_tree",
    "depth_tree",
    "depth_bst",

    # "dragen",
    # "loaded_dice",
    # "pe_sized_list_5",
    # "pe_sized_list_10",

    "p1_sized_list",
    "p2_sized_list",
    "p2_even_list",
    "p2_depth_bst",
    "p2_depth_tree",
    # "p2_rb_tree",

    # "ur_depth_tree",
    # "ur_depth_bst",
    # "ur_rb_tree",
    "ur_sized_list",
    "ur_even_list"
]

for test in tests:
    cmd = f"dune exec -- Cobb_Frequency {test}".split(" ")
    # cmd = f"dune exec -- Cobb_Frequency {test} -i 80000 -r 80".split(" ")
    # cmd = f"dune exec Cobb_Frequency {test}".split(" ")
    subprocess.run(cmd)