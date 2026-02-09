import subprocess
import os
import re
from pathlib import Path
import argparse

parser = argparse.ArgumentParser(description='Run tests')
parser.add_argument("test", help="Specify which set of test to run [a|r].")
parser.add_argument("strategy", help="Specify which strategy [sa|dir|diw].")
args = parser.parse_args()

test_choice = args.test
strat = args.strategy

tests_all = [
    "sized_list",
    "even_list",
    "rb_tree",
    "depth_tree",
    "depth_bst",

    "dragen",
    "loaded_dice",
    "pe_sized_list_5",
    "pe_sized_list_10",

    "p2_sized_list",
    "p2_even_list",
    "p2_depth_bst",
    "p2_depth_tree",
    "p2_rb_tree",

    "ur_depth_tree",
    "ur_depth_bst",
    "ur_rb_tree",
    "ur_sized_list",
    "ur_even_list"
]

tests_diw = [
    "sized_list",
    "even_list",
    "rb_tree",
    "depth_tree",
    "depth_bst",

    "dragen",
    "loaded_dice",
    "pe_sized_list_5",
    "pe_sized_list_10",

    "p2_sized_list",
    "p2_even_list",
    "p2_depth_bst",
    "p2_depth_tree",
    "p2_rb_tree",

    "ur_depth_tree",
    "ur_depth_bst",
    "ur_rb_tree",
    "ur_sized_list",
    "ur_even_list"
]

tests_dir = [
    "sized_list",
    "even_list",
    "rb_tree",
    "depth_tree",
    "depth_bst",

    "ur_depth_tree",
    "ur_depth_bst",
    "ur_sized_list",
    "ur_even_list"
]

if test_choice == "a":
    test_set = tests_all
elif test_choice == "r":
    test_set = tests_dir

for test in test_set:
    # cmd = f"dune exec -- Cobb_Frequency {test}".split(" ")
    cmd = f"dune exec -- Cobb_Frequency {test} -s {strat}".split(" ")
    # cmd = f"dune exec -- Cobb_Frequency {test} -i 80000 -r 80".split(" ")
    # cmd = f"dune exec Cobb_Frequency {test}".split(" ")
    subprocess.run(cmd)