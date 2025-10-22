import subprocess
import os
import re
from pathlib import Path

# tests = [
#     # "sized_list",
#     # "ur_depth_tree",
#     # "ur_rb_tree",
#     "p2_sized_list",
# ]

tests = {
    "p2_sized_list" : "./bin/results/parametrized/sized_list",
}

iterations = [
    # 5000,
    10000,
    # 15000,
    20000,
    # 25000,
    30000,
    # 45000,
    50000,
]

restarts = [
    1,
    5,
    10,
    15,
    20,
    25,
    30,
    35,
    40,
    45,
    50,   
]

# run Cobb_Frequency

for test in tests:
    for i in iterations:
        for r in restarts:
            print(f"running {test} with {i} and {r}")
            cmd = f"dune exec -- Cobb_Frequency {test} -i {i} -r {r}".split(" ")
            subprocess.run(cmd)


# compile into csv

out_dir_str = "./bin/tables/"

for test in tests:
    out_str = out_dir_str + test + "RQ1.csv"

    with open(out_str, "w") as fout:
        fout.write(f"iterations,restarts,score\n")

        for f in os.listdir(tests[test]):
            for i in iterations:
                    for r in restarts:
                        if f"{i}_{r}" in f:
                            in_file = f"{tests[test]}/{f}"
                            try:
                                with open(in_file, "r") as fin:
                                    csv_file = csv.DictReader(fin)

                                    for line in csv_file:

                                        if line["version"] == "final":
                                            score = line["score"]

                                    fout.write(f"{i},{r},{score}\n")
                
                            except FileNotFoundError:
                                print(f"Error: The file '{file}' was not found.")



            

