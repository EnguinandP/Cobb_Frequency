import subprocess
import os
import re
import csv
from pathlib import Path
import numpy as np

# tests = [
#     # "sized_list",
#     # "ur_depth_tree",
#     # "ur_rb_tree",
#     "p2_sized_list",
# ]

tests = {
    # "rq3_p2_sized_list" : ["./bin/results/parametrized/sized_list", "uni_len_10._"],
    "pe_sized_list_10" : ["bin/results/parametrized_enumeration/sized_list_10", "uni_len_10._"],
    # "rq3_ur_depth_tree" : ["./bin/results/unrolled/depth_tree", "uni_height_5._"]
    # "rq3_ur_depth_tree" : "./bin/results/unrolled/depth_tree",
}

iterations = [
    5000,
    10000,
    15000,
    20000,
    25000,
    # 30000,
    # 35000,
    # 40000,
    # 45000,
    # 50000,
    # 55000,
    # 60000,
    # 65000,
]

iterations_arr = np.arange(5000, 200001, 5000)
iterations = iterations_arr.tolist()

restarts_arr = np.arange(5, 401, 5)
restarts = restarts_arr.tolist()
restarts.insert(0, 1)

print(iterations)
print(restarts)

restarts = [
    1,
    5,
    10,
    15,
    20,
    25,
    # 30,
    # 35,
    # 40,
    # 45,
    # 50,   
]


# run Cobb_Frequency

for test in tests:
    for i in iterations:
        for r in restarts:
            print(f"running {test} with {i} and {r}")
            cmd = f"dune exec -- Cobb_Frequency {test} -i {i} -r {r} -one".split(" ")
            subprocess.run(cmd)


# compile into csv

out_dir_str = "./bin/tables/"

for test in tests:
    out_str = out_dir_str + test + ".csv"

    with open(out_str, "w") as fout:
        fout.write(f"iterations,restarts,score\n")

        for f in os.listdir(tests[test][0]):
            for i in iterations:
                    for r in restarts:
                        # print(f"{i}_{r}     {f}")
                        if f"{tests[test][1]}{i}_{r}." in f:
                            print(f"{i}_{r}     {f}")

                            in_file = f"{tests[test][0]}/{f}"
                            try:
                                with open(in_file, "r") as fin:
                                    csv_file = csv.DictReader(fin)

                                    for line in csv_file:

                                        if line["version"] == "final":
                                            score = line["score"]

                                    fout.write(f"{i},{r},{score}\n")
                                    # print(f"{i},{r},{score}")
                            except FileNotFoundError:
                                print(f"Error: The file '{file}' was not found.")
                            break



            

