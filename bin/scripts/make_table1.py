import subprocess
import os
import re
import csv
from pathlib import Path

out_str = "./bin/tables/table1.csv"
in_dir_str = "./bin/results"

folder_names = ["depth_bst", "depth_tree", "Dragen", "even_list", "rb_tree", "sized_list"]
n_weights = {"depth_bst":2, "depth_tree":2, "Dragen":6, "even_list":2, "rb_tree":4, "sized_list":2}

in_dir = Path(in_dir_str)
assert in_dir.is_dir()

with open(out_str, "w") as fout:
    fout.write("data type & feature vector & \#bool\_gen & \#weights & target & start dist & end dist & time & iterations \\\\ \n")

    for d in os.listdir(in_dir):
        print(d)
        if d in folder_names:
            fout.write("\\midrule \n")

            dir = in_dir_str + "/" + d
            for f in os.listdir(Path(dir)):
                # print(f)

                in_file = f"{dir}/{f}"

                try:
                    with open(in_file, "r") as fin:
                        csv_file = csv.DictReader(fin)

                        for line in csv_file:
                            # print(line.keys())
                            fv = line["fv"].replace("_", "\\_")
                            goal = line["goal"]
                            iterations = line["iterations"]
                            time = line["time"]

                            if line["version"] == "initial":
                                start = line["dist"]
                            else:
                                end = line["dist"]

                        fout.write(f"{d.replace("_", "\\_")} & {fv} & {int (n_weights[d] / 2)} & {n_weights[d]} & {goal} & {start} & {end} & {time} & {iterations} \\\\ \n")
    
                except FileNotFoundError:
                    print(f"Error: The file '{file}' was not found.")

        # fout.write("\\midrule \n")
        

