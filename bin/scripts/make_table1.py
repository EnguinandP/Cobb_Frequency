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

                            # fout.write(f"{d} & {line["fv"]} & {n_weights[d] / 2} & 0 & {n_weights[d]} & 1 & {line["goal"]} &  \n")

                        fout.write(f"{d.replace("_", "\\_")} & {fv} & {int (n_weights[d] / 2)} & {n_weights[d]} & {goal} & {start} & {end} & {time} & {iterations} \\\\ \n")
    
                except FileNotFoundError:
                    print(f"Error: The file '{file}' was not found.")

        # fout.write("\\midrule \n")
        



    # match = re.search(r"^(?!.*_freq\.ml$).+\.ml$", f)
    # if match: 
    #     in_file = f"{out_dir_str}/{f}"
    #     base, ext, nothing = f.partition(".ml")
    #     out_file = f"{out_dir_str}/{base}_freq{ext}"

    #     try:
    #         with open(in_file, "r") as fin:
    #             lines = fin.readlines()

    #         with open(in_file, "w") as fout:
    #             for line in lines:
    #                 new_line = re.sub(t_pattern, "True", line) 
    #                 new_line = re.sub(f_pattern, "False", new_line) 
    #                 fout.write(new_line)
        
    #     except FileNotFoundError:
    #         print(f"Error: The file '{file}' was not found.")
        

    #     cmd = f"dune exec frequify -- -f frequency_gen_list -o {out_file} {in_file}".split(" ")
    #     subprocess.run(cmd)
    #     # print(in_file, out_file)

    #     try:
    #         with open(in_file, "w") as fout:
    #             for line in lines:
    #                 fout.write(line)
        
    #     except FileNotFoundError:
    #         print(f"Error: The file '{file}' was not found.")

