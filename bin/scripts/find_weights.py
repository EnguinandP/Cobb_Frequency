import subprocess
import os
import re
import csv
from pathlib import Path

out_dir_str = "./bin/results/weights.csv"

in_dir_str = "./bin/results"

subfolder_names = [
    "depth_bst", 
    "depth_tree", 
    "Dragen", 
    "LoadedDice", 
    "even_list", 
    "rb_tree", 
    "sized_list", 
    "sized_list_5", 
    "sized_list_10",
    "sized_list_1_const"
    ]

folder_names = ["unrolled", "unrolled_linear", "parametrized", "parametrized_enumeration", "frequency", "Dragen", "LoadedDice"]

n_weights = {
    "frequency/depth_bst":2, 
    "frequency/depth_tree":2, 
    "frequency/even_list":2, 
    "frequency/rb_tree":4, 
    "frequency/sized_list":2, 
    "parametrized_enumeration/sized_list_5":12,
    "parametrized_enumeration/sized_list_10":22,
    "parametrized/sized_list_1_const":2,
    "parametrized/sized_list":4,
    "parametrized/even_list":4,
    "parametrized/depth_tree":4,
    "parametrized/depth_bst":4,
    "parametrized/rb_tree":8,
    "unrolled/depth_bst":6, 
    "unrolled/depth_tree":6, 
    "unrolled/even_list":2, 
    "unrolled/rb_tree":20, 
    "unrolled/sized_list":2, 
    "unrolled_linear/depth_bst":12, 
    "unrolled_linear/depth_tree":12, 
    "LoadedDice":40,
    "Dragen":6, 
    }

in_dir = Path(in_dir_str)
assert in_dir.is_dir()

with open(out_dir_str, "w") as csv_fout:
    for d in os.listdir(in_dir):

        if d in folder_names:

            # csv_fout.write("data type,feature vector,#weights,target,success,weights,ratios\n")
            csv_fout.write("ratios\n")


            dir = in_dir_str + "/" + d
            for sd in os.listdir(Path(dir)):
                # print(sd)
                if sd in subfolder_names:
                    subdir = dir + "/" + sd

                    for f in os.listdir(Path(subdir)):
                        
                        if ("20000_20" in f):
                        # or ("80000_80" in f):

                            in_file = f"{subdir}/{f}"
                            # if subdir =='./bin/results/parametrized/depth_bst':
                            #     print(f)

                            try:
                                with open(in_file, "r") as fin:
                                    csv_file = csv.DictReader(fin)

                                    for line in csv_file:
                                        # print(line.keys())
                                        fv = line["fv"]
                                        goal = line["goal"]

                                        if line["version"] == "initial":
                                            start = line["dist"]
                                            score_start = line["score"]
                                        else:
                                            end = line["dist"]
                                            score_end = line["score"]
                                            weights = line["weights"]

                                    success = score_end > score_start

                                    nums = [int(x) for x in weights.strip("() ").split(",") if x.strip()]
                                    if len(nums) % 2 != 0:
                                        ratios_str = ""
                                    else:
                                        ratios = [p1 / (p1 + p2) for p1, p2 in zip(nums[0::2], nums[1::2])]
                                        ratios_str =  "\"(" + ", ".join(str(x) for x in ratios) + ")\""   
                                        

                                    id = d + "/" + sd
                                    # csv_fout.write(f"{sd},{fv},{n_weights[id]},\"{goal}\",{success},\"{weights}\",{ratios_str}\n")
                                    csv_fout.write(f"{ratios_str}\n")
                
                            except FileNotFoundError:
                                print(f"Error: The file '{file}' was not found.")

                elif (d == "Dragen" or d == "LoadedDice") and "20000_20" in sd:
                    in_file = dir + "/" + sd
                    # print(in_file)

                    try:
                        with open(in_file, "r") as fin:
                            csv_file = csv.DictReader(fin)

                            for line in csv_file:
                                # print(line.keys())
                                fv = line["fv"]
                                goal = line["goal"]

                                if line["version"] == "initial":
                                    start = line["dist"]
                                    score_start = line["score"]
                                else:
                                    end = line["dist"]
                                    score_end = line["score"]
                                    weights = line["weights"]

                            success = score_end > score_start

                            nums = [int(x) for x in weights.strip("() ").split(",") if x.strip()]
                            if len(nums) % 2 != 0:
                                ratios_str = ""
                            else:
                                ratios = [p1 / (p1 + p2) for p1, p2 in zip(nums[0::2], nums[1::2])]
                                ratios_str =  "\"(" + ", ".join(str(x) for x in ratios) + ")\"" 
                                

                            id = d
                            # csv_fout.write(f"{sd},{fv},{n_weights[id]},\"{goal}\",{success},\"{weights}\",{ratios_str}\n")
                            csv_fout.write(f"{ratios_str}\n")


                    except FileNotFoundError:
                        print(f"Error: The file '{file}' was not found.")
                    

# folder_copy = ["unrolled", "parametrized", "parametrized_enumeration", "frequency"]

# cmd = f"cp ./bin/tables/frequency.csv ./bin/tables/rq2/frequency_full.csv".split(" ")
# folder_copy
# for f in folder_copy:
#     cmd = f"cp ./bin/tables/{f}.csv ./bin/tables/rq2/{f}.csv".split(" ")
#     subprocess.run(cmd)

