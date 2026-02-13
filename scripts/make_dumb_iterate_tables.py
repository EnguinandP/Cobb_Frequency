import subprocess
import os
import re
import csv
import argparse
from pathlib import Path

parser = argparse.ArgumentParser(description='Generate tables from dumb iterate ratios data')
parser.add_argument("version", default="", help="Specify which version to count results [weights | ratios | ratios_w | ratios_w2 | ratios_smaller ].")
# parser.add_argument('-r', '--ratios', action='store_true', 
#                     help='process dumb_iterate_ratio')
# parser.add_argument('-rs', '--ratios_smaller', action='store_true', 
#                     help='process dumb_iterate_ratio_smaller')
args = parser.parse_args()

folder = args.version

dumb_out_dir_str = "./tables/dumb_iterate"
dumb_in_dir_str = "./results/dumb_iterate"

orig_out_dir_str = "./tables"
orig_in_dir_str = "./bin/results"

failed_table_out_str = "./results/failed_table_" + folder + ".csv"

label = "_" + folder
label = label.replace("_", "\\_")
dumb_out_dir_str += "_" + folder
dumb_in_dir_str += "_" + folder

# if ratio_mode:
#     dumb_out_dir_str += "_ratios"
#     dumb_in_dir_str += "_ratios"
#     label = "\\_ratios"

# if ratio_smaller_mode:
#     dumb_out_dir_str += "_ratios_smaller"
#     dumb_in_dir_str += "_ratios_smaller"
#     label = "\\_ratios_smaller"

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
    "parametrized/sized_list_1_const":3,
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

in_dir = Path(dumb_in_dir_str)
assert in_dir.is_dir()

out_str = dumb_out_dir_str + ".tex"
csv_out_str = dumb_out_dir_str + ".csv"

total_sa_success = 0
total_success = 0

line_count = 0

with open(failed_table_out_str, "w") as failed_fout:
    with open(out_str, "w") as fout:
        with open(csv_out_str, "w") as csv_fout:
            csv_fout.write("data type,feature vector,succcess,weights,ratios,#weights,target,end score,time\n")
            failed_fout.write("data type,feature vector,target,d success,o success,d weights,o weights,d ratios,o ratios,d score,o score,d time,o time\n")
            
            fout.write("\\begin{table*}[t!]\n" +
                "\\caption{\\small dumb\\_iterate" + label + "}\n" +
                "\\renewcommand{\\arraystretch}{0.8}\n" +
                "\\centering \\vspace*{-.05in} \\footnotesize\n" +
                "\\begin{tabular}{r|r|r| p{2cm}|p{1cm}|r | p{1.5cm} r r}\n\n")
            fout.write("\\midrule \n")
            fout.write("data type & feature & success & weights & ratios & \\#weights & target & end score & time \\\\ \n")        
            fout.write("\\midrule \n")

            for d in os.listdir(in_dir):

                if d in folder_names:

                    dir = dumb_in_dir_str + "/" + d
                    for sd in os.listdir(Path(dir)):
                        # print(sd)
                        if sd in subfolder_names:
                            if line_count >= 25:
                                line_count = 0
                                fout.write("\n\\end{tabular}\n" +
                                    "% \\vspace{-.5in}\n" +
                                    "\\end{table*}\n")
                                
                                fout.write("\n\\begin{table*}[t!]\n" +
                                    "\\caption{\\small dumb\\_iterate" + label + "}\n" +
                                    "\\renewcommand{\\arraystretch}{0.8}\n" +
                                    "\\centering \\vspace*{-.05in} \\footnotesize\n" +
                                    "\\begin{tabular}{r|r|r| p{2cm}|p{1cm}|r | p{1.5cm} r r}\n\n")
                                fout.write("\\midrule \n")
                                fout.write("data type & feature & success & weights & ratios & \\#weights & target & end score & time \\\\ \n")        
                                # fout.write("\\midrule \n")


                            fout.write("\\midrule \n")
                            line_count += 1

                            subdir = dir + "/" + sd

                            for f in os.listdir(Path(subdir)):
                                
                                if ("20000_20" in f):

                                    in_file = f"{subdir}/{f}"
                                    orig_in_file = f"{orig_in_dir_str}/{d}/{sd}/{f}"

                                    try:
                                        with open(orig_in_file, "r") as orig_fin:
                                            with open(in_file, "r") as fin:
                                                orig_csv_file = csv.DictReader(orig_fin)
                                                csv_file = csv.DictReader(fin)
                                                

                                                for line in csv_file:
                                                    # print(line.keys())
                                                    fv = line["fv"]
                                                    goal = line["goal"]
                                                    iterations = line["iterations"]
                                                    time = line["time"]

                                                    if line["version"] == "initial":
                                                        start = line["dist"]
                                                        score_start = line["score"]
                                                    else:
                                                        end = line["dist"]
                                                        score_end = line["score"]
                                                        weights = line["weights"]

                                                for line in orig_csv_file:
                                                    orig_time = line["time"]

                                                    if line["version"] == "initial":
                                                        orig_start = line["dist"]
                                                        orig_score_start = line["score"]
                                                    else:
                                                        orig_end = line["dist"]
                                                        orig_score_end = line["score"]
                                                        orig_weights = line["weights"]

                                                data_type = d[0] + " " + sd
                                                id = d + "/" + sd

                                                success = (float(score_end) <= 0)
                                                close = (float(score_end) <= 0.1)

                                                orig_success = (float(orig_score_end) <= 0.0)
                                                orig_close = (float(orig_score_end) <= 0.1)

                                                if orig_success:
                                                    total_sa_success += 1

                                                if (not success) and orig_success:
                                                    print(" - " + data_type + " " + fv + " " + goal + " " + score_end) 
                                                    total_success += 1

                                                nums = [int(x) for x in weights.strip("() ").split(",") if x.strip()]
                                                if len(nums) % 2 != 0:
                                                    ratios_str = ""
                                                else:
                                                    ratios = [round(p1 / (p1 + p2), 3) for p1, p2 in zip(nums[0::2], nums[1::2])]
                                                    ratios_str =  "\"(" + ", ".join(str(x) for x in ratios) + ")\""  

                                                if len(nums) % 2 != 0:
                                                    orig_ratios_str = ""
                                                else:
                                                    orig_ratios = [round(p1 / (p1 + p2), 3) for p1, p2 in zip(nums[0::2], nums[1::2])]
                                                    orig_ratios_str =  "\"(" + ", ".join(str(x) for x in ratios) + ")\""           

                                                if (not success or not orig_success):
                                                    failed_fout.write(f"{data_type},{fv},\"{goal}\",{success},{orig_success},\"{weights}\",\"{orig_weights}\",{ratios_str},{orig_ratios_str},{score_end},{orig_score_end},{time},{orig_time}\n")

                                                    # print(f"{data_type}, {fv}, {score_end}\n")

                                                fout.write(f"{data_type.replace("_", "\\_")} & {fv.replace("_", "\\_")} & {success} & {weights} & {ratios} & {n_weights[id]} & {goal} & {score_end} & {time} \\\\ \n")
                                                csv_fout.write(f"{data_type},{fv},{success},\"{weights}\",\"{ratios}\",{n_weights[id]},\"{goal}\",{score_end},{time}\n")
                                                line_count += 1

                                    except FileNotFoundError:
                                        print(f"Error: The file '{orig_in_file}' or {in_file} was not found.")

                            # fout.write("\\midrule \n")
                        # elif (d == "Dragen" or d == "LoadedDice") and "20000_20" in sd:
                        #     in_file = dir + "/" + sd
                        #     # print(in_file)

                        #     try:
                        #         with open(in_file, "r") as fin:
                        #             csv_file = csv.DictReader(fin)

                        #             for line in csv_file:
                        #                 # print(line.keys())
                        #                 fv = line["fv"]
                        #                 goal = line["goal"]
                        #                 iterations = line["iterations"]
                        #                 time = line["time"]

                        #                 if line["version"] == "initial":
                        #                     start = line["dist"]
                        #                     score_start = line["score"]
                        #                 else:
                        #                     end = line["dist"]
                        #                     score_end = line["score"]
                        #                     # chi_end = line["chi"]

                        #             id = d
                        #             fout.write(f"{d.replace("_", "\\_")} & {fv.replace("_", "\\_")} & {int (n_weights[id] / 2)} & {n_weights[id]} & {goal} & {start} & {end} & {score_end} & {time} \\\\ \n")
                        #             csv_fout.write(f"{sd},{fv},{int (n_weights[id] / 2)},{n_weights[id]},\"{goal}\",\"{start}\",\"{end}\",{score_start},{score_end},{time}\n")

                        #     except FileNotFoundError:
                        #         print(f"Error: The file '{file}' was not found.")
                            
                        # fout.write("\\midrule \n")
            fout.write("\n\\end{tabular}\n" +
                "% \\vspace{-.5in}\n" +
                "\\end{table*}\n")

print(f"\nTotal fails (failed where SA succeeded): {total_success} / {total_sa_success}")
# print(f"Total close-to-success: {total_success} / {total}")


