import subprocess
import os
import re
from pathlib import Path
# cmd : dune exec frequify -- -f [freq] -o [out] [dir]

# in_dir_str = "./Cobb/underapproximation_type/data/validation/"
out_dir_str = "./bin/generators"

folder_names = {
    "completetree":"complete_tree",
    "depth_bst":"bst",
    "depthtree":"depth_tree",
    "duplicatelist":"duplicate_list",
    "even_list":"even_list",
    "rbtree":"red_black_tree",
    "sizedlist":"sized_list",
    "sortedlist":"sorted_list",
    "uniquelist":"unique_list",
}

t_pattern = r"true"
f_pattern = r"false"

in_dir = Path(out_dir_str)
assert in_dir.is_dir()

# prog*.ml

for f in os.listdir(in_dir):
    match = re.search(r"^(?!.*_freq\.ml$).+\.ml$", f)
    if match: 
        in_file = f"{out_dir_str}/{f}"
        base, ext, nothing = f.partition(".ml")
        # if d in folder_names:
        #     out_file = f"{out_dir_str}{folder_names[d]}/{base}_syn{ext}"
        # else:
        #     out_file = f"{out_dir_str}{d}/{base}_syn{ext}"
        out_file = f"{out_dir_str}/{base}_freq{ext}"

        try:
            with open(in_file, "r") as fin:
                lines = fin.readlines()

            with open(in_file, "w") as fout:
                for line in lines:
                    new_line = re.sub(t_pattern, "True", line) 
                    new_line = re.sub(f_pattern, "False", new_line) 
                    fout.write(new_line)
        
        except FileNotFoundError:
            print(f"Error: The file '{file}' was not found.")
        
        # if d == "rbtree":
        #     cmd = f"dune exec frequify -- -f unif_gen -o {out_file} {in_file}".split(" ")
        # else:
        #     cmd = f"dune exec frequify -- -f freq_gen -o {out_file} {in_file}".split(" ")

        cmd = f"dune exec frequify -- -f frequency_gen_list -o {out_file} {in_file}".split(" ")
        subprocess.run(cmd)
        # print(in_file, out_file)

        try:
            with open(in_file, "w") as fout:
                for line in lines:
                    fout.write(line)
        
        except FileNotFoundError:
            print(f"Error: The file '{file}' was not found.")

