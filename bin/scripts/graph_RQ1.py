import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import csv

path = f"bin/tables/rq1/frequency.csv"

start = []
end = []
names = []
explicit_names = []

names_simple = [    
    "bst", 
    "tree", 
    "elist", 
    "rbt", 
    "list", 
    "", 
    ]

gen_group = []
gen_alpha = []

name_dict = {
    "depth_bst":"bst", 
    "depth_tree":"tree", 
    "even_list":"elist", 
    "rb_tree":"rbt", 
    "sized_list":"list", 
    "LoadedDice":"LDt",
    "Dragen":"Dt", 
    }

with open(path, "r") as csv_file:
    csv_reader = csv.DictReader(csv_file)

    t = "sized_list"
    ti = 0
    gen_group.append(1)
    for i, line in enumerate(csv_reader):
        if not (t == line['data type']):
            gen_group.append(i)
            ti = i
            t = line['data type']
        
        if line['feature vector'] == "h_bal":
            fv_name = "balance"
        else:
            fv_name = line['feature vector']

        names.append(f"{name_dict[line['data type']]} {i - ti}")
        explicit_names.append(f"{name_dict[line['data type']]} x {line['feature vector']}")
        print(i, ti)
        start_score = float(line['start score']) 
        end_score = float(line['end score']) 

        # print(start_score, end_score)
        if end_score < 0:
            end.append(0)
        else:
            end.append(end_score)

        if start_score < 0:
            start.append(0)
        else:
            start.append(start_score)


gen_group.append(len(start) - 1)
print(names)
print(len(names))

for i in range(len(gen_group) - 1):
    gen_alpha.append((gen_group[i], gen_group[i + 1], (i + 3) * (1.0 / 8)))

print(gen_alpha)

barWidth = .4

bar1 = np.arange(len(start)) * (barWidth * 2.5)
bar2 = [x + (barWidth) for x in bar1]

fig, ax = plt.subplots()

tick_pos = bar1 +  (barWidth / 2)
plt.xticks(tick_pos, explicit_names, rotation = 45, fontsize=7)

ax.bar(bar1, start, color='lightcoral', width = (barWidth), label ='start') 
ax.bar(bar2, end, color='midnightblue', width = (barWidth), label ='end') 



# for x in end:
#     if x <= 0.005:
#         plt.text(x, 0, 'x')

plt.xlabel('Benchmarks')
plt.ylabel('Distance from Target')

ax.set_yscale('log', base=10)
plt.ylim(bottom=1e-1)


plt.text(100, 0, 'x', fontsize=20)


plt.tight_layout()
# plt.legend()
fig_path = "bin/graphs/rq1.pdf"
plt.savefig(fig_path, format="pdf") 

plt.show() 
