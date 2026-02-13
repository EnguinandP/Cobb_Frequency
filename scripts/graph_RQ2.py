import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import csv

freq_path = f"tables/rq2/frequency.csv"
para_path = f"tables/rq2/parametrized.csv"
unroll_path = f"tables/rq2/unrolled.csv"

start = []
freq = []
para = []
unroll = []

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

tests = [
    ("sized_list","uni_len","10.000"),
    ("depth_bst","stick","0.800"),
    ("depth_bst","h_bal","2.000"),
    ("depth_bst","uni_height","(0.000, 1.000, 2.000, 3.000, 4.000, 5.000)"),
    ("depth_tree","h_bal","2.000"),
    ("depth_tree","stick","0.800"),
    ("depth_tree","uni_height","(0.000, 1.000, 2.000, 3.000, 4.000, 5.000)"),
    ("even_list","uni_len","(1.000, 2.000, 3.000, 4.000, 5.000, 6.000, 7.000, 8.000, 9.000, 10.000, 11.000)"),
    ("rb_tree","uni_height","(2.000, 3.000, 4.000)"),
    ("rb_tree","black","5.000"),
]

name_dict = {
    "depth_bst":"bst", 
    "depth_tree":"tree", 
    "even_list":"elist", 
    "rb_tree":"rbt", 
    "sized_list":"list", 
    "LoadedDice":"LDt",
    "Dragen":"Dt", 
    }

gen_group = []
gen_alpha = []

with open(freq_path, "r") as csv_file:
    csv_reader = csv.DictReader(csv_file)

    t = "sized_list"
    ti = 0
    gen_group.append(1)

    for i, line in enumerate(csv_reader):
        t = (line['data type'], line['feature vector'], line['target'])

        if t in tests:
        
            if line['feature vector'] == "h_bal":
                fv_name = "balance"
            else:
                fv_name = line['feature vector']

            names.append(f"{name_dict[line['data type']]} {i - ti}")
            explicit_names.append(f"{name_dict[line['data type']]} x {line['feature vector']}")
            start_score = float(line['start score']) 
            score = float(line['end score']) 

            # print(start_score, score)
            if score < 0:
                freq.append(0)
            else:
                freq.append(score)

            if start_score < 0:
                start.append(0)
            else:
                start.append(start_score)

with open(para_path, "r") as csv_file:
    csv_reader = csv.DictReader(csv_file)

    last_line = ()
    for line in csv_reader:
        t = (line['data type'], line['feature vector'], line['target'])

        if t in tests:
            # print(t, )
            if not (t == last_line):
                last_line = t
                score = float(line['end score']) 

                if score < 0:
                    para.append(0)
                else:
                    para.append(score)

with open(unroll_path, "r") as csv_file:
    csv_reader = csv.DictReader(csv_file)

    for line in csv_reader:
        t = (line['data type'], line['feature vector'], line['target'])
        if t in tests:
            # print(t)

            score = float(line['end score']) 

            # print(start_score, end_score)
            if score < 0:
                unroll.append(0)
            else:
                unroll.append(score)


print(start)
print(freq)
print(para)
print(unroll)

for i in range(len(gen_group) - 1):
    gen_alpha.append((gen_group[i], gen_group[i + 1], (i + 3) * (1.0 / 8)))


print(len(start), len(freq), len(unroll), len(para))

barWidth = .05

bar1 = np.arange(len(start)) * (barWidth * 4.5)
bar2 = [x + barWidth for x in bar1]
bar3 = [x + barWidth for x in bar2]
bar4 = [x + barWidth for x in bar3]

fig, ax = plt.subplots(figsize =(5, 3.5))

tick_pos = bar1 + (barWidth + 0.025)

plt.xticks(tick_pos, explicit_names, rotation = 45, fontsize=9, ha='right')

color1 = 'lightcoral'
color2 = 'midnightblue'
ax.bar(bar1, start, color=color1, width = barWidth, label ='initial')
ax.bar(bar2, freq, color=color2, width = barWidth, label ='no repair')
ax.bar(bar3, para, color='cornflowerblue', width = barWidth, label ='parametrized')
ax.bar(bar4, unroll, color='lightsteelblue', width = barWidth, label ='unrolled')

for x in range(len(start)):
    if freq[x] <= 0.105:
        plt.text((((x - 1) * 4.5 + 2)/ (len(start) * 4.91)) + .128, 0.01, '✓', transform=ax.transAxes, color='g')
    if para[x] <= 0.105:
        plt.text((((x - 1) * 4.5 + 3)/ (len(start) * 4.91)) + .128, 0.01, '✓', transform=ax.transAxes, color='g')
    if unroll[x] <= 0.105:
        plt.text((((x - 1) * 4.5 + 4)/ (len(start) * 4.91)) + .128, 0.01, '✓', transform=ax.transAxes, color='g')

plt.xlabel('Benchmarks')
plt.ylabel('Dist from Target')

ax.set_yscale('log', base=10)
plt.ylim(bottom=1e-1)

plt.tight_layout()
plt.legend(loc='upper center', bbox_to_anchor=(0.37, 0.99))
fig_path = "graphs/rq2.pdf"
plt.savefig(fig_path, format="pdf") 

plt.show() 
