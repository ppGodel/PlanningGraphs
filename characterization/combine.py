import os
from sys import argv
times = dict()
steps = dict()
stats = dict()
stages = {'int1': 1, 'trans1': 2, 'int2': 3, 'trans2': 4, 'int3': 5}
oh = None
sh = set()

with open('../IPCResults.csv') as data:
    oh = data.readline().strip().replace('Problem,', '').split(',')
    for line in data:
        fields = line.strip().split(',')
        problem = fields[2]
        filename = '{:s}.stats'.format(problem)
        if os.path.isfile(filename):
            stats[problem] = dict()
            for stage in range(1, 6):
                stats[problem][stage] = dict()
            stats[problem]['Planner'] = fields[0]
            stats[problem]['Dom'] = fields[1]
            stats[problem]['Time'] = fields[3]
            stats[problem]['Steps'] = fields[4]
            stats[problem]['Comp'] = fields[5] 
            with open(filename) as measurements:
                for records in measurements:
                    fields = records.split()
                    kind= fields.pop(0)
                    level = str(int(fields.pop(0)) + 1) # they start at 0 originally, now they start at 1
                    stage = stages.get(kind + level, None)
                    if stage is not None: # in case characterizations went further than the limit
                        descr = fields.pop(0)
                        value = float(fields.pop(0))
                        sh.add(descr)
                        stats[problem][stage][descr] = value
shl = list(sh)
print('Problem,{:s},Stage,{:s}'.format(','.join(oh), ','.join(shl)))
for problem in stats:
    shared = ','.join([stats[problem][h] for h in oh])
    for stage in range(1, 6):
        specific = ','.join([str(stats[problem][stage].get(h, 'NA')) for h in shl])
        print(f'{problem},{shared},{stage},{specific}')
