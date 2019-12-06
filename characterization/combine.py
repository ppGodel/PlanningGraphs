import os
times = dict()
steps = dict()
stats = dict()
header = set()
with open('IPCResults.csv') as data:
    data.readline() # header row
    for line in data:
        fields = line.split(',')
        label = fields[2]
        filename = 'solved/layers/{:s}.layers'.format(label) 
        if os.path.isfile(filename):
            times[label] = int(float(fields[3]))
            steps[label] = int(fields[4])
            stats[label] = dict()
            ccID = 0
            with open(filename) as measurements:
                if 'stats' in filename:
                    if len(header) == 0:
                        header = {'facts', 'actions', 'n', 'm', 'maxlevel'}
                    stats[label]['facts'] = int(measurements.readline().split()[-1])
                    stats[label]['actions'] = int(measurements.readline().split()[-1])
                    fields = measurements.readline().split()
                    stats[label]['n'] = int(fields.pop(0))
                    stats[label]['m'] = int(fields.pop(0))
                    stats[label]['maxlevel'] = int(fields.pop(0))
                for line in measurements:
                    fields = line.split()
                    value = fields[-1]
                    descr = ' '.join(fields[:-1])
                    if '.stats' not in filename: # layers or cc
                        descr = 'lvl' + descr.replace(' ', '')                        
                    if 'cc n ' in line:
                        ccID += 1
                        sID = 'cc n {:d}'.format(ccID)
                        header.add(sID)
                        stats[label][sID] = fields[2]
                        sID = 'cc m {:d}'.format(ccID)
                        header.add(sID)                        
                        stats[label][sID] = value
                    elif 'cc' in line and '.stats' in filename:
                        sID = '{:s} {:d}'.format(descr, ccID)
                        header.add(sID)
                        stats[label][sID] = value
                    elif 'seconds' in line:
                        header.add('proctime')
                        stats[label]['proctime'] = float(fields[0])
                    else:
                        header.add(descr)
                        stats[label][descr] = value
hdr = list(header)
print('Problem,ms,stepcount,' + ','.join(hdr))
for label in stats:
    s = ','.join([str(stats[label].get(h, 'NA')) for h in hdr])
    print('{:s},{:d},{:d},{:s}'.format(label, times[label], steps[label], s))
