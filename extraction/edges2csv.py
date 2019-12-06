import simplejson as json
with open('edges2_clean.json') as data:
    line = data.readline()[2:-3]
    for token in line.split('","'):
        e = json.loads(token)
        lvls = []
        for t in e['level']:
            for l in e['level'][t]:
                lvls.append(t + l)
        print('{:s} {:d} {:s} {:s}'.format(e['graph_name'], e['source'], e['target'], ' '.join(lvls)))
