import simplejson as json
with open('nodes2_clean.json') as data:
    line = data.readline()[2:-3]
    for token in line.split('","'):
        n = json.loads(token)
        lvls = ' '.join(str(l) for l in n['level'])
        print('{:s} {:s} {:s} {:s} {:s}'.format(n['graph_name'], n['node_name'], n['type'], n['hash'], lvls))
