import simplejson as json
with open('graphs2_clean.json') as data:
    line = data.readline()[2:-3]
    for token in line.split('","'):
        g = json.loads(token)
        print(g['name'])
