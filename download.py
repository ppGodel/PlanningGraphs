from pymongo import MongoClient
from bson import ObjectId
import json

# yeah, I am not putting these on GitHub
server = None
port = None
username = None 
password = None

class JSONEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, ObjectId):
            return str(o)
        return json.JSONEncoder.default(self, o)

if server is None:
    quit()
c = MongoClient(f'mongodb://{username}:{password}@{server}:{port}/')
print('Requesting collections')
db = c['planning']
done = ['system.users', 'edges', 'graphs2']

for coll in db.collection_names():
    if coll not in done:
        print('Retrieving', coll)
        data = db[coll] 
        file = open(coll + '.json', 'w')
        file.write('[')
        for doc in data.find({}):
            file.write(json.dumps(JSONEncoder().encode(doc)))
            file.write(',')
        file.write(']')
        print(coll, 'has been stored locally')
print('End of collections')
