from os import popen
from sys import argv
from time import time
import networkx as nx
import matplotlib.pyplot as plt
import networkx.algorithms as algs
from collections import defaultdict
import networkx.algorithms.approximation as approx

plt.rcParams.update({'font.size': 32})
figdim = 20
lvlCount = 3
nodeTypes = 'fa' # fact action
edgeTypes = 'moid' # mutex out in delete
name = argv[1]
start = time()
G = nx.Graph()
V = defaultdict(set) # node levels
E = dict() # edge levels
T = dict() # types
for t in nodeTypes: # types
    T[t] = set()

with open(name) as data:
    for line in data:
        tokens = line.split()
        t = tokens[0]
        if t in nodeTypes: 
            nID = int(tokens[1])
            for l in [int(l) for l in tokens[2:]]:
                V[l].add(nID)
                if l not in E:
                    E[l] = defaultdict(list)
            G.add_node(nID)
            T[t].add(nID)
        else:
            sID = int(tokens[0])
            tID = int(tokens[1])
            for e in tokens[2:]:
                t = e[0]
                assert t in edgeTypes                
                l = int(e[1:])
                E[l][t].append((sID, tID))
                G.add_edge(sID, tID)

pos = nx.shell_layout(G, [T['a'], T['f']])
gname = (name.split('/')[-1]).split('.')[0]
popen(f'rm -f {gname}*.png') # reset previous images
stage = 1
for l in range(lvlCount): # the first three levels
    plt.figure(figsize=(figdim, figdim))
    N = V[l]
    F = list(T['f'] & N)
    A = list(T['a'] & N)
    N = list(N)
    print(len(N), 'nodes on level', l + 1)
    print(len(F), 'facts')
    print(len(A), 'actions')
    nx.draw_networkx_nodes(G, pos, nodelist = F, node_color = 'b', node_size = 500, alpha = 0.5)
    nx.draw_networkx_nodes(G, pos, nodelist = A, node_color = 'g', node_size = 300, alpha = 0.5)
    nx.draw_networkx_edges(G, pos, edgelist = E[l]['m'], width = 2, alpha = 0.5, edge_color='m')
    nx.draw_networkx_edges(G, pos, edgelist = E[l]['d'], width = 2, alpha = 0.5, edge_color='c')
    if 'hide' not in argv:            
        plt.title('Stage {:d}: level {:d} (internal)'.format(stage, l + 1))
        stage += 1
        nx.draw_networkx_labels(G, pos, nodelist = N, font_size = 8)
    plt.axis('off')
    plt.savefig('{:s}_{:d}_internal.png'.format(gname, l + 1))
    plt.clf()
    if l < lvlCount - 1:
        plt.figure(figsize=(figdim, figdim)) 
        nx.draw_networkx_nodes(G, pos, nodelist = F, node_color = 'b', node_size = 500, alpha = 0.5)
        nx.draw_networkx_nodes(G, pos, nodelist = A, node_color = 'g', node_size = 300, alpha = 0.5)
        nx.draw_networkx_edges(G, pos, edgelist = E[l]['i'], width = 2, alpha = 0.5, edge_color='k') 
        if 'hide' not in argv:
            nx.draw_networkx_labels(G, pos, nodelist = N, font_size = 8)                    
            plt.title('Stage {:d}: level {:d} to level {:d}'.format(stage, l + 1, l + 2))
            stage += 1
        plt.axis('off')
        plt.savefig('{:s}_{:d}_trans.png'.format(gname, l + 1))        
        plt.clf()
popen(f'convert -delay 150 -size 300x300 {gname}*.png -loop 0 {gname}.gif') # animate

