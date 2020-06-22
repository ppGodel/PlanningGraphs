from sys import argv
from time import time
import networkx as nx
import matplotlib.pyplot as plt
import networkx.algorithms as algs
from collections import defaultdict
import networkx.algorithms.approximation as approx

plt.rcParams.update({'font.size': 22})
figdim = 20

nodeTypes = 'fa' # fact action
edgeTypes = 'moid' # mutex out in delete
minimumOrder = 20
name = argv[1]
start = time()
with open(name) as data:
    G = nx.Graph()
    V = defaultdict(set) # node levels
    E = dict() # edge levels
    T = dict() # types
    for t in nodeTypes: # types
        T[t] = set()
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
    for t in T:
        print(t, len(T[t]))
    pos = nx.shell_layout(G, [T['a'], T['f']])
    maxLevel = max(V.keys())    
    print(len(G), G.number_of_edges(), maxLevel)
    if 'draw' in argv:
        gname = (name.split('/')[-1]).split('.')[0]
        for l in range(maxLevel + 1):
            plt.figure(figsize=(figdim, figdim))
            N = V[l]
            F = list(T['f'] & N)
            A = list(T['a'] & N)
            N = list(N)
            print(len(N), 'nodes on level', l + 1)
            print(len(F), 'facts')
            print(len(A), 'actions')
            nx.draw_networkx_nodes(G, pos, nodelist = F, node_color = 'b', node_size = 250, alpha = 0.5)
            nx.draw_networkx_nodes(G, pos, nodelist = A, node_color = 'g', node_size = 150, alpha = 0.5)
            nx.draw_networkx_edges(G, pos, edgelist = E[l]['m'], width = 2, alpha = 0.5, edge_color='r')
            nx.draw_networkx_edges(G, pos, edgelist = E[l]['d'], width = 2, alpha = 0.5, edge_color='y')
            if 'hide' not in argv:            
                plt.title('Level {:d} (internal)'.format(l + 1))
                nx.draw_networkx_labels(G, pos, nodelist = N, font_size = 8)
            plt.axis('off')
            plt.savefig('{:s}_{:d}_internal.png'.format(gname, l + 1))
            plt.clf()
            if l < maxLevel:
                plt.figure(figsize=(figdim, figdim)) 
                nx.draw_networkx_nodes(G, pos, nodelist = F, node_color = 'b', node_size = 250, alpha = 0.5)
                nx.draw_networkx_nodes(G, pos, nodelist = A, node_color = 'g', node_size = 150, alpha = 0.5)
                nx.draw_networkx_edges(G, pos, edgelist = E[l]['i'], width = 2, alpha = 0.5, edge_color='k') 
                if 'hide' not in argv:
                    nx.draw_networkx_labels(G, pos, nodelist = N, font_size = 8)                    
                    plt.title('Level {:d} to level {:d}'.format(l + 1, l + 2))
                plt.axis('off')
                plt.savefig('{:s}_{:d}_trans.png'.format(gname, l + 1))        
                plt.clf()
        quit() # just the drawing
    print('max betweenness centrality', max(nx.betweenness_centrality(G).values()))
    print('avg clustering', algs.average_clustering(G))
    print('vertex cover order', len(approx.vertex_cover.min_weighted_vertex_cover(G)))
    print('degree assortativity', algs.assortativity.degree_assortativity_coefficient(G))
    print('max avg deg connectivity', max(algs.assortativity.average_degree_connectivity(G).values()))
    try:
        print('max eigenvector centrality', max(algs.centrality.eigenvector_centrality(G).values()))
    except nx.exception.PowerIterationFailedConvergence as e:
        print('max eigenvector centrality', 'NA')
    print('max closeness centrality', max(algs.centrality.closeness_centrality(G).values()))
    print('max load centrality', max(nx.algorithms.centrality.load_centrality(G).values()))
    print('max triangles', max(algs.cluster.triangles(G).values()))
    print('transitivity', algs.cluster.transitivity(G))
    print('greedy colors', len(set(algs.coloring.greedy_color(G).values())))    
    for CC in nx.connected_components(G):
        S = G.subgraph(CC)
        n = len(S)
        if n > minimumOrder:
            print('cc n {:d} m {:d}'.format(n, S.number_of_edges()))
            print('cc radius {:d}'.format(nx.radius(S)))
            print('cc diameter {:d}'.format(nx.diameter(S)))
            print('cc edge cover order', len(algs.covering.min_edge_cover(S)))
            print('cc max eccentricity {:d}'.format(max(nx.eccentricity(S).values())))
            print('cc % center {:f}'.format(100 * len(nx.center(S)) / n))
            print('cc % periphery {:f}'.format(100 * len(nx.periphery(S)) / n))
            print('cc density {:f}'.format(nx.density(S)))
#            print('cc max richclub', max(algs.richclub.rich_club_coefficient(S).values())) 
print(time() - start, 'seconds')
