from sys import argv
import networkx as nx
import networkx.algorithms as algs
from collections import defaultdict
import networkx.algorithms.approximation as approx
import numpy as np
np.seterr(divide='ignore', invalid='ignore')

nodeTypes = 'fa' # fact action
edgeTypes = 'moid' # mutex out in delete
minimumOrder = 10
redo = False
name = argv[1]
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
    maxLevel = max(V.keys())
    for l in range(maxLevel + 1):
        N = V[l]
        F = list(T['f'] & N)
        A = list(T['a'] & N)
        N = list(N)
        L = G.subgraph(N)
        n = len(N)
        isolated = len(list(nx.isolates(L)))
        print(l, 'order', n)
        print(l, 'size', L.number_of_edges())
        print(l, 'isolated', isolated)
        if n > minimumOrder:
            if redo:
                print(l, 'maxBetwCent', max(nx.betweenness_centrality(L).values()))
                print(l, 'avgClust', algs.average_clustering(L))
                print(l, 'vertexCover', len(approx.vertex_cover.min_weighted_vertex_cover(L)))
                if isolated == 0:
                    print(l, 'edgeCover', len(algs.covering.min_edge_cover(L)))
                print(l, 'degAssort', algs.assortativity.degree_assortativity_coefficient(L))
                print(l, 'maxAvgDegConn', max(algs.assortativity.average_degree_connectivity(L).values()))
                print(l, 'maxEigCentr', max(algs.centrality.eigenvector_centrality(L).values()))
                print(l, 'maxCloseCentr', max(algs.centrality.closeness_centrality(L).values()))
                print(l, 'maxLoadCentr', max(nx.algorithms.centrality.load_centrality(L).values()))
                print(l, 'maxTriangles', max(algs.cluster.triangles(L).values()))
                print(l, 'trans', algs.cluster.transitivity(L))
                print(l, 'greedyCol', len(set(algs.coloring.greedy_color(L).values())))
            ccID = 0
            smallest = n
            biggest = 0
            for CC in nx.connected_components(L):
                k = len(CC)
                if k < smallest:
                    smallest = k
                if k > biggest:
                    biggest = k
                ccID += 1
                if redo:
                    S = L.subgraph(CC)
                    n = len(S)
                    if n > minimumOrder:
                        print(l, 'cc{:d}N {:d}'.format(ccID, n))
                        print(l, 'cc{:d}M {:d}'.format(ccID, S.number_of_edges()))
                        print(l, 'cc{:d}Rad {:d}'.format(ccID, nx.radius(S)))
                        print(l, 'cc{:d}Diam {:d}'.format(ccID, nx.diameter(S)))
                        print(l, 'cc{:d}MaxEcc {:d}'.format(ccID, max(nx.eccentricity(S).values())))
                        print(l, 'cc{:d}Cent {:f}'.format(ccID, 100 * len(nx.center(S)) / n))
                        print(l, 'cc{:d}Periph {:f}'.format(ccID, 100 * len(nx.periphery(S)) / n))
                        print(l, 'cc{:d}Dens {:f}'.format(ccID, nx.density(S)))
            print(l, 'ccCount', ccID)
            print(l, 'ccMin', smallest)
            print(l, 'ccMax', biggest)
