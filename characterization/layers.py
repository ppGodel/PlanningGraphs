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
    maxLevel = min(3, max(V.keys())) # only process the first three, if present
    for l in range(maxLevel):
        N = V[l]
        F = list(T['f'] & N) if 'f' in T else {}
        A = list(T['a'] & N) if 'a' in T else {}
        N = list(N)
        L = G.subgraph(N)
        Ei = E[l]['m'] + E[l]['d']
        internal = L.edge_subgraph(Ei)
        targets = [('int', internal)]
        if l < maxLevel - 1:
            both = list(V[l + 1] | V[l])
            T = G.subgraph(both)
            Et = list(E[l]['i'])
            transitional = T.edge_subgraph(Ei)
            targets.append(('trans', T))
        for (label, target) in targets:
            n = len(target)
            print(label, l, 'order', n)
            print(label, l, 'size', target.number_of_edges())
            isolated = len(list(nx.isolates(target)))
            print(label, l, 'isolated', isolated)
            if n >= minimumOrder:
                ccID = 0
                print(label, l, 'maxBetwCent', max(nx.betweenness_centrality(target).values()))
                print(label, l, 'avgClust', algs.average_clustering(target))
                print(label, l, 'vertexCover', len(approx.vertex_cover.min_weighted_vertex_cover(target)))
                if isolated == 0:
                    print(label, l, 'edgeCover', len(algs.covering.min_edge_cover(target)))
                print(label, l, 'degAssort', algs.assortativity.degree_assortativity_coefficient(target))
                print(label, l, 'maxAvgDegConn', max(algs.assortativity.average_degree_connectivity(target).values()))
                print(label, l, 'maxEigCentr', max(algs.centrality.eigenvector_centrality(target).values()))
                print(label, l, 'maxCloseCentr', max(algs.centrality.closeness_centrality(target).values()))
                print(label, l, 'maxLoadCentr', max(nx.algorithms.centrality.load_centrality(target).values()))
                print(label, l, 'maxTriangles', max(algs.cluster.triangles(target).values()))
                print(label, l, 'trans', algs.cluster.transitivity(target))
                print(label, l, 'greedyCol', len(set(algs.coloring.greedy_color(target).values())))
                smallest = n
                biggest = 0
                for CC in nx.connected_components(target):
                    k = len(CC)
                    if k < smallest:
                        smallest = k
                    if k > biggest:
                        biggest = k
                    S = target.subgraph(CC)
                    n = len(S)
                    if n > minimumOrder:
                        print(label, l, 'cc{:d}N {:d}'.format(ccID, n))
                        print(label, l, 'cc{:d}M {:d}'.format(ccID, S.number_of_edges()))
                        print(label, l, 'cc{:d}Rad {:d}'.format(ccID, nx.radius(S)))
                        print(label, l, 'cc{:d}Diam {:d}'.format(ccID, nx.diameter(S)))
                        print(label, l, 'cc{:d}MaxEcc {:d}'.format(ccID, max(nx.eccentricity(S).values())))
                        print(label, l, 'cc{:d}Cent {:f}'.format(ccID, 100 * len(nx.center(S)) / n))
                        print(label, l, 'cc{:d}Periph {:f}'.format(ccID, 100 * len(nx.periphery(S)) / n))
                        print(label, l, 'cc{:d}Dens {:f}'.format(ccID, nx.density(S)))
                    ccID += 1
                print(label, l, 'ccCount', ccID)
                print(label, l, 'ccMin', smallest)
                print(label, l, 'ccMax', biggest)
