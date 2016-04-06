import os
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# Animates the growth of a network based on numbered files like those growNetworkExample.f03 generates

# opens every file t[int].txt from a directory and drawsa network with the containing edges
dataDir = 'data/'
fileNamePrefix = 't'
fileExtension = '.txt'

fileNames = os.listdir(dataDir)
fileNumbers = []

# get the number of each file and sort them
for fileName in fileNames:
    number = int(fileName[len(fileNamePrefix):len(fileName)-len(fileExtension)])
    fileNumbers.append(number)

fileNumbers = np.array(fileNumbers)
fileNumbers.sort()

# setup network graph and plot
fig = plt.figure()
G = nx.DiGraph()

def getDataFromFileNumber(i):

    global dataDir
    global fileNamePrefix
    global fileNumbers
    global fileExtension

    # return empty array if there are no more files
    if fileNumbers.size < i + 1:
        return np.array([])

    fileName = dataDir+fileNamePrefix+str(fileNumbers[i])+fileExtension

    data = np.loadtxt(fileName)

    # fix dimensions
    if len(data.shape) == 1:
        edges = data.reshape((1, 2))

    else:
        edges = data

    return edges

def anim(i):

    edges = getDataFromFileNumber(i)

    if edges.size == 0:
        # nothing more to change
        return


    plt.clf()


    for edge in edges:
        G.add_edge(edge[0], edge[1])

    #print edges

    positions = nx.spring_layout(G)
    nx.draw_networkx(G, positions)


ani = animation.FuncAnimation(fig, anim, interval = 1000)

plt.show()