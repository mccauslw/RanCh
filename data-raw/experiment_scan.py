import sys, string, re, codecs, operator
from itertools import product
from scipy.stats import binom
from math import exp

letterCodes = [
    'A', 'B', 'C', 'D', 'A', # 1-10
    'C', 'D', 'B', 'B', 'A',
    'A', 'C', 'D', 'B', 'C', # 11-20
    'A', 'D', 'D', 'C', 'A',
    'A', 'C', 'B', 'A', 'B', # 21-30
    'C', 'D', 'B', 'A', 'B',
    'C', 'D', 'D', 'C', 'A', # 31-40
    'B', 'D', 'D', 'B', 'A',
    'C', 'A', 'D', 'C', 'B', # 41-50
    'B', 'B', 'C', 'C', 'A',
    'D', 'D', 'A', 'B', 'C', # 51-60
    'C', 'D', 'D', 'B', 'A',
    'B', 'B', 'A', 'B', 'C', # 61-70
    'D', 'A', 'C', 'D', 'A',
    'C', 'D', 'A', 'B', 'B', # 71-80
    'D', 'C', 'C', 'D', 'D',
    'A', 'A', 'B', 'C', 'D', # 81-90
    'A', 'B', 'C', 'D', 'A',
    'B', 'D', 'C', 'A', 'B', # 91-100
    'C', 'D', 'A', 'B', 'C',
    'D', 'A', 'B', 'C', 'D', # 101-110
    'A', 'B', 'C', 'D', 'A',
    'B', 'B', 'C', 'D', 'D', # 111-120
    'C', 'A', 'C', 'A', 'B',
    'D', 'C', 'A', 'C', 'D', # 121-130
    'A', 'A', 'B', 'D', 'A',
    'C', 'D', 'A', 'A', 'B', # 131-140
    'C', 'D', 'D', 'B', 'C',
    'A' # 141 
]

trialFile = open('MC_trial_raw', 'w')
trialFile.write(headerString)

# Iterate over participant's data files
for index, letter in enumerate(letterCodes):
    
    # Construct file name of this partipant's file
    indexAsString = str(index+1)
    fileName = 'MC_data_files/' + indexAsString + letter + '.txt'
    
    # Read in all lines of this participant's file
    pFile = codecs.open(fileName, 'r', 'utf-16')
    lines = pFile.readlines()

    # First line in participant file is original filename of raw data, for index=0,...,80
    if index<81:
        fName = lines[0]
        # Second line gives variable names
        varNames = lines[1].split('\t')
        startLine = 2
    else:
        varNames = lines[0].split('\t')
        startLine = 1
    
    # There is one line in a participant's file for every click.
    # A click is ignored if the click is not on a choice object
    # A choice is recorded differently if it is a gamble or a distractor trial
    # Sets {A}, {B}, {C}, {D} and {E} are represented by 1, 2, 4, 8, 16
    for nline, line in enumerate(lines[startLine:]):
        varValues = line.split('\t') # Values of all variables for this log item
        lineDict = dict(zip(varNames, varValues))
        choiceSet = 0
        if lineDict['ClickedGamble'] == '': # Ignored click, no choice object
            continue
        sheetChoice = int(lineDict['ClickedGamble']) - 1
        choice = -1
        # Build choice set, update choice counts
        for i, g in enumerate(['g1', 'g2', 'g3', 'g4', 'g5']):
            for j, gamble in enumerate(['A', 'B', 'C', 'D', 'E']):
                if lineDict[g] == ('Gamble_' + gamble + '.bmp'):
                    trialType = 'gamble'
                    # Gamble is in choice set, update choice set
                    choiceSet += pow(2, j)
                    if sheetChoice == i:
                        # Gamble is the chosen gamble, set choice to index of gamble
                        choice = j
            for j, distractor in enumerate(['1', '2', '3', '4', '5', '6', '7']):
                if lineDict[g] == ('D' + distractor + '.bmp'):
                    trialType = 'distractor'
                    choiceSet += pow(2, j)
                    if sheetChoice == i:
                        # Distractor is the chosen distractor
                        choice = j
        if trialType == 'gamble':
            # trial is a regular trial, with gambles from {A, B, C, D, E},
            # update appropriate choice count
            participantChoiceCounts[choiceSet][choice] += 1
            participantChoiceSequences[choiceSet].append(choice)
            nCountObjects[choice] += 1
        if trialType == 'distractor':
            # trial is a distractor trial, with gambles from D1, D2, ..., D7
            if (choiceSet & 1)>0:
                nBestAvail += 1
                if (choice != 0):
                    nBestNotChosen += 1
            if (choiceSet & 64)>0:
                nWorstAvail += 1
                if (choice == 6):
                    nWorstChosen += 1
            if ((choiceSet & 1)>0) and ((choiceSet & 64)>0):
                nBothAvail += 1
                if (choice == 6):
                    nDoubleError += 1
