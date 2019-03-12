import numpy as np
import csv
from os import listdir
from re import sub

for file in listdir("../data/"):
    if file.endswith(".asc"):

        eyeFilename = "../data/"+file
        pptNo = eyeFilename[15] + eyeFilename[16]
        pptNo = sub(r'[^\w\s]', '', pptNo)

        # Set up data variables
        participantInfo = [pptNo, 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA']
        trialData = []
        startTime = None
        block = 0
        totalTrial = 0


        # Lists of string in relevant and irrelevant lines
        relevant = ['MSG', 'EFIX', 'START', 'END']
        irrelevant = ['RECCFG', 'ELCLCFG', 'GAZE_COORDS', 'THRESHOLDS', 'ELCL_WINDOW_SIZES',
                      'CAMERA_LENS_FOCAL_LENGTH', 'ELCL_PROC', 'ELCL_EFIT_PARAMS', '!MODE', 'INPUT',
                      'CALIBRATION', '!CAL', 'PRESCALER', 'PUPIL', 'GAZE']

        # Set up csv file for saving
        csvFilename = '../data/fixations/NS01longFixations' + pptNo + '.csv'
        header = [['participantNo', 'block', 'task', 'blockTrial', 'response', 'trialStage', 'trialStartTime',
                   'trialFinishTime', 'rt', 'trial',
                   'eye', 'fixStart', 'fixEnd', 'fixLength', 'fixPosX', 'fixPosY', 'dilation',
                   'aoi']]

        aoiBoundaries = {"vertical": [1080/2-192, 1080/2+192],
                         "left": [1920/4-257, 1920/4+257],
                         "right": [1920*0.75-257, 1920*0.75+257],
                         "likertX": [1920*0.2, 1920*0.8],
                         "likertY": [1080*0.2, 1080*0.8]}

        with open(csvFilename, 'w') as csvFile:
            writer = csv.writer(csvFile)
            writer.writerows(header)

        with open(eyeFilename, 'r') as f:

            for line in f:
                if not any(x in line for x in relevant):
                    continue

                if line[:5] == 'START':
                    trialData = []
                    aoi = ["NA"]
                    fixation = False
                    participantInfo[4] = 'NA'
                    participantInfo[5] = 'fixationCross'
                    participantInfo[6] = 'NA'
                    participantInfo[7] = 'NA'
                    participantInfo[8] = 'NA'
                    continue

                if line[:3] == 'END':
                    for l in trialData:
                        l[4] = response
                        l[6] = startTime
                        l[7] = responseTime

                        if isinstance(startTime, float):
                            l[8] = responseTime - startTime
                        else:
                            l[8] = 'NA'

                    with open(csvFilename, 'a') as csvFile:
                        writer = csv.writer(csvFile)
                        for row in trialData:
                            writer.writerow(row)
                    continue

                if line[:4] == 'EFIX':
                    fixation = True
                    line = line.strip()
                    line = line.split()

                    aoi = ['NA']

                    if aoiBoundaries["right"][0] < float(line[5]) < aoiBoundaries["right"][1]:
                        if aoiBoundaries['vertical'][0] < float(line[6]) < aoiBoundaries['vertical'][1]:
                            aoi = ['right']
                    elif aoiBoundaries["left"][0] < float(line[5]) < aoiBoundaries["left"][1]:
                        if any([participantInfo[2] == 'continuous', participantInfo[2] == 'binary']):
                            if aoiBoundaries['vertical'][0] < float(line[6]) < aoiBoundaries['vertical'][1]:
                                 aoi = ['left']
                        elif participantInfo[2] == 'valuation':
                            if aoiBoundaries['likertY'][0] < float(line[6]) < aoiBoundaries['likertY'][1]:
                                aoi = ['likertVertical']
                    elif aoiBoundaries['vertical'][1] < float(line[6]):
                        if aoiBoundaries["likertX"][0] < float(line[5]) < aoiBoundaries["likertX"][1]:
                            aoi = ["likertHorizontal"]

                    trialData.append(participantInfo + line[1:] + aoi)

                    continue

                if line[:3] == 'MSG':
                    # Skip irrelevant lines
                    if any(x in line for x in irrelevant):
                        continue

                    # Get trial stage
                    if 'START_BINARY_TASK' in line:
                        block = block + 1
                        participantInfo[1] = str(block)
                        participantInfo[2] = 'binary'
                        continue

                    if 'START_CONTINUOUS_TASK' in line:
                        block = block + 1
                        participantInfo[1] = str(block)
                        participantInfo[2] = 'continuous'
                        continue

                    if 'START_VALUATION_TASK' in line:
                        block = block + 1
                        participantInfo[1] = str(block)
                        participantInfo[2] = 'valuation'
                        continue

                    # Things that do require line editing
                    line = line.strip()
                    line = line.split()

                    if 'TrialN' in line:
                        participantInfo[3] = line[-1]
                        totalTrial += 1
                        participantInfo[9] = str(totalTrial)
                        continue

                    if 'TRIAL_START' in line:
                        participantInfo[5] = 'trial'
                        startTime = float(line[1])
                        continue

                    if 'TRIAL_RESULT' in line:
                        response = line[-1]
                        if participantInfo[1] != 'valuation':
                            participantInfo[5] = 'postResponse'
                            participantInfo[6] = 'NA'
                        continue

                    if 'RESPONSE_MADE' in line:
                        responseTime = float(line[1])

                        if not fixation:
                            line = ['NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA']
                            aoi = ['NONE']
                            trialData.append(participantInfo + line + aoi)


