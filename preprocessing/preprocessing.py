import numpy as np
import csv

# Read in file
eyeFilename = '../data/NS01edf2.asc'

# Set up data variables
participantInfo = [eyeFilename[-5], 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA']
trialData = []
startTime = None

# Lists of string in relevant and irrelevant lines
relevant = ['MSG', 'EFIX', 'START', 'END']
irrelevant = ['RECCFG', 'ELCLCFG', 'GAZE_COORDS', 'THRESHOLDS', 'ELCL_WINDOW_SIZES',
              'CAMERA_LENS_FOCAL_LENGTH', 'ELCL_PROC', 'ELCL_EFIT_PARAMS', '!MODE', 'INPUT',
              'CALIBRATION', '!CAL', 'PRESCALER', 'PUPIL', 'GAZE']

# Set up csv file for saving
csvFilename = '../data/fixations/NS01longFixations' + eyeFilename[-5] + '.csv'
header = [['participantNo', 'expPhase', 'trialN', 'response', 'trialStage', 'trialStartTime', 'trialFinishTime', 'rt',
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
            participantInfo[3] = 'NA'
            participantInfo[4] = 'fixationCross'
            participantInfo[5] = 'NA'
            participantInfo[6] = 'NA'
            participantInfo[7] = 'NA'
            continue

        if line[:3] == 'END':
            for l in trialData:
                l[3] = response
                l[5] = startTime
                l[6] = responseTime

                if isinstance(startTime, float):
                    l[7] = responseTime - startTime
                else:
                    l[7] = 'NA'

            with open(csvFilename, 'a') as csvFile:
                writer = csv.writer(csvFile)
                for row in trialData:
                    writer.writerow(row)
            continue

        if line[:4] == 'EFIX':
            line = line.strip()
            line = line.split()

            aoi = ['NA']

            if aoiBoundaries["right"][0] < float(line[5]) < aoiBoundaries["right"][1]:
                if aoiBoundaries['vertical'][0] < float(line[6]) < aoiBoundaries['vertical'][1]:
                    aoi = ['right']
            elif aoiBoundaries["left"][0] < float(line[5]) < aoiBoundaries["left"][1]:
                if any([participantInfo[1] == 'continuous', participantInfo[1] == 'binary']):
                    if aoiBoundaries['vertical'][0] < float(line[6]) < aoiBoundaries['vertical'][1]:
                         aoi = ['left']
                elif participantInfo[1] == 'valuation':
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
                participantInfo[1] = 'binary'
                continue

            if 'START_CONTINUOUS_TASK' in line:
                participantInfo[1] = 'continuous'
                continue

            if 'START_VALUATION_TASK' in line:
                participantInfo[1] = 'valuation'
                continue

            # Things that do require line editing
            line = line.strip()
            line = line.split()

            if 'TrialN' in line:
                participantInfo[2] = line[-1]
                continue

            if 'TRIAL_START' in line:
                participantInfo[4] = 'trial'
                startTime = float(line[1])
                continue

            if 'TRIAL_RESULT' in line:
                response = line[-1]
                if participantInfo[1] != 'valuation':
                    participantInfo[4] = 'postResponse'
                    participantInfo[5] = 'NA'
                continue

            if 'RESPONSE_MADE' in line:
                responseTime = float(line[1])


