%% NS01 - Binary choice vs. binary ratings
% C. E. R. Edmunds - 3/11/2018
%% Setup
close all;
clearvars; 
sca;

% Adding directories
addpath('Functions')
addpath('Stimuli')
addpath('Data')
PsychDefaultSetup(1);

% Control variables
testMode = 1; % run: 1=without eyetracker, 0=with eyetracker
topPriorityLevel = 1;

% Participant level (ppt) variables
noBadCalibrations = 0; % initialises number of failed cross fixations

% Get descriptives
expName = "NS01"; % Experiment name
totalTrials = 300; % Total number of trials
% descriptives.pptNo = 1;
[descriptives, cancelled] = getDescriptiveStatistics;
if cancelled==1
    warning('Quitting the experiment')
    return;
end

%% Experiment setup
% Randomisation
rng(descriptives.pptNo); % Set seed 

% Screen 
Screen('Preference', 'SkipSyncTests', 1); 
screenNumber = max(Screen('Screens'));
% oldResolution = Screen('Resolution', screenNumber, 1920 , 1080, 60, 32);
[params.window, params.windowRect] = Screen('OpenWindow', screenNumber, ...
    0,[],32,2);
Screen(params.window, 'BlendFunction', ...
    GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
[params.windowWidth, params.windowHeight] = WindowSize(params.window);
[params.xCenter, params.yCenter] = RectCenter(params.windowRect);

    
% Text
Screen('TextFont', params.window, 'Helvetica');
Screen('TextStyle', params.window, 0);
Screen('TextSize', params.window, 20);
Screen('TextColor', params.window, [255 255 255]);

% Response keys
spaceKey = KbName('space');
escapeKey = KbName('ESCAPE');
RestrictKeysForKbCheck([spaceKey, escapeKey]);

%% Timings
params.ITIsecs = 0.5; % Inter-trial interval in seconds

params.nBlocks = 2;
params.nChoices = 4; % FIX: CHANGE TO 50 

%% Stimuli
% Set parameters
params.baseRect = [0 0 30 30];
params.fixCrossDimPix = 20;
params.lineWidthPix = 4;
params.stimulusSize = [0 0 1024 768]/2;
params.leftStimulusPos = CenterRectOnPoint(params.stimulusSize, ...
    0.5*params.xCenter, params.yCenter);
params.rightStimulusPos = CenterRectOnPoint(params.stimulusSize, ... 
    1.5*params.xCenter, params.yCenter);

% Get choices
% 1=participant no, 2=lChoice, 3=lValue, 4=rChoice, 5=rValue
choices = csvread('Data/NS01choices.csv', ((descriptives.pptNo-1)*100+1),...
    0, [((descriptives.pptNo-1)*100+1), 0, descriptives.pptNo*100, 4]);
stimuli = [choices(:,2), choices(:,4)];
likertStimuli = [choices(:,2:3); choices(:,4:5)];
likertStimuli = likertStimuli(randperm(length(likertStimuli)),:);

%% Data
if exist(strcat('Data/', expName, '_behav_', ...
        num2str(descriptives.pptNo), '.csv'), 'file')==2
    warning('Changing participant number because of pre-existing data file')
    while exist(strcat('Data/', expName, '_behav_', ...
                num2str(descriptives.pptNo), '.csv'), 'file')==2
        descriptives.pptNo = descriptives.pptNo + 1;
    end
end
descriptives.behavDataFileName = strcat('Data/', expName, ...
    '_behav_', num2str(descriptives.pptNo), '.csv');
descriptives.edfFile = strcat('Data/','ARel_', ...
    num2str(descriptives.pptNo));

% Save column headers in behavioural data file
header = {'rowN', 'DAU', 'participantN', 'age', 'gender', 'task', 'taskOrder'...
    'trial', 'lImage', 'lValue', 'rImage', 'rValue', 'xBoxPos', 'yBoxPos', ...
    'response', 'RT'};
dataTypes = {'double', 'string', 'int8', 'int8', 'string', 'string', ...
    'int8', 'int8', 'string', 'string', 'string', 'string', ...
    'double', 'double', 'double', 'double'};
data = table('Size', [totalTrials length(header)], ...
    'VariableTypes', dataTypes, 'VariableNames', header);
data.rowN = (1:totalTrials)';
data.DAU = repmat(expName, totalTrials, 1);
data.participantN = repmat(int8(descriptives.pptNo), totalTrials, 1);
data.age = repmat(int8(descriptives.age), totalTrials, 1);
data.gender = repmat(int8(descriptives.gender), totalTrials, 1);
data.task(1:200) = repmat("likert", 200, 1);
data.taskOrder = [ones(200, 1); repmat(2, 50, 1); repmat(3, 50, 1)];
data.trial = [(1:200)'; (1:50)'; (1:50)'];
data.lImage = [repmat("NA", 200, 1); string(choices(:, 2))];
data.lValue = [repmat("NA", 200, 1); string(choices(:, 3))];
data.rImage = string([likertStimuli(:, 1); choices(:, 4)]);
data.rValue = string([likertStimuli(:, 2); choices(:, 5)]);
 
writetable(data, descriptives.behavDataFileName);

reactionTimes = NaN(params.nBlocks*params.nChoices, 1);
choices = NaN(params.nBlocks*params.nChoices, 1); 

Priority(topPriorityLevel);
try
    %% Set up experiment
    % Get order
    descriptives.blockOrder = ["binary"; "continuous"];
    if mod(descriptives.pptNo, 2)
        descriptives.blockOrder = flipud(descriptives.blockOrder);
    end
    data.task(201:250) = repmat(descriptives.blockOrder(1), 50, 1);
    data.task(251:300) = repmat(descriptives.blockOrder(2), 50, 1);
    
    HideCursor;
    openingPageInstructions = [
       'Thank you for taking part in the experiment\n\n\n'...
        'Please wait until the experimenter signals the start of the experiment\n\n\n'...
        'Do not press any keys in the meantime\n\n\n'...
        'After receiving the signal from the experimenter,\n\n'...
        'please press the space bar to continue'];

    % Draw welcome page
    DrawFormattedText(params.window, openingPageInstructions, 'center', 'center');
    Screen('Flip', params.window);
    KbStrokeWait;
    params.task = 'likert';
    row = 0;
    for iTrials = 1:2
        row = row + 1;
        [data.xBoxPos(row), data.yBoxPos(row), data.response(row), data.RT(row)] = trial(params, likertStimuli(:,1));
        writetable(data, descriptives.behavDataFileName);
    end
    
    row = 200;
    for iBlock = 1:params.nBlocks
        for jTrials = 1:2 %params.nChoices
            row = row + 1;
            params.task = descriptives.blockOrder(iBlock);
            [data.xBoxPos(row), data.yBoxPos(row), ...
                data.response(row), data.RT(row)] = trial(params, ...
                      stimuli((iBlock-1)*params.nChoices + jTrials, :));
            writetable(data, descriptives.behavDataFileName);
        end
        row = 250;
    end
catch 
    sca;
    ShowCursor;
    writetable(data, descriptives.behavDataFileName);
    
    fprintf('\n We''ve hit an error.\n');
    psychrethrow(psychlasterror);
    fprintf('This last text never prints.\n');
end


%% Close down
writetable(data, descriptives.behavDataFileName);
Priority(0);
RestrictKeysForKbCheck([]);
sca;

%% Functions
function [xBoxPos, yBoxPos, response, rt] = trial(params, stimuli)   
    % Define lines
    if isequal(params.task, 'continuous')
        orientation = 'horizontal';
        likertParams.xPos = [0.5 1.5]*params.xCenter;
        likertParams.yPos = 0.9*params.windowHeight;
        xCoords = [-1 1 -1 -1 1 1 0 0]*params.xCenter/2;
        yCoords = [0 0 -1 1 -1 1 -0.5 0.5 ]*params.fixCrossDimPix;
        likertParams.continuousPos = linspace(params.windowWidth*0.25, ...
            params.windowWidth*0.75, 101);
    elseif isequal(params.task, 'likert')
        orientation = 'vertical';
        likertParams.xPos = 0.25*params.windowWidth;
        likertParams.yPos = [-1 -0.66 -0.33 0 0.33 ...
            0.66 1]*params.yCenter/2+params.yCenter;
        xCoords = [0 0 -1 1 -1 1 -1 1 ...
            -1 1 -1 1 -1 1 -1 1]*params.fixCrossDimPix;
        yCoords = [-1 1 -1 -1 1 1 0 0 ...
            -0.33 -0.33 -0.66 -0.66 0.33 0.33 0.66 0.66]*params.yCenter/2;
    elseif isequal(params.task, 'binary')
        orientation = 'horizontal';
        likertParams.xPos = [0.5 1.5]*params.xCenter;
        likertParams.yPos = 0.9*params.windowHeight;
        xCoords = [-1 1 -1 -1 1 1]*params.xCenter/2;
        yCoords = [0 0 -1 1 -1 1]*params.fixCrossDimPix; 
    end  
    likertParams.lineCoords = [xCoords; yCoords];
    
    if isequal(params.task, 'likert')
        leftImageTexture = NaN;
    else
        leftImage = imread(['Stimuli/' char(string(stimuli(1))) '.jpg']);
        leftImageTexture = Screen('MakeTexture',  params.window, leftImage);
    end
    rightImage = imread(['Stimuli/' char(string(stimuli(2))) '.jpg']); 
    rightImageTexture = Screen('MakeTexture', params.window, rightImage);
    
    % Get mouse position
    [xMousePos, yMousePos, ~] = GetMouse(params.window);
    buttons = [0 0 0];
    
    % Start display and get time
    drawScale(params, likertParams, orientation) 
    startTime = Screen('Flip', params.window);
    
    while ~buttons(1)
       % Draw scale
        drawStimuli(params, leftImageTexture, rightImageTexture)
        drawScale(params, likertParams, orientation);
        drawScaleLabels(params, likertParams.yPos);
        [xBoxPos, yBoxPos, response] = drawScaleMarker([xMousePos, yMousePos],...
            params, likertParams); 
        Screen('Flip', params.window);
        
        % Get mouse position
        [xMousePos, yMousePos, buttons] = GetMouse(params.window);
    end % while
    
    % Get output
    rt = GetSecs() - startTime;
    
    % ITI
    Screen('Flip', params.window);
    WaitSecs(params.ITIsecs)
    
    % Clear mouse click
    while any(buttons)
        [~, ~, buttons] = GetMouse(params.window);     
    end
end

function drawScale(params, likertParams, orientation)
    if isequal(orientation, 'horizontal')
        Screen('DrawLines', params.window, likertParams.lineCoords, ...
            params.lineWidthPix, [255 255 255], ...
            [params.xCenter likertParams.yPos], 2);
    elseif isequal(orientation, 'vertical')
        Screen('DrawLines', params.window, likertParams.lineCoords, ...
            params.lineWidthPix, [255 255 255], ...
            [0.25*params.windowWidth params.yCenter], 2);
    end
end

function drawScaleLabels(params, yPos)
    if isequal(params.task, 'likert')
        DrawFormattedText(params.window, 'Strongly \n prefer',0.25*params.xCenter+40, 1.5*params.yCenter);
        DrawFormattedText(params.window, 'Strongly \n prefer', 0.25*params.xCenter+40, 0.5*params.yCenter); 
        DrawFormattedText(params. window, 'Weakly prefer', 0.25*params.xCenter+40 , 1.25*params.yCenter);
        DrawFormattedText(params. window, 'Weakly prefer', 0.25*params.xCenter+40, 0.75*params.yCenter);
        DrawFormattedText(params.window, 'No preference', 0.25*params.xCenter+40, params.yCenter+5);
    elseif isequal(params.task, 'continuous')
        DrawFormattedText(params.window, 'Strongly \n prefer', 0.5*params.xCenter-40, yPos+40);
        DrawFormattedText(params.window, 'Strongly \n prefer', 1.5*params.xCenter-40, yPos+40); 
        DrawFormattedText(params. window, 'Weakly prefer', 0.75*params.xCenter - 75, yPos+40);
        DrawFormattedText(params. window, 'Weakly prefer', 1.25*params.xCenter -50, yPos+40);
        DrawFormattedText(params.window, 'No preference', params.xCenter-65 , yPos+40);
    elseif isequal(params.task, 'binary')
        DrawFormattedText(params.window, 'Option A', 0.5*params.xCenter-40, yPos+40);
        DrawFormattedText(params.window, 'Option B', 1.5*params.xCenter-40, yPos+40); 
    end
end % drawScaleLabels

function [xBoxPos, yBoxPos, response] = drawScaleMarker(mousePos, params, likertParams)
     % Move rectangle
    if isequal(params.task, 'binary')
        [~, response] = min(abs(likertParams.xPos - mousePos(1)));
        xBoxPos = likertParams.xPos(response);
        yBoxPos = likertParams.yPos;
    elseif isequal(params.task, 'continuous')
        if mousePos(1) > likertParams.xPos(1) && mousePos(1) < likertParams.xPos(2)
            xBoxPos = mousePos(1);
        else
            [~, response] = min(abs(likertParams.xPos - mousePos(1)));
            xBoxPos = likertParams.xPos(response);
        end
        yBoxPos = likertParams.yPos;
        [~, response] = min(abs(likertParams.continuousPos - mousePos(1)));
    elseif isequal(params.task, 'likert')
        [~, response] = min(abs(likertParams.yPos - mousePos(2)));
        xBoxPos = likertParams.xPos;
        yBoxPos = likertParams.yPos(response);
    end 
    
    centeredRect = CenterRectOnPoint(params.baseRect, xBoxPos, yBoxPos);
    
    Screen('FillRect', params.window, [255 0 0], centeredRect);
end

function drawStimuli(params, leftImageTexture, rightImageTexture)
    if ~isnan(leftImageTexture)
        Screen('DrawTexture', params.window, leftImageTexture, [], ...
            params.leftStimulusPos, 0);
    end
    if ~isnan(rightImageTexture)
        Screen('DrawTexture', params.window, rightImageTexture, [], ...
            params.rightStimulusPos, 0);
    end
end
