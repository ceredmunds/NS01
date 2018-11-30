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
expName = 'NS01'; % Experiment name
descriptives.pptNo = 1;
% [descriptives, cancelled] = getDescriptiveStatistics;
% if cancelled==1
%     warning('Quitting the experiment')
%     return;
% end
% 
% if exist(strcat('Data/', expName, '_behav_', ...
%         num2str(descriptives.pptNo), '.csv'), 'file')
%     warning('Changing participant number because of pre-existing '...
%         + 'data file')
%     while exist(strcat('Data/', expName, '_behav_', ...
%                 num2str(descriptives.pptNo), '.csv'), 'file')
%         descriptives.pptNo = descriptives.pptNo + 1;
%     end
% end
% descriptives.behavDataFileName = strcat('Data/', expName, ...
%     '_behav_', num2str(descriptives.pptNo), '.csv');
% descriptives.edfFile = strcat('Data/','ARel_', ...
%     num2str(descriptives.pptNo));

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

Priority(topPriorityLevel);
try
    %% Set up experiment
    % Get order
    descriptives.blockOrder = ["binary"; "continuous"];
    if mod(descriptives.pptNo, 2)
        descriptives.blockOrder = flipud(descriptives.blockOrder);
    end
    

    
    
    HideCursor;
%     openingPageInstructions = [
%        'Thank you for taking part in the experiment\n\n\n'...
%         'Please wait until the experimenter signals the start of the experiment\n\n\n'...
%         'Do not press any keys in the meantime\n\n\n'...
%         'After receiving the signal from the experimenter,\n\n'...
%         'please press the space bar to continue'];
% 
%     % Draw welcome page
%     DrawFormattedText(params.window, openingPageInstructions, 'center', 'center');
%     Screen('Flip', params.window);
%     KbStrokeWait;

    
    
    params.baseRect = [0 0 30 30];
    params.fixCrossDimPix = 20;
    params.lineWidthPix = 4;
    
    stimulusFiles = dir('Stimuli/*.jpg'); 
    stimulusNames = {stimulusFiles.name};
    
    stimuli = [stimulusNames(randperm(4)); stimulusNames(randperm(4))];
    
    params.stimulusSize = [0 0 1024 768]/2;
     
    params.leftStimulusPos = CenterRectOnPoint(params.stimulusSize, 0.5*params.xCenter, params.yCenter);
    params.rightStimulusPos = CenterRectOnPoint(params.stimulusSize, 1.5*params.xCenter, params.yCenter);
    
    for iBlock = 1:2
        for jTrials = 1:4
                % Get stimuli
    

                    
            
            params.condition = descriptives.blockOrder(iBlock);
            [response, rtSecs] = trial(params, stimuli(:, jTrials));
        end
    end
catch 
    sca;
    ShowCursor;
    
    fprintf('\n We''ve hit an error.\n');
    psychrethrow(psychlasterror);
    fprintf('This last text never prints.\n');
end


%% Close down
Priority(0);
RestrictKeysForKbCheck([]);
sca;


%% Functions
function [boxPosition, rt] = trial(params, stimuli)   
    % Define limits of scale
    likertParams.xPos = [0.5 1.5]*params.xCenter;
    likertParams.yPos = 0.9*params.windowHeight;
    % Define lines
    if isequal(params.condition, 'continuous')
        xCoords = [-1 1 -1 -1 1 1 0 0]*params.xCenter/2;
        yCoords = [0 0 -1 1 -1 1 -0.5 0.5 ]*params.fixCrossDimPix; 
    elseif isequal(params.condition, 'binary')
        xCoords = [-1 1 -1 -1 1 1]*params.xCenter/2;
        yCoords = [0 0 -1 1 -1 1]*params.fixCrossDimPix; 
    end  
    likertParams.lineCoords = [xCoords; yCoords];
    
    leftImage = imread(['Stimuli/' char(stimuli(1))]);
    rightImage = imread(['Stimuli/' char(stimuli(2))]);    

    leftImageTexture = Screen('MakeTexture',  params.window, leftImage);
    rightImageTexture = Screen('MakeTexture', params.window, rightImage);
    
    % Get mouse position
    [mousePos, ~, ~] = GetMouse(params.window);
    buttons = [0 0 0];
    
    % Start display and get time
    drawScale(params, likertParams) 
    startTime = Screen('Flip', params.window);
    
    while ~buttons(1)
       % Draw scale
        drawStimuli(params, leftImageTexture, rightImageTexture)
        drawScale(params, likertParams);
        drawScaleLabels(params, likertParams.yPos);
        boxPosition = drawScaleMarker(mousePos, params, likertParams); 
        Screen('Flip', params.window);
        
        % Get mouse position
        [mousePos, ~, buttons] = GetMouse(params.window);
    end % while
    rt = GetSecs() - startTime;  
    
    % Clear mouse click
    while any(buttons)
        [~, ~, buttons] = GetMouse(params.window);     
    end
end

function drawScale(params, likertParams)
    Screen('DrawLines', params.window, likertParams.lineCoords, params.lineWidthPix, ...
            [255 255 255], [params.xCenter likertParams.yPos], 2);
end

function drawScaleLabels(params, yPos)
    if isequal(params.condition, 'continuous')
        DrawFormattedText(params.window, 'Strongly \n prefer', 0.5*params.xCenter-40, yPos+40);
        DrawFormattedText(params.window, 'Strongly \n prefer', 1.5*params.xCenter-40, yPos+40); 
        DrawFormattedText(params. window, 'Weakly prefer', 0.75*params.xCenter - 75, yPos+40);
        DrawFormattedText(params. window, 'Weakly prefer', 1.25*params.xCenter -50, yPos+40);
        DrawFormattedText(params.window, 'No preference', params.xCenter-65 , yPos+40);
    elseif isequal(params.condition, 'binary')
        DrawFormattedText(params.window, 'Option A', 0.5*params.xCenter-40, yPos+40);
        DrawFormattedText(params.window, 'Option B', 1.5*params.xCenter-40, yPos+40); 
    end % if
end % drawScaleLabels

function boxPosition = drawScaleMarker(mousePos, params, likertParams)
    % Get distance of mouse from option
    [value, index] = min(abs(likertParams.xPos - mousePos));
    
     % Move rectangle
    if (mousePos <= likertParams.xPos(1)) || (mousePos>=likertParams.xPos(2)) || ...
            isequal(params.condition, 'binary')
        boxPosition = likertParams.xPos(index);
    elseif isequal(params.condition,'continuous')
        boxPosition = mousePos;
    end 
    
    centeredRect = CenterRectOnPoint(params.baseRect, boxPosition, ...
        likertParams.yPos);
    
    Screen('FillRect', params.window, [255 0 0], centeredRect);
end

function drawStimuli(params, leftImageTexture, rightImageTexture)
    Screen('DrawTexture', params.window, leftImageTexture, [], params.leftStimulusPos, 0);
    Screen('DrawTexture', params.window, rightImageTexture, [], params.rightStimulusPos, 0);
end