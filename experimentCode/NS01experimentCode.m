%% Setup
clear all;

% Adding directories
addpath('functions', 'stimuli', 'data')
PsychDefaultSetup(1);

%cleanUp;
commandwindow;

try
    %% Get global parameters 
    global par
    global el
    
    % Development parameters
    test = false;
    if test
        par.nChoices = 2; % Number of choices per task
        par.nImages = 4; 
    else 
        par.nChoices = 50;
        par.nImages = 200;
    end
       
    % Eye-tracking
    par.dummymode = 0;       % set to 1 to initialize in dummymode
    par.nBadCalib = 0;
    
    % Fixation cross
    par.fixCrossLenPx = 20;  
    par.lineWidthPx = 4;
    
    % Experiment
    par.expName = "NS01";
    par.ITI = 0.5;
    par.screenColor = [0 0 0];
    par.textColor = [255 255 255];
    
    %% Get behavioural data file
    totalTrials = 300; % Total number of trials
    
    % Get descriptive statistics
    [descriptives, cancelled] = getDescriptiveStatistics;
    if cancelled==1
        warning('Quitting the experiment')
        return;
    end
    par.pptNo = descriptives.pptNo;

    getDataFileNames;
    
    % Randomisation
    rng(par.pptNo); % Set seed 
    
    % Get stimuli
    % 1=participant no, 2=lChoice, 3=lValue, 4=rChoice, 5=rValue
    choices = csvread('data/NS01choices.csv', ((descriptives.pptNo-1)*100+1),...
        0, [((descriptives.pptNo-1)*100+1), 0, descriptives.pptNo*100, 4]);
    stimuli = [choices(:,2), choices(:,4)];
    likertStimuli = [choices(:,2:3); choices(:,4:5)];
    likertStimuli = likertStimuli(randperm(length(likertStimuli)),:);
    
    % Save column headers in behavioural data file
    header = {'rowN', 'DAU', 'participantN', 'age', 'gender', 'task', ...
        'taskOrder', 'trial', 'lImage', 'lValue', 'rImage', 'rValue', ...
        'xBoxPos', 'yBoxPos', 'response', 'RT'};
    
    par.data = table((1:totalTrials)', repmat(par.expName, totalTrials, 1),...
        repmat(int8(descriptives.pptNo), totalTrials, 1),...
        repmat(int8(descriptives.age), totalTrials, 1),...
        repmat(descriptives.gender, totalTrials, 1),...
        [repmat("likert", 300, 1)],...
        [ones(50, 1); repmat(2, 50, 1); repmat(3, 200, 1)],...
        [(1:200)'; (1:50)'; (1:50)'],...
        [string(choices(:, 2)); repmat("NA", 200, 1)],...
        [string(choices(:, 3)); repmat("NA", 200, 1)],...
        string([choices(:, 4); likertStimuli(:, 1)]),...
        string([choices(:, 5); likertStimuli(:, 2)]),...
        zeros(totalTrials, 1),...
        zeros(totalTrials, 1),...
        zeros(totalTrials, 1),...
        zeros(totalTrials, 1),'VariableNames', header);
    
    par.blockNames = ["binary"; "continuous"];
    if mod(par.pptNo, 2)
        par.blockNames = flipud(par.blockNames);
    end
    par.data.task(1:50) = repmat(string(par.blockNames(1)), 50, 1);
    par.data.task(51:100) = repmat(string(par.blockNames(2)), 50, 1);

    writetable(par.data, par.behavDataFileName);
    
    %% Open Screen
    screenNumber = max(Screen('Screens'));
    [par.window, par.windowRect] = Screen('OpenWindow', screenNumber, ...
        par.screenColor);
    par.screenCenter = par.windowRect(3:4)/2;
    Screen(par.window, 'BlendFunction', ...
        GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    Screen('TextFont', par.window, 'Helvetica');
    Screen('TextStyle', par.window, 0);
    Screen('TextSize', par.window, 20);
    Screen('TextColor', par.window, par.textColor);
    
    par.stimulusSize = [0 0 1024 768]/2;
    par.leftStimulusPos = CenterRectOnPoint(par.stimulusSize, ...
        0.5*par.screenCenter(1), par.screenCenter(2));
    par.rightStimulusPos = CenterRectOnPoint(par.stimulusSize, ... 
        1.5*par.screenCenter(1), par.screenCenter(2));
    
    % Tidy input devices (mouse/keyboard)
    ListenChar(2); % Disable key output to Matlab window:
    HideCursor; % Hide cursor
    
    %% Initialise eye-tracker
    el=EyelinkInitDefaults(par.window); % Get info for eyetracker
    
    % We are changing calibration to a black background with white targets,
    % no sound and smaller targets
    el.backgroundcolour = BlackIndex(el.window);
    el.msgfontcolour  = WhiteIndex(el.window);
    el.imgtitlecolour = WhiteIndex(el.window);
    el.targetbeep = 0;
    el.calibrationtargetcolour= WhiteIndex(el.window);
    el.calibrationtargetsize= 1;
    el.calibrationtargetwidth=0.5;
    
    EyelinkUpdateDefaults(el);
    
    % Initialise eyetracker, exit if fails
    if ~EyelinkInit(par.dummymode, 1)
        fprintf('Eyelink Init aborted.\n');
        cleanUp;  % cleanup function
        return;
    end
    
    % SET UP TRACKER CONFIGURATION
    % Setting the proper recording resolution, proper calibration type,
    % as well as the data file content;
    
    Eyelink('command', 'add_file_preamble_text ''Recorded by EyelinkToolbox demo-experiment''');
    % This command is crucial to map the gaze positions from the tracker to
    % screen pixel positions to determine fixation
    Eyelink('command','screen_pixel_coords = %ld %ld %ld %ld', 0, 0, par.windowRect(3)-1, par.windowRect(4)-1);
    Eyelink('message', 'DISPLAY_COORDS %ld %ld %ld %ld', 0, 0, par.windowRect(3)-1, par.windowRect(4)-1);
    % set calibration type.
    Eyelink('command', 'calibration_type = HV13');
    Eyelink('command', 'generate_default_targets = YES');
    % set parser (conservative saccade thresholds)
    Eyelink('command', 'saccade_velocity_threshold = 35');
    Eyelink('command', 'saccade_acceleration_threshold = 9500');
    % set EDF file contents
    % 5.1 retrieve tracker version and tracker software version
    [v,vs] = Eyelink('GetTrackerVersion');
    fprintf('Running experiment on a ''%s'' tracker.\n', vs );
    vsn = regexp(vs,'\d','match');
    
    if v == 3 && str2double(vsn{1}) == 4 % if EL 1000 and tracker version 4.xx
        % remote mode possible add HTARGET ( head target)
        Eyelink('command', 'file_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,INPUT');
        Eyelink('command', 'file_sample_data  = LEFT,RIGHT,GAZE,HREF,AREA,GAZERES,STATUS,INPUT,HTARGET');
        % set link data (used for gaze cursor)
        Eyelink('command', 'link_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,FIXUPDATE,INPUT');
        Eyelink('command', 'link_sample_data  = LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS,INPUT,HTARGET');
    else
        Eyelink('command', 'file_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,FIXUPDATE,INPUT');
        Eyelink('command', 'file_sample_data  = LEFT,RIGHT,GAZE,HREF,AREA,GAZERES,STATUS,INPUT');
        % set link data (used for gaze cursor)
        Eyelink('command', 'link_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,FIXUPDATE,INPUT');
        Eyelink('command', 'link_sample_data  = LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS,INPUT');
    end
    
    % Open file to record data to
    edfFileTemp=['A' num2str(par.pptNo) '.edf'];
    Eyelink('Openfile', edfFileTemp);
    
    %% Instructions
    displayInstructions("welcome", false);

    %% Start experimemt
    row = 0;
    for jBlock = 1:2
        par.task = par.blockNames(jBlock);
        getTaskParameters;
         
        if isequal(par.task, "binary")
            Eyelink('Message', 'START_BINARY_TASK')
        elseif isequal(par.task, "continuous")
            Eyelink('Message', 'START_CONTINUOUS_TASK')
        end
        
        displayInstructions(par.task, false);
        EyelinkDoTrackerSetup(el); % Calibrate the eye tracker
        displayInstructions(par.task, true);
         
        par.eye_used = -1
        
        for iTrials = 1:par.nChoices
            row = row + 1;
            Eyelink('Message', 'TrialN %d', iTrials); % Mark start of trial
            Eyelink('command', 'record_status_message "TRIAL %d/%d"', ...
                row, totalTrials);
            
            Eyelink('Command', 'set_idle_mode');
            WaitSecs(0.05);
            Eyelink('StartRecording');
            WaitSecs(0.05);
            Eyelink('Message', 'TRIALID %d', iTrials);
            % record a few samples before we actually start displaying
            % otherwise you may lose a few msec of data
            WaitSecs(0.1);
            
            fixationCross;
            [par.data.xBoxPos(row), par.data.yBoxPos(row), ...
                par.data.response(row), par.data.RT(row)] = ...
                trial(stimuli((jBlock-1)*par.nChoices + iTrials, :));
            writetable(par.data, par.behavDataFileName);
            Eyelink('StopRecording'); % Stop recording eye-movements
            
            if any(iTrials==[25])
                EyelinkDoTrackerSetup(el);
                par.eye_used = -1
            end
        end
        row = 50;
    end
    
        % Value task
    par.task = "likert";
    row = 100;
    getTaskParameters;
    
    Eyelink('Message', 'START_VALUATION_TASK'); % Mark end of valuation task
    WaitSecs(0.05);
    
    displayInstructions(par.task, false);
    EyelinkDoTrackerSetup(el); % Calibrate the eye tracker
    displayInstructions(par.task, true);
    par.eye_used = -1
    
    for iTrials = 1:par.nImages
        row = row + 1;
        
        if any(mod(iTrials, 50)==[1 26]) && iTrials>1
            EyelinkDoTrackerSetup(el);
            par.eye_used = -1
        end
        
        Eyelink('Message', 'TrialN %d', iTrials); % Mark start of trial
        Eyelink('command', 'record_status_message "TRIAL %d/%d"', ...
            row, totalTrials);
        
        Eyelink('Command', 'set_idle_mode');
            WaitSecs(0.05);
            Eyelink('StartRecording');
            WaitSecs(0.05);
            Eyelink('Message', 'TRIALID %d', iTrials);
            % record a few samples before we actually start displaying
            % otherwise you may lose a few msec of data
            WaitSecs(0.1);
            
        fixationCross;
        
        [par.data.xBoxPos(row), par.data.yBoxPos(row), ...
            par.data.response(row), par.data.RT(row)] = ...
            trial(likertStimuli(iTrials,1));
        writetable(par.data, par.behavDataFileName);
         
        Eyelink('StopRecording'); % Stop recording eye-movements

        if mod(iTrials, 50)==0 && iTrials<200
            displayInstructions("break", true);
        end
    end
    Eyelink('Message', 'END_VALUATION_TASK'); % Mark end of valuation task
    WaitSecs(0.05);
    
    displayInstructions("goodbye", false);
    
    %% Close down
    Eyelink('CloseFile'); % Close data file
    
    % Transfer data file
    try
        fprintf('Receiving data file ''%s''\n', edfFileTemp );
        status=Eyelink('ReceiveFile');
        if status > 0
            fprintf('ReceiveFile status %d\n', status);
        end
        if 2==exist(edfFileTemp, 'file')
            movefile(edfFileTemp, par.edfDataFileName)
            fprintf('Data file ''%s'' can  be found in ''%s''\n', ...
                par.edfDataFileName, pwd);
        end
    catch error
        fprintf('Problem receiving data file ''%s''\n', edfFileTemp );
        rethrow(error)
    end % try..catch transfer edf file
    
    cleanUp; % Close screens and eye-tracker
catch 
    cleanUp;
    
    fprintf('\n We hit an error.\n');
    psychrethrow(psychlasterror);
    fprintf('This last text never prints.\n');
end % try..catch
