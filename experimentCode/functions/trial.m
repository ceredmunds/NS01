function [xBoxPos, yBoxPos, response, RT] = trial(stimuli)
    global par
    
    if isequal(par.task, "likert")
        leftImageTexture = NaN;
        rightImage = imread(['Stimuli/' char(string(stimuli)) '.jpg']); 
    else
        leftImage = imread(['Stimuli/' char(string(stimuli(1))) '.jpg']);
        leftImageTexture = Screen('MakeTexture',  par.window, leftImage);
        rightImage = imread(['Stimuli/' char(string(stimuli(2))) '.jpg']); 
    end
    rightImageTexture = Screen('MakeTexture', par.window, rightImage);
    
    % Get mouse position
    SetMouse(par.screenCenter(1), par.screenCenter(2));
    [xMousePos, yMousePos, ~] = GetMouse(par.window);
    buttons = [0 0 0];
    response = NaN;
    
    drawScale;
    drawStimuli(leftImageTexture, rightImageTexture);
    startTime = Screen('Flip', par.window);
    Eyelink('Message', 'TRIAL_START')
    
    while ~all([buttons(1)~=0 ~isnan(response)])
       % Draw scale
        drawStimuli(leftImageTexture, rightImageTexture);
        drawScale;
        [xBoxPos, yBoxPos, response] = drawScaleMarker([xMousePos, yMousePos]); 
        Screen('Flip', par.window);
        
        % Get mouse position
        [xMousePos, yMousePos, buttons] = GetMouse(par.window);
    end % while
    RT = GetSecs() - startTime;
    Eyelink('Message', 'RESPONSE_MADE');
    
    % ITI
    Screen('Flip', par.window);
    WaitSecs(par.ITI-0.2); 
    Eyelink('Message', ['TRIAL_RESULT ', num2str(response)]);
   
    % Clear mouse click
    while any(buttons)
        [~, ~, buttons] = GetMouse(par.window);     
    end
end

function drawScale
    global par

    if isequal(par.scale.orientation, "horizontal")
        horizontalScale = CenterRectOnPoint(par.scale.contiuousScaleRect, ...
            par.screenCenter(1), par.scale.yPos);
        Screen('FillRect', par.window, [255 255 255], horizontalScale);
        
%         Screen('DrawLines', par.window, par.scale.lineCoords, ...
%             40, [255 255 255], ...
%             [par.screenCenter(1) par.scale.yPos], 2);
    elseif isequal(par.scale.orientation, "vertical")
        Screen('DrawLines', par.window, par.scale.lineCoords, ...
            par.lineWidthPx, [255 255 255], ...
            [0.5*par.screenCenter(1) par.screenCenter(2)]);
    end
    
    drawScaleLabels;
end

function drawScaleLabels
    global par

    if isequal(par.task, "likert")
        DrawFormattedText(par.window, '7 = Strongly like', 0.5*par.screenCenter(1)+30, ...
            0.5*par.screenCenter(2)+10);
        DrawFormattedText(par.window, '6', 0.5*par.screenCenter(1)+30, ...
            0.67*par.screenCenter(2)+10);
        DrawFormattedText(par.window, '5', 0.5*par.screenCenter(1)+30, ...
            0.835*par.screenCenter(2)+10);
        DrawFormattedText(par.window, '4', 0.5*par.screenCenter(1)+30, ...
            par.screenCenter(2)+10);
        DrawFormattedText(par.window, '3', 0.5*par.screenCenter(1)+30, ...
            1.165*par.screenCenter(2)+10);
        DrawFormattedText(par.window, '2', 0.5*par.screenCenter(1)+30, ...
            1.330*par.screenCenter(2)+10);
        DrawFormattedText(par.window, '1 = Strongly dislike', 0.5*par.screenCenter(1)+30, ...
            1.5*par.screenCenter(2)+10);
    elseif isequal(par.task, "continuous")
        DrawFormattedText(par.window, 'Prefer A', ...
            0.5*par.screenCenter(1)-40, par.scale.yPos+50);
        DrawFormattedText(par.window, 'Prefer B', ...
            1.5*par.screenCenter(1)-40, par.scale.yPos+50); 
%         DrawFormattedText(par. window, 'Weakly prefer', 0.75*par.screenCenter(1) - 75, par.scale.yPos+40);
%         DrawFormattedText(par. window, 'Weakly prefer', 1.25*par.screenCenter(1) -50, par.scale.yPos+40);
%         DrawFormattedText(par.window, 'No preference', par.screenCenter(1)-65 , par.scale.yPos+40);
    elseif isequal(par.task, "binary")
        DrawFormattedText(par.window, 'Option A', 0.5*par.screenCenter(1)-60, par.scale.yPos+50);
        DrawFormattedText(par.window, 'Option B', 1.5*par.screenCenter(1)-60, par.scale.yPos+50); 
    end
end % drawScaleLabels

function [xBoxPos, yBoxPos, response] = drawScaleMarker(mousePos)
    global par
     % Move rectangle
    if isequal(par.task, "binary")
        if mousePos(1)==par.screenCenter(1)
            xBoxPos = par.screenCenter(1);
            response = NaN;
        else
            [~, response] = min(abs(par.scale.xPos - mousePos(1)));
            xBoxPos = par.scale.xPos(response);
        end
        yBoxPos = par.scale.yPos;
    elseif isequal(par.task, "continuous")
        if mousePos(1) > par.scale.xPos(1) && mousePos(1) < par.scale.xPos(2)
            xBoxPos = mousePos(1);
        else
            [~, response] = min(abs(par.scale.xPos - mousePos(1)));
            xBoxPos = par.scale.xPos(response);
        end
        yBoxPos = par.scale.yPos;
        [~, response] = min(abs(par.scale.continuousPos - mousePos(1)));
    elseif isequal(par.task, "likert")
        [~, response] = min(abs(par.scale.yPos - mousePos(2)));
        xBoxPos = par.scale.xPos;
        yBoxPos = par.scale.yPos(response);
        response = 8 - response;
    end 
    
    centeredRect = CenterRectOnPoint(par.scale.baseRect, xBoxPos, yBoxPos);
    
    Screen('FillRect', par.window, [255 0 0], centeredRect);
end % drawScaleMarker

function drawStimuli(leftImageTexture, rightImageTexture)
    global par

    if ~isnan(leftImageTexture)
        Screen('DrawTexture', par.window, leftImageTexture, [], ...
            par.leftStimulusPos, 0);
    end
    if ~isnan(rightImageTexture)
        Screen('DrawTexture', par.window, rightImageTexture, [], ...
            par.rightStimulusPos, 0);
    end
end
