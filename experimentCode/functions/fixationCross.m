function fixationCross
    global par
    global el

    if par.dummymode == 1
        crossfixated = true;
    else
        crossfixated = false;
    end
    lenFixation = 0;
    
    drawFixationCross;
    fixstart = GetSecs;
            
    while ~crossfixated && par.nBadCalib<5 && GetSecs-fixstart<10
        if Eyelink('NewFloatSampleAvailable') > 0
            % get the sample in the form of an event structure
            evt = Eyelink('NewestFloatSample');

            if par.eye_used ~= -1 % do we know which eye to use yet?
            % if we do, get current gaze position from sample
                x = evt.gx(par.eye_used+1); % +1 as we're accessing MATLAB array
                y = evt.gy(par.eye_used+1);
            else
            % if we don't, first find eye that's being tracked
                par.eye_used = Eyelink('EyeAvailable'); % get eye that's tracked
                if par.eye_used == el.BINOCULAR; % if both eyes are tracked
                    par.eye_used = el.LEFT_EYE; % use left eye
                end
            end

            if sqrt((par.screenCenter(1)-x).^2+(par.screenCenter(2)-y).^2)<100
                lenFixation = lenFixation + 0.05;
                WaitSecs(0.05);
            end
        end
        
        if lenFixation>=0.2
            crossfixated = true;
        end
    end
    
    if GetSecs-fixstart>=10
        par.nBadCalib = par.nBadCalib+1;
    end 
    
    Eyelink('Message', 'FIXATION_FOUND');
end

function drawFixationCross
    
    global par
    
    fixCrossCoords = [-par.fixCrossLenPx/2, par.fixCrossLenPx/2, 0, 0;...
                      0, 0, -par.fixCrossLenPx/2, par.fixCrossLenPx/2];
    
    Screen('DrawLines', par.window, fixCrossCoords, par.lineWidthPx, ...
        [255 255 255], par.screenCenter);
   
    Screen('Flip', par.window);
end
