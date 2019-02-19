function fixationCross
    global par

    if par.dummymode == 1
        crossfixated = true;
    else
        crossfixated = false;
    end
    
    drawFixationCross;
    fixstart = GetSecs;
            
    while ~crossfixated && par.nBadCalib<5 && GetSecs-fixstart<10
        if Eyelink('NewFloatSampleAvailable') > 0
            % get the sample in the form of an event structure
            evt = Eyelink('NewestFloatSample');

            if sqrt((par.screenCenter(1)-evt.gx(1)).^2 + ...
                 (par.screenCenter(2)-evt.gy(1)).^2) < 100
                crossfixated = true;
            end
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
