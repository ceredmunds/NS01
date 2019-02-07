function getTaskParameters
     global par
     
     par.scale.baseRect = [0 0 30 30];
     par.scale.continuousScaleWidth = 40;
     
     if isequal(par.task, "continuous")
        par.scale.orientation = "horizontal";
        par.scale.xPos = [0.5 1.5]*par.screenCenter(1);
        par.scale.yPos = 0.9*par.windowRect(4);
        xCoords = [-1 1 -1 -1 1 1 0 0]*par.screenCenter(1)/2;
        yCoords = [0 0 -1 1 -1 1 -0.5 0.5 ]*0;%par.fixCrossLenPx;
        par.scale.continuousPos = linspace(par.windowRect(3)*0.25, ...
            par.windowRect(3)*0.75, 100);
        par.scale.contiuousScaleRect = [0 0 par.screenCenter(1)+40 40];
    elseif isequal(par.task, "likert")
        par.scale.orientation = "vertical";
        par.scale.xPos = 0.25*par.windowRect(3);
        par.scale.yPos = [-1 -0.66 -0.33 0 0.33 ...
            0.66 1]*par.screenCenter(2)/2+par.screenCenter(2);
        xCoords = [0 0 -1 1 -1 1 -1 1 ...
            -1 1 -1 1 -1 1 -1 1]*par.fixCrossLenPx;
        yCoords = [-1 1 -1 -1 1 1 0 0 ...
            -0.33 -0.33 -0.66 -0.66 0.33 0.33 0.66 0.66]*par.screenCenter(2)/2;
    elseif isequal(par.task, "binary")
        par.scale.orientation = "none";
        par.scale.xPos = [0.5 1.5]*par.screenCenter(1);
        par.scale.yPos = 0.9*par.windowRect(4);
        xCoords = [-1 1 -1 -1 1 1]*par.screenCenter(1)/2;
        yCoords = [0 0 -1 1 -1 1]*par.fixCrossLenPx; 
    end  
    par.scale.lineCoords = [xCoords; yCoords];
end