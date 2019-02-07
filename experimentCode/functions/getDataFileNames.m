function getDataFileNames
    global par
    
    % Check if data file exists
    if exist(strcat('data/', par.expName, '_behav_', ...
            num2str(par.pptNo), '.csv'), 'file')==2
        warning('Changing participant number because of pre-existing data file')

        while exist(strcat('data/', par.expName, '_behav_', ...
                    num2str(par.pptNo), '.csv'), 'file')==2
            par.pptNo = par.pptNo + 1;
        end
    end
    
    % Get data file name
    par.behavDataFileName = strcat('data/', par.expName, ...
        '_behav_', num2str(par.pptNo), '.csv');
    par.edfDataFileName =  strcat('data/', par.expName, ...
        'edf', num2str(par.pptNo), '.edf');
end
