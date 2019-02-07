% Clean up routine
function cleanUp
    % Shutdown Eyelink:
    Eyelink('Shutdown');

    % Close figures and windows:
    close all;
    Screen('CloseAll')

    ListenChar(0); % Restore keyboard output to Matlab
    ShowCursor; % Show cursor
end