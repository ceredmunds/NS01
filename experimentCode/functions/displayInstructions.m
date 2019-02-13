function displayInstructions(expPhase)
    global par
    
    if isequal(expPhase, "welcome")
        instructions = [
        'Thank you for taking part in the experiment\n\n\n'...
        'Please wait until the experimenter signals the start of the experiment\n\n\n'...
        'Do not press any keys in the meantime\n\n\n'];
    elseif  isequal(expPhase, "likert")
        instructions = ['In the first part of the experiment,\n\n\n'...
        'you will be asked to rate how much you like a series of pictures.\n\n\n'...
        'Please respond using the mouse and left mouse button.'];
        pressKeyContinue
    elseif isequal(expPhase, "binary")
        instructions = ['In this part of the experiment,\n\n\n'...
        'you will be asked to chose which picture you prefer.\n\n\n'...
        'Please respond using the mouse and left mouse button.\n\n\n'...
        'Note that the trial will not start until you look at the fixation cross.'];
        pressKeyContinue
    elseif isequal(expPhase, "continuous")
        instructions = ['In this part of the experiment,\n\n\n'...
        'you will be asked to rate by how much you prefer each picture.\n\n\n'...
        'Please respond using the mouse amd left mouse button.\n\n\n'...
        'Note that the trial will not start until you look at the fixation cross.'];
        pressKeyContinue
    elseif isequal(expPhase, "break")
        instructions = "Please take a break, if you'd like.\n\n\n";
        pressKeyContinue
    elseif isequal(expPhase, "goodbye")
        instructions = 'Thank you for taking part in the experiment.';
    end
    
    displaySingleInstruction(instructions);
end

function displaySingleInstruction(text)
    global par
    
    DrawFormattedText(par.window, text, 'center', 'center')
    Screen('Flip', par.window)
    WaitSecs(0.5);
    KbStrokeWait;
end

function pressKeyContinue
    global par

    pressSpaceText = 'Please press any key to continue';
    DrawFormattedText(par.window, pressSpaceText, ...
        'center', 0.8*par.winRect(4));
end