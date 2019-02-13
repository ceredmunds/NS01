function displayInstructions(expPhase, reminder)
    global par
    
    if isequal(expPhase, "welcome")
        instructions = [
        'Thank you for taking part in the experiment\n\n\n'...
        'Please wait until the experimenter signals the start of the experiment\n\n\n'...
        'Do not press any keys in the meantime\n\n\n'];
    elseif  isequal(expPhase, "likert") && reminder==false
        instructions = ['In this part of the experiment,\n\n\n'...
        'you will be asked to rate how much you like a series of pictures.\n\n\n'...
        'Please respond using the mouse and left mouse button.\n\n\n'
        'The more you like the picture, the higher the marker should be on the screen.'];
        pressKeyContinue
    elseif  isequal(expPhase, "likert") && reminder==true
        instructions = ['LOADING EXPERIMENT\n\n\n\n\n'...
        'REMEMBER: you are about to judge how much you like a series of pictures, using the mouse to respond.\n\n\n'...
        'The more you like the picture, the higher the marker should be on the screen.'];
        pressKeyContinue
    elseif isequal(expPhase, "binary") && reminder==false
        instructions = ['In this part of the experiment,\n\n\n'...
        'you will be asked to chose which picture you prefer.\n\n\n'...
        'Please respond by moving the red marker under the picture you prefer and press left mouse button.\n\n\n'...
        'Note that the trial will not start until you look at the fixation cross.'];
        pressKeyContinue
    elseif isequal(expPhase, "binary") && reminder==true
        instructions = ['LOADING EXPERIMENT\n\n\n\n\n'...
        'you will be asked to chose which picture you prefer.\n\n\n'...
        'Please respond by moving the red marker under the picture you prefer and press left mouse button.\n\n\n'...
        "Don't forget to look at the fixation cross every trial."];
        pressKeyContinue
    elseif isequal(expPhase, "continuous") && reminder==false
        instructions = ['In this part of the experiment,\n\n\n'...
        'you will be asked to rate by how much you prefer each picture.\n\n\n'...
        'Please respond using the mouse and left mouse button.\n\n\n'...
        'The slider indicates how much you prefer one picture compared to the other.\n\n\n'...
        'Far left=strongly prefer left; slightly right=slightly prefer right.\n\n\n'
        'Note that the trial will not start until you look at the fixation cross.'];
        pressKeyContinue
    elseif isequal(expPhase, "continuous") && reminder==true
        instructions = ['LOADING EXPERIMENT\n\n\n\n\n'...
        'Remember: you will be asked to judge much you prefer each picture using the mouse.\n\n\n'...
        'The slider indicates how much you prefer one picture compared to the other.\n\n\n'...
        'Far left=strongly prefer left; slightly right=slightly prefer right.\n\n\n'
        "Don't forget to look at the fixation cross every trial."];
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
        'center', 0.8*par.windowRect(4));
end