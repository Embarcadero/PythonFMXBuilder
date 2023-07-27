object MenuActionsContainer: TMenuActionsContainer
  Height = 305
  Width = 656
  PixelsPerInch = 144
  object actlMenu: TActionList
    Images = ImageContainer.images
    OnUpdate = actlMenuUpdate
    Left = 78
    Top = 12
    object actUpdateEnvironment: TAction
      Tag = 1
      Category = 'Entities'
      Text = 'Update Environment'
      Hint = 'Update Environment'
      ImageIndex = 0
      OnExecute = actUpdateEnvironmentExecute
      ImageIndex = 0
    end
    object actUpdateCurrentProject: TAction
      Tag = 2
      Category = 'Entities'
      Text = 'Update Project'
      Hint = 'Update Project'
      ImageIndex = 1
      OnExecute = actUpdateCurrentProjectExecute
      ImageIndex = 1
    end
    object actBuildCurrentProject: TAction
      Tag = 2
      Category = 'Build'
      Text = 'Build Project'
      Hint = 'Build Project'
      ImageIndex = 11
      OnExecute = actBuildCurrentProjectExecute
      ImageIndex = 11
    end
    object actDeployCurrentProject: TAction
      Tag = 2
      Category = 'Build'
      Text = 'Deploy Project'
      Hint = 'Deploy Project'
      ImageIndex = 12
      OnExecute = actDeployCurrentProjectExecute
      ImageIndex = 12
    end
    object actOpenProject: TAction
      Tag = 1
      Category = 'Project'
      Text = 'Open Project'
      Hint = 'Open Project'
      ImageIndex = 9
      OnExecute = actOpenProjectExecute
      ImageIndex = 9
    end
    object actCloseCurrentProject: TAction
      Tag = 9
      Category = 'Project'
      Text = 'Close Project'
      Hint = 'Close Project'
      ImageIndex = 10
      OnExecute = actCloseCurrentProjectExecute
      ImageIndex = 10
    end
    object actRunCurrentProject: TAction
      Tag = 2
      Category = 'Build'
      Text = 'Run Project'
      Hint = 'Run Project'
      ImageIndex = 13
      OnExecute = actRunCurrentProjectExecute
      ImageIndex = 13
    end
    object actBuildCurrentProjectAsync: TAction
      Tag = 2
      Category = 'BuildAsync'
      Text = 'Build Project'
      Hint = 'Build Project'
      ImageIndex = 11
      OnExecute = actBuildCurrentProjectAsyncExecute
      ImageIndex = 11
    end
    object actDeployCurrentProjectAsync: TAction
      Tag = 2
      Category = 'BuildAsync'
      Text = 'Deploy Project'
      Hint = 'Deploy Project'
      ImageIndex = 12
      OnExecute = actDeployCurrentProjectAsyncExecute
      ImageIndex = 12
    end
    object actRunCurrentProjectAsync: TAction
      Tag = 2
      Category = 'BuildAsync'
      Text = 'Run Project'
      Hint = 'Run Project'
      ImageIndex = 13
      OnExecute = actRunCurrentProjectAsyncExecute
      ImageIndex = 13
    end
    object actDebugCurrentProjectAsync: TAction
      Tag = 9
      Category = 'Debug'
      Text = 'Debug Project'
      Hint = 'Debug Project'
      ImageIndex = 14
      OnExecute = actDebugCurrentProjectAsyncExecute
      ImageIndex = 14
    end
    object actStepIn: TAction
      Tag = 3
      Category = 'Debug'
      Text = 'Step In'
      Hint = 'Step In'
      ImageIndex = 15
      OnExecute = actStepInExecute
      ImageIndex = 15
    end
    object actStepOver: TAction
      Tag = 4
      Category = 'Debug'
      Text = 'Step Over'
      Hint = 'Step Over'
      ImageIndex = 17
      OnExecute = actStepOverExecute
      ImageIndex = 17
    end
    object actStepOut: TAction
      Tag = 5
      Category = 'Debug'
      Text = 'Step Out'
      Hint = 'Step Out'
      ImageIndex = 16
      OnExecute = actStepOutExecute
      ImageIndex = 16
    end
    object actPause: TAction
      Tag = 6
      Category = 'Debug'
      Text = 'Pause'
      Hint = 'Pause'
      ImageIndex = 18
      OnExecute = actPauseExecute
      ImageIndex = 18
    end
    object actStop: TAction
      Tag = 7
      Category = 'Debug'
      Text = 'Stop'
      Hint = 'Stop'
      ImageIndex = 19
      OnExecute = actStopExecute
      ImageIndex = 19
    end
    object actContinue: TAction
      Tag = 10
      Category = 'Debug'
      Text = 'Continue'
      Hint = 'Continue'
      ImageIndex = 20
      OnExecute = actContinueExecute
      ImageIndex = 20
    end
    object actSaveState: TAction
      Category = 'Save'
      Text = 'Save'
      Hint = 'Save'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = actSaveStateExecute
      ImageIndex = 2
    end
    object actSaveAllState: TAction
      Category = 'Save'
      Text = 'Save All'
      Hint = 'Save All'
      ImageIndex = 22
      ShortCut = 49235
      OnExecute = actSaveAllStateExecute
      ImageIndex = 22
    end
    object actFileExit: TFileExit
      Category = 'File'
      Hint = 'Quit|Quits the application'
    end
    object actFileHideApp: TFileHideApp
      Category = 'File'
      Hint = 'Hide|Hide current application'
    end
    object actFileHideAppOthers: TFileHideAppOthers
      Category = 'File'
      Hint = 'Hide Others|Hide all other applications'
    end
    object actNewBlankProject: TAction
      Tag = 1
      Category = 'Project'
      Text = 'New Blank Project'
      Hint = 
        'The blank project template is a jumping off point for creating a' +
        ' custom project.'
      ImageIndex = 32
      OnExecute = actNewBlankProjectExecute
      ImageIndex = 32
    end
    object actNewProject: TAction
      Tag = 1
      Category = 'Project'
      Text = 'New Project'
      Hint = 
        'The new project template will create the main module alongside a' +
        ' custom project.'
      ImageIndex = 35
      OnExecute = actNewProjectExecute
      ImageIndex = 35
    end
  end
  object sdProject: TSaveDialog
    DefaultExt = '.pyfmxproj'
    Filter = 'Python FMX Project|*.pyfmxproj'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 78
    Top = 114
  end
  object sdModule: TSaveDialog
    DefaultExt = '.py'
    Filter = 'Python Module|*.py'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 186
    Top = 114
  end
  object odProject: TOpenDialog
    DefaultExt = '.pyfmxproj'
    Filter = 'Python FMX Project|*.pyfmxproj'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 78
    Top = 198
  end
  object odModule: TOpenDialog
    DefaultExt = '.py'
    Filter = 'Python Module|*.py'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 186
    Top = 198
  end
  object odPackage: TOpenDialog
    DefaultExt = '.whl'
    Filter = 'Zip imports|*.zip|PIP wheel|*.whl'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 438
    Top = 198
  end
  object odOther: TOpenDialog
    Filter = '*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 540
    Top = 198
  end
  object odFMXModule: TOpenDialog
    DefaultExt = '.py'
    Filter = 'Python Module|*.py|Delphi FMX File|*.pyfmx'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 312
    Top = 198
  end
end
