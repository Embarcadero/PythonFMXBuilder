object MenuActionsContainer: TMenuActionsContainer
  OnCreate = DataModuleCreate
  Height = 480
  Width = 640
  object actlMenu: TActionList
    Images = ImageContainer.images
    OnUpdate = actlMenuUpdate
    Left = 304
    Top = 224
    object actUpdateEnvironment: TAction
      Tag = 1
      Category = 'Entities'
      Text = 'Update Environment'
      ImageIndex = 0
      OnExecute = actUpdateEnvironmentExecute
      ImageIndex = 0
    end
    object actUpdateCurrentProject: TAction
      Tag = 2
      Category = 'Entities'
      Text = 'Update Project'
      ImageIndex = 9
      OnExecute = actUpdateCurrentProjectExecute
      ImageIndex = 9
    end
    object actBuildCurrentProject: TAction
      Tag = 2
      Category = 'Build'
      Text = 'Build Project'
      ImageIndex = 8
      OnExecute = actBuildCurrentProjectExecute
      ImageIndex = 8
    end
    object actDeployCurrentProject: TAction
      Tag = 2
      Category = 'Build'
      Text = 'Deploy Project'
      ImageIndex = 5
      OnExecute = actDeployCurrentProjectExecute
      ImageIndex = 5
    end
    object actNewProject: TAction
      Tag = 1
      Category = 'Project'
      Text = 'New Project'
      Hint = 'New Project'
      ImageIndex = 1
      OnExecute = actNewProjectExecute
      ImageIndex = 1
    end
    object actOpenProject: TAction
      Tag = 1
      Category = 'Project'
      Text = 'Open Project'
      Hint = 'Open Project'
      ImageIndex = 10
      OnExecute = actOpenProjectExecute
      ImageIndex = 10
    end
    object actRemoveCurrentProject: TAction
      Tag = 1
      Category = 'Project'
      Text = 'Remove Project'
      Hint = 'Remove Project'
      ImageIndex = 4
      OnExecute = actRemoveCurrentProjectExecute
      ImageIndex = 4
    end
    object actRunCurrentProject: TAction
      Tag = 2
      Category = 'Build'
      Text = 'Run Project'
      ImageIndex = 2
      OnExecute = actRunCurrentProjectExecute
      ImageIndex = 2
    end
    object actBuildCurrentProjectAsync: TAction
      Tag = 2
      Category = 'BuildAsync'
      Text = 'Build Project'
      ImageIndex = 8
      OnExecute = actBuildCurrentProjectAsyncExecute
      ImageIndex = 8
    end
    object actDeployCurrentProjectAsync: TAction
      Tag = 2
      Category = 'BuildAsync'
      Text = 'Deploy Project'
      ImageIndex = 5
      OnExecute = actDeployCurrentProjectAsyncExecute
      ImageIndex = 5
    end
    object actRunCurrentProjectAsync: TAction
      Tag = 2
      Category = 'BuildAsync'
      Text = 'Run Project'
      ImageIndex = 2
      OnExecute = actRunCurrentProjectAsyncExecute
      ImageIndex = 2
    end
    object actDebugCurrentProjectAsync: TAction
      Tag = 2
      Category = 'Debug'
      Text = 'Debug Project'
      ImageIndex = 16
      OnExecute = actDebugCurrentProjectAsyncExecute
      ImageIndex = 16
    end
    object actStepInto: TAction
      Tag = 3
      Category = 'Debug'
      Text = 'Step In'
      ImageIndex = 17
      OnExecute = actStepIntoExecute
      ImageIndex = 17
    end
    object actStepOver: TAction
      Tag = 3
      Category = 'Debug'
      Text = 'Step Over'
      ImageIndex = 19
      OnExecute = actStepOverExecute
      ImageIndex = 19
    end
    object actStepOut: TAction
      Tag = 3
      Category = 'Debug'
      Text = 'Step Out'
      ImageIndex = 18
      OnExecute = actStepOutExecute
      ImageIndex = 18
    end
    object actPause: TAction
      Tag = 4
      Category = 'Debug'
      Text = 'Pause'
      Hint = 'Pause'
      ImageIndex = 20
      OnExecute = actPauseExecute
      ImageIndex = 20
    end
    object actStop: TAction
      Tag = 4
      Category = 'Debug'
      Text = 'Stop'
      Hint = 'Stop'
      ImageIndex = 21
      OnExecute = actStopExecute
      ImageIndex = 21
    end
  end
end
