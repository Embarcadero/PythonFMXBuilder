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
      Category = 'Entities'
      Text = 'Update Environment'
      ImageIndex = 0
      OnExecute = actUpdateEnvironmentExecute
      ImageIndex = 0
    end
    object actUpdateCurrentProject: TAction
      Category = 'Entities'
      Text = 'Update Project'
      ImageIndex = 9
      OnExecute = actUpdateCurrentProjectExecute
      ImageIndex = 9
    end
    object actBuildCurrentProject: TAction
      Category = 'Build'
      Text = 'Build Project'
      ImageIndex = 8
      OnExecute = actBuildCurrentProjectExecute
      ImageIndex = 8
    end
    object actDeployCurrentProject: TAction
      Category = 'Build'
      Text = 'Deploy Project'
      ImageIndex = 5
      OnExecute = actDeployCurrentProjectExecute
      ImageIndex = 5
    end
    object actNewProject: TAction
      Category = 'Project'
      Text = 'New Project'
      Hint = 'New Project'
      ImageIndex = 1
      OnExecute = actNewProjectExecute
      ImageIndex = 1
    end
    object actOpenProject: TAction
      Category = 'Project'
      Text = 'Open Project'
      Hint = 'Open Project'
      ImageIndex = 10
      OnExecute = actOpenProjectExecute
      ImageIndex = 10
    end
    object actRemoveCurrentProject: TAction
      Category = 'Project'
      Text = 'Remove Project'
      Hint = 'Remove Project'
      ImageIndex = 4
      OnExecute = actRemoveCurrentProjectExecute
      ImageIndex = 4
    end
    object actRunCurrentProject: TAction
      Category = 'Build'
      Text = 'Run Project'
      ImageIndex = 2
      OnExecute = actRunCurrentProjectExecute
      ImageIndex = 2
    end
    object actBuildCurrentProjectAsync: TAction
      Category = 'BuildAsync'
      Text = 'Build Project'
      ImageIndex = 8
      OnExecute = actBuildCurrentProjectAsyncExecute
      ImageIndex = 8
    end
    object actDeployCurrentProjectAsync: TAction
      Category = 'BuildAsync'
      Text = 'Deploy Project'
      ImageIndex = 5
      OnExecute = actDeployCurrentProjectAsyncExecute
      ImageIndex = 5
    end
    object actRunCurrentProjectAsync: TAction
      Category = 'BuildAsync'
      Text = 'Run Project'
      ImageIndex = 2
      OnExecute = actRunCurrentProjectAsyncExecute
      ImageIndex = 2
    end
  end
end
