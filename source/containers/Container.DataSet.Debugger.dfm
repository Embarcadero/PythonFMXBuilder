object DebuggerDataSetContainer: TDebuggerDataSetContainer
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 414
  Width = 601
  object fdmtThread: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 32
    Top = 64
    object fdmtThreadthread_id: TIntegerField
      Alignment = taLeftJustify
      DisplayLabel = 'Id'
      FieldName = 'thread_id'
    end
    object fdmtThreadthread_name: TStringField
      DisplayLabel = 'Name'
      FieldName = 'thread_name'
      Size = 100
    end
  end
  object fdmtEvent: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 32
    Top = 8
    object fdmtEventevent_desc: TStringField
      DisplayLabel = 'Description'
      FieldName = 'event_desc'
      Size = 500
    end
  end
  object fdmtSource: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 32
    Top = 120
    object fdmtSourcesource_name: TStringField
      FieldName = 'source_name'
      Size = 255
    end
    object fdmtSourcesource_local_path: TStringField
      FieldName = 'source_local_path'
      Size = 500
    end
    object fdmtSourcesource_remote_path: TStringField
      FieldName = 'source_remote_path'
      Size = 500
    end
  end
  object fdmtBreakpoint: TFDMemTable
    AfterPost = fdmtBreakpointAfterPost
    AfterDelete = fdmtBreakpointAfterDelete
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 32
    Top = 176
    object fdmtBreakpointbreakpoint_id: TIntegerField
      DisplayLabel = 'Id'
      FieldName = 'breakpoint_id'
      Visible = False
    end
    object fdmtBreakpointbreakpoint_source_name: TStringField
      DisplayLabel = 'Source'
      DisplayWidth = 30
      FieldName = 'breakpoint_source_name'
      Size = 255
    end
    object fdmtBreakpointbreakpoint_line: TIntegerField
      Alignment = taLeftJustify
      DisplayLabel = 'Line'
      FieldName = 'breakpoint_line'
    end
    object fdmtBreakpointbreakpoint_confirmed: TBooleanField
      FieldName = 'breakpoint_confirmed'
      Visible = False
    end
  end
  object fdmtStackTrace: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 152
    Top = 128
    object fdmtStackTracestacktrace_id: TIntegerField
      FieldName = 'stacktrace_id'
      Visible = False
    end
    object fdmtStackTracestacktrace_thread_id: TIntegerField
      FieldName = 'stacktrace_thread_id'
      Visible = False
    end
    object fdmtStackTracestacktrace_name: TStringField
      FieldName = 'stacktrace_name'
      Visible = False
      Size = 255
    end
    object fdmtStackTracestacktrace_line: TIntegerField
      FieldName = 'stacktrace_line'
      Visible = False
    end
    object fdmtStackTracestacktrace_line_content: TStringField
      FieldName = 'stacktrace_line_content'
      Visible = False
      Size = 500
    end
    object fdmtStackTracestacktrace_source_local_path: TStringField
      FieldName = 'stacktrace_source_local_path'
      Visible = False
      Size = 500
    end
    object fdmtStackTracestacktrace_description: TStringField
      DisplayLabel = 'Description'
      FieldName = 'stacktrace_description'
      Size = 500
    end
  end
  object fdmtVariable: TFDMemTable
    IndexFieldNames = 'variable_stacktrace_id;variable_reference'
    MasterSource = dsScope
    MasterFields = 'scope_stacktrace_id;scope_variables_reference'
    DetailFields = 'variable_stacktrace_id;variable_reference'
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 352
    Top = 128
    object fdmtVariablevariable_stacktrace_id: TIntegerField
      FieldName = 'variable_stacktrace_id'
      Visible = False
    end
    object fdmtVariablevariable_reference: TIntegerField
      FieldName = 'variable_reference'
      Visible = False
    end
    object fdmtVariablevariable_name: TStringField
      DisplayLabel = 'Name'
      DisplayWidth = 8
      FieldName = 'variable_name'
      Size = 50
    end
    object fdmtVariablevariable_evaluate_name: TStringField
      FieldName = 'variable_evaluate_name'
      Visible = False
      Size = 50
    end
    object fdmtVariablevariable_type: TStringField
      DisplayLabel = 'Type'
      DisplayWidth = 12
      FieldName = 'variable_type'
      Size = 120
    end
    object fdmtVariablevariable_value: TStringField
      DisplayLabel = 'Value'
      DisplayWidth = 100
      FieldName = 'variable_value'
      Size = 500
    end
  end
  object fdmtScope: TFDMemTable
    IndexFieldNames = 'scope_stacktrace_id'
    MasterSource = dsStackTrace
    MasterFields = 'stacktrace_id'
    DetailFields = 'scope_stacktrace_id'
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 256
    Top = 128
    object fdmtScopescope_stacktrace_id: TIntegerField
      FieldName = 'scope_stacktrace_id'
    end
    object fdmtScopescope_name: TStringField
      FieldName = 'scope_name'
      Size = 255
    end
    object fdmtScopescope_variables_reference: TIntegerField
      FieldName = 'scope_variables_reference'
    end
  end
  object dsStackTrace: TDataSource
    DataSet = fdmtStackTrace
    Left = 152
    Top = 184
  end
  object dsScope: TDataSource
    DataSet = fdmtScope
    Left = 256
    Top = 184
  end
  object fdmtActiveSource: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 32
    Top = 232
    object fdmtActiveSourceactive_source_thread_id: TIntegerField
      FieldName = 'active_source_thread_id'
    end
    object fdmtActiveSourceactive_source_local_file_path: TStringField
      FieldName = 'active_source_local_file_path'
      Size = 500
    end
    object fdmtActiveSourceactive_source_line: TIntegerField
      FieldName = 'active_source_line'
    end
    object fdmtActiveSourceactive_source_line_indicator: TBooleanField
      FieldName = 'active_source_line_indicator'
    end
  end
end
