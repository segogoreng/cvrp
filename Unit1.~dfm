object FormCVRP: TFormCVRP
  Left = 190
  Top = 110
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Capacitated Vehicle Routing Problem'
  ClientHeight = 628
  ClientWidth = 1080
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 24
    Top = 16
    Width = 313
    Height = 433
    TabOrder = 0
    object Label1: TLabel
      Left = 48
      Top = 344
      Width = 143
      Height = 19
      Caption = 'Number of customers'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 48
      Top = 219
      Width = 129
      Height = 19
      Caption = 'Number of vehicles'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 48
      Top = 387
      Width = 131
      Height = 19
      Caption = 'Maximum Capacity'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 48
      Top = 293
      Width = 100
      Height = 15
      Caption = 'Searching Solution ...'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object EditNumberOfCustomer: TEdit
      Left = 216
      Top = 344
      Width = 49
      Height = 21
      Enabled = False
      ReadOnly = True
      TabOrder = 0
    end
    object EditNumberOfVehicles: TEdit
      Left = 216
      Top = 219
      Width = 49
      Height = 21
      TabOrder = 2
      Text = '10'
      OnKeyPress = EditNumberOfVehiclesKeyPress
    end
    object EditMaxCapacity: TEdit
      Left = 216
      Top = 385
      Width = 49
      Height = 21
      Enabled = False
      ReadOnly = True
      TabOrder = 4
    end
    object ButtonLoadFile: TButton
      Left = 128
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Load File'
      TabOrder = 1
      OnClick = ButtonLoadFileClick
    end
    object RadioGroupMethods: TRadioGroup
      Left = 64
      Top = 80
      Width = 201
      Height = 121
      Caption = 'Solving Method'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
    end
    object RadioButtonBF: TRadioButton
      Left = 88
      Top = 112
      Width = 113
      Height = 17
      Caption = 'Brute-force Search'
      Checked = True
      TabOrder = 6
      TabStop = True
    end
    object RadioButtonNNI: TRadioButton
      Left = 88
      Top = 136
      Width = 161
      Height = 17
      Caption = 'Nearest Neighbour Insertion'
      TabOrder = 7
    end
    object RadioButtonACO: TRadioButton
      Left = 88
      Top = 160
      Width = 145
      Height = 17
      Caption = 'Ant Colony Optimization'
      TabOrder = 8
    end
    object ButtonStart: TButton
      Left = 48
      Top = 256
      Width = 57
      Height = 25
      Caption = 'Start'
      Enabled = False
      TabOrder = 3
      OnClick = ButtonStartClick
    end
    object ProgressBar1: TProgressBar
      Left = 48
      Top = 309
      Width = 217
      Height = 17
      TabOrder = 9
      Visible = False
    end
    object ButtonPause: TButton
      Left = 128
      Top = 256
      Width = 57
      Height = 25
      Caption = 'Pause'
      Enabled = False
      TabOrder = 10
      OnClick = ButtonPauseClick
    end
    object ButtonStop: TButton
      Left = 208
      Top = 256
      Width = 57
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 11
      OnClick = ButtonStopClick
    end
    object ButtonSetParam: TButton
      Left = 223
      Top = 160
      Width = 26
      Height = 17
      Caption = '...'
      TabOrder = 12
      OnClick = ButtonSetParamClick
    end
  end
  object Panel2: TPanel
    Left = 352
    Top = 16
    Width = 329
    Height = 729
    TabOrder = 1
    object Label4: TLabel
      Left = 17
      Top = 10
      Width = 168
      Height = 15
      Caption = 'The Problem (TSPLIB Format)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object MemoProblem: TMemo
      Left = 16
      Top = 32
      Width = 297
      Height = 681
      ReadOnly = True
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 24
    Top = 464
    Width = 313
    Height = 273
    TabOrder = 2
    object Label5: TLabel
      Left = 16
      Top = 8
      Width = 168
      Height = 15
      Caption = 'The Solution (TSPLIB Format)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object MemoSolution: TMemo
      Left = 16
      Top = 32
      Width = 273
      Height = 185
      ReadOnly = True
      TabOrder = 0
    end
    object ButtonSave: TButton
      Left = 16
      Top = 232
      Width = 75
      Height = 25
      Caption = 'Save Solution'
      TabOrder = 1
      OnClick = ButtonSaveClick
    end
    object ButtonLoadSolution: TButton
      Left = 112
      Top = 232
      Width = 75
      Height = 25
      Caption = 'Load Solution'
      TabOrder = 2
      OnClick = ButtonLoadSolutionClick
    end
    object ButtonSaveLog: TButton
      Left = 212
      Top = 232
      Width = 75
      Height = 25
      Caption = 'Save Log'
      TabOrder = 3
      OnClick = ButtonSaveLogClick
    end
    object MemoLog: TMemo
      Left = 212
      Top = 4
      Width = 77
      Height = 25
      TabOrder = 4
      Visible = False
    end
  end
  object Panel4: TPanel
    Left = 696
    Top = 16
    Width = 561
    Height = 729
    TabOrder = 3
    object Image1: TImage
      Left = 15
      Top = 33
      Width = 532
      Height = 645
    end
    object Label6: TLabel
      Left = 23
      Top = 10
      Width = 122
      Height = 15
      Caption = 'Visual Representation'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label8: TLabel
      Left = 24
      Top = 696
      Width = 36
      Height = 15
      Caption = 'Show :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object RadioButtonIndex: TRadioButton
      Left = 80
      Top = 696
      Width = 113
      Height = 17
      Caption = 'Index'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonIndexClick
    end
    object RadioButtonDemand: TRadioButton
      Left = 176
      Top = 696
      Width = 113
      Height = 17
      Caption = 'Demand'
      TabOrder = 1
      OnClick = RadioButtonDemandClick
    end
    object ButtonZoomOut: TButton
      Left = 362
      Top = 690
      Width = 75
      Height = 25
      Caption = 'Zoom Out'
      TabOrder = 2
      OnClick = ButtonZoomOutClick
    end
    object ButtonZoomIn: TButton
      Left = 466
      Top = 690
      Width = 75
      Height = 25
      Caption = 'Zoom In'
      Enabled = False
      TabOrder = 3
      OnClick = ButtonZoomInClick
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 88
    Top = 40
  end
  object SaveDialog1: TSaveDialog
    Left = 264
    Top = 40
  end
end
