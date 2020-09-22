object FormSetParam: TFormSetParam
  Left = 843
  Top = 218
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Set Parameters ACO'
  ClientHeight = 266
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 281
    Height = 233
    TabOrder = 0
    object Label1: TLabel
      Left = 28
      Top = 16
      Width = 125
      Height = 19
      Caption = 'Number of iteration :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 52
      Top = 50
      Width = 101
      Height = 19
      Caption = 'Number of ants :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 118
      Top = 82
      Width = 35
      Height = 19
      Caption = 'Beta :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 14
      Top = 110
      Width = 139
      Height = 19
      Caption = 'Evaporation Rate (%) :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 103
      Top = 145
      Width = 50
      Height = 19
      Caption = 'q0 (%) :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object EditIteration: TEdit
      Left = 163
      Top = 16
      Width = 62
      Height = 21
      TabOrder = 0
      Text = '1000'
      OnKeyPress = EditIterationKeyPress
    end
    object EditNumberOfAnts: TEdit
      Left = 163
      Top = 49
      Width = 46
      Height = 21
      TabOrder = 1
      Text = '20'
      OnKeyPress = EditNumberOfAntsKeyPress
    end
    object EditBeta: TEdit
      Left = 163
      Top = 83
      Width = 30
      Height = 21
      TabOrder = 2
      Text = '2'
      OnKeyPress = EditBetaKeyPress
    end
    object EditEvaporationRate: TEdit
      Left = 163
      Top = 111
      Width = 38
      Height = 21
      TabOrder = 3
      Text = '10'
      OnExit = EditEvaporationRateExit
      OnKeyPress = EditEvaporationRateKeyPress
    end
    object EditQ0: TEdit
      Left = 163
      Top = 145
      Width = 30
      Height = 21
      TabOrder = 4
      Text = '90'
      OnExit = EditQ0Exit
      OnKeyPress = EditQ0KeyPress
    end
    object ButtonSaveParam: TButton
      Left = 144
      Top = 189
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 5
      OnClick = ButtonSaveParamClick
    end
  end
end
