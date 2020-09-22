unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormSetParam = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EditIteration: TEdit;
    EditNumberOfAnts: TEdit;
    EditBeta: TEdit;
    EditEvaporationRate: TEdit;
    EditQ0: TEdit;
    ButtonSaveParam: TButton;
    procedure ButtonSaveParamClick(Sender: TObject);
    procedure EditIterationKeyPress(Sender: TObject; var Key: Char);
    procedure EditNumberOfAntsKeyPress(Sender: TObject; var Key: Char);
    procedure EditBetaKeyPress(Sender: TObject; var Key: Char);
    procedure EditEvaporationRateKeyPress(Sender: TObject; var Key: Char);
    procedure EditQ0KeyPress(Sender: TObject; var Key: Char);
    procedure EditEvaporationRateExit(Sender: TObject);
    procedure EditQ0Exit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSetParam: TFormSetParam;

implementation

{$R *.dfm}

// This procedure saves the parameters and close the form
procedure TFormSetParam.ButtonSaveParamClick(Sender: TObject);
begin
  FormSetParam.Close;
end;


// -------------------------Number only inputs------------------------------- //

procedure TFormSetParam.EditIterationKeyPress(Sender: TObject;
  var Key: Char);
begin
  // #8 is Backspace
  if not (Key in [#8, '0'..'9']) then Key := #0;
end;

procedure TFormSetParam.EditNumberOfAntsKeyPress(Sender: TObject;
  var Key: Char);
begin
  // #8 is Backspace
  if not (Key in [#8, '0'..'9']) then Key := #0;
end;

procedure TFormSetParam.EditBetaKeyPress(Sender: TObject; var Key: Char);
begin
  // #8 is Backspace
  if not (Key in [#8, '0'..'9']) then Key := #0;
end;

procedure TFormSetParam.EditEvaporationRateKeyPress(Sender: TObject;
  var Key: Char);
begin
  // #8 is Backspace
  if not (Key in [#8, '0'..'9']) then Key := #0;
end;

procedure TFormSetParam.EditQ0KeyPress(Sender: TObject; var Key: Char);
begin
  // #8 is Backspace
  if not (Key in [#8, '0'..'9']) then Key := #0;
end;

// -------------------------------------------------------------------------- //


// This procedure makes sure the input is under 100%
procedure TFormSetParam.EditEvaporationRateExit(Sender: TObject);
var
  temp : integer;
begin
  temp := StrToInt(EditEvaporationRate.Text);
  if (temp > 100) then
  begin
    temp := 100;
    EditEvaporationRate.Text := IntToStr(temp);
  end;
end;

// This procedure makes sure the input is under 100%
procedure TFormSetParam.EditQ0Exit(Sender: TObject);
var
  temp : integer;
begin
  temp := StrToInt(EditQ0.Text);
  if (temp > 100) then
  begin
    temp := 100;
    EditQ0.Text := IntToStr(temp);
  end;
end;

end.
