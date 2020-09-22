program CVRP;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FormCVRP},
  UnitBFS in 'UnitBFS.pas',
  OtherClasses in 'OtherClasses.pas',
  UnitNNI in 'UnitNNI.pas',
  UnitACO in 'UnitACO.pas',
  UnitGraph in 'UnitGraph.pas',
  Unit2 in 'Unit2.pas' {FormSetParam};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCVRP, FormCVRP);
  Application.CreateForm(TFormSetParam, FormSetParam);
  Application.Run;
end.
