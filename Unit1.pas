unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Unit2, UnitGraph, UnitBFS, UnitNNI,
  UnitACO, OtherClasses;

type
  TFormCVRP = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditNumberOfCustomer: TEdit;
    EditNumberOfVehicles: TEdit;
    EditMaxCapacity: TEdit;
    ButtonLoadFile: TButton;
    RadioGroupMethods: TRadioGroup;
    RadioButtonBF: TRadioButton;
    RadioButtonNNI: TRadioButton;
    RadioButtonACO: TRadioButton;
    ButtonStart: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    MemoSolution: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MemoProblem: TMemo;
    Label4: TLabel;
    Label5: TLabel;
    Panel4: TPanel;
    Image1: TImage;
    Label6: TLabel;
    ProgressBar1: TProgressBar;
    ButtonPause: TButton;
    ButtonStop: TButton;
    Label7: TLabel;
    Label8: TLabel;
    RadioButtonIndex: TRadioButton;
    RadioButtonDemand: TRadioButton;
    ButtonSave: TButton;
    ButtonLoadSolution: TButton;
    ButtonZoomOut: TButton;
    ButtonZoomIn: TButton;
    ButtonSetParam: TButton;
    ButtonSaveLog: TButton;
    MemoLog: TMemo;
    procedure initializeVariables;
    procedure initializeVehicles;
    procedure reinitializeCustomers;
    procedure loadNumberOfCustomers;
    procedure loadMaxCapacity;
    procedure loadCoordinatesAndDemands;
    procedure loadRoutes;
    procedure countDistance;
    function countCostOfTheSolution : integer;
    procedure displaySolution;
    procedure checkFormat;
    function allCustomersVisited : boolean;
    procedure BFSDone(Sender : TObject);
    procedure ACODone(Sender : TObject);
    procedure disableComponents;
    procedure enableComponents;
    procedure ButtonLoadFileClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditNumberOfVehiclesKeyPress(Sender: TObject; var Key: Char);
    procedure RadioButtonIndexClick(Sender: TObject);
    procedure RadioButtonDemandClick(Sender: TObject);
    procedure ButtonLoadSolutionClick(Sender: TObject);
    procedure ButtonZoomOutClick(Sender: TObject);
    procedure ButtonZoomInClick(Sender: TObject);
    procedure ButtonSetParamClick(Sender: TObject);
    procedure ButtonSaveLogClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCVRP : TFormCVRP;

  distances   : TDistances; // distance between nodes
  N           : integer;          // number of Customers
  v           : integer;          // number of vehicles
  maxCapacity : integer;          // maximum capacity of the vehicles
  cost        : integer;          // the cost of the solution

  numberOfVehiclesUsed : integer; // number of vehicles used to generate solution
  searchDone : boolean;           // true if solution has been generated
  loadDone   : boolean;           // true if load file has been done
  

//-----------------------------Class Object-------------------------------------

 vehicles  : TVehicles;
 customers : TCustomers;
 Graph     : TGraph;
 BFS       : TBFS;
 NNI       : TNNI;
 ACO       : TACO;

//------------------------------------------------------------------------------


implementation

{$R *.dfm}

//====================PROCEDURES AND FUNCTIONS FOR CVRP=========================

// This procedure initializes number of customers, maximum capacity
// and customer's data
procedure TFormCVRP.initializeVariables;
var
  i : integer;
begin
  N := 0;
  maxCapacity := 0;
  for i:=0 to 299 do
  begin
    customers[i].x       := 0;
    customers[i].y       := 0;
    customers[i].index   := i;
    customers[i].demand  := 0;
    customers[i].visited := false;
  end;
end;

// This procedure initializes vehicles
procedure TFormCVRP.initializeVehicles;
var
  i : integer;
begin
  for i:=1 to 50 do
  begin
    vehicles[i] := TVehicle.create(maxCapacity);
  end;
  numberOfVehiclesUsed := 0;
end;

// This procedure reinitializes customer's visited
procedure TFormCVRP.reinitializeCustomers;
var i : integer;
begin
 for i:=1 to N do
 begin
   customers[i].visited := false;
 end;
end;

// This procedure loads the information about number of Customers
procedure TFormCVRP.loadNumberOfCustomers;
var
  i,lastSpaceIndex : integer;
  currentLine : string;
  stop : boolean;
begin
  stop := false;
  for i:=0 to MemoProblem.Lines.Count-1 do
  begin
    currentLine := Trim(MemoProblem.Lines[i]);
    if CompareText('DIMENSION',Copy(currentLine,1,9)) = 0 then
    begin
      lastSpaceIndex := LastDelimiter(' ',currentLine);
      N := StrToInt(copy(currentLine,lastSpaceIndex+1,5)) - 1;
      stop := true;
    end;
    if stop then break;
  end;
  EditNumberOfCustomer.Text := IntToStr(N);
end;


// This procedure loads the information about maximum capacity of vehicles
// from MemoProblem into maxCapacity variable
procedure TFormCVRP.loadMaxCapacity;
var
  i,lastSpaceIndex : integer;
  currentLine : string;
  stop : boolean;
begin
  stop := false;
  for i:=0 to MemoProblem.Lines.Count-1 do
  begin
    currentLine := Trim(MemoProblem.Lines[i]);
    if CompareText('CAPACITY',Copy(currentLine,1,8)) = 0 then
    begin
      lastSpaceIndex := LastDelimiter(' ',currentLine);
      maxCapacity := StrToInt(copy(currentLine,lastSpaceIndex+1,5));
      stop := true;
    end;
    if stop then break;
  end;
  EditMaxCapacity.Text := IntToStr(maxCapacity);
end;

// This procedure loads the information about coordinates and demands from
// the MemoProblem into x,y for coordinates and q for demands
procedure TFormCVRP.loadCoordinatesAndDemands;
var
  i,j,lastSpaceIndex : integer;
  currentLine : string;
  startObtainXY : boolean;
  startObtainDemands : boolean;
begin
  startObtainXY := false;
  startObtainDemands := false;
  j := 0;
  for i:=0 to MemoProblem.Lines.Count-1 do
  begin
    currentLine := Trim(MemoProblem.Lines[i]);
    if CompareText('DEPOT_SECTION',currentLine) = 0 then
    begin
      startObtainDemands := false;
    end else
    if startObtainDemands then
    begin
      lastSpaceIndex := LastDelimiter(' ',currentLine);
      customers[j].demand := StrToInt(Copy(currentLine,lastSpaceIndex+1,4));
      inc(j);
    end else
    if CompareText('DEMAND_SECTION',currentLine) = 0 then
    begin
      startObtainXY := false;
      startObtainDemands := true;
      j := 0;
    end else
    if startObtainXY then
    begin
      lastSpaceIndex := LastDelimiter(' ',currentLine);
      customers[j].y := StrToInt(Copy(currentLine,lastSpaceIndex+1,4));
      Delete(currentLine,lastSpaceIndex,4);
      lastSpaceIndex := LastDelimiter(' ',currentLine);
      customers[j].x := StrToInt(Copy(currentLine,lastSpaceIndex+1,4));
      Inc(j);
    end else
    if CompareText('NODE_COORD_SECTION',currentLine) = 0 then
    begin
      startObtainXY := true;
      j := 0;
    end;
  end;
end;

// This procedure load routes from memo solution to vehicles data
procedure TFormCVRP.loadRoutes;
var
  i, lastSpaceIndex, ci : integer;
  numberOfCustomers : integer;
  currentLine, temp : string;
  stop, valid : boolean;
begin
  v := MemoSolution.Lines.Count-1;
  if (v < 0) or (v > 49) then v := 0;
  initializeVehicles;
  reinitializeCustomers;
  numberOfVehiclesUsed := v;
  numberOfCustomers := 0;
  valid := true;
  for i:=1 to v do
  begin
    currentLine := Trim(MemoSolution.Lines[i-1]);
    stop := false;
    while (not stop) and (valid) do
    begin
      lastSpaceIndex := LastDelimiter(' ',currentLine);
      temp := Copy(currentLine,lastSpaceIndex+1,4);
      if (Copy(temp,1,1) <> '#') then
      begin
        ci := StrToInt(temp);
        if (ci < 300) then
        begin
          if (customers[ci].demand = 0) then valid := false
          else begin
            vehicles[i].addRoute(customers[ci]);
            inc(numberOfCustomers);
          end;
        end else valid := false;
        Delete(currentLine,lastSpaceIndex,4);
      end else stop := true;
      if (lastSpaceIndex = 0) then stop := true;
    end;
    // Check if the vehicle doesn't exceed the maxCapacity
    if (vehicles[i].getCapacity>maxCapacity) then valid := false;
  end;
  // Check if the solution is valid for the problem loaded
  if (numberOfCustomers <> N) or (not allCustomersVisited) then
    valid := false;
  // If the solution is valid then draw the solution
  if (valid) then
  begin
    // Display the graph and the routes
    searchDone := true;
    Graph.clear;
    Graph.drawLegends;
    Graph.drawRoutes(vehicles,numberOfVehiclesUsed);
    if (RadioButtonIndex.Checked) then Graph.drawIndex;
    if (RadioButtonDemand.Checked) then Graph.drawDemand;
    Graph.drawNodes;
  end else begin
    MemoSolution.Clear;
    initializeVehicles;
    searchDone := false;
    ShowMessage('The solution is not valid for the problem loaded');
  end;
end;

// This procedure counts the distance between each nodes
procedure TFormCVRP.countDistance;
var
  i,j : integer;
  xd,yd : integer;
begin
  for i:=0 to N do
  begin
    for j:=0 to N do
    begin
      xd := customers[i].x - customers[j].x;
      yd := customers[i].y - customers[j].y;
      distances[i][j] := round(sqrt((xd*xd)+(yd*yd)));
    end;
  end;
end;

// This function returns the cost of the solution generated
function TFormCVRP.countCostOfTheSolution : integer;
var
  i,j,k,cost : integer;
begin
  cost := 0;
  for i:=1 to numberOfVehiclesUsed do
  begin
    k := 0;
    for j:=1 to vehicles[i].getTotalCust do
    begin
      cost := cost + distances[k][vehicles[i].getCustomerIndex(j)];
      k := vehicles[i].getCustomerIndex(j);
    end;
    cost := cost + distances[k][0];
  end;
  Result := cost;
end;

// This procedure displays the solution generated to Memo Solution
procedure TFormCVRP.displaySolution;
var
  i,j : integer;
  line : string;
begin
  MemoSolution.Clear;
  for i:=1 to numberOfVehiclesUsed do
  begin
    line := 'Route #' + IntToStr(i) + ':';
    for j:=1 to vehicles[i].getTotalCust do
    begin
      line := line + ' ' + inttostr(vehicles[i].getCustomerIndex(j));
    end;
    MemoSolution.Lines.Add(line);
  end;
  MemoSolution.Lines.Add('cost ' + IntToStr(countCostOfTheSolution));
  // Display the graph and the routes
  Graph.clear;
  Graph.drawLegends;
  Graph.drawRoutes(vehicles,numberOfVehiclesUsed);
  if (RadioButtonIndex.Checked) then Graph.drawIndex;
  if (RadioButtonDemand.Checked) then Graph.drawDemand;
  Graph.drawNodes;
end;

//This procedure checks for the TSPLIB Format of the file that has been loaded
procedure TFormCVRP.checkFormat;
var
  i : integer;
begin
  if (N = 0) or (maxCapacity = 0) then
  begin
    MemoProblem.Clear;
    ShowMessage('The file is not in correct TSPLIB format');
    loadDone := false;
  end;
  i := 1;
  while (loadDone) and (i<=N) do
  begin
    if (customers[i].x=0) or (customers[i].y=0) or (customers[i].demand=0) then
    begin
      MemoProblem.Clear;
      ShowMessage('The file is not in correct TSPLIB format');
      loadDone := false;
    end;
    inc(i);
  end;
end;

// This function returns true if all customers are visited
function TFormCVRP.allCustomersVisited : boolean;
var
  temp : boolean;
  i : integer;
begin
  temp := true;
  for i:=1 to N do
  begin
    if (customers[i].visited = false) then temp := false;
  end;
  Result := temp;
end;

// This procedure is run when BFS searching process is done
procedure TFormCVRP.BFSDone(Sender : TObject);
begin
  numberOfVehiclesUsed := BFS.noOfVehiclesUsed;
  displaySolution;
  enableComponents;
  searchDone := true;
end;

// This procedure is run when ACO searching process is done
procedure TFormCVRP.ACODone(Sender : TObject);
begin
  numberOfVehiclesUsed := ACO.noOfVehiclesUsed;
  displaySolution;
  enableComponents;
  ProgressBar1.Visible := false;
  searchDone := true;
end;

// This procedure disables buttons and other components during the searching
// process
procedure TFormCVRP.disableComponents;
begin
  ButtonLoadFile.Enabled       := false;
  RadioButtonBF.Enabled        := false;
  RadioButtonNNI.Enabled       := false;
  RadioButtonACO.Enabled       := false;
  EditNumberOfVehicles.Enabled := false;
  ButtonSetParam.Enabled       := false;
  ButtonStart.Enabled          := false;
  ButtonSave.Enabled           := false;
  ButtonLoadSolution.Enabled   := false;
  ButtonSaveLog.Enabled        := false;
  Label7.Visible               := true;

  ButtonPause.Enabled := true;
  ButtonStop.Enabled := true;
end;

// This procedure enables buttons and other components after the searching
// process is done or stopped
procedure TFormCVRP.enableComponents;
begin
  ButtonLoadFile.Enabled       := true;
  RadioButtonBF.Enabled        := true;
  RadioButtonNNI.Enabled       := true;
  RadioButtonACO.Enabled       := true;
  EditNumberOfVehicles.Enabled := true;
  ButtonSetParam.Enabled       := true;
  ButtonStart.Enabled          := true;
  ButtonSave.Enabled           := true;
  ButtonLoadSolution.Enabled   := true;
  ButtonSaveLog.Enabled        := true;
  label7.Visible               := false;

  ButtonPause.Enabled := false;
  ButtonStop.Enabled := false;
end;


// =============================================================================


// =============================OBJECT EVENTS===================================

// This procedure creates canvas for visual representation of the problem
procedure TFormCVRP.FormCreate(Sender: TObject);
begin
  searchDone := false;
  loadDone   := false;
  //create canvas
  Image1.Canvas.Create;
  Image1.Canvas.Pen.Color   := clBlack;
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.Rectangle(0,0,Image1.Width,Image1.Height);
  //initialize progressbar
  ProgressBar1.Smooth := true;
end;

// This procedure loads TSPLIB format file for the problem to be solved
// and displays it on the Memo Problem
procedure TFormCVRP.ButtonLoadFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    MemoProblem.Lines.LoadFromFile(OpenDialog1.FileName);
    MemoSolution.Clear;
    searchDone := false;
    loadDone   := true;
    initializeVariables;
    // Parsing text file and loads into variables
    try
      loadNumberOfCustomers;
      loadMaxCapacity;
      loadCoordinatesAndDemands;
    except on EConvertError do
      begin
        MemoProblem.Clear;
        ShowMessage('The file is not in correct TSPLIB format');
        loadDone := false;
      end;
    end;
    // Check TSPLIB Format
    if (loadDone) then checkFormat;
    // Drawing Customers
    Graph := TGraph.create(Image1,customers,N);
    Graph.clear;
    if (loadDone) then
    begin
      Graph.drawLegends;
      if (RadioButtonIndex.Checked) then Graph.drawIndex;
      if (RadioButtonDemand.Checked) then Graph.drawDemand;
      Graph.drawNodes;
      // Count the distance between all nodes
      countDistance;
      // Set focus and enable start button
      EditNumberOfVehicles.SetFocus;
      ButtonStart.Enabled := true;
    end;
  end;
end;

// This procedure opens a form to set the ACO parameters
procedure TFormCVRP.ButtonSetParamClick(Sender: TObject);
begin
  FormSetParam.ShowModal;
end;

// This procedure generates a solution using one of the methods and
// displays it on the Memo Solution
procedure TFormCVRP.ButtonStartClick(Sender: TObject);
var
  iteration,numberOfAnts : integer;
  beta,p,q0 : real;
begin
  searchDone := false;
  // Disabling buttons and other components during the searching process
  disableComponents;
  // Clear the memo solution and clear the routes
  MemoSolution.Clear;
  Graph := TGraph.create(Image1,customers,N);
  Graph.clear;
  Graph.drawLegends;
  if (RadioButtonIndex.Checked) then Graph.drawIndex;
  if (RadioButtonDemand.Checked) then Graph.drawDemand;
  Graph.drawNodes;
  // Vehicles Initialization
  try
    v := StrToInt(EditNumberOfVehicles.Text);
  except on EConvertError do
    begin
      showmessage('Empty field, default value will be used (10)');
      v := 10;
    end;
  end;
  if (v > 50) then
  begin
    v := 50; // max number of vehicles is 50
    showmessage('The maximum number of vehicle is 50');
  end;
  EditNumberOfVehicles.Text := IntToStr(v);
  initializeVehicles;
  reinitializeCustomers;

  // A solution will be generated by one of the methods(BFS, NNI, ACO)
  if RadioButtonBF.Checked then
  begin
    BFS := TBFS.Create(true);
    BFS.FreeOnTerminate := true;
    BFS.OnTerminate := BFSDone;
    BFS.input(vehicles,customers,N,distances,v);
    BFS.Resume;
  end else
  if RadioButtonNNI.Checked then
  begin
    NNI := TNNI.Create(vehicles,customers,N,distances,v);
    NNI.searchSolution(numberOfVehiclesUsed);
    displaySolution;
    enableComponents;
    searchDone := true;
    if (NNI.allCustomersVisited = false) then
    begin
      ShowMessage('NNI failed to find a feasible solution');
    end;
  end else
  if RadioButtonACO.Checked then
  begin
    ProgressBar1.Visible := true;
    ACO := TACO.Create(true);
    ACO.FreeOnTerminate := true;
    ACO.OnTerminate := ACODone;
    ACO.setProgressBar(ProgressBar1);
    ACO.setMemoLog(MemoLog);
    iteration := StrToInt(FormSetParam.EditIteration.Text);
    numberOfAnts := StrToInt(FormSetParam.EditNumberOfAnts.Text);
    beta := StrToFloat(FormSetParam.EditBeta.Text);
    p := StrToFloat(FormSetParam.EditEvaporationRate.Text)/100;
    q0 := StrToFloat(FormSetParam.EditQ0.Text)/100;
    ACO.input(vehicles,customers,N,distances,v,iteration,
              numberOfAnts,beta,p,q0);
    ACO.Resume;
  end;
end;

// This procedure saves the memo solution to a text file
procedure TFormCVRP.ButtonSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    MemoSolution.Lines.SaveToFile(SaveDialog1.FileName);
  end;
end;

// This procedure loads solution from a text file
procedure TFormCVRP.ButtonLoadSolutionClick(Sender: TObject);
begin
  if (loadDone) then
  begin
    if OpenDialog1.Execute then
    begin
      MemoSolution.Lines.LoadFromFile(OpenDialog1.FileName);
      try
        loadRoutes;
      except on EConvertError do
        begin
          MemoSolution.Clear;
          initializeVehicles;
          searchDone := false;
          ShowMessage('The file is not in correct TSPLIB format');
        end;
      end;
    end;
  end else
  begin
    showmessage('You must load the problem first');
  end;
end;

// This procedure saves the log into a text file
procedure TFormCVRP.ButtonSaveLogClick(Sender: TObject);
begin
  if (searchDone) and (RadioButtonACO.Checked) then
  begin
    if SaveDialog1.Execute then
    begin
      MemoLog.Lines.SaveToFile(SaveDialog1.FileName);
    end;
  end;
end;

// This procedure pauses/resumes the searching process
procedure TFormCVRP.ButtonPauseClick(Sender: TObject);
begin
  if (RadioButtonBF.Checked) then
  begin
    if BFS.Suspended then
    begin
      BFS.Resume;
      ButtonPause.Caption := 'Pause';
      ButtonStop.Enabled  := true;
      Label7.Caption := 'Searching Solution ...';
    end else begin
      BFS.Suspend;
      ButtonPause.Caption := 'Resume';
      ButtonStop.Enabled  := false;
      Label7.Caption := 'Paused';
      numberOfVehiclesUsed := BFS.noOfVehiclesUsed;
      displaySolution;
    end;
  end;
  if (RadioButtonACO.Checked) then
  begin
    if ACO.Suspended then
    begin
      ACO.Resume;
      ButtonPause.Caption := 'Pause';
      ButtonStop.Enabled  := true;
      Label7.Caption := 'Searching Solution ...';
    end else begin
      ACO.Suspend;
      ButtonPause.Caption := 'Resume';
      ButtonStop.Enabled  := false;
      Label7.Caption := 'Paused';
      numberOfVehiclesUsed := ACO.noOfVehiclesUsed;
      displaySolution;
    end;
  end;
end;

// This procedure stops the searching process
procedure TFormCVRP.ButtonStopClick(Sender: TObject);
begin
  if (RadioButtonBF.Checked) then BFS.Terminate;
  if (RadioButtonACO.Checked) then ACO.Terminate;
  enableComponents;
  searchDone := true;
end;

// This procedure ensures that number of vehicles input is numerical
procedure TFormCVRP.EditNumberOfVehiclesKeyPress(Sender: TObject;
  var Key: Char);
begin
  // #8 is Backspace
  if not (Key in [#8, '0'..'9']) then Key := #0;
end;

// This procedure shows index when radiobutton index is clicked
procedure TFormCVRP.RadioButtonIndexClick(Sender: TObject);
begin
  if (loadDone) then
  begin
    Graph.clear;
    Graph.drawLegends;
    if (searchDone) then Graph.drawRoutes(vehicles,numberOfVehiclesUsed);
    Graph.drawIndex;
    Graph.drawNodes;
  end;
end;

// This procedure shows demand when radiobutton demand is clicked
procedure TFormCVRP.RadioButtonDemandClick(Sender: TObject);
begin
  if (loadDone) then
  begin
    Graph.clear;
    Graph.drawLegends;
    if (searchDone) then Graph.drawRoutes(vehicles,numberOfVehiclesUsed);
    Graph.drawDemand;
    Graph.drawNodes;
  end;
end;

// This procedure zooms out the graph
procedure TFormCVRP.ButtonZoomOutClick(Sender: TObject);
begin
  if (Graph <> nil) then
  begin
    Graph.zoomOut;
    if (Graph.getScale = 1) then
      ButtonZoomOut.Enabled := false;
    if (Graph.getScale = 4) then
      ButtonZoomIn.Enabled := true;
    // Graph re-draw
    if (loadDone) then
    begin
      Graph.clear;
      Graph.drawLegends;
      if (searchDone) then Graph.drawRoutes(vehicles,numberOfVehiclesUsed);
      if (RadioButtonIndex.Checked) then Graph.drawIndex;
      if (RadioButtonDemand.Checked) then Graph.drawDemand;
      Graph.drawNodes;
    end;
  end;
end;

// This procedure zooms in the graph
procedure TFormCVRP.ButtonZoomInClick(Sender: TObject);
begin
  if (Graph <> nil) then
  begin
    Graph.zoomIn;
    if (Graph.getScale = 5) then
      ButtonZoomIn.Enabled := false;
    if (Graph.getScale = 2) then
      ButtonZoomOut.Enabled := true;
    // Graph re-draw
    if (loadDone) then
    begin
      Graph.clear;
      Graph.drawLegends;
      if (searchDone) then Graph.drawRoutes(vehicles,numberOfVehiclesUsed);
      if (RadioButtonIndex.Checked) then Graph.drawIndex;
      if (RadioButtonDemand.Checked) then Graph.drawDemand;
      Graph.drawNodes;
    end;
  end;
end;

// This procedure ensures that all worker threads is terminated before
// this application is terminated
procedure TFormCVRP.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ButtonPause.Enabled) then
  begin
    Action := caNone;
    ShowMessage
      ('You must stop the searching process before closing this application');
  end;
end;


end.
