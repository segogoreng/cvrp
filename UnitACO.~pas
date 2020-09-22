unit UnitACO;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Math, OtherClasses;

type
  TACO = class(TThread)
    private
      ProgressBar          : TProgressBar;
      MemoLog              : TMemo;
      vehicles             : TVehicles;
      customers            : TCustomers;
      N                    : integer;
      distances            : TDistances;
      v                    : integer;
      numberOfVehiclesUsed : integer;
      bestSolution         : TVehicles;
      bestNumberOfVehicles : integer;
      minCost              : integer;
      iteration            : integer;
      numberOfAnts         : integer;
      beta                 : real;
      p                    : real;
      q0                   : real;
      T0                   : real;
      pheromone            : array [0..300,0..300] of real;
      visibility           : array [0..300,0..300] of real;
      vIsNotEnough         : boolean;
    public
      property noOfVehiclesUsed : integer read numberOfVehiclesUsed;
      procedure setProgressBar(PB : TProgressBar);
      procedure setMemoLog(ML : TMemo);
      procedure writeLog;
      procedure input
          (vehicles : TVehicles; customers : TCustomers; N : integer;
           distances : TDistances; v : integer; iteration : integer;
           numberOfAnts : integer; beta : real; p : real; q0 : real);
      procedure initializePheromone;
      procedure countVisibility;
      function allCustomersVisited : boolean;
      procedure saveSolution;
      procedure loadSolution;
      function countCost : integer;
      function countBestCost : integer;
      procedure restartTour(var vi : integer);
      function chooseNextCustomer(i : integer; vi : integer) : integer;
      procedure generateTour;
      procedure localPheromoneUpdate;
      procedure globalPheromoneUpdate;
    protected
      procedure Execute; override;
  end;


var
  ctr : integer;

implementation


// This procedure sets the progress bar
procedure TACO.setProgressBar(PB : TProgressBar);
begin
  ProgressBar := PB;
end;

// This procedure sets the memo for log file
procedure TACO.setMemoLog(ML : TMemo);
begin
  MemoLog := ML;
  MemoLog.Clear;
end;

// This procedure writes log to memo log
procedure TACO.writeLog;
var
  i,j  : integer;
  line : string;
begin
  MemoLog.Lines.Add('Iteration : ' + IntToStr(ctr));
  for i:=1 to bestNumberOfVehicles do
  begin
    line := 'Route #' + IntToStr(i) + ':';
    for j:=1 to bestSolution[i].getTotalCust do
    begin
      line := line + ' ' + inttostr(bestSolution[i].getCustomerIndex(j));
    end;
    MemoLog.Lines.Add(line);
  end;
  MemoLog.Lines.Add('cost ' + IntToStr(countBestCost));
  MemoLog.Lines.Add(#13);
end;

// This procedure sets up the input
procedure TACO.input
          (vehicles : TVehicles; customers : TCustomers; N : integer;
           distances : TDistances; v : integer; iteration : integer;
           numberOfAnts : integer; beta : real; p : real; q0 : real);
var
  i : integer;
begin
  Self.vehicles     := vehicles;
  Self.customers    := customers;
  Self.N            := N;
  Self.distances    := distances;
  Self.v            := v;
  Self.iteration    := iteration;
  Self.numberOfAnts := numberOfAnts;
  Self.beta         := beta;
  Self.p            := p;
  Self.q0           := q0;
  countVisibility;
  initializePheromone;
  bestNumberOfVehicles := 0;
  for i:=1 to v do
    bestSolution[i] := TVehicle.create(vehicles[i].getMaxCapacity);
end;

// This procedure initializes pheromone on each route;
procedure TACO.initializePheromone;
var
  i,j : integer;
begin
  for i:=0 to N do
    for j:=0 to N do
      if ( i <> j ) then
      begin
        pheromone[i][j] := 1;
      end;
  // Build a tour based on visibility only
  generateTour;
  // If vehicle number is not enough then searching process fails
  if (vIsNotEnough) then
  begin
    Terminate;
    Exit;
  end;
  // Initial pheromone trail (T0)
  T0 := 1 / ( N * countCost);
  for i:=0 to N do
    for j:=0 to N do
      if ( i <> j ) then
      begin
        pheromone[i][j] := T0;
      end;
end;

// This procedure counts the visibility between nodes using a particular formula
procedure TACO.countVisibility;
var
  i,j : integer;
//  max : real;
begin
//  max := 0;
  for i:=0 to N do
    for j:=0 to N do
      if ( i <> j ) then
      begin
        if distances[i][j] = 0 then
          visibility[i][j] := 1
        else begin
          visibility[i][j] := 1 / distances[i][j];
        end;

{        visibility[i][j] := distances[i][0] + distances [0][j] - 2*distances[i][j]
                            + 2*abs(distances[i][0]-distances[0][j]);
        if ( visibility[i][j] <= 0 ) then visibility[i][j] := 0.000001;
        if ( visibility[i][j] > max ) then max := visibility[i][j]; }

      end;

{  // Normalization to 0..1
  for i:=0 to N do
    for j:=0 to N do
      if ( i <> j ) and ( visibility[i][j] >= 1 ) then
      begin
        visibility[i][j] := visibility[i][j] / ( max*2 );
      end; }
end;

// This function returns true if all customers are visited
function TACO.allCustomersVisited : boolean;
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

// This procedure saves the vehicle data to a variable
procedure TACO.saveSolution;
var
  i : integer;
begin
  for i:=1 to numberOfVehiclesUsed do
  begin
    bestSolution[i].clear;
    bestSolution[i].copyData(vehicles[i]);
  end;
  bestNumberOfVehicles := numberOfVehiclesUsed;
end;

// This procedure loads the vehicle data from a variable
procedure TACO.loadSolution;
var
  i : integer;
begin
  for i:=1 to bestNumberOfVehicles do
  begin
    vehicles[i].clear;
    vehicles[i].copyData(bestSolution[i]);
  end;
  numberOfVehiclesUsed := bestNumberOfVehicles;
end;

// This function counts the cost of the current solution
function TACO.countCost : integer;
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

// This function counts the cost of the best solution found so far
function TACO.countBestCost : integer;
var
  i,j,k,cost : integer;
begin
  cost := 0;
  for i:=1 to bestNumberOfVehicles do
  begin
    k := 0;
    for j:=1 to bestSolution[i].getTotalCust do
    begin
      cost := cost + distances[k][bestSolution[i].getCustomerIndex(j)];
      k := bestSolution[i].getCustomerIndex(j);
    end;
    cost := cost + distances[k][0];
  end;
  Result := cost;
end;

//  This procedure restarts the tour because not all customers are served
procedure TACO.restartTour(var vi : integer);
var i : integer;
begin
  for i:=1 to v do vehicles[i].clear;
  for i:=1 to N do customers[i].visited := false;
  vi := 1;
end;

// This function returns the index of the next customer to serve based on
// pheromone trail and visibility
function TACO.chooseNextCustomer(i : integer; vi : integer) : integer;
var
  q, r, max   : real;
  j, next     : integer;
  total       : real;
  p           : array [0..300] of real;
  pk          : real;
  stop        : boolean;
begin
  Randomize;
  q := Random;
  next := 0;
  //Pseudo-Random-Proportional
  if ( q <= q0 ) then
  begin
    //Exploitation
    max := 0;
    for j:=0 to N do
    begin
      if ( customers[j].visited = false ) and (i <> j) and
      ( vehicles[vi].isAvailable(customers[j].demand) ) and
      ( pheromone[i,j]*power(visibility[i,j],beta) > max ) then
      begin
        next := j;
        max  := pheromone[i,j]*power(visibility[i,j],beta);
      end;
    end;
    //End of Exploitation
  end else
  begin
    //Exploration
    total := 0;
    r     := Random;
    pk    := 0;
    for j:=0 to N do
      if ( customers[j].visited = false ) and (i <> j)
      and ( vehicles[vi].isAvailable(customers[j].demand)) then
      begin
        total := total + pheromone[i,j] * power(visibility[i,j],beta);
      end;
    j := 0; stop := false;
    while ( j <= N ) and ( not stop ) do
    begin
      if ( customers[j].visited = false ) and (i <> j)
      and ( vehicles[vi].isAvailable(customers[j].demand)) then
      begin
        p[j]  := (pheromone[i,j]*power(visibility[i,j],beta)) / total;
        pk    := pk + p[j];
        if ( r <= pk ) then
        begin
          next := j;
          stop := true;
        end;
      end;
      inc(j);
    end;
    //End of Exploration
  end;
  Result := next;
end;

// This procedure generates a tour
// k is the parameter for ant's index
procedure TACO.generateTour;
var
  vi,current : integer;
  ctr        : integer;
begin
  vi := 1;
  current := 0;
  restartTour(vi);
  vIsNotEnough := false;
  ctr := 0;
  while (not allCustomersVisited) and (ctr<1000) do
  begin
    current := chooseNextCustomer(current,vi);
    if ( current = 0 ) then
    begin
      inc(vi);
    end else begin
      vehicles[vi].addRoute(customers[current]);
    end;
    if ( vi > v) then
    begin
      restartTour(vi);
      inc(ctr);
    end;
  end;
  numberOfVehiclesUsed := vi;
  if (ctr = 1000) then
  begin
    vIsNotEnough := true;
    numberOfVehiclesUsed := 0;
  end;
end;

// This procedure performs update to pheromone concentration on edges that
// are choosed by an ants to build a tour
procedure TACO.localPheromoneUpdate;
var
  i,j,k,l : integer;
begin
  for i:=1 to numberOfVehiclesUsed do
  begin
    k := 0;
    for j:=1 to vehicles[i].getTotalCust do
    begin
      l := vehicles[i].getCustomerIndex(j);
      pheromone[k][l] := (1-p)*pheromone[k][l] + p*T0;
      k := l;
    end;
    pheromone[k][0] := (1-p)*pheromone[k][0] + p*T0;
  end;
end;

// This procedure performs update to pheromone concentration on all edges
procedure TACO.globalPheromoneUpdate;
var
  i,j,k,l : integer;
begin
  for i:=1 to bestNumberOfVehicles do
  begin
    k := 0;
    for j:=1 to bestSolution[i].getTotalCust do
    begin
      l := bestSolution[i].getCustomerIndex(j);
      pheromone[k][l] := (1-p)*pheromone[k][l] + p*(1/minCost);
      k := l;
    end;
    pheromone[k][0] := (1-p)*pheromone[k][0] + p/(minCost*N*v);
  end;
end;

// This procedure generates a solution using Ant Colony Optimization
procedure TACO.Execute;
var
  i,k : integer;
begin
  minCost := 1000000000;
  ProgressBar.Min := 0;
  ProgressBar.Max := iteration;
  i := 1;
  while (i<=iteration) and (not terminated) do
  begin
    for k:=1 to numberOfAnts do
    begin
      generateTour;
      localPheromoneUpdate;
      if (countCost<minCost) and (allCustomersVisited) then
      begin
        saveSolution;
        minCost := countCost;
      end;
    end;
    globalPheromoneUpdate;
    ctr := i;
    Synchronize(writeLog);
    ProgressBar.Position := i;
    inc(i);
  end;
  loadSolution;
end;

end.
