unit UnitNNI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, OtherClasses;

type
  TNNI = class
    private
      vehicles             : TVehicles;
      customers            : TCustomers;
      N                    : integer;
      distances            : TDistances;
      v                    : integer;
    public
      constructor create
          (vehicles : TVehicles; customers : TCustomers; N : integer;
           distances : TDistances; v : integer);
      function allCustomersVisited : boolean;
      function nearestUnvisitedCustomer(index : integer) : integer;
      procedure searchAvailableVehicle(index : integer);
      procedure searchSolution(var numberOfVehiclesUsed : integer);
  end;

implementation


// This is the constructor of class TNNI
constructor TNNI.create
          (vehicles : TVehicles; customers : TCustomers; N : integer;
           distances : TDistances; v : integer);
begin
  Self.vehicles    := vehicles;
  Self.customers   := customers;
  Self.N           := N;
  Self.distances   := distances;
  Self.v           := v;
end;

// This function returns true if all customers are visited
function TNNI.allCustomersVisited : boolean;
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

// This function returns the nearest unvisited customer of customer-index
function TNNI.nearestUnvisitedCustomer(index : integer) : integer;
var
  i,min,nearest : integer;
begin
  min := 99999;
  nearest := 0;
  for i:=1 to N do
  begin
    if (distances[index][i]<min) and (i<>index)
    and (customers[i].visited=false)then
    begin
      min     := distances[index][i];
      nearest := i;
    end;
  end;
  Result := nearest;
end;

// This procedure search for available vehicle for customer-index
procedure TNNI.searchAvailableVehicle(index : integer);
var
  i,remainingCapacity,min,carIndex : integer;
begin
  min := vehicles[1].getMaxCapacity;
  carIndex := 0;
  for i:=1 to v do
  begin
    if (vehicles[i].isAvailable(customers[index].demand)) then
    begin
      remainingCapacity := vehicles[i].getMaxCapacity - vehicles[i].getCapacity;
      if (remainingCapacity < min) then
      begin
        carIndex := i;
        min := remainingCapacity;
      end;
    end;
  end;
  if (carIndex > 0) then
  begin
    vehicles[carIndex].addRoute(customers[index]);
  end;
end;

// This procedure performs NNI method to generate a solution
procedure TNNI.searchSolution(var numberOfVehiclesUsed : integer);
var
  i,k,current : integer;
begin
  k := 1;
  while (k<=v) and (not allCustomersVisited) do
  begin
    current := nearestUnvisitedCustomer(0);
    while (vehicles[k].isAvailable(customers[current].demand)) and (current<>0) do
    begin
      vehicles[k].addRoute(customers[current]);
      current := nearestUnvisitedCustomer(current);
    end;
    inc(k);
  end;
  if not allCustomersVisited then
  begin
    //perform a modification to complete NNI method
    for i:=1 to N do
    begin
      if (customers[i].visited=false) then
      begin
        searchAvailableVehicle(i);
      end;
    end;
  end;
  numberOfVehiclesUsed := k-1;
end;

end.
