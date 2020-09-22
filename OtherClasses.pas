unit OtherClasses;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

{ ----------------------------Type Declaration-------------------------------- }

type

  TDistances = array [0..300,0..300] of integer;

  TCustomer = record
    x       : integer;
    y       : integer;
    index   : integer;
    demand  : integer;
    visited : boolean;
  end;

  TCustomers = array [0..300] of TCustomer;

  TVehicle = class
  private
    routes      : array [1..100] of TCustomer;
    capacity    : integer;
    totalCust   : integer;
    maxCapacity : integer;
  public
    constructor create(maxCapacity : integer);
    procedure clear;
    procedure addRoute(var customer : TCustomer);
    procedure copyData(vehicle : TVehicle);
    function deleteRoute(var customers : TCustomers) : TCustomer;
    function isAvailable(demand : integer) : boolean;
    function getRoute(index : integer) : TCustomer;
    function getCapacity : integer;
    function getTotalCust : integer;
    function getCustomerIndex(index : integer) : integer;
    function getMaxCapacity : integer;
  end;

  TVehicles = array [1..50] of TVehicle;

{ ---------------------------------------------------------------------------- }

implementation

{ --------------------TVehicle procedures and functions----------------------- }

// Constructor of class TVehicle
constructor TVehicle.create(maxCapacity : integer);
begin
  capacity := 0;
  totalCust := 0;
  Self.maxCapacity := maxCapacity;
end;

// This procedure deletes all customers
procedure TVehicle.clear;
begin
  capacity  := 0;
  totalCust := 0;
end;

// This procedure adds a customer to vehicle's route
procedure TVehicle.addRoute(var customer : TCustomer);
begin
  inc(totalCust);
  routes[totalCust] := customer;
  capacity := capacity + customer.demand;
  customer.visited := true;
end;

// This procedure copies data from a vehicle specified
procedure TVehicle.copyData(vehicle : TVehicle);
var
  i : integer;
begin
  for i:=1 to vehicle.totalCust do
  begin
    routes[i] := vehicle.getRoute(i);
  end;
  capacity    := vehicle.getCapacity;
  totalCust   := vehicle.getTotalCust;
  maxCapacity := vehicle.getMaxCapacity;
end;

// This procedure deletes last customer on the route
function TVehicle.deleteRoute(var customers : TCustomers) : TCustomer;
var
  ci : integer; // for customer index
begin
  capacity := capacity - routes[totalCust].demand;
  ci := routes[totalCust].index;
  customers[ci].visited := false;
  Result := routes[totalCust];
  dec(totalCust);
end;

// This function returns true if vehicle capacity doesn't exceed the max
// capacity when serving a particular demand
function TVehicle.isAvailable(demand : integer) : boolean;
begin
  if (capacity+demand <= maxCapacity) then
    Result := true
  else
    Result := false;
end;

// This function returns the customer on the route(index)
function TVehicle.getRoute(index : integer) : TCustomer;
begin
  Result := routes[index];
end;

// This function returns the current capacity of the vehicle
function TVehicle.getCapacity : integer;
begin
  Result := capacity;
end;

// This function returns the total customers served
function TVehicle.getTotalCust : integer;
begin
  Result := totalCust;
end;

// This function returns the index of the customer on the route[index]
function TVehicle.getCustomerIndex(index : integer) : integer;
begin
  Result := routes[index].index;
end;

// This function returns the maximum capacity of the vehicle
function TVehicle.getMaxCapacity : integer;
begin
  Result := maxCapacity;
end;

{ ---------------------------------------------------------------------------- }

end.
