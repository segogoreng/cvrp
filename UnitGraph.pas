unit UnitGraph;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, OtherClasses;

type
  TGraph = class
    private
      Image     : TImage;
      scale     : integer;
      r         : integer;
      customers : TCustomers;
      N         : integer;
    public
      constructor create
          (var Image : TImage; customers : TCustomers; N : integer);
      procedure clear;
      function getScale : integer;
      procedure drawLine(x1 : integer; y1 : integer; x2 : integer; y2 : integer);
      procedure setPenColor(index : integer);
      procedure drawLegends;
      procedure drawNodes;
      procedure drawRoutes(vehicles : TVehicles; numberOfVehiclesUsed : integer);
      procedure drawIndex;
      procedure drawDemand;
      procedure zoomOut;
      procedure zoomIn;
  end;

implementation


// This is the constructor of TGraph class
constructor TGraph.create
          (var Image : TImage; customers : TCustomers; N : integer);
begin
  Self.Image     := Image;
  scale          := 5;
  r              := 9;
  Self.customers := customers;
  Self.N         := N;
end;

// This procedure clears the image
procedure TGraph.clear;
begin
  Image.Canvas.Pen.Color   := clBlack;
  Image.Canvas.Brush.Color := clWhite;
  Image.Canvas.Rectangle(0,0,Image.Width,Image.Height);
end;

// This function returns the scale of the image
function TGraph.getScale : integer;
begin
  Result := scale;
end;

// This procedure draws line from x1,y1 to x2,y2
procedure TGraph.drawLine(x1 : integer; y1 : integer; x2 : integer; y2 : integer);
begin
  Image.Canvas.MoveTo(x1,y1);
  Image.Canvas.LineTo(x2,y2);
end;

// This procedure sets the color of the pen(used to draw image)
procedure TGraph.setPenColor(index : integer);
begin
  case ( (index-1) mod 9 ) + 1 of
    1 : Image.Canvas.Pen.Color := clYellow;
    2 : Image.Canvas.Pen.Color := $0E90FF; //orange
    3 : Image.Canvas.Pen.Color := clRed;
    4 : Image.Canvas.Pen.Color := clSkyBlue;
    5 : Image.Canvas.Pen.Color := clFuchsia;
    6 : Image.Canvas.Pen.Color := clAqua;
    7 : Image.Canvas.Pen.Color := clBlack;
    8 : Image.Canvas.Pen.Color := $BC9D66; //darkblue
    9 : Image.Canvas.Pen.Color := clLime;
  end;
end;

// This procedure draws the legends
procedure TGraph.drawLegends;
begin
  // Draw a line
  Image.Canvas.Pen.Color   := clBlack;
  Image.Canvas.MoveTo(0,Image.Width);
  Image.Canvas.LineTo(Image.Width,Image.Width);
  // Write text
  Image.Canvas.TextOut(10,Image.Width+10,'LEGENDS :');
  Image.Canvas.TextOut(30,Image.Width+30,'=  Central Depot');
  Image.Canvas.TextOut(30,Image.Width+50,'=  Customer');
  Image.Canvas.TextOut(180,Image.Width+30,'=  Route #1');
  Image.Canvas.TextOut(180,Image.Width+50,'=  Route #2');
  Image.Canvas.TextOut(180,Image.Width+70,'=  Route #3');
  Image.Canvas.TextOut(320,Image.Width+30,'=  Route #4');
  Image.Canvas.TextOut(320,Image.Width+50,'=  Route #5');
  Image.Canvas.TextOut(320,Image.Width+70,'=  Route #6');
  Image.Canvas.TextOut(460,Image.Width+30,'=  Route #7');
  Image.Canvas.TextOut(460,Image.Width+50,'=  Route #8');
  Image.Canvas.TextOut(460,Image.Width+70,'=  Route #9');
  // Draw central depot's legend
  Image.Canvas.Pen.Color   := clRed;
  Image.Canvas.Brush.Color := clRed;
  Image.Canvas.Ellipse(9,Image.Width+29,9+2*r,Image.Width+29+2*r);
  // Draw customer's legend
  Image.Canvas.Pen.Color   := clGreen;
  Image.Canvas.Brush.Style := bsClear;
  Image.Canvas.Ellipse(9,Image.Width+49,9+2*r,Image.Width+49+2*r);
  Image.Canvas.Brush.Style := bsSolid;
  // Draw routes's legend
  Image.Canvas.Pen.Width := 3;
  Image.Canvas.Pen.Color := clYellow;
  drawLine(140,Image.Width+37,170,Image.Width+37);    // route #1
  Image.Canvas.Pen.Color := $0E90FF; //orange
  drawLine(140,Image.Width+57,170,Image.Width+57);    // route #2
  Image.Canvas.Pen.Color := clRed;
  drawLine(140,Image.Width+77,170,Image.Width+77);    // route #3
  Image.Canvas.Pen.Color := clSkyBlue;
  drawLine(280,Image.Width+37,310,Image.Width+37);    // route #4
  Image.Canvas.Pen.Color := clFuchsia;
  drawLine(280,Image.Width+57,310,Image.Width+57);    // route #5
  Image.Canvas.Pen.Color := clAqua;
  drawLine(280,Image.Width+77,310,Image.Width+77);    // route #6
  Image.Canvas.Pen.Color := clBlack;
  drawLine(420,Image.Width+37,450,Image.Width+37);    // route #7
  Image.Canvas.Pen.Color := $BC9D66; //darkblue
  drawLine(420,Image.Width+57,450,Image.Width+57);    // route #8
  Image.Canvas.Pen.Color := clLime;
  drawLine(420,Image.Width+77,450,Image.Width+77);    // route #9
  Image.Canvas.Pen.Width := 1;
end;

// This procedure draws visual representation of the problem in graph model
procedure TGraph.drawNodes;
var
  i : integer;
begin
  // Drawing the central depot (red)
  Image.Canvas.Pen.Color   := clRed;
  Image.Canvas.Brush.Color := clRed;
  Image.Canvas.Ellipse
  (customers[0].x*scale-r,customers[0].y*scale-r,customers[0].x*scale+r,customers[0].y*scale+r);
  // Drawing the customers (green)
  for i:=1 to N do
  begin
    Image.Canvas.Pen.Color   := clGreen;
    Image.Canvas.Brush.Style := bsClear;
    Image.Canvas.Ellipse
    (customers[i].x*scale-r,customers[i].y*scale-r,customers[i].x*scale+r,customers[i].y*scale+r);
  end;
  Image.Canvas.Brush.Style := bsSolid;
end;

// This procedure draws routes generated from a method choosen before
procedure TGraph.drawRoutes(vehicles : TVehicles; numberOfVehiclesUsed : integer);
var
  i,j,k,l : integer;
begin
  Image.Canvas.Pen.Width := 3;
  for i:=1 to numberOfVehiclesUsed do
  begin
    setPenColor(i);
    k := 0;
    for j:=1 to vehicles[i].getTotalCust do
    begin
      l := vehicles[i].getCustomerIndex(j);
      drawLine(customers[k].x*scale,customers[k].y*scale,customers[l].x*scale,customers[l].y*scale);
      k := l;
    end;
    drawLine(customers[k].x*scale,customers[k].y*scale,customers[0].x*scale,customers[0].y*scale);
  end;
  Image.Canvas.Pen.Width := 1;
end;

// This procedure shows customer's index
procedure TGraph.drawIndex;
var
  i : integer;
begin
  for i:=1 to N do
  begin
    Image.Canvas.Brush.Color := clWhite;
    if (customers[i].index<10) then
      Image.Canvas.TextOut
        (customers[i].x*scale-4,customers[i].y*scale-7,IntToStr(customers[i].index))
    else
      Image.Canvas.TextOut
        (customers[i].x*scale-7,customers[i].y*scale-7,IntToStr(customers[i].index))
  end;
end;

// This procedure shows the demands of the customers
procedure TGraph.drawDemand;
var
  i : integer;
begin
  for i:=1 to N do
  begin
    Image.Canvas.Brush.Color := clWhite;
    if (customers[i].demand<10) then
      Image.Canvas.TextOut
        (customers[i].x*scale-4,customers[i].y*scale-7,IntToStr(customers[i].demand))
    else
      Image.Canvas.TextOut
        (customers[i].x*scale-7,customers[i].y*scale-7,IntToStr(customers[i].demand))
  end;
end;

// This procedure zooms out by decrementing the scale
procedure TGraph.zoomOut;
begin
  if (scale > 1) then dec(scale);
end;

// This procedure zooms in by incrementing the scale
procedure TGraph.zoomIn;
begin
  if (scale < 5) then inc(scale);
end;

end.
 