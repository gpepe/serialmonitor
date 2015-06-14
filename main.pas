unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, synaser;


type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckGroup1: TCheckGroup;
    ComboBox1: TComboBox;
    baud: TComboBox;
    bit: TEdit;
    stop: TEdit;
    par: TComboBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    IdleTimer1: TIdleTimer;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure FormException(Sender : TObject; E : Exception);
    procedure paramsChange(Sender: TObject);
  private
    ser:TBlockSerial;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

const cNoneConn = 'Connected: none';

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Edit1.Text:='';
  GroupBox1.Caption:=cNoneConn;
  ser := nil;
  Application.OnActivate:=@FormActivate;
  Application.OnDeactivate:=@FormDeactivate;
  Application.OnException:=@FormException;
  stop.text:='0';
  bit.Text:='8';
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
  IdleTimer1.AutoEnabled:=false;
  IdleTimer1.Enabled:=false;
  GroupBox1.Caption:=cNoneConn;
  freeandnil(ser);
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  s:string;
  t:TStringList;
  i, x:integer;
begin
  s:=GetSerialPortNames;
  t := TStringList.Create;
  try
  t.CommaText:=',';
  t.DelimitedText:=GetSerialPortNames+',';
  i:=0;
  while(i<t.Count)
  do begin
    s := t.Strings[i];
    x := CheckGroup1.Items.IndexOf(s);
    if (s<>'') and (x =-1)
    then begin
      x := CheckGroup1.Items.add(t.Strings[i]);
      CheckGroup1.Checked[x]:=true;
    end;
    inc(i);
  end;
  if assigned(sender) then Edit1.SetFocus;
  finally
    t.free;
  end;
  IdleTimer1.AutoEnabled:=true;
  IdleTimer1.Enabled:=true;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  freeandnil(IdleTimer1);
  freeandnil(ser);
end;



procedure TForm1.CheckGroup1ItemClick(Sender: TObject; Index: integer);
begin
  freeandnil(ser);
  GroupBox1.Caption:=cNoneConn;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Edit1.SetFocus;
end;



procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: char);
var
  s:string;
begin
  if (key=#13) and assigned(ser)
  then begin
    try
      s := Edit1.Text;
      if ComboBox1.Items[ComboBox1.ItemIndex] = '\n'
      then s := s+AnsiChar(LF)
      else if ComboBox1.Items[ComboBox1.ItemIndex] = '\r\n'
      then s := s+ AnsiString(CR+LF);
      ser.SendString(s);
      ser.Flush;
      Edit1.Text:= '';
    except
      on E : Exception do begin
        GroupBox1.Caption:=cNoneConn;
        Memo1.Lines.add(e.Message);
        freeandnil(ser);
      end;
    end;
  end;
end;


procedure TForm1.IdleTimer1Timer(Sender: TObject);
var
  s:string;
  i:integer;
  p:char;
begin
  if ser=nil
  then begin
    i:=0;
    while(i<CheckGroup1.Items.Count)
    do begin
      if CheckGroup1.Checked[i]
      then begin
          try
          s := CheckGroup1.Items.Strings[i];
          ser:=TBlockserial.Create;
          ser.RaiseExcept:=true;
          ser.LinuxLock:=false;
          p := par.Items[par.ItemIndex][1];
          ser.Connect(s);
          ser.Config(StrToInt(baud.Items[baud.ItemIndex]),StrToInt(bit.text),p,StrToInt(stop.Text),false,false);
          GroupBox1.Caption:='Connected: '+s;
          exit;
          except
            GroupBox1.Caption:=cNoneConn;
              freeandnil(ser);
          end;
      end;
      inc(i);
    end;
    FormActivate(nil);
  end
  else
  begin
    try
    if (ser.CanRead(100))
    then begin
      Memo1.Lines.Add(trim(ser.RecvPacket(100)));
    end;
    except
      on E : Exception do begin
        GroupBox1.Caption:=cNoneConn;
        Memo1.Lines.add(e.Message);
        freeandnil(ser);
      end;
    end;
  end;
end;

procedure TForm1.FormException(Sender: TObject; E: Exception);
begin
  FreeAndNil(ser);
  IdleTimer1.AutoEnabled:=true;
  IdleTimer1.Enabled:=true;
end;


procedure TForm1.paramsChange(Sender: TObject);
begin
  GroupBox1.Caption:=cNoneConn;
  FreeAndNil(ser);
  Edit1.SetFocus;
end;







end.

