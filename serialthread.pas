unit SerialThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synaser, StdCtrls, ExtCtrls;

type

  { TSerialThread }

  TSerialThread = class (TThread)
  private
    ser:TBlockSerial;
    Log:TMemo;
    GroupBox1:TGroupBox;
    CheckGroup1: TRadioGroup;
    Data : string;
    RequestReset : Boolean;
    RequestSuspended : Boolean;
    Send:String;
    FAutoscroll : boolean;

    baud,bit,parity,stop:string;

    procedure ToLog;
    procedure OnException;
    procedure FindSerial;
    procedure Connect;
  protected
    procedure Execute; override;
  public
    constructor Create(m:TMemo; gb:TGroupBox; cb: TRadioGroup); overload;
    destructor Destroy; override;
    procedure Reset(const xbaud,xbit,xparity,xstop:string);
    procedure SendString(const xsend:string);
    procedure pause;
    property Autoscroll:boolean read FAutoscroll write FAutoscroll;
  end;

implementation
const
  cNoneConn = 'Connected: none';

{ TSerialThread }

procedure TSerialThread.ToLog();
var
  p :TPoint;
begin
  if data<>'' then begin
    log.Lines.BeginUpdate;
    try
      p := log.CaretPos;
      Log.Lines.Add(data);
      if not(Autoscroll)
      then log.CaretPos := p;
    finally
      log.Lines.EndUpdate;
    end;
  end;
end;

procedure TSerialThread.OnException;
begin
  if Assigned(ser)
  then ser.RaiseExcept:=false;
  GroupBox1.Caption:=cNoneConn;
  FreeAndNil(ser);
  ToLog;
end;

procedure TSerialThread.FindSerial;
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
        if CheckGroup1.Items.Count=1 then CheckGroup1.ItemIndex:=0;
      end;
      inc(i);
    end;
  finally
    t.free;
  end;
end;

procedure TSerialThread.Connect;
var
  s : String;
begin
  if CheckGroup1.ItemIndex>-1
  then begin
    try
      s := CheckGroup1.Items.Strings[CheckGroup1.ItemIndex];
      ser:=TBlockserial.Create;
      ser.RaiseExcept:=true;
      ser.LinuxLock:=false;
      ser.Connect(s);
      ser.Config(StrToInt(baud),StrToInt(bit),parity[1],StrToInt(stop),false,false);
      GroupBox1.Caption:='Connected: '+s;
    except
      GroupBox1.Caption:=cNoneConn;
        freeandnil(ser);
    end;
  end;
end;

procedure TSerialThread.Execute;
begin
  while (not Terminated)
  do try
    if RequestSuspended
    then begin
      RequestSuspended := false;
      data := '';
      Synchronize(@OnException);
      Suspend;
    end;
    if RequestReset
    then begin
      FreeAndNil(ser);
      RequestReset:=false;
    end;
    if Assigned(ser)
    then begin
      if (send <> '')
      then begin
        ser.SendString(Send);
        Send:='';
        Sleep(100);
      end;
      if ser.CanRead(100)
      then begin
        data := Trim(ser.RecvPacket(1000));
        Synchronize(@ToLog);
      end;
    end
    else begin
      Synchronize(@FindSerial);
      Synchronize(@Connect);
      Sleep(1000);
    end;
  except
    on E : Exception do begin
      data := e.Message;
      Synchronize(@OnException);
    end;
  end;
end;


constructor TSerialThread.Create(m:TMemo; gb:TGroupBox; cb: TRadioGroup);
begin
  Priority:=tpIdle;
  FreeOnTerminate:=true;
  Log := m;
  GroupBox1 := gb;
  CheckGroup1 := cb;
  RequestSuspended := False;
  RequestReset:=false;
  Send := '';
  autoscroll := true;
  inherited Create(true);
end;

destructor TSerialThread.Destroy;
begin
  FreeAndNil(ser);
  inherited Destroy;
end;

procedure TSerialThread.Reset(const xbaud, xbit, xparity, xstop: string);
begin
  baud := xbaud;
  bit := xbit;
  parity := xparity;
  stop := xstop;
  RequestReset:=true;
end;

procedure TSerialThread.SendString(const xsend: string);
begin
  Send:= xsend;
end;

procedure TSerialThread.pause;
begin
  RequestSuspended:=true;
end;

end.

