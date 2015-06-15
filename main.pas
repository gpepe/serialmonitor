unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, synaser, SerialThread;


type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    autoscroll1: TCheckBox;
    ComboBox2: TComboBox;
    reset: TButton;
    CheckGroup1: TCheckGroup;
    ComboBox1: TComboBox;
    baud: TComboBox;
    bit: TEdit;
    Panel2: TPanel;
    stop: TEdit;
    par: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure autoscroll1Change(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure ComboBox2KeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormException(Sender : TObject; E : Exception);
    procedure paramsChange(Sender: TObject);
    procedure resetClick(Sender: TObject);
  private
    ST:TSerialThread;
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
  ComboBox2.Text:='';
  GroupBox1.Caption:=cNoneConn;
  Application.OnActivate:=@FormActivate;
  Application.OnDeactivate:=@FormDeactivate;
  Application.OnException:=@FormException;
  stop.text:='0';
  bit.Text:='8';
  ST := TSerialThread.Create(Memo1, GroupBox1, CheckGroup1);
  ST.Reset(baud.Items[baud.ItemIndex], bit.Text, par.items[par.ItemIndex], stop.Text);
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
  ST.pause;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  ST.resume;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  ST.terminate;
end;

procedure TForm1.CheckGroup1ItemClick(Sender: TObject; Index: integer);
begin
  paramsChange(Sender);
end;


procedure TForm1.ComboBox2KeyPress(Sender: TObject; var Key: char);
var
  s:string;
begin
  if (key=#13)
  then begin
    try
      s := ComboBox2.Text;
      if (s<>'')
      then begin
        if (ComboBox2.Items.IndexOf(s)=-1)
        then begin
          ComboBox2.Items.add(s);
        end;
      end;
      if ComboBox1.Items[ComboBox1.ItemIndex] = '\n'
      then s := s+AnsiChar(LF)
      else if ComboBox1.Items[ComboBox1.ItemIndex] = '\r\n'
      then s := s+ AnsiString(CR+LF);
      ST.SendString(s);
      ComboBox2.Text:= '';
    except
      on E : Exception do begin
        GroupBox1.Caption:=cNoneConn;
        Memo1.Lines.add(e.Message);
        ST.Reset(baud.Items[baud.ItemIndex], bit.Text, par.items[par.ItemIndex], stop.Text);
      end;
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  ComboBox2.SetFocus;
end;

procedure TForm1.autoscroll1Change(Sender: TObject);
begin
  ST.AutoScroll := autoscroll1.Checked;
end;



procedure TForm1.FormException(Sender: TObject; E: Exception);
begin
  ST.Reset(baud.Items[baud.ItemIndex], bit.Text, par.items[par.ItemIndex], stop.Text);
end;

procedure TForm1.paramsChange(Sender: TObject);
begin
  GroupBox1.Caption:=cNoneConn;
  ST.Reset(baud.Items[baud.ItemIndex], bit.Text, par.items[par.ItemIndex], stop.Text);
  ComboBox2.SetFocus;
end;

procedure TForm1.resetClick(Sender: TObject);
begin
  CheckGroup1.Items.Clear;
  paramsChange(Sender);
end;

end.

