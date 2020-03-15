unit uMain;

interface

uses
   Winapi.ShellAPI,  Winapi.Windows,
   System.Generics.Collections, RegularExpressions, System.StrUtils,
   System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
   FMX.Types,FMX.Objects, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
   FMX.Controls.Presentation, FMX.Edit, FMX.SearchBox, FMX.Layouts, FMX.StdCtrls,
   FMX.Effects, FMX.ScrollBox, FMX.Memo, FMX.EditBox, FMX.SpinBox;

type
  TShapeHelper = class helper for TShape
    procedure TextoCustomizado(Texto:String;FontSize :Real = 10);
    procedure ClickURL(Sender:TObject);
end;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Rectangle1: TRectangle;
    SpinBox1: TSpinBox;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    procedure Memo1ChangeTracking(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TShapeHelper.ClickURL(Sender: TObject);
begin
   ShellExecute(GetDesktopWindow, 'open', PChar(TText(Sender).Text), '', '', SW_SHOWNORMAL);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   Rectangle3.TextoCustomizado('<b><blue>DELPHI <b><blue>CREATIVE - <i><s><red>DIEGO <i><s><red>CATANEO',24);
   Rectangle2.TextoCustomizado('<b>CURSO DE DELPHI COM <b>FIREBASE <b>REALTIME',20);
end;

procedure TForm1.Memo1ChangeTracking(Sender: TObject);
begin
   Rectangle1.TextoCustomizado(Memo1.Lines.Text,SpinBox1.Value);
end;

{ TShapeHelper }
procedure TShapeHelper.TextoCustomizado(Texto:String;FontSize :Real = 10);
var
   Arr :TArray<string>;
   I :Integer;
   T :TText;
   FL :TFlowLayout;
   L : TLayout;
begin
   Self.BeginUpdate;
   for I := Self.ComponentCount - 1  downto 0 do
      Self.Components[i].DisposeOf;

   FL := TFlowLayout.Create(Self);
   FL.Align := TAlignLayout.Client;

   Arr := TRegEx.Split(texto,' ');
   for i := 0 to TRegEx.Matches(texto,' ').Count do begin

      if AnsiContainsText(arr[i],'<p>') then begin
         L := TLayout.Create(FL);
         L.Height := 16;
         L.Width := 2000;
         Fl.AddObject(L);
      end;

      T := TText.Create(FL);
      T.Text := arr[i];
      T.TextSettings.FontColor := TAlphaColors.Black;
      T.TextSettings.Font.Size := FontSize ;
      T.Height := 16;
      T.Width := 1000;
      T.AutoSize := True;

      T.TextSettings.Font.Style := [];

      if AnsiContainsText(arr[i],'<b>') then
         T.TextSettings.Font.Style := T.TextSettings.Font.Style + [TFontStyle.fsBold];

      if AnsiContainsText(arr[i],'<i>') then
         T.TextSettings.Font.Style := T.TextSettings.Font.Style + [TFontStyle.fsItalic];

      if AnsiContainsText(arr[i],'<s>') then
         T.TextSettings.Font.Style := T.TextSettings.Font.Style + [TFontStyle.fsUnderline];

      if AnsiContainsText(arr[i],'<r>') then
         T.TextSettings.Font.Style := T.TextSettings.Font.Style + [TFontStyle.fsStrikeOut];

      if AnsiContainsText(arr[i],'www.') then begin
         T.TextSettings.FontColor := TAlphaColors.Steelblue;
         T.Cursor := crHandPoint;
         T.OnClick := ClickURL;
      end;

      if AnsiContainsText(arr[i],'<red>') then
         T.TextSettings.FontColor := TAlphaColors.Red
      else if AnsiContainsText(arr[i],'<green>') then
         T.TextSettings.FontColor := TAlphaColors.Green
      else if AnsiContainsText(arr[i],'<yellow>') then
         T.TextSettings.FontColor := TAlphaColors.Yellow
      else if AnsiContainsText(arr[i],'<blue>') then
         T.TextSettings.FontColor := TAlphaColors.Royalblue;

      T.Text := arr[i]
                .Replace('<red>','')
                .Replace('<yellow>','')
                .Replace('<blue>','')
                .Replace('<green>','')
                .Replace('<r>','')
                .Replace('<s>','')
                .Replace('<b>','')
                .Replace('<i>','')
                .Replace('<p>','');

      T.TextSettings.HorzAlign := TTextAlign.Center;
      T.TextSettings.VertAlign := TTextAlign.Center;

      FL.AddObject(T);

   end;

   FL.EndUpdate;
   Self.AddObject(FL);
   Self.EndUpdate;

end;

end.
