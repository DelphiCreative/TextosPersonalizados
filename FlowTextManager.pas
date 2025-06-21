unit FlowTextManager;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.StrUtils,
  System.RegularExpressions, FMX.Types, FMX.Controls, FMX.Layouts, FMX.Graphics,
  FMX.Objects, FMX.StdCtrls;

type
  TFlowTextManager = class
  private
    FFlowLayout: TFlowLayout;
    FFontSize: Single;
    procedure ConfigureTextComponent(const TextContent: String; Component: TText);
  public
    constructor Create(AFlowLayout: TFlowLayout; AFontSize: Single = 10);
    procedure AddFormattedText(const Text: String);
    procedure Clear;
  end;

implementation

{ TFlowTextManager }

constructor TFlowTextManager.Create(AFlowLayout: TFlowLayout; AFontSize: Single);
begin
  FFlowLayout := AFlowLayout;
  FFontSize := AFontSize;
end;

procedure TFlowTextManager.Clear;
var
  I: Integer;
begin
  FFlowLayout.BeginUpdate;
  try
    for I := FFlowLayout.ChildrenCount - 1 downto 0 do
      FFlowLayout.Children[I].DisposeOf;
  finally
    FFlowLayout.EndUpdate;
  end;
end;

procedure TFlowTextManager.ConfigureTextComponent(const TextContent: String; Component: TText);
begin
  Component.Text := TextContent;
  Component.TextSettings.FontColor := TAlphaColors.Black;
  Component.TextSettings.Font.Size := FFontSize;
  Component.Height := 16;
  Component.AutoSize := True;
  Component.TextSettings.HorzAlign := TTextAlign.Center;
  Component.TextSettings.VertAlign := TTextAlign.Center;
  Component.Width := 1000;
  Component.AutoSize := True;

  // Aplicando estilos com base nos marcadores
  if AnsiContainsText(TextContent, '<b>') then
    Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsBold];
  if AnsiContainsText(TextContent, '<i>') then
    Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsItalic];
  if AnsiContainsText(TextContent, '<s>') then
    Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsUnderline];
  if AnsiContainsText(TextContent, '<r>') then
    Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsStrikeOut];

  // Definição de cores
  if AnsiContainsText(TextContent, '<red>') then
    Component.TextSettings.FontColor := TAlphaColors.Red
  else if AnsiContainsText(TextContent, '<green>') then
    Component.TextSettings.FontColor := TAlphaColors.Green
  else if AnsiContainsText(TextContent, '<yellow>') then
    Component.TextSettings.FontColor := TAlphaColors.Yellow
  else if AnsiContainsText(TextContent, '<blue>') then
    Component.TextSettings.FontColor := TAlphaColors.Royalblue;

  // Remover tags do texto final
  Component.Text := TextContent
    .Replace('<red>', '')
    .Replace('<green>', '')
    .Replace('<yellow>', '')
    .Replace('<blue>', '')
    .Replace('<b>', '')
    .Replace('<i>', '')
    .Replace('<s>', '')
    .Replace('<r>', '')
    .Replace('<p>', '');
end;

procedure TFlowTextManager.AddFormattedText(const Text: String);
var
  TextArray: TArray<String>;
  TextComponent: TText;
  Layout: TLayout;
  Img: TImage;
  I: Integer;
  Match: TMatch;
begin
  Clear; // Limpa os componentes antes de adicionar novos

  FFlowLayout.BeginUpdate;
  try
    TextArray := TRegEx.Split(Text, ' ');

    for I := 0 to High(TextArray) do
    begin
      // Verifica se é uma imagem no formato <img=path,width,height>
      Match := TRegEx.Match(TextArray[I], '<img=([^,]+),(\d+),(\d+)>');
      if Match.Success then
      begin
        if FileExists(Match.Groups[1].Value.Replace('"', '')) then
        begin
          Img := TImage.Create(FFlowLayout);
          Img.Width := Match.Groups[2].Value.ToInteger;
          Img.Height := Match.Groups[3].Value.ToInteger;
          Img.WrapMode := TImageWrapMode.Stretch;

          try
            Img.Bitmap.LoadFromFile(Match.Groups[1].Value.Replace('"', ''));
          except
            Img.Bitmap.Clear(TAlphaColors.Red);
          end;

          FFlowLayout.AddObject(Img);
        end;
      end
      // Verifica se é um marcador <p> para espaçamento
      else if AnsiContainsText(TextArray[I], '<p>') then
      begin
        Layout := TLayout.Create(FFlowLayout);
        Layout.Height := 16;
        Layout.Width := 2000;
        FFlowLayout.AddObject(Layout);
      end
      else
      begin
        TextComponent := TText.Create(FFlowLayout);
        ConfigureTextComponent(TextArray[I], TextComponent);
        FFlowLayout.AddObject(TextComponent);
      end;
    end;
  finally
    FFlowLayout.EndUpdate;
  end;
end;

end.

