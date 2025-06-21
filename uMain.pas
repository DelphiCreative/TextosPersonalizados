unit uMain;

interface

uses
   Winapi.ShellAPI,  Winapi.Windows, Math,
   System.Generics.Collections, RegularExpressions, System.StrUtils,
   System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
   FMX.Types,FMX.Objects, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
   FMX.Controls.Presentation, FMX.Edit, FMX.SearchBox, FMX.Layouts, FMX.StdCtrls,
   FMX.Effects, FMX.ScrollBox, FMX.Memo, FMX.EditBox, FMX.SpinBox,
  FMX.Memo.Types;

type
  TShapeHelper = class helper for TShape
    procedure TextoCustomizado(Texto:String;FontSize :Real = 10);
    procedure ApplyCustomText(const Text: String; FontSize: Real = 10);
    procedure ApplyCustomText2(const Text: String; FontSize: Real = 10);
    procedure ClickURL(Sender:TObject);
end;

type
  TFlowLayoutHelper = class helper for TFlowLayout

    procedure ApplyMarkDown(const Text: String; FontSize: Real = 10);

    procedure ApplyCustomText(const Text: String; FontSize: Real = 10);

  end;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Rectangle1: TRectangle;
    SpinBox1: TSpinBox;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    FlowLayout1: TFlowLayout;
    procedure Memo1ChangeTracking(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
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
   //FlowLayout1.ApplyMarkDown(Memo1.Lines.Text,SpinBox1.Value);

   FlowLayout1.ApplyCustomText(Memo1.Lines.Text,SpinBox1.Value);

end;

procedure TForm1.SpinBox1Change(Sender: TObject);
begin

end;

{ TShapeHelper }

procedure TShapeHelper.ApplyCustomText(const Text: String; FontSize: Real = 10);
var
  TextArray: TArray<String>;
  TextComponent: TText;
  FlowLayout: TFlowLayout;
  Layout: TLayout;
  I: Integer;
  Img: TImage;
  Match: TMatch;

  // Procedimento para configurar o componente de texto
  procedure ConfigureTextComponent(const TextContent: String; Component: TText);
  begin
    Component.Text := TextContent;
    Component.TextSettings.FontColor := TAlphaColors.Black;
    Component.TextSettings.Font.Size := FontSize;
    Component.Height := 16;
    Component.AutoSize := True;
    Component.TextSettings.HorzAlign := TTextAlign.Center;
    Component.TextSettings.VertAlign := TTextAlign.Center;
    Component.Width := 1000;
    Component.AutoSize := True;

    // Aplica estilos com base nos marcadores
    if AnsiContainsText(TextContent, '<b>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsBold];
    if AnsiContainsText(TextContent, '<i>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsItalic];
    if AnsiContainsText(TextContent, '<s>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsUnderline];
    if AnsiContainsText(TextContent, '<r>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsStrikeOut];

    // Estilo de cor
    if AnsiContainsText(TextContent, '<red>') then
      Component.TextSettings.FontColor := TAlphaColors.Red
    else if AnsiContainsText(TextContent, '<green>') then
      Component.TextSettings.FontColor := TAlphaColors.Green
    else if AnsiContainsText(TextContent, '<yellow>') then
      Component.TextSettings.FontColor := TAlphaColors.Yellow
    else if AnsiContainsText(TextContent, '<blue>') then
      Component.TextSettings.FontColor := TAlphaColors.Royalblue;

    // Remove os marcadores do texto final
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

begin
  // Limpa componentes existentes
  Self.BeginUpdate;
  for I := Self.ComponentCount - 1 downto 0 do
    Self.Components[I].DisposeOf;

  // Cria o FlowLayout
  FlowLayout := TFlowLayout.Create(Self);
  FlowLayout.Align := TAlignLayout.Client;

  // Divide o texto em palavras
  TextArray := TRegEx.Split(Text, ' ');

  // Cria componentes para cada palavra
  for I := 0 to High(TextArray) do
  begin
    // Verifica se é uma imagem com tamanho especificado no formato <img=path,width,height>
    Match := TRegEx.Match(TextArray[I], '<img=([^,]+),(\d+),(\d+)>');
    if Match.Success then
    begin
     if FileExists(Match.Groups[1].Value.Replace('"','')) then begin
        Img := TImage.Create(FlowLayout);
        Img.Width := Match.Groups[2].Value.ToInteger;
        Img.Height := Match.Groups[3].Value.ToInteger;
        Img.WrapMode := TImageWrapMode.Stretch;

        try
          // Carrega a imagem do caminho especificado
          Img.Bitmap.LoadFromFile(Match.Groups[1].Value.Replace('"',''));
        except
          on E: Exception do
          begin
            Img.Bitmap.Clear(TAlphaColors.Red); // Substituir por cor de erro
          end;
        end;
        FlowLayout.AddObject(Img);
      end;
    end
    // Verifica se a palavra é um marcador <p>
    else if AnsiContainsText(TextArray[I], '<p>') then
    begin
      Layout := TLayout.Create(FlowLayout);
      Layout.Height := 16;
      Layout.Width := 2000;
      FlowLayout.AddObject(Layout);
    end
    // Caso contrário, trata como texto
    else
    begin
      TextComponent := TText.Create(FlowLayout);
      ConfigureTextComponent(TextArray[I], TextComponent);
      FlowLayout.AddObject(TextComponent);
    end;
  end;

  Self.AddObject(FlowLayout);
  Self.EndUpdate;
end;

procedure TShapeHelper.ApplyCustomText2(const Text: String; FontSize: Real = 10);
var
  TextArray: TArray<String>;
  TextComponent: TText;
  FlowLayout: TFlowLayout;
  Layout: TLayout;
  I: Integer;

  procedure ConfigureTextComponent(const TextContent: String; Component: TText);
  begin
    Component.Text := TextContent;
    Component.TextSettings.FontColor := TAlphaColors.Black;
    Component.TextSettings.Font.Size := FontSize;
    Component.Height := 16;
    Component.AutoSize := True;
    Component.TextSettings.HorzAlign := TTextAlign.Center;
    Component.TextSettings.VertAlign := TTextAlign.Center;
    Component.Width := 1000;
    Component.AutoSize := True;

    // Aplica estilos com base nos marcadores
    if AnsiContainsText(TextContent, '<b>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsBold];
    if AnsiContainsText(TextContent, '<i>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsItalic];
    if AnsiContainsText(TextContent, '<s>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsUnderline];
    if AnsiContainsText(TextContent, '<r>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsStrikeOut];

    // Estilo de cor
    if AnsiContainsText(TextContent, '<red>') then
      Component.TextSettings.FontColor := TAlphaColors.Red
    else if AnsiContainsText(TextContent, '<green>') then
      Component.TextSettings.FontColor := TAlphaColors.Green
    else if AnsiContainsText(TextContent, '<yellow>') then
      Component.TextSettings.FontColor := TAlphaColors.Yellow
    else if AnsiContainsText(TextContent, '<blue>') then
      Component.TextSettings.FontColor := TAlphaColors.Royalblue;

    // Configuração para URLs
    if AnsiContainsText(TextContent, 'www.') then
    begin
      Component.TextSettings.FontColor := TAlphaColors.Steelblue;
      Component.Cursor := crHandPoint;
//      Component.OnClick := HandleURLClick;
    end;

    // Remove os marcadores do texto final
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

begin
  // Limpa componentes existentes
  Self.BeginUpdate;
  for I := Self.ComponentCount - 1 downto 0 do
    Self.Components[I].DisposeOf;

  // Cria o FlowLayout
  FlowLayout := TFlowLayout.Create(Self);
  FlowLayout.Align := TAlignLayout.Client;

  // Divide o texto em palavras
  TextArray := TRegEx.Split(Text, ' ');

  // Cria componentes para cada palavra
  for I := 0 to High(TextArray) do
  begin
    if AnsiContainsText(TextArray[I], '<p>') then
    begin
      Layout := TLayout.Create(FlowLayout);
      Layout.Height := 16;
      Layout.Width := 2000;
      FlowLayout.AddObject(Layout);
    end;

    TextComponent := TText.Create(FlowLayout);
    ConfigureTextComponent(TextArray[I], TextComponent);
    FlowLayout.AddObject(TextComponent);
  end;

  Self.AddObject(FlowLayout);
  Self.EndUpdate;
end;


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

      if AnsiContainsText(arr[i],'*') then
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

{ TFlowLayoutHelper }

procedure TFlowLayoutHelper.ApplyCustomText(const Text: String; FontSize: Real);
var
  TextArray: TArray<String>;
  TextComponent: TText;
  FlowLayout: TFlowLayout;
  Layout: TLayout;
  I: Integer;
  Img: TImage;
  Match: TMatch;

  // Procedimento para configurar o componente de texto
  procedure ConfigureTextComponent(const TextContent: String; Component: TText);
  begin
    Component.Text := TextContent;
    Component.TextSettings.FontColor := TAlphaColors.Black;
    Component.TextSettings.Font.Size := FontSize;
    Component.Height := 16;
    Component.AutoSize := True;
    Component.TextSettings.HorzAlign := TTextAlign.Center;
    Component.TextSettings.VertAlign := TTextAlign.Center;
    Component.Width := 1000;
    Component.AutoSize := True;

    // Aplica estilos com base nos marcadores
    //if AnsiContainsText(TextContent, '*') then

    if TextContent.StartsWith('*') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsBold];
    if AnsiContainsText(TextContent, '<i>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsItalic];
    if AnsiContainsText(TextContent, '<s>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsUnderline];
    if AnsiContainsText(TextContent, '<r>') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsStrikeOut];

    // Estilo de cor
    if AnsiContainsText(TextContent, '<red>') then
      Component.TextSettings.FontColor := TAlphaColors.Red
    else if AnsiContainsText(TextContent, '<green>') then
      Component.TextSettings.FontColor := TAlphaColors.Green
    else if AnsiContainsText(TextContent, '<yellow>') then
      Component.TextSettings.FontColor := TAlphaColors.Yellow
    else if AnsiContainsText(TextContent, '<blue>') then
      Component.TextSettings.FontColor := TAlphaColors.Royalblue;

    // Remove os marcadores do texto final
    Component.Text := TextContent
      .Replace('<red>', '')
      .Replace('<green>', '')
      .Replace('<yellow>', '')
      .Replace('<blue>', '')
      .Replace('*', '')
      .Replace('<i>', '')
      .Replace('<s>', '')
      .Replace('<r>', '')
      .Replace('<p>', '');
  end;

begin
  // Limpa componentes existentes
  Self.BeginUpdate;
  for I := Self.ComponentCount - 1 downto 0 do
    Self.Components[I].DisposeOf;

  // Divide o texto em palavras
  TextArray := TRegEx.Split(Text.Replace('  ',' '), ' ');

  // Cria componentes para cada palavra
  for I := 0 to High(TextArray) do
  begin
    // Verifica se é uma imagem com tamanho especificado no formato <img=path,width,height>
    Match := TRegEx.Match(TextArray[I], '<img=([^,]+),(\d+),(\d+)>');
    if Match.Success then
    begin
     if FileExists(Match.Groups[1].Value.Replace('"','')) then begin
        Img := TImage.Create(Self);
        Img.Width := Match.Groups[2].Value.ToInteger;
        Img.Height := Match.Groups[3].Value.ToInteger;
        Img.WrapMode := TImageWrapMode.Stretch;

        try
          // Carrega a imagem do caminho especificado
          Img.Bitmap.LoadFromFile(Match.Groups[1].Value.Replace('"',''));
        except
          on E: Exception do
          begin
            Img.Bitmap.Clear(TAlphaColors.Red); // Substituir por cor de erro
          end;
        end;
        Self.AddObject(Img);
      end;
    end
    // Verifica se a palavra é um marcador <p>
    else if AnsiContainsText(TextArray[I], '<p>') then
    begin
      Layout := TLayout.Create(Self);
      Layout.Height := 16;
      Layout.Width := 2000;
      Self.AddObject(Layout);
    end
    // Caso contrário, trata como texto
    else
    begin
      TextComponent := TText.Create(Self);
      ConfigureTextComponent(TextArray[I], TextComponent);
      Self.AddObject(TextComponent);
    end;
  end;


  Self.EndUpdate;


end;

procedure TFlowLayoutHelper.ApplyMarkDown(const Text: String; FontSize: Real);
var
  TextArray: TArray<String>;
  TextComponent: TText;
  Layout: TLayout;
  I: Integer;
  Img: TImage;
  Match: TMatch;

  // Procedimento para configurar o componente de texto
  procedure ConfigureTextComponent(const TextContent: String; Component: TText);
  begin
    Component.Text := TextContent;
    Component.TextSettings.FontColor := TAlphaColors.Black;
    Component.TextSettings.Font.Size := FontSize;
    Component.Height := 16;
    Component.AutoSize := True;
    Component.TextSettings.HorzAlign := TTextAlign.Center;
    Component.TextSettings.VertAlign := TTextAlign.Center;
    Component.Width := 1000;

    // **Aplicar estilos Markdown**
    if TextContent.StartsWith('**') and TextContent.EndsWith('**') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsBold];

    if TextContent.StartsWith('*') and TextContent.EndsWith('*') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsItalic];

    if TextContent.StartsWith('~~') and TextContent.EndsWith('~~') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsStrikeOut];

    if TextContent.StartsWith('__') and TextContent.EndsWith('__') then
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsUnderline];

    // **Detecta títulos**
    if TextContent.StartsWith('#') then
    begin
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsBold];
      Component.TextSettings.Font.Size := FontSize * 1.5; // Aumenta tamanho
      Component.Text := TextContent.Remove(0, 1);
    end
    else if TextContent.StartsWith('##') then
    begin
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsBold];
      Component.TextSettings.Font.Size := FontSize * 1.3;
      Component.Text := TextContent.Remove(0, 3);
    end
    else if TextContent.StartsWith('###') then
    begin
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsBold];
      Component.TextSettings.Font.Size := FontSize * 1.1;
      Component.Text := TextContent.Remove(0, 4);
    end;

    // **Detecta citação**
    if TextContent.StartsWith('> ') then
    begin
      Component.TextSettings.FontColor := TAlphaColors.Gray;
      Component.TextSettings.Font.Style := Component.TextSettings.Font.Style + [TFontStyle.fsItalic];
      Component.Text := '“' + TextContent.Remove(0, 2) + '”';
    end;

    // **Detecta código inline**
    if TextContent.StartsWith('`') and TextContent.EndsWith('`') then
    begin
      Component.TextSettings.Font.Style := [TFontStyle.fsBold];
      Component.TextSettings.Font.Family := 'Courier New';
      Component.TextSettings.FontColor := TAlphaColors.Darkgray;
      Component.Text := TextContent.Replace('`', '');
    end;

    // **Detecta cores**
    if TextContent.Contains('<red>') then
      Component.TextSettings.FontColor := TAlphaColors.Red
    else if TextContent.Contains('<green>') then
      Component.TextSettings.FontColor := TAlphaColors.Green
    else if TextContent.Contains('<yellow>') then
      Component.TextSettings.FontColor := TAlphaColors.Yellow
    else if TextContent.Contains('<blue>') then
      Component.TextSettings.FontColor := TAlphaColors.Royalblue;

    // **Remove os marcadores**
    Component.Text := Component.Text
      .Replace('**', '')
      .Replace('*', '')
      .Replace('~~', '')
      .Replace('__', '')
      .Replace('<red>', '')
      .Replace('<green>', '')
      .Replace('<yellow>', '')
      .Replace('<blue>', '');
  end;

begin
  // Limpa componentes existentes
  Self.BeginUpdate;
  for I := Self.ComponentCount - 1 downto 0 do
    Self.Components[I].Free;

  // Divide o texto em palavras
  TextArray := TRegEx.Split(Text, ' ');

  // Cria componentes para cada palavra
  for I := 0 to High(TextArray) do
  begin
    // **Verifica se é uma imagem no formato Markdown**
    Match := TRegEx.Match(TextArray[I], '!\[.*\]\(([^=)]+)(?:=(\d+),(\d+))?\)');
    if Match.Success then
    begin
      if FileExists(Match.Groups[1].Value) then
      begin
        Img := TImage.Create(Self);

        // Definir tamanho da imagem (se especificado)
        if Match.Groups[2].Success and Match.Groups[3].Success then
        begin
          Img.Width := Match.Groups[2].Value.ToInteger;
          Img.Height := Match.Groups[3].Value.ToInteger;
        end
        else
        begin
          Img.Width := 50;  // Tamanho padrão
          Img.Height := 50;
        end;

        Img.WrapMode := TImageWrapMode.Stretch;
        try
          Img.Bitmap.LoadFromFile(Match.Groups[1].Value);
        except
          Img.Bitmap.Clear(TAlphaColors.Red); // Indica erro de carregamento
        end;
        Self.AddObject(Img);
      end;
    end
    // **Verifica se é uma quebra de linha Markdown (`---`)**
    else if TextArray[I] = '---' then
    begin
      Layout := TLayout.Create(Self);
      Layout.Height := 16;
      Layout.Width := 2000;
      Self.AddObject(Layout);
    end
    // **Caso contrário, trata como texto formatado**
    else
    begin
      TextComponent := TText.Create(Self);
      ConfigureTextComponent(TextArray[I], TextComponent);
      Self.AddObject(TextComponent);
    end;
  end;

  Self.EndUpdate;
end;


end.
