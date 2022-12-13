unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Objects, FMX.Platform, FMX.Platform.iOS,  DW.ShareItems;
  (* https://github.com/DelphiWorlds/Kastri sitesinden kod zip olarak indirilip,
    Project > Options > Building > Delphi Compiler > Search path :
    C:\Kastri-master\API;C:\Kastri-master\Core;C:\Kastri-master\Include;C:\Kastri-master\Features;C:\Kastri-master\Features\ShareItems
    dizinleri kaydedilmelidir *)

type
  TForm1 = class(TForm)
    ButtonDahiliyeDosyaKaydet: TButton;
    ButtonDahilidenHariciyeKopyalaPaylasMetin: TButton;
    Memo1: TMemo;
    ButtonDosyalarCetveli: TButton;
    ButtonDahiliDosyalariSil: TButton;
    ButtonDahilidenHariciyeKopyalaPaylasPdf: TButton;
    ButtonResimPaylas: TButton;
    ImageControl1: TImageControl;
    procedure ButtonDosyalarCetveliClick(Sender: TObject);
    procedure ButtonDahiliyeDosyaKaydetClick(Sender: TObject);
    procedure ButtonDahiliDosyalariSilClick(Sender: TObject);
    procedure ButtonDahilidenHariciyeKopyalaPaylasMetinClick(Sender: TObject);
    procedure ButtonDahilidenHariciyeKopyalaPaylasPdfClick(Sender: TObject);
    procedure ButtonResimPaylasClick(Sender: TObject);
  private
    FExcluded: TShareActivities;
    FShareItems: TShareItems;
    procedure ShareItemsShareCompletedHandler(Sender: TObject;
      const AActivity: TShareActivity; const AError: string);
    function AppHandler(AAppEvent: TApplicationEvent;
      AContext: TObject): Boolean;
    function OpenFile(const aURL: string): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;
  yol: string;

implementation

{$R *.fmx}

uses
  System.IOUtils, Macapi.Helpers, iOSapi.Foundation, Posix.Unistd;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  // Paylaþýrken hariç tutulabilecekler: (Denemek için alt satýr açýlabilir)
  // FExcluded := TShareItems.AllShareActivities - [TShareActivity.Message, TShareActivity.Mail, TShareActivity.CopyToPasteboard];
  FShareItems := TShareItems.Create;
  FShareItems.OnShareCompleted := ShareItemsShareCompletedHandler;
  IFmxApplicationEventService(TPlatformServices.Current.GetPlatformService
    (IFmxApplicationEventService)).SetApplicationEventHandler(AppHandler);
  yol := TPath.GetDocumentsPath;
end;

destructor TForm1.Destroy;
begin
  FShareItems.Free;
  inherited;
end;

procedure TForm1.ShareItemsShareCompletedHandler(Sender: TObject;
  const AActivity: TShareActivity; const AError: string);
begin
  if AActivity = TShareActivity.None then
    ShowMessage('Paylaþma iptal edildi')
  else
    ShowMessage('Paylaþma tamamlandý');
end;

procedure TForm1.ButtonDahiliDosyalariSilClick(Sender: TObject);
var
  FileList: TStringDynArray;
  s: string;
begin
  DeleteFile(TPath.Combine(yol, 'Metin.txt'));
  try
    FileList := TDirectory.GetFiles(TPath.Combine(yol, 'Inbox'));
    for s in FileList do
      DeleteFile(s);
  except
    Memo1.Lines.Add('Hata! Inbox sil');
  end;
end;

procedure TForm1.ButtonResimPaylasClick(Sender: TObject);
begin
  FShareItems.AddText('Paylaþ bakalým');
  FShareItems.AddImage(ImageControl1.Bitmap);
  FShareItems.Share((Sender as TButton), FExcluded);
end;

procedure TForm1.ButtonDahilidenHariciyeKopyalaPaylasMetinClick
  (Sender: TObject);
begin
  FShareItems.AddFile(TPath.Combine(yol, 'Metin.txt'));
  FShareItems.Share(ButtonDahilidenHariciyeKopyalaPaylasMetin, FExcluded);
end;

procedure TForm1.ButtonDahilidenHariciyeKopyalaPaylasPdfClick(Sender: TObject);
begin
  FShareItems.AddFile(TPath.Combine(yol, 'delphican.pdf'));
  FShareItems.Share((Sender as TButton), FExcluded);
end;

procedure TForm1.ButtonDahiliyeDosyaKaydetClick(Sender: TObject);
begin
  Memo1.Lines.Add(TimeToStr(Time) + '''de kaydedildi');
  Memo1.Lines.SaveToFile(TPath.Combine(yol, 'Metin.txt'));
end;

procedure TForm1.ButtonDosyalarCetveliClick(Sender: TObject);
var
  FileList: TStringDynArray;
  s: string;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add(sLineBreak + '- Application_Home -');
  FileList := TDirectory.GetFiles(TPath.GetHomePath);
  for s in FileList do
    Memo1.Lines.Add(s);

  Memo1.Lines.Add(sLineBreak + '- <Application_Home>/Documents/ -');
  FileList := TDirectory.GetFiles(yol);
  for s in FileList do
    Memo1.Lines.Add(s);

  Memo1.Lines.Add(sLineBreak + '- <Application_Home>/Library/ -');
  FileList := TDirectory.GetFiles(TPath.Combine(TPath.GetHomePath, 'Library'));
  for s in FileList do
    Memo1.Lines.Add(s);

  Memo1.Lines.Add(sLineBreak + '- <Application_Home>/tmp/ -');
  FileList := TDirectory.GetFiles(TPath.Combine(TPath.GetHomePath, 'tmp'));
  for s in FileList do
    Memo1.Lines.Add(s);

  Memo1.Lines.Add(sLineBreak + '- <Application_Home>/Documents/Inbox -');
  try
    FileList := TDirectory.GetFiles(TPath.Combine(yol, 'Inbox'));
    for s in FileList do
      Memo1.Lines.Add(s);
  except
    Memo1.Lines.Add('Hata! Inbox dizini bulunamadý');
  end;
end;

function GetPhysicalPath(const URL: string): string;
var
  FileURL: NSURL;
begin
{$IF CompilerVersion >= 27.0}
  FileURL := TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(URL)));
{$ELSE}
  FileURL := TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSStr(URL)));
{$ENDIF}
  Result := UTF8ToString(FileURL.path.UTF8String);
end;

function TForm1.AppHandler(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  Result := true;
  case AAppEvent of
    TApplicationEvent.OpenURL:
      begin
        Result := true;
        // ShowMessage('AppHandler: ' + (AContext as TiOSOpenApplicationContext).URL);
        OpenFile(GetPhysicalPath((AContext as TiOSOpenApplicationContext).URL));
      end;
  end;
end;

function TForm1.OpenFile(const aURL: string): Boolean;
var
  dosya: String;
begin
  Result := true;
  try
    try
      dosya := GetPhysicalPath(aURL);
    finally
      Memo1.Lines.LoadFromFile(dosya);
      // TFile.Delete(dosya);  // Dosyayý okuduk. Þimdi silmemiz lazým, yoksa inbox içinde hep kalacak. Çalýþmasa bile silinmeli
    end;
  except
    Result := false;
    Memo1.Lines.Add('Hata! OpenFile');
  end;
  Memo1.Lines.Add(sLineBreak + 'Ýçe alýnan dosya: ' + dosya);
end;

end.
