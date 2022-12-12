# Delphi - iOS File Storage & Sharing
ObjectPascal Delphi	iOS FileStorage	stars	issues	Original in Turkish
While most codes are same, there is a fundamental difference between iOS and Windows: Files in iOS are in a sandbox. You can’t just open a file in “My Documents” and save it in another place in the disk. Actually, you can’t access any file outside the folder where your application is installed. How to deal with the file sandbox is what we are going to cover on the rest of this file.

The document Sandbox
When working in Windows, applications can access almost any file in the hard drive. Which is a nice thing from a usability point of view, but a complete nightmare from a security point of view. Imagine you download an application from the internet, how do you prevent it from encrypting all the documents in your hard drive and then asking for some ransom money in order to decrypt them again?
For this reason, in iOS your application can only read and write to the folder where it is installed or its subfolders. This gives you the added advantage that when you uninstall the app it is gone completely, as it can’t leave garbage all over your hard disk. But on the other side, how do you work with a restriction like this? How do you create a file in Excel, open it with your app (i.e.FlexCel), modify its values and give it back to Excel, if your app and Excel can’t see each other at all?

Sharing Files 
The first way to share things is for special files: Apps can access certain other files such as address book data and photos, but only through APIs specifically designed for that purpose. But this isn’t a general solution, and while it might work for images, it won’t work for xlsx or PDF files.
The solution for more general files comes in 2 parts:
1.	In order to do anything useful with the xlsx, pdf, or html files, you need to Export them to other apps.
2.	To be able to read files from other apps like Dropbox or the email, you need to Import the files from the other apps.
With this Import/Export system, your application can’t open any file that wasn’t given to it. In order to open a file, the user needs to export it to your app.
With this Import/Export system, your application can’t open any file that wasn’t given to it. In order to open a file, the user needs to export it to your app.

A look at some of the available folders for your application
Before we continue, and having established that you can’t write to any folder in the device, let’s look at the folders where you can read or write:
•	<Application_Home>/Documents/ This is where you normally will put your files. Backed up by iTunes.
•	<Application_Home>/Documents/Inbox This is where other apps will put the files they want to share when exporting to your app. Read only. Backed up by iTunes.
•	<Application_Home>/Library/ This is for the files that ship with your app, but not for user files. You could for example put xls/x templates here.
•	<Application_Home>/tmp/ The files you write here might be deleted when your app is not running. Not backed up by iTunes.

Those are at a glance the most important folders you need to know about. You can get a more complete description of the available folders in the Apple documentation.

Importing files from other apps
Registering your app with iOS
In order to be able to interact with files from other applications, you need to register your application as a program that can handle the required file extensions. To do so, you need to modify the file Info.plist in your app bundle.
For handling xls or xlsx files, you would need to add the following to your Info.plist:
<dict>
<key>CFBundleDocumentTypes</key>
<array>
    <dict>
        <key>CFBundleTypeName</key>
        <string>Excel document</string>
        <key>CFBundleTypeRole</key>
        <string>Editor</string>
        <key>LSHandlerRank</key>
        <string>Owner</string>
        <key>LSItemContentTypes</key>
        <array>
            <string>com.microsoft.excel.xls</string>
            <string>com.tms.flexcel.xlsx</string>
            <string>org.openxmlformats.spreadsheetml.sheet</string>
        </array>
    </dict>
</array>

<key>UTExportedTypeDeclarations</key>
 <array>
<dict>
    <key>UTTypeDescription</key>
    <string>Excel xlsx document</string>
    <key>UTTypeTagSpecification</key>
    <dict>
        <key>public.filename-extension</key>
        <string>xlsx</string>
        <key>public.mime-type</key>
        <string>application/vnd.openxmlformats-officedocument.spreadsheetml.sheet</string>
    </dict>
    <key>UTTypeConformsTo</key>
    <array>
        <string>public.data</string>
    </array>
    <key>UTTypeIdentifier</key>
    <string>com.tms.flexcel.xlsx</string>
</dict>
</array>
</dict>
The good news is that Delphi allows you to modify individual keys of Info.plist in the “Version info” section of the project preferences.
But the bad news is that it only allows you to add string keys, and we need to add dictionaries. So we can’t use the built-in system.
We want to keep the Info.plist generated by Delphi (it contains stuff like the version or the application name), but we also want to add our own keys. For that purpose, FlexCel comes with a handy little tool: infoplist.exe
To register a file handler, we need to enter a more complex dictionary. As this is not possible in Delphi at the time of this writing (Delphi 11), we are going to do a workaround.
(The way of merging two xml files into one info.plist by infoplist.exe of FlexCel component “post-build event” is not included in this annotation. If required, you can find step-by-step information on how to register your app in the iOS tutorial.)
When you configure everything, your app will appear in the “Open in” dialog from other applications on your phone.

Answering to an “Open in” event
Once you’ve registered your application as an app that can handle xlsx files, it will appear in the other application’s “Open in” dialogs. When the user clicks in your app icon, iOS will copy the file to <Your app folder>/Documents/Inbox (See “A look at some of the available folders for your application” above).
After that iOS will start your app, and send it an application:OpenUrl: message so you can actually open the file. So in order to do something useful, you will need to listen to application:OpenUrl: event.
In Delphi, you would listen to this event with the following commands:
Initialization code: (Put it in your form create event, or in the initialization of the project)

IFmxApplicationEventService(TPlatformServices.Current.GetPlatformService(IFmxApplicationEventService)).SetApplicationEventHandler(AppHandler);
And then handle the event:
function TFormFlexView.AppHandler(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
 Result := true;

 case AAppEvent of
  TApplicationEvent.aeOpenURL:
  begin
   Result := OpenURL((AContext as TiOSOpenApplicationContext).URL);
  end;
 end;
end;

Note: OpenURL would get the URL of the file (something like “file://localhost/folder...”) instead of a filename. It requires converting the URL into a path by handling OpenUrl event.

Reading the file sent by another application
You can read files by getting their URLs.  
In iOS, we are going to get the URL of the file, not the filename. For example, the URL could be:
'file://localhost/private/var/mobile/Applications/9D16227A-CB01-465D-B8F4-AC43D70C8461/Documents/Inbox/test.xlsx'
And the actual filename would be: ‘/private/var/mobile/Applications/9D16227A-CB01-465D-B8F4-AC43D70C8461/Documents/Inbox/test.xlsx’
But while iOS methods can normally use an URL or a path, Delphi’s TFileStream expects a path. This is why we need to convert the URL to a path, using the GetPhysicalPath function above.
We’ll use internal iOS functions to do the conversion, and so we will define it as:
function GetPhysicalPath(const URL: string): string;
var
 FileName: string;
 FileURL: NSURL;
begin
  FileURL := TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSStr(URL)));
  Result := UTF8ToString(FileURL.path.UTF8String);
end;
Note: Up to Delphi 10.4, in iOS and Android you had Automated Reference Counting (ARC), which means that you didn’t need to call Free on mobile. The above code could have been simplified to remove the xls.Free line and the full try/except, but the original code for Windows will still work. For Delphi 10.4 or newer, mobile platforms don't have ARC anymore, so you need to call Free.
We are using ARC here, so we don’t need to worry about freeing the objects. If this code was for Win32 FlexCel, we would have to free all objects.

Backing up files
A note about the files: Not all of them might need to be backed up, and Apple considers it a reason for App Store rejection if your application is backing up static files (as this will increase backup times and sizes for all users).
If you are using xls or xlsx files as templates for your app, but they aren’t actual data and shouldn’t be backed up, you should use the NSURLIsExcludedFromBackupKey or kCFURLIsExcludedFromBackupKey properties to exclude them from backup.
You can find more information about this topic at: https://developer.apple.com/library/ios/#qa/qa1719/_index.html

Other ways to share files
Besides exporting and importing files, there are two other ways in how you can get files from and to your application:

iTunes file sharing
Your application can offer “Share in iTunes” functionality. To allow it, you need to add the key:
<key>UIFileSharingEnabled</key>
    <true/>
To your Info.plist file. Once you add this entry, your app will appear in iTunes and the user will be able to read and write documents from the “Documents” folder of it. The interface is kind of primitive, but it gets the work done. 
Note: Again as in the case of registering the application to consume some file types, the Delphi IDE doesn’t allow you to do it directly. You need to add a Boolean key, and Delphi will only add string keys. So, again you will have to create a different Info.plist and merge it, as we did in “Registering your app with iOS”. If you are already doing so to registering files to import, then you can use that same file to add this entry.
Warning: If you decide to enable iTunes sharing for your app, make sure that the documents in the “Documents” folder are actual documents the user cares about, and put the others somewhere else, like in “Library”. Failing to do so can result in a rejection from the App store. (as you can see here: http://stackoverflow.com/questions/10767517/rejected-by-uifilesharingenabled-key-set-to-true )

Delphi’s Project -> Deployment
The last way to put files in your app is to use the “Menu->Project->Deployment” option in Delphi.
Note that you can only put files inside your app bundle, you can’t put a file directly in the “Documents” folder. Because you will not be uploading the Documents folder to the app store; you will upload only the app bundle.
If your application is called “MyApp” and the folders look something like this:
https://doc.tmssoftware.com/flexcel/vcl/images/ios-document-model.svg
Then you can only put files in the green folders in the diagram. This is because “MyApp.app” is what will be distributed to the App store, and what your users will download when they download your app.
So, how do we put a file on the blue folders in our distribution package? The usual technique is iOS is to copy them to some folder inside MyApp.app, and on startup of your application, copy those files to “/Documents” or “/Library”
But Delphi has this functionality built it, so you don’t need to worry about writing the code to copy the files on startup. If you look at your project source (Right click your app in Delphi’s project explorer, then choose “View source”), you will see that the code is as follows:
program IOSTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  ...
Where the first unit your project uses is named “System.StartUpCopy”. If you put the cursor on the line and press ctrl-enter to open it, you will see that this unit just looks for a folder in “YourApp.App/StartUp” and copies all files and folders to root.** So, if you want to deploy to “/Documents”, you should deploy to “StartUp/Documents” instead. To deploy to “/Library”, deploy to “StartUp/Library”, and so on. Those files will be copied inside your app bundle, and when your app starts, they will be copied to the root.
Important
IMPORTANT: On the device, filenames are Case Sensitive. So you must deploy exactly to “StartUp/Documents”. If you deploy for example to “Startup/Documents” Then those files will be copied to YourApp.App/Startup/Documents, but Delphi won’t copy them to /Documents when your app starts.

 
* Article above annotated from the source: TMS Software FlexCel iOS Guide and iOS Tutorial

** P.S. In spite of the statement, iOS places the files from “/StartUp/” to application home path in sandbox.

Registering File Type & Altering info.plist For Importing
In above article, the way altering info.plist.xml file (special for FlexCel component) is not applicable in general and deducted. Instead, we should do it manually. 

Steps for registering file type to be imported: 
1.       Decide a file extension to be imported. I.e. .dnm, .xls, .xlsx, .sheet, .txt, … 
2.       Edit info.plist.TemplateiOS.xml file
3.       Among the present lines
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
<%VersionInfoPListKeys%>
<%ExtraInfoPListKeys%>
<%StoryboardInfoPListKey%>
</dict>
</plist>
insert the script below above </dict> line at the bottom: 
<key>CFBundleDocumentTypes</key>
<array>
	<dict>
		<key>CFBundleTypeName</key>
		<string>iOSDosyaDepolamaPaylasma dosyasi</string>
		<key>CFBundleTypeRole</key>
		<string>Editor</string>
		<key>LSHandlerRank</key>
		<string>Owner</string>
		<key>LSItemContentTypes</key>
		<array>
			<string>com.delphican.iOSDosyaDepolamaPaylasma.dnm</string>
		</array>
	</dict>
</array>
<key>UTExportedTypeDeclarations</key>
 <array>
	<dict>
		<key>UTTypeDescription</key>
		<string>DNM dosyasi</string>
		<key>UTTypeTagSpecification</key>
		<dict>
			<key>public.filename-extension</key>
			<string>dnm</string>
			<key>public.mime-type</key>
			<string>application/vnd.sqlite3</string>
		</dict>
		<key>UTTypeConformsTo</key>
		<array>
			<string>public.data</string>
		</array>
		<key>UTTypeIdentifier</key>
		<string>com.delphican.iOSDosyaDepolamaPaylasma.dnm</string>
	</dict>
</array>	
 
4.       Buradaki bilgileri ihtiyacınıza göre düzenlemesiniz. 
CFBundleDocumentTypes: The document types supported by the bundle (CFBundle). 
CFBundle: allows you to use a folder hierarchy called a bundle to organize and locate many types of application resources including images, sounds, localized strings, and executable code. In macOS, bundles can also be used by CFM applications to load and execute functions from Mach-O frameworks. You can use bundles to support multiple languages or execute your application on multiple operating environments.
CFM: Code Fragment Manager MacOS memory management system. Responsible for handling system calls from applications running on the Mac.
LS Launch Services: Launch and open documents in other apps from your current app process.. 
UTExportedTypeDeclarations: The uniform type identifiers owned and exported by the app.  System-Declared Uniform Type Identifiers
UTI Uniform Type Identifiers framework provides a collection of common types that map to MIME and file types. Use these types in your project to describe the file types in your app. These descriptions help the system properly handle file storage formats or in-memory data for transfer — for example, transferring data to or from the pasteboard. The identifier types can also identify other resources, such as directories, volumes, or packages 
public.mime-type: https://mimetype.io/ 
 
CFBundleDocumentTypes 
CFBundleTypeName: Description about file type (iOSDosyaDepolamaPaylasma file, Excel document, …)
CFBundleTypeRole: Editor, Viewer, Shell, QLGenerator, None. Use “Editor” for editing files and “Viewer” for temporarly importing files as read-only. 
LSHandlerRank: Owner, Default, Alternate, None. “Owner” is used.
LSItemContentTypes:  One or more file types can be defined in <array><string></string></array> block. (com.delphican.iOSDosyaDepolamaPaylasma.dnm, com.microsoft.excel.xls, com.tms.flexcel.xlsx, org.openxmlformats.spreadsheetml.sheet)
UTExportedTypeDeclarations
UTTypeDescription: (Extension) full file type name. 
UTTypeTagSpecification: Multiple types can be defined in Array. “public.filename-extension” is used.
Extensions of files to be imported are added in “public.filename-extension” key. ( .dnm, .xls, .xlsx, .sheet, .txt, …)
UTTypeConformsTo: Multiple types can be defined in Array. “public.data” (, public.database, …)
UTTypeIdentifier: com.company.project.extension (com.delphican.iOSDosyaDepolamaPaylasma.dnm, com.tms.flexcel.xlsx, …)
 
5.       Check your project compiling errors and projectname. info.plist file in iOSDevice64\Debug or iOSDevice64\Release folders. 
6.       iOS will handle importing files by using file type details in info.plist. You can test importing by sending an email or Whatsapp, etc. message with “.dnm” extension file to your device. If your app is not visible in Open in menu, it means something wrong in info.plist.TemplateiOS.xml. 
 
________________________________________

Exporting and Sharing Files To Other Apps

Exporting files to other apps is the reverse of what we’ve seen in the previous section: Now we want to show a dialog where we show the user all the applications that can open the file we generated.
In iOS, both exporting and sharing files are same way. In file sharing interface menu “Copy, Add to New Quick Note, Save to Files, Add Tag” options are shown. When “Save to Files” is selected, “Browse” interface shows Locations (On My iPhone, iCloud Drive, Downloads, …) folders where files can be saved. Only user can reach those file by Files app. Our apps cannot reach the files even thought they save them.  
Delphi demo \Samples\Object Pascal\Mobile Snippets\ShareSheet project represents ability of taking sharing photos. No file exporting and sharing features exist.

“Kastri“Cross-platform library, prepared by Australian Delphi Worlds community, supports a number of newer APIs that you won't find in FMX/RTL, and "backfills" for missing APIs of RAD Studio Delphi. 
“ShareItems” support is intended as an alternative for ShareSheet actions in Delphi. It allows to share text, images, and files, and the number being shared is restricted only by the OS. 
Create an instance of TShareItems and call one of AddFile, AddImage or AddText for each item to be shared.
Call the Share method to share the items that have been added. On iOS, you can restrict which activities can be shared to by using the AExcludedActivities parameter. At present these are:
•	Facebook
•	Twitter
•	Message
•	Mail
•	Printer
•	Pasteboard
•	Contacts
•	Camera Roll
•	Reading List
•	Flickr
•	Vimeo
•	Weibo
•	Tencent Weibo
•	AirDrop
•	IBooks
•	PDF
If you wish to know the result of sharing items, assign a handler for the OnShareCompleted property. 
Kastri deserves donation from professionals: https://github.com/DelphiWorlds/Kastri “Donate” link
Source: Kastri and ShareItems
 
To enable “ShareItems” support, download the Code as zip file from https://github.com/DelphiWorlds/Kastri
Save the search folders: 
    Project > Options > Building > Delphi Compiler > Search path :
    C:\Kastri-master\API;C:\Kastri-master\Core;C:\Kastri-master\Include;C:\Kastri-master\Features;C:\Kastri-master\Features\ShareItems

Initialization code: (Put it in your form create event, or in the initialization of the project)
FShareItems := TShareItems.Create;
FShareItems.OnShareCompleted := ShareItemsShareCompletedHandler;

Exporting & Sharing:
procedure TForm1.ButtonTextFileCopyFromInternalToExternalShareClick
 (Sender: TObject);
begin
 FShareItems.AddFile(TPath.Combine(yol, 'TextFile.txt'));
 FShareItems.Share(ButtonTextFileCopyFromInternalToExternalShare, FExcluded);
end;

Handling Result:
procedure TForm1.ShareItemsShareCompletedHandler(Sender: TObject; const AActivity: TShareActivity; const AError: string);
begin
  // If AActivity is TShareActivity.None, then the user cancelled 
  if AActivity = TShareActivity.None then
    ShowMessage('Share cancelled')
  else
    ShowMessage('Share completed');
end;

________________________________________
 
File System Programming Guide https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html#//apple_ref/doc/uid/TP40010672-CH2-SW4
 
iOS Folders - Delphi Function Results
 
GetHomePath: /private/var/mobile/Containers/Data/Application/D232D883-5618-42FB-AC55-84A26D4FEF10
GetDocumentsPath: /var/mobile/Containers/Data/Application/D232D883-5618-42FB-AC55-84A26D4FEF10/Documents
GetCachePath: /var/mobile/Containers/Data/Application/D232D883-5618-42FB-AC55-84A26D4FEF10/Library/Caches
GetLibraryPath: /var/mobile/Containers/Data/Application/D232D883-5618-42FB-AC55-84A26D4FEF10/Library
GetTempPath: /private/var/mobile/Containers/Data/Application/D232D883-5618-42FB-AC55-84A26D4FEF10/tmp
GetPublicPath: 
GetPicturesPath: 
GetCameraPath: 
GetMusicPath: 
GetMoviesPath: 
GetAlarmsPath: 
GetRingtonesPath: 
GetDownloadsPath: 
GetSharedDocumentsPath: 
GetSharedDownloadsPath: 
GetSharedCameraPath: 
GetSharedAlarmsPath: 
GetSharedPicturesPath: 
GetSharedMusicPath: 
GetSharedMoviesPath: 
GetSharedRingTonesPath: 
 
(Project > Deployment > Remote Path : \StartUp\Documents\  = GetDocumentsPath)
 
* GetHomePath, GetDocumentsPath, GetCachePath, GetLibraryPath, GetTempPath are Internal Storage folders and we can read their files only. External shared folders are out of sandbox reach and returns void. As a result, we have only Internal Storage and we cannot reach External Storage in iOS.

* Delphi 11.2 Architect Trial, MacOS 12 Monterey, iOS 16.1.1 used. Also no problem in writing iOS apps by AMD processor PCs and VirtualBox, nevertheless settings take time. To connect iPhone device by VirtualBox MacOS, open Finder. Connect Usb cable to iPhone. Settings > Cellular > Personal Hotspot turn on Allow Others to Join. Wait till notification tone. Turn off Allow Others to Join in Personal Hotspot, wait about half a minute and check device visible in Finder. Then, PAServer on, iPhone device name will appear next to iOS Device 64-bit in Delphi.
