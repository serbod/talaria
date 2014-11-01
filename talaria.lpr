program talaria;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, StatusFrame, ChatFrame, dnmp_unit,
  dnmp_services, Core, DnmpNodeFrame, Misc, DnmpServicesFrame, laz_synapse,
  ConfigFrame, GrpcServiceFrame, MailboxFrame, dnmp_mail, adapters,
  LinkListFrame, LinkInfoFrame, dnmp_serializers, dnmp_grpc,
  ChatRoomListFrame, dnmp_auth, DnmpWizardFrame, dnmp_chat, DnmpChatFrame,
  dnmp_info, DnmpContactsFrame, dnmp_filebox;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

