program talaria;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, StatusFrame, ChatFrame, LinkInfoListFrame, dnmp_unit,
  dnmp_services, Core, DnmpNodeFrame, Misc, DnmpServicesFrame, laz_synapse,
  ConfigFrame, GrpcServiceFrame, MailboxFrame, dnmp_mail, adapters,
  ContactListFrame, LinkListFrame, LinkInfoFrame, dnmp_serializers, dnmp_grpc,
  ChatRoomListFrame, dnmp_auth, DnmpWizardFrame, dnmp_chat, DnmpChatFrame,
  dnmp_info;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormDnmpWizard, FormDnmpWizard);
  Application.Run;
end.

