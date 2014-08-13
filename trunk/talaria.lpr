program talaria;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, StatusFrame, ChatFrame, PointListFrame, dnmp_unit,
  dnmp_services, Core, DnmpNodeFrame, Misc, DnmpServicesFrame, laz_synapse,
  ConfigFrame, GrpcServiceFrame, MailboxFrame, dnmp_mail;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

