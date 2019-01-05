{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit wFrameWork;

{$warn 5023 off : no warning about unused units}
interface

uses
  wfResourceStrings, wfTypes, wfClasses, wfFunc, wfPlugins, wfVersions, 
  wfIBConnection, wfODBCConnection, wfBase, wfSQLQuery, wfSQLScript, 
  wfSQLTransaction, wfTreeView, wfDBGrid, wfComboBox, wfStatusProgressBar, 
  wfReport, wfSettings, wfEntity, wfValueListEditor, TwfProgressU, 
  wfReportViewer, wfDialogs, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('wfPlugins', @wfPlugins.Register);
  RegisterUnit('wfVersions', @wfVersions.Register);
  RegisterUnit('wfIBConnection', @wfIBConnection.Register);
  RegisterUnit('wfODBCConnection', @wfODBCConnection.Register);
  RegisterUnit('wfBase', @wfBase.Register);
  RegisterUnit('wfSQLQuery', @wfSQLQuery.Register);
  RegisterUnit('wfSQLScript', @wfSQLScript.Register);
  RegisterUnit('wfSQLTransaction', @wfSQLTransaction.Register);
  RegisterUnit('wfTreeView', @wfTreeView.Register);
  RegisterUnit('wfDBGrid', @wfDBGrid.Register);
  RegisterUnit('wfComboBox', @wfComboBox.Register);
  RegisterUnit('wfStatusProgressBar', @wfStatusProgressBar.Register);
  RegisterUnit('wfReport', @wfReport.Register);
  RegisterUnit('wfSettings', @wfSettings.Register);
  RegisterUnit('wfEntity', @wfEntity.Register);
  RegisterUnit('wfValueListEditor', @wfValueListEditor.Register);
end;

initialization
  RegisterPackage('wFrameWork', @Register);
end.
