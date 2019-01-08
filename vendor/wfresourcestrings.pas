{
This file is part of wfFrameWork.

 -= Vendor/ResourceStrings =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfResourceStrings;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils;

  resourcestring
    rsExceptErrorDefault                              = '%s';
    rsExceptObjectNotAssigned                         = 'Object % not assigned!';

    {-= Base =-}
    rsExceptErrorSQLParams                            = 'Error SQL Params!';
    rsExceptErrorFindFieldName                        = 'Error find field Name %s !';
    rsExceptErrorLongTransactionIsActive              = 'Long Transaction is active!';
    rsExceptErrorDatabaseEngineIsNotAvailable         = 'This database engine is not available!';
    rsMessageCreatedDataBaseSucefull                  = 'Created DataBase Sucefull';
    rsMessageCreatedDataBaseInterrupted               = 'Database creation was interrupted!';
    rsMessageConectedSucefull                         = 'Conected Sucefull';
    rsMessageDisconnectSucefull                       = 'Disconnect Sucefull';
    rsWarningNumberOfEntriesWasLimited                = 'Error! The number of entries exceeded the maximum (%d)';

    {-= DBTree =-}
    rsDBTreeTextCreateNodeName                        = 'New Node';
    rsDBTreeTextNodePromt                             = 'Enter the name of the node';
    rsDBTreeTextCreateNodeCaption                     = 'Create a new node';
    rsDBTreeTextEditNodeCaption                       = 'Changing the node';
    rsDBTreeTextDeleteNodeCaption                     = 'Remove node';
    rsDBTreeTextDeleteNodePromt                       = 'Delete node(s) " %s " and all child nodes?';

    rsDBTreeErrorMovingNode                           = 'Error moving node!';
    rsDBTreeErrorProcedureIsExists                    = 'Proceduries to %s is exists!';
    rsDBTreeErrorProcedureIsMissing                   = 'Proceduries to %s is missing!';
    rsDBTreeErrorRootTreeNotFound                     = 'The root of the tree is not found!';
    rsDBTreeErrorItemsIsEmpty                         = 'Items is empty!';
    rsDBTreeErrorNodeIsEmpty                          = 'Node is empty!';
    rsDBTreeErrorParentID                             = 'Error Parent ID (%d)!';
    rsDBTreeMessageProceduresSuccessfullyCreated      = 'Procedures successfully created!';
    rsDBTreeMessageproceduresSuccessfullyRemoved      = 'The procedures successfully removed!';

    rsPopupMenuAdd                                    = 'Add Item';
    rsPopupMenuEdit                                   = 'Edit Item';
    rsPopupMenuDel                                    = 'Del Item';

    {-= DBGrid =-}
    rsDBGridErrorMultiselectOff                       = 'Multiselect state is off!';
    rsDBGridMessageDataSelection                      = 'Data selection... Please wait';

    {-= Entity =-}
    rsEntityErrorNoFoundTreeOrGrid                    = 'Object % not assigned! Use Init!';

    {-= Report =-}
    rsReportNoFileNameSpecified                       = 'No file name specified!';
    rsReportNoSQLQuerySpecified                       = 'No SQLQuery specified!';

    {-= Plugins =-}
    rsPluginsNoFindedPanel                            = 'Not found TPanel on %s';
    rsPluginPopupMenuToForm                           = 'Unpin';
    rsPluginPopupMenuClose                            = 'Close';

implementation

end.

