{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Linker;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Tiger.Common;

type
  //----------------------------------------------------------------------------
  // Common linker types (shared across all object formats)
  //----------------------------------------------------------------------------

  { TLinkerSectionKind - Categories of sections the linker merges }
  TLinkerSectionKind = (
    lskText,
    lskRData,
    lskData,
    lskBSS,
    lskPData,
    lskXData,
    lskOther
  );

  { TLinkerContribution - Tracks where an object's section ended up in merged output }
  TLinkerContribution = record
    ObjectIndex: Integer;
    OrigSectionIndex: Integer;
    MergedOffset: Cardinal;
    MergedSize: Cardinal;
  end;

  { TLinkerResolvedSymbol - A symbol that was successfully resolved }
  TLinkerResolvedSymbol = record
    SymbolName: string;
    SectionKind: TLinkerSectionKind;
    OffsetInMerged: Cardinal;
  end;

  { TLinkerPendingReloc - A relocation that needs final address fixup }
  TLinkerPendingReloc = record
    SectionKind: TLinkerSectionKind;
    OffsetInMerged: Cardinal;
    RelocationType: Word;
    TargetSymbol: string;
    Addend: Int32;
  end;

  //----------------------------------------------------------------------------
  // TTigerLinker - Base linker class with virtual interface
  //----------------------------------------------------------------------------

  { TTigerLinker }
  TTigerLinker = class(TTigerStatusObject)
  public
    constructor Create(); override;
    destructor Destroy(); override;

    //--------------------------------------------------------------------------
    // Input — add files to link
    //--------------------------------------------------------------------------
    procedure AddObjectFile(const APath: string); virtual;
    procedure AddLibraryFile(const APath: string); virtual;
    procedure AddObjectFromMemory(const AData: TBytes; const AName: string); virtual;

    //--------------------------------------------------------------------------
    // Processing — resolve symbols and merge sections
    //--------------------------------------------------------------------------
    procedure Resolve(const ANeededSymbols: TStrings); virtual;

    //--------------------------------------------------------------------------
    // Output — merged section data
    //--------------------------------------------------------------------------
    function GetMergedText(): TBytes; virtual;
    function GetMergedTextSize(): Cardinal; virtual;
    function GetMergedRData(): TBytes; virtual;
    function GetMergedRDataSize(): Cardinal; virtual;
    function GetMergedData(): TBytes; virtual;
    function GetMergedDataSize(): Cardinal; virtual;
    function GetMergedPData(): TBytes; virtual;
    function GetMergedPDataSize(): Cardinal; virtual;
    function GetMergedXData(): TBytes; virtual;
    function GetMergedXDataSize(): Cardinal; virtual;

    //--------------------------------------------------------------------------
    // Symbol info
    //--------------------------------------------------------------------------
    function GetResolvedSymbols(): TDictionary<string, TLinkerResolvedSymbol>; virtual;
    function GetUnresolvedSymbols(): TStringList; virtual;
    function GetPendingRelocations(): TList<TLinkerPendingReloc>; virtual;

    //--------------------------------------------------------------------------
    // Diagnostics
    //--------------------------------------------------------------------------
    function GetObjectCount(): Integer; virtual;
    function GetSelectedObjectCount(): Integer; virtual;
  end;

implementation

{ TTigerLinker }

constructor TTigerLinker.Create();
begin
  inherited;
end;

destructor TTigerLinker.Destroy();
begin
  inherited;
end;

procedure TTigerLinker.AddObjectFile(const APath: string);
begin
  // Base does nothing — override in platform-specific subclass
end;

procedure TTigerLinker.AddLibraryFile(const APath: string);
begin
end;

procedure TTigerLinker.AddObjectFromMemory(const AData: TBytes; const AName: string);
begin
end;

procedure TTigerLinker.Resolve(const ANeededSymbols: TStrings);
begin
end;

function TTigerLinker.GetMergedText(): TBytes;
begin
  SetLength(Result, 0);
end;

function TTigerLinker.GetMergedTextSize(): Cardinal;
begin
  Result := 0;
end;

function TTigerLinker.GetMergedRData(): TBytes;
begin
  SetLength(Result, 0);
end;

function TTigerLinker.GetMergedRDataSize(): Cardinal;
begin
  Result := 0;
end;

function TTigerLinker.GetMergedData(): TBytes;
begin
  SetLength(Result, 0);
end;

function TTigerLinker.GetMergedDataSize(): Cardinal;
begin
  Result := 0;
end;

function TTigerLinker.GetMergedPData(): TBytes;
begin
  SetLength(Result, 0);
end;

function TTigerLinker.GetMergedPDataSize(): Cardinal;
begin
  Result := 0;
end;

function TTigerLinker.GetMergedXData(): TBytes;
begin
  SetLength(Result, 0);
end;

function TTigerLinker.GetMergedXDataSize(): Cardinal;
begin
  Result := 0;
end;

function TTigerLinker.GetResolvedSymbols(): TDictionary<string, TLinkerResolvedSymbol>;
begin
  Result := nil;
end;

function TTigerLinker.GetUnresolvedSymbols(): TStringList;
begin
  Result := nil;
end;

function TTigerLinker.GetPendingRelocations(): TList<TLinkerPendingReloc>;
begin
  Result := nil;
end;

function TTigerLinker.GetObjectCount(): Integer;
begin
  Result := 0;
end;

function TTigerLinker.GetSelectedObjectCount(): Integer;
begin
  Result := 0;
end;

end.
