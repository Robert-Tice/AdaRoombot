------------------------------------------------------------------------------
--                              AdaRoombot                                  --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Communication is

    function Communication_Init (Data_Rate : Baud_Code;
                                 Name : String)
                                 return Serial_Port
    is
        Port : Serial_Port;
        Ret : Boolean;
    begin
        Port := new Serial_Port_Inst;
        Ret := Port.Open (Name => Name);

        if not Ret then
            Raise_Error("Unable open file.");
        end if;

        return Port;
    end Communication_Init;

    procedure Free_Serial_Port is new Ada.Unchecked_Deallocation
      (Object => Serial_Port_Inst, Name => Serial_Port);

    procedure Communications_Close (Port : in out Serial_Port)
    is
    begin
        Port.Close;
        Free_Serial_Port (Port);
    end Communications_Close;

    procedure Clear_Comm_Buffer (Port : in Serial_Port)
    is
        Ret : Integer;
        PP : Boolean;
        Buf : UByte_Array (1 .. 256);
    begin
        loop
            PP := Port.Poll;
            exit when PP = False;
            Ret := Port.Read (Buffer => Buf);
        end loop;
    end Clear_Comm_Buffer;

    procedure Send_Command (Port : in Serial_Port;
                            Rec  : in Comm_Rec)
    is
        Raw_TX : UByte_Array (1 .. Rec'Size / 8)
          with Address => Rec'Address;
        Ret    : Integer;
    begin
        Ret := Port.Write (Buffer => Raw_TX);
    end Send_Command;

    procedure Send_Command (Port : in Serial_Port;
                            Rec  : in Comm_Rec;
                            Data : in UByte_Array)
    is
        Raw_TX : UByte_Array (1 .. Rec'Size / 8)
          with Address => Rec'Address;
        Ret    : Integer;
    begin
        Ret := Port.Write (Buffer => Raw_TX & Data);
    end Send_Command;

    procedure Read_Sensors (Port   : in Serial_Port;
                            Buffer : out UByte_Array)
    is
        Ret_Poll : Boolean;
        Ret_Read : Integer;
    begin
        Ret_Poll := Port.Poll;
        Ret_Read := Port.Read (Buffer => Buffer);
    end Read_Sensors;

    function Open (Self  : in out Serial_Port_Inst;
                   Name  : in String)
                   return Boolean
    is
        CName     : constant String := Name & ASCII.NUL;
        Name_Addr : constant C_File_Name := CName (CName'First)'Address;
    begin
        Self.Fd := File_Descriptor (C_Open (Pathname => Name_Addr,
                                            Flags    => C.Int(Self.Flags)));
        if Self.Fd = Invalid_FD then
            return False;
        end if;
        return True;
    end Open;

    procedure Close (Self : in out Serial_Port_Inst)
    is
    begin
        GNAT.OS_Lib.Close (FD => Self.Fd);
        Self.Fd := 0;
    end Close;

    function Read (Self   : in Serial_Port_Inst;
                   Buffer : out UByte_Array)
                   return Integer
    is
        Ret : Integer;
    begin
        Ret := GNAT.OS_Lib.Read (FD => Self.Fd,
                                 A  => Buffer'Address,
                                 N  => Buffer'Length);
        if Ret = -1 then
            Raise_Error ("Read failed.");
        end if;

        return Ret;
    end Read;

    function Write (Self : in Serial_Port_Inst;
                    Buffer : in UByte_Array)
                    return Integer
    is
        Ret : Integer;
    begin
        Ret := GNAT.OS_Lib.Write (FD => Self.Fd,
                                  A  => Buffer'Address,
                                  N  => Buffer'Length);
        if Ret = -1 then
            Raise_Error ("Write failed.");
        end if;

        return Ret;
    end Write;

    function Poll (Self : in Serial_Port_Inst;
                   Seconds : Natural := 0)
                   return Boolean
    is
        Readfds : aliased Fd_Set;
        Timeout : aliased Timeval := (others => 0);
        Timeout_Acc : access Timeval := null;
        Ret : C.int;
    begin
        if Seconds > 0 then
            Timeout.Tv_Sec := C.Int (Seconds);
            Timeout_Acc := Timeout'Access;
        end if;

        loop
            Readfds := 2 ** Natural (Self.Fd);
            Ret := C_Select (Nfds      => C.Int(Self.Fd) + 1,
                             Readfds   => Readfds'Access,
                             Writefds  => null,
                             Exceptfds => null,
                             Timeout   => Timeout_Acc);
            exit when Ret >= 0;
        end loop;

        if Ret = 0 then
            return False;
        end if;

        return True;
    end Poll;

    Serial_Error : exception;

    procedure Raise_Error (Message : String;
                           Error   : Integer := Errno)
    is
    begin
        raise Serial_Error with Message
          & (if Error /= 0
             then " (" & Errno_Message (Err => Error) & ')'
             else "");
    end Raise_Error;

end Communication;
