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


package body Communication is
    G_Port : aliased Serial_Port_Inst;

    function Communication_Init (Data_Rate : Baud_Code;
                                 Name : String)
                                 return Serial_Port
    is
        Ret : Boolean;
    begin
        Ret := G_Port.Open (Name => Name);

        if not Ret then
            Raise_Error("Unable open file.");
        end if;

        return G_Port'Access;
    end Communication_Init;

    procedure Communications_Close (Port : in out Serial_Port)
    is
    begin
        Port.Close;
        Port := null;
    end Communications_Close;

    procedure Clear_Comm_Buffer (Port : Serial_Port)
    is
        PP : Boolean;
        Buf : UByte_Array (1 .. 256);
    begin
        loop
            PP := Port.Poll;
            exit when PP = False;
            Port.Read (Buffer => Buf);
        end loop;
    end Clear_Comm_Buffer;

    function Open (Self  : Serial_Port_Inst;
                   Name  : constant String)
                   return Boolean;
    is
        CName     : constant String := Name & ASCII.NUL;
        Name_Addr : constant Chars := CName (CName'First)'Address;
    begin
        Self.Fd := File_Descriptor (Open (Filename => Name_Addr,
                                          Oflag    => Self.Flags));
        if Self.Fd = Invalid_FD then
            return False;
        end if;
        return True;
    end File_Open;

    procedure Close (Self : Serial_Port_Inst)
    is
    begin
        Close (Fd => C.int (Self.Fd));
        Self.Fd := 0;
    end Serial_Close;

    function Read (Self   : Serial_Port_Inst;
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

    function Write (Self : Serial_Port_Inst;
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

    function Poll (Self : Serial_Port_Inst;
                   Seconds : Natural := 0)
                   return Boolean
    is
        Readfds : Fd_Arr;
        Timeout : Timeval := (others => 0);
        Timeout_Acc : access Timeval := null;
        Ret : C.int;
    begin
        if Seconds > 0 then
            Timeout.Tv_Sec := C.Int (Seconds);
            Timeout_Acc := Timeout'Access;
        end if;

        loop
            FD_ZERO (Set => access Readfds);
            FD_SET (Fd  => Self.Fd;
                    Set => access Readfds);
            Ret := C_Select (Nfds      => Self.Fd + 1,
                             Readfds   => access Readfds,
                             Writefds  => Null,
                             Exceptfds => Null,
                             Timeout   => Timeout_Acc);
            exit when Ret /= -1 and then Errno /= EINTR;
        end loop;

        if Ret > 0 then
            if FD_ISSET (Fd => Self.Fd;
                         Set => access Readfds) > 0 then
                return True;
            else
                return False;
            end if;
        end if;

        Raise_Error ("Select failed.");
        return False;

    end Poll;

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
