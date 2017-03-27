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

with Types; use Types;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Interfaces.C; use Interfaces.C;

package Communication is

    package C renames Interfaces.C;

    use type C.Int;

    type Baud_Code is
      (B300,
       B600,
       B1200,
       B2400,
       B4800,
       B9600,
       B14400,
       B19200,
       B28800,
       B38400,
       B57600,
       B115200);

    O_RDONLY : constant Integer := 16#0000#;
    O_WRONLY : constant Integer := 16#0001#;
    O_RDWR : constant Integer := 16#00002#;
    O_ACCMODE : constant Integer := 16#0003#;

    O_NOCTTY : constant Integer := 16#0000#;

    type Serial_Port_Inst is tagged record
        Fd    : File_Descriptor := 0;
        Flags : Integer := O_RDWR + O_NOCTTY;
    end record;

    function Open (Self : in out Serial_Port_Inst;
                   Name : String)
                   return Boolean;

    procedure Close (Self : in out Serial_Port_Inst);

    function Read (Self   : Serial_Port_Inst;
                   Buffer : out UByte_Array)
                   return Integer;

    function Write (Self   : Serial_Port_Inst;
                    Buffer : in UByte_Array)
                    return Integer;

    function Poll (Self : Serial_Port_Inst;
                   Seconds: Natural := 0)
                   return Boolean;

    type Serial_Port is access all Serial_Port_Inst;


    function Communication_Init (Data_Rate : Baud_Code;
                                 Name      : String)
                                 return Serial_Port
      with Post => Communication_Init'Result /= null;

    procedure Clear_Comm_Buffer (Port : Serial_Port)
      with Pre => Port /= null;

    procedure Communications_Close (Port : in out Serial_Port)
      with Pre => Port /= null,
      Post => Port = null;

private
    procedure Raise_Error (Message : String;
                           Error   : Integer := Errno);
    pragma No_Return (Raise_Error);

    function C_Open (Pathname : C_File_Name;
                     Flags    : C.Int)
                     return C.Int;
    pragma Import (C, C_Open, "open");

    type Fd_Set is mod 2 ** 32;
    pragma Convention (C, Fd_Set);

    type Timeval is record
        Tv_Sec : C.Int;
        Tv_Usec : C.Int;
    end record;
    pragma Convention (C, Timeval);

    function C_Select (Nfds : C.Int;
                       Readfds : access Fd_Set;
                       Writefds : access Fd_Set;
                       Exceptfds : access Fd_Set;
                       Timeout   : access Timeval)
                       return C.int;
    pragma Import (C, C_Select, "select");


end Communication;
