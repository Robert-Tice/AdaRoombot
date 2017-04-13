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
with System;
with System.OS_Constants;

package Communication is

    package C renames Interfaces.C;

    use type C.Int;

    type Serial_Port_Inst is private;
    type Serial_Port is access all Serial_Port_Inst;

    function Communication_Init (Data_Rate : Baud_Code;
                                 Name      : String)
                                 return Serial_Port
      with Post => Communication_Init'Result /= null;

    procedure Clear_Comm_Buffer (Port : in Serial_Port)
      with Pre => Port /= null;

    procedure Communications_Close (Port : in out Serial_Port)
      with Pre => Port /= null,
      Post => Port = null;

    procedure Send_Command (Port : in Serial_Port;
                            Rec  : in Comm_Rec)
      with Pre => Port /= null;

    procedure Send_Command (Port : in Serial_Port;
                            Rec  : in Comm_Rec;
                            Data : in UByte_Array)
      with Pre => Port /= null;

    procedure Read_Sensors (Port   : in Serial_Port;
                            Buffer : out UByte_Array)
      with Pre => Port /= null;

private

    O_RDONLY  : constant Integer := 16#0000#;
    O_WRONLY  : constant Integer := 16#0001#;
    O_RDWR    : constant Integer := 16#00002#;
    O_ACCMODE : constant Integer := 16#0003#;

    O_NOCTTY : constant Integer := 16#0000#;

    type Serial_Port_Inst is tagged record
        Fd    : File_Descriptor := 0;
        Flags : Integer := O_RDWR + O_NOCTTY;
    end record;

    procedure Open (Self : in out Serial_Port_Inst;
                   Name : in String;
                   Data_Rate : Baud_Code);

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

    type Termios is record
        C_Iflag : C.Unsigned;
        C_Oflag : C.Unsigned;
        C_Cflag : C.Unsigned;
        C_Lflag : C.Unsigned;
        C_Line : C.Unsigned_Char;
        C_Cc : C.Char_Array (0 .. 31);
        C_Ispeed : C.Unsigned;
        C_Ospeed : C.Unsigned;
    end record;
    pragma Convention (C, Termios);

    function C_Tcgetattr (Fildes : C.Int;
                             Termios_P : System.Address)
                             return C.int;
    pragma Import (C, C_Tcgetattr, "tcgetattr");

    function C_Cfsetispeed (Termios_P : System.Address;
                            Speed     : C.Unsigned)
                            return C.Unsigned;
    pragma Import (C, C_Cfsetispeed, "cfsetispeed");

    function C_Cfsetospeed (Termios_P : System.Address;
                            Speed     : C.Unsigned)
                            return C.Unsigned;
    pragma Import (C, C_Cfsetospeed, "cfsetospeed");

    function C_Tcsetattr (Fildes : C.Int;
                          Optional_Actions : C.Int;
                          Termios_P        : SYSTEM.aDDRESS)
                          return C.Int;
    pragma Import (C, C_Tcsetattr, "tcsetattr");

    C_Data_Rate : constant array (Baud_Code) of C.Unsigned :=
                    (B300 => System.OS_Constants.B300,
                     B600 => System.OS_Constants.B600,
                     B1200 => System.OS_Constants.B1200,
                     B2400 => System.OS_Constants.B2400,
                     B4800 => System.OS_Constants.B4800,
                     B9600 => System.OS_Constants.B9600,
                     B19200 => System.OS_Constants.B19200,
                     B38400 => System.OS_Constants.B38400,
                     B57600 => System.OS_Constants.B57600,
                     B115200 => System.OS_Constants.B115200);

    function C_Tcflush (Fd : C.Int;
                        Queue_Selector : C.Int)
                        return C.Int;
    pragma Import (C, C_Tcflush, "tcflush");


end Communication;
