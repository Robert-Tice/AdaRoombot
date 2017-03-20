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

with Ada.Streams;

package Communication is

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

    type Comm_Port is access all Ada.Streams.Root_Stream_Type'Class;
    Port : Comm_Port;


    Default_Baud     : constant Baud_Code := B115200;
    Default_COM_Name : constant String := "ttyUSB0";
    Is_Init          : Boolean := False;

    Configd_Baud    : Baud_Code;
    Configd_COM_Name : String := Default_COM_Name;

    Comm_Uninit_Exception : exception;


    function Communication_Init (BC       : Baud_Code := Default_Baud;
                                 COM_Name : String := Default_COM_Name)
                                 return Comm_Port
      with Post => Is_Init or else raise Comm_Uninit_Exception;

    procedure Clear_Comm_Buffer (Port : in out Comm_Port)
      with Pre => Is_Init or else raise Comm_Uninit_Exception;

    procedure Set_Host_Baud (Port : Comm_Port;
                             BC   : Integer)
      with Pre => Is_Init or else raise Comm_Uninit_Exception;

    procedure Communications_Close (Port : Comm_Port)
      with Post => not Is_Init or else raise Comm_Uninit_Exception;


end Communication;
