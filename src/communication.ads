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



package Communication is

   Default_Baud : constant Integer := 115_200;
   Is_Init : Boolean := False;
   Baud : Integer := Default_Baud;

   type Serial_Byte is new Integer range 0 .. 255
     with Size => 8;
   type Serial_Payload is array (Integer range <>) of Serial_Byte
     with Pack;

   Comm_Uninit_Exception : exception;

   procedure Communication_Init (BC : Integer := Default_Baud)
     with Post => Is_Init or else raise Comm_Uninit_Exception;

   procedure Serial_TX (Payload : Serial_Payload)
     with Pre => Is_Init or else raise Comm_Uninit_Exception;

   function Serial_RX (Msg_Size : Integer) return Serial_Payload
     with Pre => Is_Init or else raise Comm_Uninit_Exception;

   procedure Set_Host_Baud (BC : Integer)
     with Pre => Is_Init or else raise Comm_Uninit_Exception;

private

   procedure Await_Send_Ready with Inline;
   procedure Await_Data_Available with Inline;
end Communication;
