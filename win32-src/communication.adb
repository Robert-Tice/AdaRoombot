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

with Communication; use Communication;

with Ada.Containers; use Ada.Containers;
with Ada.Streams; use Ada.Streams;

with GNAT.Serial_Communications; use GNAT.Serial_Communications;

package body Communication is

   Parity : constant Parity_Check := None;
   Bits : constant Data_Bits := CS8;
   End_Bits : constant Stop_Bits_Number := One;
   Control : constant Flow_Control := None;

   COM_Num : constant Natural := 7;
   G_Port : Serial_Port;

   procedure Communication_Init (BC : Integer := Default_Baud) is
      PName : constant Port_Name := Name (Number => COM_Num);
      Rate : Data_Rate;
   begin
      Open(Port => G_Port,
           Name => PName);
      case BC is
         when 75 =>
            Rate := B75;
         when 110 =>
            Rate := B110;
         when 150 =>
            Rate := B150;
         when 300 =>
            Rate := B300;
         when 600 =>
            Rate := B600;
         when 1200 =>
            Rate := B1200;
         when 2400 =>
            Rate := B2400;
         when 4800 =>
            Rate := B4800;
         when 9600 =>
            Rate := B9600;
         when 19200 =>
            Rate := B19200;
         when 38400 =>
            Rate := B38400;
         when 57600 =>
            Rate := B57600;
         when others =>
            Rate := B115200;
      end case;

      Set(Port => G_Port,
          Rate => Rate,
          Bits => Bits,
          Stop_Bits => End_Bits,
          Parity => Parity,
          Flow => Control);
      Is_Init := True;
   end Communication_Init;

   procedure Serial_TX (Payload : Serial_Payload) is
      Buffer : Stream_Element_Array(1 .. Payload'Size / 8)
        with Address => Payload'Address;
   begin
      Write(Port => G_Port,
            Buffer => Buffer);
   end Serial_TX;

   function Serial_RX (Msg_Size : Integer) return Serial_Payload is
      Ret : Serial_Payload(1 .. Msg_Size);
      Buffer : Stream_Element_Array(1 .. Ret'Size / 8)
        with Address => Ret'Address;
      Index : Stream_Element_Offset := 0;
   begin
      while Integer(Index) < Msg_Size loop
         Read(Port => G_Port,
              Buffer => Buffer,
              Last => Index);
      end loop;
      return Ret;
   end Serial_RX;

   procedure Set_Host_Baud (BC : Integer) is
   begin
      null;
   end Set_Host_Baud;

   procedure Communications_Close is
   begin
      GNAT.Serial_Communications.Close(Port => G_Port);
      Is_Init := False;
   end Communications_Close;

end Communication;
