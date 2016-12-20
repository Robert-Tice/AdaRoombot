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

with Commands; use Commands;
with Communication; use Communication;

with Last_Chance_Handler; pragma Unreferenced (Last_Chance_Handler);

package body AdaRoombot_System is

   procedure System_Init is
   begin
      Communication_Init;
      Is_Init := True;
   end System_Init;

   procedure System_Cleanup is
   begin
      Is_Init := False;
   end System_Cleanup;

   procedure System_Loop is
      Rx_Data : Sensor_Data (S => Charging_Sources_Avail);
   begin
      System_Init;
      Send_Command (Comm_Rec'(Op => Start));
      Send_Command (Comm_Rec'(Op => Mode_Safe));
      Send_Command (Comm_Rec'(Op => Clean));
      delay until (Now + Seconds (10));
      Send_Command (Comm_Rec'(Op => Seek_Dock));

      loop
         Rx_Data := Get_Sensor_Single (Charging_Sources_Avail);
         exit when Rx_Data.Home_Base;
         delay until (Now + Milliseconds (50));
      end loop;

      Send_Command (Comm_Rec'(Op => Stop));
      System_Cleanup;
   end System_Loop;

   function Now return Time is
   begin
      return Clock;
   end Now;

end AdaRoombot_System;
