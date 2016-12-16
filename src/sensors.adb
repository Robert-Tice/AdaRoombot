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
with Sensors; use Sensors;

package body Sensors is

   function Get_Sensor_Single(Pkt : Sensor_Packets) return Serial_Payload is
      Rec : Comm_Rec(Sensors);
      Ret : Serial_Payload(0 .. 0);
   begin
      Send_Command(Rec);
      -- TODO: listen for response
      return Ret;
   end Get_Sensor_Single;

   function Get_Sensor_List (List : Sensor_Array) return Serial_Payload is
      Rec : Comm_Rec(Query_List);
      Ret : Serial_Payload(1 .. List'Length);
      Payload : Serial_Payload (1 .. List'Size / 8)
	with Address => List'Address;
   begin
      Send_Command(Rec, Payload);
      -- TODO: listen for response
      return Ret;
   end Get_Sensor_List;


end Sensors;
