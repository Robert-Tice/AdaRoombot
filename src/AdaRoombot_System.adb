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
with Mode; use Mode;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Streams;

procedure AdaRoombot_System is

    --    Rx_Data : Sensor_Data (S => Charging_Sources_Avail);
    Port : access Ada.Streams.Root_Stream_Type'Class := Communication_Init;

    function Now return Time
    is
    begin
        return Clock;
    end Now;

begin
    Send_Command (Port => Port,
                  Rec  => Comm_Rec'(Op => Start),
                  Unsafe => True);
--      Read_Mode_From_Target (Port => Port);
--      Send_Command (Port => Port,
--                    Rec  => Comm_Rec'(Op => Mode_Safe));
--      Send_Command (Port => Port,
--                    Rec  => Comm_Rec'(Op => Clean));

    delay until (Now + Seconds (1));

--      Send_Command (Port => Port,
--                    Rec  => Comm_Rec'(Op => Seek_Dock));
--
--      loop
--          Rx_Data := Get_Sensor_Single (Port => Port,
--                                        Pkt  => Charging_Sources_Avail);
--          exit when Rx_Data.Home_Base;
--          delay until (Now + Milliseconds (50));
--      end loop;
--
--      Send_Command (Port => Port,
--                    Rec  => Comm_Rec'(Op => Stop));
    Communications_Close (Port);

end AdaRoombot_System;
