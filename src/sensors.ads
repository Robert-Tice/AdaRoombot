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

package Sensors is

   type Sensor_Packets is
     (Everything,
      Bumps_and_Wheel_Drops,
      Wall,
      Cliff_Left,
      Cliff_Front_Left,
      Cliff_Front_Right,
      Cliff_Right,
      Virtual_Wall,
      Wheel_Overcurrent,
      Dirt_Detect,
      IR_Char_Omni,
      Buttons,
      Distance,
      Angle,
      Charging_State,
      Voltage,
      Current,
      Temperature,
      Battery_Charge,
      Battery_Capacity,
      Wall_Signal,
      Cliff_Left_Signal,
      Cliff_Front_Left_Signal,
      Cliff_Front_Right_Signal,
      Cliff_Right_Signal,
      Charging_Sources_Avail,
      OI_Mode,
      Song_Number,
      Song_Playing,
      Number_Stream_Packets,
      Req_Velocity,
      Req_Radius,
      Req_Right_Velocity,
      Req_Left_Velocity,
      Left_Encoder_Counts,
      Right_Encoder_Counts,
      Light_Bumper,
      Light_Bump_Left_Signal,
      Light_Bump_Front_Left_Signal,
      Light_Bump_Center_Left_Signal,
      Light_Bump_Center_Right_Signal,
      Light_Bump_Front_Right_Signal,
      Light_Bump_Right_Signal,
      IR_Char_Left,
      IR_Char_Right,
      Left_Motor_Current,
      Right_Motor_Current,
      Main_Brush_Motor_Current,
      Side_Brush_Motor_Current,
      Stasis) with Size=>8;

   type Sensor_Array is array (Integer range <>) of Sensor_Packets;

   function Get_Sensor_Single (Pkt : Sensor_Packets) return Serial_Payload;
   function Get_Sensor_List (List : Sensor_Array) return Serial_Payload;
   -- TODO: figure out sensor stream definiton

   for Sensor_Packets use
     (Everything => 6,
      Bumps_and_Wheel_Drops => 7,
      Wall => 8,
      Cliff_Left => 9,
      Cliff_Front_Left => 10,
      Cliff_Front_Right => 11,
      Cliff_Right => 12,
      Virtual_Wall => 13,
      Wheel_Overcurrent => 14,
      Dirt_Detect => 15,
      IR_Char_Omni => 17,
      Buttons => 18,
      Distance => 19,
      Angle => 20,
      Charging_State => 21,
      Voltage => 22,
      Current => 23,
      Temperature => 24,
      Battery_Charge => 25,
      Battery_Capacity => 26,
      Wall_Signal => 27,
      Cliff_Left_Signal => 28,
      Cliff_Front_Left_Signal => 29,
      Cliff_Front_Right_Signal => 30,
      Cliff_Right_Signal => 31,
      Charging_Sources_Avail => 34,
      OI_Mode => 35,
      Song_Number => 36,
      Song_Playing => 37,
      Number_Stream_Packets => 38,
      Req_Velocity => 39,
      Req_Radius => 40,
      Req_Right_Velocity => 41,
      Req_Left_Velocity => 42,
      Left_Encoder_Counts => 43,
      Right_Encoder_Counts => 44,
      Light_Bumper => 45,
      Light_Bump_Left_Signal => 46,
      Light_Bump_Front_Left_Signal => 47,
      Light_Bump_Center_Left_Signal => 48,
      Light_Bump_Center_Right_Signal => 49,
      Light_Bump_Front_Right_Signal => 50,
      Light_Bump_Right_Signal => 51,
      IR_Char_Left => 52,
      IR_Char_Right => 53,
      Left_Motor_Current => 54,
      Right_Motor_Current => 55,
      Main_Brush_Motor_Current => 56,
      Side_Brush_Motor_Current => 57,
      Stasis => 58);

end Sensors;
