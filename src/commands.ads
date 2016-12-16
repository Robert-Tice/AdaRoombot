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

package Commands is

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

   type Day is
     (Sunday,
      Monday,
      Tuesday,
      Wednesday,
      Thursday,
      Friday,
      Saturday);

   type Hour is new Integer range 0 .. 23;
   type Minute is new Integer range 0 .. 59;

   type Velocity is new Integer range -500 .. 500;
   type Radius is new Integer range -32768 .. 32767
     with Static_Predicate => Radius in -2000 .. 2000 | -32768 | 32767;

   type Drive_Special is
     (Straight,
      CW,
      CCW);

   type Hour_Min is record
      Hr : Hour;
      Min : Minute;
   end record
     with Pack;

   for Hour_Min use record
      Hr at 0 range 0 .. 7;
      Min at 1 range 0 .. 7;
   end record;

   type Hour_Min_List is array (Day) of Hour_Min
     with Pack;

   type Opcode is
     (
      Reset,
      Start,
      Baud,
      Mode_Safe,
      Mode_Full,
      Power,
      Spot_Clean,
      Clean,
      Max_Clean,
      Drive,
      Motors,
      LEDs,
      Song,
      Play,
      Sensors,
      Seek_Dock,
      PWM_Motors,
      Drive_Direct,
      Drive_PWM,
      Stream,
      Query_List,
      Pause_Resume_Stream,
      Scheduling_LEDs,
      Digital_LEDs_Raw,
      Digital_LEDs_ASCII,
      Buttons,
      Schedule,
      Set_Day_Time,
      Stop);

   -- Representation clause
   for Opcode use
     (Reset => 7,
      Start => 128,
      Baud => 129,
      Mode_Safe => 131,
      Mode_Full => 132,
      Power => 133,
      Spot_Clean =>134,
      Clean => 135,
      Max_Clean => 136,
      Drive => 137,
      Motors=> 138,
      LEDs => 139,
      Song => 140,
      Play => 141,
      Sensors => 142,
      Seek_Dock => 143,
      PWM_Motors => 144,
      Drive_Direct => 145,
      Drive_PWM => 146,
      Stream => 148,
      Query_List => 149,
      Pause_Resume_Stream => 150,
      Scheduling_LEDs => 162,
      Digital_LEDs_RAW => 163,
      Digital_LEDs_ASCII => 164,
      Buttons => 165,
      Schedule => 167,
      Set_Day_Time => 168,
      Stop => 173);


   type Packet_ID is new Integer range 0 .. 128
     with Static_Predicate => Packet_ID in 0 .. 58 | 100 | 101 | 106 | 107, Size => 8;

   type Packet_Array is array (Integer range <>) of Packet_ID
     with Pack;

   type Midi_Note is new Integer range 0 .. 127
     with Static_Predicate => Midi_Note in 0 | 31 .. 127, Size => 8;

   type Midi_Tone is record
      Note : Midi_Note;
      Duration : Integer range 0 .. 255;
   end record;

   for Midi_Tone use record
      Note at 1 range 0 .. 7;
      Duration at 2 range 0 .. 7;
   end record;

   type Midi_Song is array (Integer range 1 .. 16) of Midi_Tone
     with Pack;


   type Comm_Rec(Op: Opcode) is record
	 case Op is
	 when Baud =>
	    Baud_Rate : Integer range 0 .. 11;
	 when Drive =>
	    Vel : Velocity;
	    Rad: Radius;
	 when Motors =>
	    Side_Brush : Boolean;
	    Vacuum : Boolean;
	    Main_Brush : Boolean;
	    Side_Brush_CW : Boolean;
	    Main_Brush_Dir : Boolean;
	 when LEDs =>
	    Debris : Boolean;
	    Spot : Boolean;
	    Dock : Boolean;
	    Check_Robot : Boolean;
	    Power_Color : Integer range 0 .. 255;
	    Power_Intensity : Integer range 0 .. 255;
	 when Song =>
	    Set_Song_Number : Integer range 0 .. 4;
	    Song_Length : Integer range 1 .. 16;
	 when Play =>
	    Play_Song_Number : Integer range 0 .. 4;
	 when Sensors =>
	    Sensor_Packet_ID : Packet_ID;
	 when PWM_Motors =>
	    Main_Brush_PWM : Integer range -127 .. 127;
	    Side_Brush_PWM : Integer range -127 .. 127;
	    Vacuum_PWM : Integer range 0 .. 127;
	 when Drive_Direct =>
	    Right_Velocity : Velocity;
	    Left_Velocity : Velocity;
	 when Drive_PWM =>
	    Right_PWM : Integer range -255 .. 255;
	    Left_PWM : Integer range -255 .. 255;
	 when Stream =>
	    Num_Stream_Packets : Integer range 0 .. 255;
	 when Query_List =>
	    Num_Query_Packets : Integer range 0 .. 255;
	 when Pause_Resume_Stream =>
	    Stream_State : Boolean;
	 when Scheduling_LEDs =>
	    Sun_LED : Boolean;
	    Mon_LED : Boolean;
	    Tues_LED : Boolean;
	    Wed_LED : Boolean;
	    Thurs_LED : Boolean;
	    Fri_LED : Boolean;
	    Sat_LED : Boolean;
	    Colon_LED : Boolean;
	    PM_LED : Boolean;
	    AM_LED : Boolean;
	    Clock_LED : Boolean;
	    Schedule_LED : Boolean;
	 when Digital_LEDs_Raw =>
	    A3_Seg : Boolean;
	    B3_Seg : Boolean;
	    C3_Seg : Boolean;
	    D3_Seg : Boolean;
	    E3_Seg : Boolean;
	    F3_Seg : Boolean;
	    G3_Seg : Boolean;
	    A2_Seg : Boolean;
	    B2_Seg : Boolean;
	    C2_Seg : Boolean;
	    D2_Seg : Boolean;
	    E2_Seg : Boolean;
	    F2_Seg : Boolean;
	    G2_Seg : Boolean;
	    A1_Seg : Boolean;
	    B1_Seg : Boolean;
	    C1_Seg : Boolean;
	    D1_Seg : Boolean;
	    E1_Seg : Boolean;
	    F1_Seg : Boolean;
	    G1_Seg : Boolean;
	    A0_Seg : Boolean;
	    B0_Seg : Boolean;
	    C0_Seg : Boolean;
	    D0_Seg : Boolean;
	    E0_Seg : Boolean;
	    F0_Seg : Boolean;
	    G0_Seg : Boolean;
	 when Digital_LEDs_ASCII =>
	    Digital_3_ASCII : Integer range 32 .. 126;
	    Digital_2_ASCII : Integer range 32 .. 126;
	    Digital_1_ASCII : Integer range 32 .. 126;
	    Digital_0_ASCII : Integer range 32 .. 126;
	 when Buttons =>
	    Clean_Button : Boolean;
	    Spot_Button : Boolean;
	    Dock_Button : Boolean;
	    Minute_Button : Boolean;
	    Hour_Button : Boolean;
	    Day_button : Boolean;
	    Schedule_Button : Boolean;
	    Clock_Button : Boolean;
	 when Schedule =>
	    Days : Integer range 0 .. 127;
	    HM_List : Hour_Min_List;
	 when Set_Day_Time =>
	    Dy : Integer range 0 .. 6;
	    Hr : Hour;
	    Min : Minute;
	 when others =>
	    null;
	 end case;
   end record;

   -- Representation clause
   for Comm_Rec use record
      Op at 0 range 0 .. 7;
      -- Baud Fields
      Baud_Rate at 1 range 0 .. 7;
      -- Motors Fields
      Side_Brush at 1 range 0 .. 0;
      Vacuum at 1 range 1 .. 1;
      Main_Brush at 1 range 2 .. 2;
      Side_Brush_CW at 1 range 3 .. 3;
      Main_Brush_Dir at 1 range 4 .. 4;
      -- LEDs Fields
      Debris at 1 range 0 .. 0;
      Spot at 1 range 1 .. 1;
      Dock at 1 range 2 .. 2;
      Check_Robot at 1 range 3 .. 3;
      Power_Color at 2 range 0 .. 7;
      Power_Intensity at 3 range 0 .. 7;
      -- Song Fields
      Set_Song_Number at 1 range 0 .. 7;
      Song_Length at 2 range 0 .. 7;
      -- Play Fields
      Play_Song_Number at 1 range 0 .. 7;
      -- Sensors Fields
      Sensor_Packet_ID at 1 range 0 .. 7;
      -- PWM Motors Fields
      Main_Brush_PWM at 1 range 0 .. 7;
      Side_Brush_PWM at 2 range 0 .. 7;
      Vacuum_PWM at 3 range 0 .. 7;
      -- Direct Drive Fields
      Right_Velocity at 1 range 0 .. 15;
      Left_Velocity at 3 range 0 .. 15;
      -- Drive PWM Fields
      Right_PWM at 1 range 0 .. 15;
      Left_PWM at 3 range 0 .. 15;
      -- Query List Fields
      Num_Query_Packets at 1 range 0 .. 7;
      -- Stream Fields
      Num_Stream_Packets at 1 range 0 .. 7;
      -- Pause Resume Stream
      Stream_State at 1 range 0 .. 0;
      --Scheduling LEDs
      Sun_LED at 1 range 0 .. 0;
      Mon_LED at 1 range 1 .. 1;
      Tues_LED at 1 range 2 .. 2;
      Wed_LED at 1 range 3 .. 3;
      Thurs_LED at 1 range 4 .. 4;
      Fri_LED at 1 range 5 .. 5;
      Sat_LED at 1 range 6 .. 6;
      Colon_LED at 2 range 0 .. 0;
      PM_LED at 2 range 1 .. 1;
      AM_LED at 2 range 2 .. 2;
      Clock_LED at 2 range 3 .. 3;
      Schedule_LED at 2 range 4 .. 4;
      -- Digital LEDs Raw Fields
      A3_Seg at 1 range 0 .. 0;
      B3_Seg at 1 range 1 .. 1;
      C3_Seg at 1 range 2 .. 2;
      D3_Seg at 1 range 3 .. 3;
      E3_Seg at 1 range 4 .. 4;
      F3_Seg at 1 range 5 .. 5;
      G3_Seg at 1 range 6 .. 6;
      A2_Seg at 2 range 0 .. 0;
      B2_Seg at 2 range 1 .. 1;
      C2_Seg at 2 range 2 .. 2;
      D2_Seg at 2 range 3 .. 3;
      E2_Seg at 2 range 4 .. 4;
      F2_Seg at 2 range 5 .. 5;
      G2_Seg at 2 range 6 .. 6;
      A1_Seg at 3 range 0 .. 0;
      B1_Seg at 3 range 1 .. 1;
      C1_Seg at 3 range 2 .. 2;
      D1_Seg at 3 range 3 .. 3;
      E1_Seg at 3 range 4 .. 4;
      F1_Seg at 3 range 5 .. 5;
      G1_Seg at 3 range 6 .. 6;
      A0_Seg at 4 range 0 .. 0;
      B0_Seg at 4 range 1 .. 1;
      C0_Seg at 4 range 2 .. 2;
      D0_Seg at 4 range 3 .. 3;
      E0_Seg at 4 range 4 .. 4;
      F0_Seg at 4 range 5 .. 5;
      G0_Seg at 4 range 6 .. 6;
      -- Digital LEDs ASCII Fields
      Digital_3_ASCII at 1 range 0 .. 7;
      Digital_2_ASCII at 2 range 0 .. 7;
      Digital_1_ASCII at 3 range 0 .. 7;
      Digital_0_ASCII at 4 range 0 .. 7;
      -- Button Fields
      Clean_Button at 1 range 0 .. 0;
      Spot_Button at 1 range 1 .. 1;
      Dock_Button at 1 range 2 .. 2;
      Minute_Button at 1 range 3 .. 3;
      Hour_Button at 1 range 4 .. 4;
      Day_Button at 1 range 5 .. 5;
      Schedule_Button at 1 range 6 .. 6;
      Clock_Button at 1 range 7 .. 7;
      -- Schedule Fields
      Days at 1 range 0 .. 7;
      HM_List at 2 range 0 .. Hour_Min_List'Length * 8 - 1;
      -- Set_Day_Time Fields
      Dy at 1 range 0 .. 7;
      Hr at 2 range 0 .. 7;
      Min at 3 range 0 .. 7;
      -- Drive Fields
      Vel at 1 range 0 .. 15;
      Rad at 3 range 0 .. 15;
   end record;

    -- Command Constructors
   function Construct_Baud (BC : Baud_Code) return Comm_Rec;
   function Construct_Date_Time (D : Day; H : Hour; M : Minute) return Comm_Rec;
   function Construct_Drive_Special (Special : Drive_Special) return Comm_Rec;

   procedure Send_Command (Rec : Comm_Rec);
   procedure Send_Command (Rec : Comm_Rec; Data : Serial_Payload);

private
   function Check_Valid_Mode(Op : Opcode) return Boolean;
   procedure Command_Post(Op : Opcode);
end Commands;
