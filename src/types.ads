with System; use System;

package Types is

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

    type UByte is new Natural range 0 .. 255
      with Size => 8;

    type UByte_Array is array (Positive range <>) of UByte;

    type Day is
      (Sunday, Monday, Tuesday, Wednesday,
       Thursday, Friday, Saturday);

    type Hour is new Integer range 0 .. 23;
    type Minute is new Integer range 0 .. 59;

    type Velocity is new Integer range -500 .. 500
      with Size => 16;

    type Radius is new Integer range -32768 .. 32767
      with Static_Predicate => Radius in -2000 .. 2000 | -32768 | 32767,
      Size => 16;

    type Distance is new Integer range -32768 .. 32767
      with Size => 16;

    type Voltage is new Natural range 0 .. 65535
      with Size => 16;

    type Current is new Integer range -32768 .. 32767
      with Size => 16;

    type Temperature is new Integer range -128 .. 127
      with Size => 8;

    type Charge is new Natural range 0 .. 65535
      with Size => 16;

    type Sensor_Wall_Signal is new Natural range 0 .. 1023
      with Size => 16;

    type Sensor_Cliff_Signal is new Natural range 0 .. 4095
      with Size => 16;

    type Sensor_Song_Number is new Natural range 0 .. 15
      with Size => 8;

    type Encoder_Counts is new Integer range -32768 .. 32767
      with Size => 16;

    type Light_Bump_Signal is new Natural range 0 .. 4095
      with Size => 16;

    type Sensor_Bumps_And_Wheel_Drops is record
        Bump_Right           : Boolean;
        Bump_Left            : Boolean;
        Wheel_Drop_Right     : Boolean;
        Wheel_Drop_Left      : Boolean;
    end record
      with Size => 8,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    for Sensor_Bumps_And_Wheel_Drops use record
        Bump_Right at 0 range 0 .. 0;
        Bump_Left at 0 range 1 .. 1;
        Wheel_Drop_Right at 0 range 2 .. 2;
        Wheel_Drop_Left at 0 range 3 .. 3;
    end record;

    type Sensor_Wheel_Overcurrents is record
        Side_Brush_OC        : Boolean;
        Main_Brush_OC        : Boolean;
        Right_Wheel_OC       : Boolean;
        Left_Wheel_OC        : Boolean;
    end record
      with Size => 8,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    for Sensor_Wheel_Overcurrents use record
        Side_Brush_OC at 0 range 0 .. 0;
        Main_Brush_OC at 0 range 2 .. 2;
        Right_Wheel_OC at 0 range 3 .. 3;
        Left_Wheel_OC at 0 range 4 .. 4;
    end record;

    type Sensor_Buttons is record
        Clean_But            : Boolean;
        Spot_But             : Boolean;
        Dock_But             : Boolean;
        Minute_But           : Boolean;
        Hour_But             : Boolean;
        Day_But              : Boolean;
        Schedule_But         : Boolean;
        Clock_But            : Boolean;
    end record
      with Size => 8,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    for Sensor_Buttons use record
        Clean_But at 0 range 0 .. 0;
        Spot_But at 0 range 1 .. 1;
        Dock_But at 0 range 2 .. 2;
        Minute_But at 0 range 3 .. 3;
        Hour_But at 0 range 4 .. 4;
        Day_But at 0 range 5 .. 5;
        Schedule_But at 0 range 6 .. 6;
        Clock_But at 0 range 7 .. 7;
    end record;

    type Sensors_Charging_State is
      (Not_Charging,
       Reconditiong_Charging,
       Full_Charging,
       Trickle_Charging,
       Waiting,
       Charging_Fault_Condition)
      with Size => 8;

    type Sensor_Charging_Sources_Available is record
        Internal_Charger     : Boolean;
        Home_Base            : Boolean;
    end record
      with Size => 8,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    for Sensor_Charging_Sources_Available use record
        Internal_Charger at 0 range 0 .. 0;
        Home_Base at 0 range 1 .. 1;
    end record;

    type Sensor_OI_Mode is
      (Off, Passive, Safe, Full)
      with Size => 8;

    type Sensor_Light_Bumper is record
        LT_Bump_Left         : Boolean;
        LT_Bump_Front_Left   : Boolean;
        LT_Bump_Center_Left  : Boolean;
        LT_Bump_Center_Right : Boolean;
        LT_Bump_Front_Right  : Boolean;
        LT_Bump_Right        : Boolean;
    end record
      with Size => 8,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    for Sensor_Light_Bumper use record
        LT_Bump_Left at 0 range 0 .. 0;
        LT_Bump_Front_Left at 0 range 1 .. 1;
        LT_Bump_Center_Left at 0 range 2 .. 2;
        LT_Bump_Center_Right at 0 range 3 .. 3;
        LT_Bump_Front_Right at 0 range 4 .. 4;
        LT_Bump_Right at 0 range 5 .. 5;
    end record;

    type Sensor_Stasis is record
        Stasis_Toggling      : Boolean;
        Stasis_Disabled      : Boolean;
    end record
      with Size => 8,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    for Sensor_Stasis use record
        Stasis_Toggling at 0 range 0 .. 0;
        Stasis_Disabled at 0 range 1 .. 1;
    end record;

    type Sensor_Collection is record
        Bumps_And_Wheel_Drops          : Sensor_Bumps_And_Wheel_Drops;
        Wall                           : Boolean;
        Cliff_Left                     : Boolean;
        Cliff_Front_Left               : Boolean;
        Cliff_Front_Right              : Boolean;
        Cliff_Right                    : Boolean;
        Virtual_Wall                   : Boolean;
        Wheel_Overcurrents             : Sensor_Wheel_Overcurrents;
        Dirt_Detect                    : UByte;
        Unused1                        : UByte;
        IR_Char_Omni                   : Character;
        Buttons                        : Sensor_Buttons;
        Dis                            : Distance;
        Ang                            : Radius;
        Charging_State                 : Sensors_Charging_State;
        Volt                           : Voltage;
        Cur                            : Current;
        Temp                           : Temperature;
        Batt_Charge                    : Charge;
        Batt_Cap                       : Charge;
        Wall_Sig                       : Sensor_Wall_Signal;
        Cliff_Left_Sig                 : Sensor_Cliff_Signal;
        Cliff_Front_Left_Sig           : Sensor_Cliff_Signal;
        Cliff_Front_Right_Sig          : Sensor_Cliff_Signal;
        Cliff_Right_Sig                : Sensor_Cliff_Signal;
        Unused2                        : UByte_Array (1 .. 3);
        Charging_Sources_Available     : Sensor_Charging_Sources_Available;
        OI_Mode                        : Sensor_OI_Mode;
        Song_Number                    : Sensor_Song_Number;
        Song_Playing                   : Boolean;
        Number_Of_Stream_Packets       : UByte;
        Requested_Velocity             : Velocity;
        Requested_Radius               : Radius;
        Requested_Right_Velocity       : Velocity;
        Requested_Left_Velocity        : Velocity;
        Left_Encoder_Counts            : Encoder_Counts;
        Right_Encoder_Counts           : Encoder_Counts;
        Light_Bumper                   : Sensor_Light_Bumper;
        Light_Bump_Left_Signal         : Light_Bump_Signal;
        Light_Bump_Front_Left_Signal   : Light_Bump_Signal;
        Light_Bump_Center_Left_Signal  : Light_Bump_Signal;
        Light_Bump_Center_Right_Signal : Light_Bump_Signal;
        Light_Bump_Front_Right_Signal  : Light_Bump_Signal;
        Light_Bump_Right_Signal        : Light_Bump_Signal;
        IR_Character_Left              : Character;
        IR_Character_Right             : Character;
        Left_Motor_Current             : Current;
        Right_Motor_Current            : Current;
        Main_Motor_Current             : Current;
        Side_Brush_Motor_Current       : Current;
        Stasis                         : Sensor_Stasis;
    end record
      with Alignment => 1,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;


    for Sensor_Collection use record
        Bumps_And_Wheel_Drops at 0 range 0 .. 7;
        Wall at 1 range 0 .. 7;
        Cliff_Left at 2 range 0 .. 7;
        Cliff_Front_Left at 3 range 0 .. 7;
        Cliff_Front_Right at 4 range 0 .. 7;
        Cliff_Right at 5 range 0 .. 7;
        Virtual_Wall at 6 range 0 .. 7;
        Wheel_Overcurrents at 7 range 0 .. 7;
        Dirt_Detect at 8 range 0 .. 7;
        Unused1 at 9 range 0 .. 7;
        IR_Char_Omni at 10 range 0 .. 7;
        Buttons at 11 range 0 .. 7;
        Dis at 12 range 0 .. 15;
        Ang at 14 range 0 .. 15;
        Charging_State at 16 range 0 .. 7;
        Volt at 17 range 0 .. 15;
        Cur at 19 range 0 .. 15;
        Temp at 21 range 0 .. 7;
        Batt_Charge at 22 range 0 .. 15;
        Batt_Cap at 24 range 0 .. 15;
        Wall_Sig at 26 range 0 .. 15;
        Cliff_Left_Sig at 28 range 0 .. 15;
        Cliff_Front_Left_Sig at 30 range 0 .. 15;
        Cliff_Front_Right_Sig at 32 range 0 .. 15;
        Cliff_Right_Sig at 34 range 0 .. 15;
        Unused2 at 36 range 0 .. 23;
        Charging_Sources_Available at 39 range 0 .. 7;
        OI_Mode at 40 range 0 .. 7;
        Song_Number at 41 range 0 .. 7;
        Song_Playing at 42 range 0 .. 7;
        Number_Of_Stream_Packets at 43 range 0 .. 7;
        Requested_Velocity at 44 range 0 .. 15;
        Requested_Radius at 46 range 0 .. 15;
        Requested_Right_Velocity at 48 range 0 .. 15;
        Requested_Left_Velocity at 50 range 0 .. 15;
        Left_Encoder_Counts at 52 range 0 .. 15;
        Right_Encoder_Counts at 54 range 0 .. 15;
        Light_Bumper at 56 range 0 .. 7;
        Light_Bump_Left_Signal at 57 range 0 .. 15;
        Light_Bump_Front_Left_Signal at 59 range 0 .. 15;
        Light_Bump_Center_Left_Signal at 61 range 0 .. 15;
        Light_Bump_Center_Right_Signal at 63 range 0 .. 15;
        Light_Bump_Front_Right_Signal at 65 range 0 .. 15;
        Light_Bump_Right_Signal at 67 range 0 .. 15;
        IR_Character_Left at 69 range 0 .. 7;
        IR_Character_Right at 70 range 0 .. 7;
        Left_Motor_Current at 71 range 0 .. 15;
        Right_Motor_Current at 73 range 0 .. 15;
        Main_Motor_Current at 75 range 0 .. 15;
        Side_Brush_Motor_Current at 77 range 0 .. 15;
        Stasis at 79 range 0 .. 7;
    end record;

    type Hour_Min is record
        Hr  : Hour;
        Min : Minute;
    end record;

    for Hour_Min use record
        Hr at 0 range 0 .. 7;
        Min at 1 range 0 .. 7;
    end record;

    type Hour_Min_List is array (Day) of Hour_Min
      with Pack;

    type Opcode is
      (Reset,
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
       Sensors_Single,
       Seek_Dock,
       PWM_Motors,
       Drive_Direct,
       Drive_PWM,
       Sensors_Stream,
       Sensors_List,
       Pause_Resume_Stream,
       Scheduling_LEDs,
       Digital_LEDs_Raw,
       Digital_LEDs_ASCII,
       Buttons,
       Schedule,
       Set_Day_Time,
       Stop);

    --  Representation clause
    for Opcode use
      (Reset               => 7,
       Start               => 128,
       Baud                => 129,
       Mode_Safe           => 131,
       Mode_Full           => 132,
       Power               => 133,
       Spot_Clean          => 134,
       Clean               => 135,
       Max_Clean           => 136,
       Drive               => 137,
       Motors              => 138,
       LEDs                => 139,
       Song                => 140,
       Play                => 141,
       Sensors_Single      => 142,
       Seek_Dock           => 143,
       PWM_Motors          => 144,
       Drive_Direct        => 145,
       Drive_PWM           => 146,
       Sensors_Stream      => 148,
       Sensors_List        => 149,
       Pause_Resume_Stream => 150,
       Scheduling_LEDs     => 162,
       Digital_LEDs_Raw    => 163,
       Digital_LEDs_ASCII  => 164,
       Buttons             => 165,
       Schedule            => 167,
       Set_Day_Time        => 168,
       Stop                => 173);

    type Midi_Note is new Integer range 0 .. 127
      with Static_Predicate => Midi_Note in 0 | 31 .. 127, Size => 8;

    type Midi_Tone is record
        Note     : Midi_Note;
        Duration : Integer range 0 .. 255;
    end record;

    for Midi_Tone use record
        Note at 1 range 0 .. 7;
        Duration at 2 range 0 .. 7;
    end record;

    type Midi_Song is array (Integer range 1 .. 16) of Midi_Tone
      with Pack;

    type Sensor_Packets is
      (Bumps_And_Wheel_Drops,
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
       Distance_Sensor,
       Ang,
       Charging_State,
       Voltage_Sensor,
       Current_Sensor,
       Temperature_Sensor,
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
       Stasis);

    for Sensor_Packets use
      (Bumps_And_Wheel_Drops          => 7,
       Wall                           => 8,
       Cliff_Left                     => 9,
       Cliff_Front_Left               => 10,
       Cliff_Front_Right              => 11,
       Cliff_Right                    => 12,
       Virtual_Wall                   => 13,
       Wheel_Overcurrent              => 14,
       Dirt_Detect                    => 15,
       IR_Char_Omni                   => 17,
       Buttons                        => 18,
       Distance_Sensor                => 19,
       Ang                            => 20,
       Charging_State                 => 21,
       Voltage_Sensor                 => 22,
       Current_Sensor                 => 23,
       Temperature_Sensor             => 24,
       Battery_Charge                 => 25,
       Battery_Capacity               => 26,
       Wall_Signal                    => 27,
       Cliff_Left_Signal              => 28,
       Cliff_Front_Left_Signal        => 29,
       Cliff_Front_Right_Signal       => 30,
       Cliff_Right_Signal             => 31,
       Charging_Sources_Avail         => 34,
       OI_Mode                        => 35,
       Song_Number                    => 36,
       Song_Playing                   => 37,
       Number_Stream_Packets          => 38,
       Req_Velocity                   => 39,
       Req_Radius                     => 40,
       Req_Right_Velocity             => 41,
       Req_Left_Velocity              => 42,
       Left_Encoder_Counts            => 43,
       Right_Encoder_Counts           => 44,
       Light_Bumper                   => 45,
       Light_Bump_Left_Signal         => 46,
       Light_Bump_Front_Left_Signal   => 47,
       Light_Bump_Center_Left_Signal  => 48,
       Light_Bump_Center_Right_Signal => 49,
       Light_Bump_Front_Right_Signal  => 50,
       Light_Bump_Right_Signal        => 51,
       IR_Char_Left                   => 52,
       IR_Char_Right                  => 53,
       Left_Motor_Current             => 54,
       Right_Motor_Current            => 55,
       Main_Brush_Motor_Current       => 56,
       Side_Brush_Motor_Current       => 57,
       Stasis                         => 58);

    type Comm_Rec (Op : Opcode) is record
        case Op is
        when Baud =>
            Baud_Rate          : Integer range 0 .. 11;
        when Drive =>
            Vel                : Velocity;
            Rad                : Radius;
        when Motors =>
            Side_Brush         : Boolean;
            Vacuum             : Boolean;
            Main_Brush         : Boolean;
            Side_Brush_CW      : Boolean;
            Main_Brush_Dir     : Boolean;
        when LEDs =>
            Debris             : Boolean;
            Spot               : Boolean;
            Dock               : Boolean;
            Check_Robot        : Boolean;
            Power_Color        : Integer range 0 .. 255;
            Power_Intensity    : Integer range 0 .. 255;
        when Song =>
            Set_Song_Number    : Integer range 0 .. 4;
            Song_Length        : Integer range 1 .. 16;
        when Play =>
            Play_Song_Number   : Integer range 0 .. 4;
        when Sensors_Single =>
            Sensor_Packet_ID   : Sensor_Packets;
        when PWM_Motors =>
            Main_Brush_PWM     : Integer range -127 .. 127;
            Side_Brush_PWM     : Integer range -127 .. 127;
            Vacuum_PWM         : Integer range 0 .. 127;
        when Drive_Direct =>
            Right_Velocity     : Velocity;
            Left_Velocity      : Velocity;
        when Drive_PWM =>
            Right_PWM          : Integer range -255 .. 255;
            Left_PWM           : Integer range -255 .. 255;
        when Sensors_Stream =>
            Num_Stream_Packets : Integer range 0 .. 255;
        when Sensors_List =>
            Num_Query_Packets  : Integer range 0 .. 255;
        when Pause_Resume_Stream =>
            Stream_State       : Boolean;
        when Scheduling_LEDs =>
            Sun_LED            : Boolean;
            Mon_LED            : Boolean;
            Tues_LED           : Boolean;
            Wed_LED            : Boolean;
            Thurs_LED          : Boolean;
            Fri_LED            : Boolean;
            Sat_LED            : Boolean;
            Colon_LED          : Boolean;
            PM_LED             : Boolean;
            AM_LED             : Boolean;
            Clock_LED          : Boolean;
            Schedule_LED       : Boolean;
        when Digital_LEDs_Raw =>
            A3_Seg             : Boolean;
            B3_Seg             : Boolean;
            C3_Seg             : Boolean;
            D3_Seg             : Boolean;
            E3_Seg             : Boolean;
            F3_Seg             : Boolean;
            G3_Seg             : Boolean;
            A2_Seg             : Boolean;
            B2_Seg             : Boolean;
            C2_Seg             : Boolean;
            D2_Seg             : Boolean;
            E2_Seg             : Boolean;
            F2_Seg             : Boolean;
            G2_Seg             : Boolean;
            A1_Seg             : Boolean;
            B1_Seg             : Boolean;
            C1_Seg             : Boolean;
            D1_Seg             : Boolean;
            E1_Seg             : Boolean;
            F1_Seg             : Boolean;
            G1_Seg             : Boolean;
            A0_Seg             : Boolean;
            B0_Seg             : Boolean;
            C0_Seg             : Boolean;
            D0_Seg             : Boolean;
            E0_Seg             : Boolean;
            F0_Seg             : Boolean;
            G0_Seg             : Boolean;
        when Digital_LEDs_ASCII =>
            Text               : String (1 .. 4);
        when Buttons =>
            Clean_Button       : Boolean;
            Spot_Button        : Boolean;
            Dock_Button        : Boolean;
            Minute_Button      : Boolean;
            Hour_Button        : Boolean;
            Day_Button         : Boolean;
            Schedule_Button    : Boolean;
            Clock_Button       : Boolean;
        when Schedule =>
            Days               : Integer range 0 .. 127;
            HM_List            : Hour_Min_List;
        when Set_Day_Time =>
            Dy                 : Integer range 0 .. 6;
            Hr                 : Hour;
            Min                : Minute;
        when others =>
            null;
        end case;
    end record
      with Pack, Alignment => 1;

    --  Representation clause
    for Comm_Rec use record
        Op at 0 range 0 .. 7;
        --  Baud Fields
        Baud_Rate at 1 range 0 .. 7;
        --  Motors Fields
        Side_Brush at 1 range 0 .. 0;
        Vacuum at 1 range 1 .. 1;
        Main_Brush at 1 range 2 .. 2;
        Side_Brush_CW at 1 range 3 .. 3;
        Main_Brush_Dir at 1 range 4 .. 4;
        --  LEDs Fields
        Debris at 1 range 0 .. 0;
        Spot at 1 range 1 .. 1;
        Dock at 1 range 2 .. 2;
        Check_Robot at 1 range 3 .. 3;
        Power_Color at 2 range 0 .. 7;
        Power_Intensity at 3 range 0 .. 7;
        --  Song Fields
        Set_Song_Number at 1 range 0 .. 7;
        Song_Length at 2 range 0 .. 7;
        --  Play Fields
        Play_Song_Number at 1 range 0 .. 7;
        --  Sensors Fields
        Sensor_Packet_ID at 1 range 0 .. 7;
        --  PWM Motors Fields
        Main_Brush_PWM at 1 range 0 .. 7;
        Side_Brush_PWM at 2 range 0 .. 7;
        Vacuum_PWM at 3 range 0 .. 7;
        --  Direct Drive Fields
        Right_Velocity at 1 range 0 .. 15;
        Left_Velocity at 3 range 0 .. 15;
        --  Drive PWM Fields
        Right_PWM at 1 range 0 .. 15;
        Left_PWM at 3 range 0 .. 15;
        --  Query List Fields
        Num_Query_Packets at 1 range 0 .. 7;
        --  Stream Fields
        Num_Stream_Packets at 1 range 0 .. 7;
        --  Pause Resume Stream
        Stream_State at 1 range 0 .. 0;
        --  Scheduling LEDs
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
        --  Digital LEDs Raw Fields
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
        --  Digital LEDs ASCII Fields
        Text at 1 range 0 .. 31;
        --  Button Fields
        Clean_Button at 1 range 0 .. 0;
        Spot_Button at 1 range 1 .. 1;
        Dock_Button at 1 range 2 .. 2;
        Minute_Button at 1 range 3 .. 3;
        Hour_Button at 1 range 4 .. 4;
        Day_Button at 1 range 5 .. 5;
        Schedule_Button at 1 range 6 .. 6;
        Clock_Button at 1 range 7 .. 7;
        --  Schedule Fields
        Days at 1 range 0 .. 7;
        HM_List at 2 range 0 .. Hour_Min_List'Length * 16 - 1;
        --  Set_Day_Time Fields
        Dy at 1 range 0 .. 7;
        Hr at 2 range 0 .. 7;
        Min at 3 range 0 .. 7;
        --  Drive Fields
        Vel at 1 range 0 .. 15;
        Rad at 3 range 0 .. 15;
    end record;

end Types;
