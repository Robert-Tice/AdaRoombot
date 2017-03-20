with System; use System;

package Types is

    type UByte is new Natural range 0 .. 255
      with Size => 8;

    type UByte_Array is array (Integer range <>) of UByte;

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




end Types;
