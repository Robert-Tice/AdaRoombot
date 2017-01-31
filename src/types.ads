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

    type Velocity_Type is new Integer range -500 .. 500
      with Size => 16;

    type Radius_Type is new Integer range -32768 .. 32767
      with Static_Predicate => Radius_Type in -2000 .. 2000 | -32768 | 32767,
      Size => 16;

    type Distance_Type is new Integer range -32768 .. 32767
      with Size => 16;

    type Voltage_Type is new Natural range 0 .. 65535
      with Size => 16;

    type Current_Type is new Integer range -32768 .. 32767
      with Size => 16;

    type Temperature is new Integer range -128 .. 127
      with Size => 8;

    type Charge_Type is new Natural range 0 .. 65535
      with Size => 16;

    type Sensor_Wall_Signal_Type is new Natural range 0 .. 1023
      with Size => 16;

    type Sensor_Cliff_Signal_Type is new Natural range 0 .. 4095
      with Size => 16;

    type Sensor_Song_Number is new Natural range 0 .. 15
      with Size => 8;

    type Encoder_Counts_Type is new Integer range -32768 .. 32767
      with Size => 16;

    type Light_Bump_Signal_Type is new Natural range 0 .. 4095
      with Size => 16;

    subtype Unpacked_Boolean is Boolean
      with Value_Size => 8;

    type Velocity is record
        Data : Velocity_Type := 0;
    end record
      with Size => 16,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    type Radius is record
        Data : Radius_Type := 0;
    end record
      with Size => 16,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    type Distance is record
        Data : Distance_Type := 0;
    end record
      with Size => 16,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    type Voltage is record
        Data : Voltage_Type := 0;
    end record
      with Size => 16,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    type Current is record
        Data : Current_Type := 0;
    end record
      with Size => 16,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    type Charge is record
        Data : Charge_Type := 0;
    end record
      with Size => 16,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    type Sensor_Wall_Signal is record
        Data : Sensor_Wall_Signal_Type := 0;
    end record
      with Size => 16,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    type Sensor_Cliff_Signal is record
        Data : Sensor_Cliff_Signal_Type := 0;
    end record
      with Size => 16,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    type Encoder_Counts is record
        Data : Encoder_Counts_Type := 0;
    end record
      with Size => 16,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    type Light_Bump_Signal is record
        Data : Light_Bump_Signal_Type := 0;
    end record
      with Size => 16,
      Scalar_Storage_Order => High_Order_First,
      Bit_Order => High_Order_First;

    type Sensor_Bumps_And_Wheel_Drops is record
        Bump_Right           : Boolean;
        Bump_Left            : Boolean;
        Wheel_Drop_Right     : Boolean;
        Wheel_Drop_Left      : Boolean;
    end record
      with Size => 8;

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
      with Size => 8;

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
      with Size => 8;

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
      with Size => 8;

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
      with Size => 8;

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
      with Size => 8;

    for Sensor_Stasis use record
        Stasis_Toggling at 0 range 0 .. 0;
        Stasis_Disabled at 0 range 1 .. 1;
    end record;

    type Sensor_Collection is record
        Bumps_And_Wheel_Drops          : Sensor_Bumps_And_Wheel_Drops;
        Wall                           : Unpacked_Boolean;
        Cliff_Left                     : Unpacked_Boolean;
        Cliff_Front_Left               : Unpacked_Boolean;
        Cliff_Front_Right              : Unpacked_Boolean;
        Cliff_Right                    : Unpacked_Boolean;
        Virtual_Wall                   : Unpacked_Boolean;
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
        Song_Playing                   : Unpacked_Boolean;
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
      with Alignment => 1;





end Types;
