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

with Mode; use Mode;

with Ada.Streams; use Ada.Streams;

package body Commands is

    Invalid_Mode_Exception : exception;

    function Check_Valid_Mode (Op : Opcode) return Boolean
    is
        Md    : Interface_Mode;
        Valid : Boolean;
    begin
        Md := Get_Mode;
        case Op is
        when Start | Stop | Baud | Mode_Safe | Mode_Full | Clean | Max_Clean |
             Spot_Clean | Seek_Dock | Power | Schedule | Set_Day_Time | Buttons |
             Song | Sensors_Single | Sensors_List | Sensors_Stream |
             Pause_Resume_Stream =>
            case Md is
            when Passive | Safe | Full =>
                Valid := True;
            when others =>
                Valid := False;
            end case;
        when Drive | Drive_Direct | Drive_PWM | Motors | PWM_Motors |
             LEDs | Scheduling_LEDs | Digital_LEDs_Raw | Digital_LEDs_ASCII |
             Play =>
            case Md is
            when Safe | Full =>
                Valid := True;
            when others =>
                Valid := False;
            end case;
        when others =>
            Valid := True;
        end case;

        return Valid;
    end Check_Valid_Mode;

    procedure Command_Post (Op : Opcode)
    is
    begin
        case Op is
        when Start | Clean | Max_Clean | Spot_Clean | Seek_Dock | Power =>
            Effect_Mode_Changed (Passive);
        when Reset | Stop =>
            Effect_Mode_Changed (Off);
        when Mode_Safe =>
            Effect_Mode_Changed (Safe);
        when Mode_Full =>
            Effect_Mode_Changed (Full);
        when others =>
            null;
        end case;
    end Command_Post;

    procedure Send_Command (Port : access Ada.Streams.Root_Stream_Type'Class;
                            Rec  : Comm_Rec;
                            Unsafe : Boolean := False)
    is
        Raw_TX : Ada.Streams.Stream_Element_Array (1 .. Rec'Size / 8)
          with Address => Rec'Address;
    begin
        if not Unsafe then
            if not Check_Valid_Mode (Rec.Op) then
                raise Invalid_Mode_Exception;
                return;
            end if;
        end if;

        --          Comm_Rec'Output (Port, Rec);
        Write (Stream => Port.all,
               Item   => Raw_TX);

        Command_Post (Rec.Op);
    end Send_Command;

    procedure Send_Command (Port : access Ada.Streams.Root_Stream_Type'Class;
                            Rec  : Comm_Rec;
                            Data : Stream_Element_Array;
                            Unsafe : Boolean := False)
    is
    begin
        if not Unsafe then
            if not Check_Valid_Mode (Rec.Op) then
                raise Invalid_Mode_Exception;
                return;
            end if;
        end if;

        Comm_Rec'Output (Port, Rec);
        Write (Stream => Port.all,
               Item   => Data);

        Command_Post (Rec.Op);
    end Send_Command;

    function Construct_Baud (BC : Baud_Code) return Comm_Rec
    is
        SData : Comm_Rec (Baud);
    begin
        SData.Baud_Rate := Baud_Code'Pos (BC);
        return SData;
    end Construct_Baud;

    function Construct_Date_Time (D : Day; H : Hour; M : Minute) return Comm_Rec
    is
        SData : Comm_Rec (Set_Day_Time);
    begin
        SData.Dy := Day'Pos (D);
        SData.Hr := H;
        SData.Min := M;
        return SData;
    end Construct_Date_Time;

    function Construct_Drive_Special (Special : Drive_Special)
                                      return Comm_Rec
    is
        SData : Comm_Rec (Drive);
    begin
        case Special is
        when Straight =>
            SData.Rad := 32767;
        when CW =>
            SData.Rad := -1;
        when CCW =>
            SData.Rad := 1;
        end case;
        return SData;
    end Construct_Drive_Special;

    function Sensor_Data_Size_Bytes (SD : Sensor_Data) return Integer
    is
    begin
        return ((SD'Size / 8));
    end Sensor_Data_Size_Bytes;

    function Get_Sensor_Single (Port : access Ada.Streams.Root_Stream_Type'Class;
                                Pkt  : Sensor_Packets;
                                Skip : Boolean := False)
                                return Sensor_Data
    is
        Sensor : Sensor_Data (Pkt);
        Comm   : Comm_Rec (Op => Sensors_Single);
--          RX_Raw    : Ada.Streams.Stream_Element_Array (1 .. Sensor'Size / 8)
--           with Address => Sensor'Address;
        Last : Stream_Element_Offset := 0;
    begin
        if not Skip then
            if not Check_Valid_Mode (Op => Sensors_Single) then
                raise Invalid_Mode_Exception;
            end if;
        end if;

        Comm.Sensor_Packet_ID := Pkt;

        Comm_Rec'Output (Port, Comm);
        Sensor_Data'Read (Port, Sensor);

--          Read (Stream => Port.all,
--                Item   => RX_Raw,
--                Last   => Last);

        return Sensor;
    end Get_Sensor_Single;

    --     function Get_Sensor_List (List : Sensor_Array) return Serial_Payload is
    --        Ret : Serial_Payload (1 .. List'Length);
    --        Payload : Serial_Payload (1 .. List'Size / 8)
    --          with Address => List'Address;
    --     begin
    --        Send_Command (Comm_Rec'(Op => Query_List), Payload);
    --        Ret := Serial_RX (Ret'Length);
    --        return Ret;
    --     end Get_Sensor_List;

end Commands;
