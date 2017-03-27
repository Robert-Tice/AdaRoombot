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


package body Commands is

    Invalid_Mode_Exception : exception;

    procedure Send_Command (Port : Serial_Port;
                            Rec  : Comm_Rec)
    is
        Raw_TX : UByte_Array (1 .. Rec'Size / 8)
          with Address => Rec'Address;
        Ret : Integer;
    begin
        Ret := Port.Write (Buffer => Raw_TX);
    end Send_Command;

    procedure Send_Command (Port : Serial_Port;
                            Rec  : Comm_Rec;
                            Data : UByte_Array)
    is
        Raw_TX : UByte_Array (1 .. Rec'Size / 8)
          with Address => Rec'Address;
        Ret : Integer;
    begin
        Ret := Port.Write (Buffer => Raw_TX & Data);
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

    function Construct_Drive_Special (Special : Drive_Special;
                                      V       : Velocity)
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
        SData.Vel := V;
        return SData;
    end Construct_Drive_Special;


end Commands;
