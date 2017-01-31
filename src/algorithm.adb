with Ada.Text_IO; use Ada.Text_IO;

package body Algorithm is

    procedure Process (Self : in out Pong_Algorithm)
    is
    begin 
        case Self.State is
            when Drive =>
                Send_Command (Port   => Self.Port,
                              Rec    => Construct_Drive_Special (Special => Straight,
                                                                 V       => Default_Velocity));
                Self.State := Passive_Driving;
            when Passive_Driving =>
                if Self.Detect_Collision then
                    Self.State := Collision;
                end if;
            when Collision =>
                Send_Command (Port   => Self.Port,
                              Rec    => Construct_Drive_Special (Special => Straight,
                                                                 V       => Velocity'(Data => 0)));
                Self.State := Recover;
            when Recover =>
                case Self.Last_Turn is
                    when False =>
                        Send_Command (Port   => Self.Port,
                                      Rec    => Construct_Drive_Special 
                                        (Special => CW,
                                         V       => Velocity'(Data => 250)));
                    when True =>
                        Send_Command (Port   => Self.Port,
                                      Rec    => Construct_Drive_Special 
                                        (Special => CCW,
                                         V       => Velocity'(Data => 250)));
                end case;
                Self.Last_Turn := not Self.Last_Turn;
                        
                Self.State := Passive_Recover;
                Self.Reported_Angle.Data := 0;
            when Passive_Recover =>
                Self.Reported_Angle.Data := Self.Reported_Angle.Data + 
                  abs Self.Sensors.Ang.Data;
                if Self.Reported_Angle.Data >= 180 then
                  Self.State := Drive;
                end if;                       
        end case;
    end Process;
    
    function Detect_Collision (Self : in Pong_Algorithm) return Boolean
    is
        Ret : Boolean := False;
    begin
        if Self.Sensors.Bumps_And_Wheel_Drops.Bump_Right or else 
          Self.Sensors.Bumps_And_Wheel_Drops.Bump_Left or else
          Self.Sensors.Wall or else
          Self.Sensors.Virtual_Wall or else
          Self.Sensors.Light_Bumper.LT_Bump_Left or else
          Self.Sensors.Light_Bumper.LT_Bump_Front_Left or else
          Self.Sensors.Light_Bumper.LT_Bump_Center_Left or else
          Self.Sensors.Light_Bumper.LT_Bump_Center_Right or else
          Self.Sensors.Light_Bumper.LT_Bump_Front_Right or else
          Self.Sensors.Light_Bumper.LT_Bump_Right 
        then
            Ret := True;
          end if;
        return Ret;
    end Detect_Collision;
    
    procedure Safety_Check (Self : in Pong_Algorithm)
    is
    begin
        if Self.Sensors.Bumps_And_Wheel_Drops.Wheel_Drop_Right or else
          Self.Sensors.Bumps_And_Wheel_Drops.Wheel_Drop_Left or else
          Self.Sensors.Cliff_Left or else
          Self.Sensors.Cliff_Front_Left or else
          Self.Sensors.Cliff_Front_Right or else
          Self.Sensors.Cliff_Right or else
          Self.Sensors.Wheel_Overcurrents.Side_Brush_OC or else
          Self.Sensors.Wheel_Overcurrents.Main_Brush_OC or else
          Self.Sensors.Wheel_Overcurrents.Right_Wheel_OC or else
          Self.Sensors.Wheel_Overcurrents.Left_Wheel_OC
        then 
            raise Safety_Exception with "Stopping robot.";
        end if;
    end Safety_Check;
          
    
end Algorithm;
