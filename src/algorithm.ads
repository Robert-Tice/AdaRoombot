with Types; use Types;

with Commands; use Commands;
with Communication; use Communication;


package Algorithm is
    
    Safety_Exception : exception;
    
    Default_Velocity : Velocity := 320;
    
    Sensors_Private : aliased Sensor_Collection;
    
    type Abstract_Algorithm is abstract tagged record
        Sensors : access Sensor_Collection;
    end record;
    
    procedure Process (Self : in out Abstract_Algorithm) is abstract;
    procedure Safety_Check (Self : in Abstract_Algorithm) is abstract;
    
    type Algorithm_State is
      (Drive,
       Passive_Driving,
       Collision,
       Recover,
       Passive_Recover);
    
    type Pong_Algorithm is new Abstract_Algorithm with record
        State          : Algorithm_State := Drive;
        Collision      : Boolean := False;
        Port           : Comm_Port;
        Reported_Angle : Radius;
        Last_Turn      : Boolean := False;
    end record;
    
    procedure Process (Self : in out Pong_Algorithm);
    
    function Detect_Collision (Self : in Pong_Algorithm) return Boolean;
    
    procedure Safety_Check (Self : in Pong_Algorithm);
    

end Algorithm;
