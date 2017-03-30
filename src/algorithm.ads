with Types; use Types;

with Communication; use Communication;


package Algorithm is
    
    type Algorithm_Type is
      (Pong);
    
    Safety_Exception : exception;
    
    Default_Velocity : Velocity := 320;
    
    type Abstract_Algorithm is abstract tagged record
        null;
    end record;
    
    type Algorithm_Ptr is access Abstract_Algorithm'Class;
    
    procedure Process (Self    : in out Abstract_Algorithm;
                       Port    : in Serial_Port;
                       Sensors : in Sensor_Collection) is abstract;
    
    procedure Safety_Check (Self    : in Abstract_Algorithm;
                            Sensors : in Sensor_Collection) is abstract;
    
    type Algorithm_State is
      (Drive,
       Passive_Driving,
       Collision,
       Recover,
       Passive_Recover);
    
    type Pong_Algorithm is new Abstract_Algorithm with record
        State          : Algorithm_State := Drive;
        Collision      : Boolean := False;
        Reported_Angle : Radius;
        Last_Turn      : Boolean := False;
    end record;
    
    procedure Process (Self    : in out Pong_Algorithm;
                       Port    : in Serial_Port;
                       Sensors : in Sensor_Collection);
    
    function Detect_Collision (Self    : in Pong_Algorithm;
                               Sensors : in Sensor_Collection) 
                               return Boolean;
    
    procedure Safety_Check (Self    : in Pong_Algorithm;
                            Sensors : in Sensor_Collection);
    

end Algorithm;
